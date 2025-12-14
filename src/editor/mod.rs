//! The main control flow and functionality of the `ad` editor.
use crate::{
    LogBuffer,
    buffer::{ActionOutcome, Buffer, BufferId, WELCOME_SQUIRREL},
    config::Config,
    config_handle, die,
    dot::TextObject,
    exec::{Addr, Address},
    fsys::{AdFs, LogEvent, Message, Req},
    input::Event,
    key::{Arrow, Input},
    lsp::{LspManager, LspManagerHandle},
    mode::{Mode, modes},
    plumb::PlumbingRules,
    system::{DefaultSystem, System},
    term::CurShape,
    ui::{Layout, SCRATCH_ID, StateChange, Ui, UserInterface},
};
use ad_event::Source;
use std::{
    env, fmt, panic,
    path::{Path, PathBuf},
    sync::{
        Arc, RwLock,
        mpsc::{Receiver, Sender, channel},
    },
    time::Instant,
};
use tracing::{debug, trace};

mod actions;
mod built_in_commands;
mod commands;
mod minibuffer;
mod mouse;

pub use actions::Action;
pub use minibuffer::MiniBufferState;
pub use mouse::Click;

#[cfg(feature = "fuzz")]
pub use commands::parse_command_fuzz;

pub(crate) use actions::{Actions, ViewPort};
pub(crate) use built_in_commands::built_in_commands;
pub(crate) use minibuffer::{MbSelect, MbSelector, MiniBufferSelection};

/// The mode that the [Editor] will run in following a call to [Editor::run].
pub enum EditorMode {
    /// Run as a TUI
    Terminal,
    /// Run without a user interface
    Headless,
    /// Used in scenario tests
    Boxed(Box<dyn UserInterface>),
}

impl fmt::Debug for EditorMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Terminal => f.debug_struct("EditorMode::Terminal").finish(),
            Self::Headless => f.debug_struct("EditorMode::Headless").finish(),
            Self::Boxed(_) => f.debug_struct("EditorMode::Boxed").finish(),
        }
    }
}

/// The main editor state.
#[derive(Debug)]
pub struct Editor<S>
where
    S: System,
{
    config: Arc<RwLock<Config>>,
    system: S,
    ui: Ui,
    cwd: PathBuf,
    running: bool,
    modes: Vec<Mode>,
    pending_keys: Vec<Input>,
    layout: Layout,
    lsp_manager: Arc<LspManagerHandle>,
    tx_events: Sender<Event>,
    rx_events: Receiver<Event>,
    tx_fsys: Sender<LogEvent>,
    rx_fsys: Option<Receiver<LogEvent>>,
    log_buffer: LogBuffer,
    plumbing_rules: PlumbingRules,
    held_click: Option<Click>,
    last_click_was_left: bool,
    last_click_time: Instant,
}

impl Editor<DefaultSystem> {
    /// Construct a new [Editor] with the provided config.
    pub fn new(
        cfg: Config,
        plumbing_rules: PlumbingRules,
        mode: EditorMode,
        log_buffer: LogBuffer,
    ) -> Self {
        Self::new_with_system(
            cfg,
            plumbing_rules,
            mode,
            log_buffer,
            DefaultSystem::from_env(),
        )
    }

    pub fn new_with_initial_files(
        config_res: Result<Config, String>,
        plumbing_rules_res: Result<PlumbingRules, String>,
        mode: EditorMode,
        log_buffer: LogBuffer,
        file_paths: &[impl AsRef<Path>],
    ) -> Self {
        Self::new_with_system_and_initial_files(
            config_res,
            plumbing_rules_res,
            mode,
            log_buffer,
            DefaultSystem::from_env(),
            file_paths,
        )
    }
}

impl<S> Editor<S>
where
    S: System,
{
    /// Construct a new [Editor] with the provided config and System.
    pub fn new_with_system(
        config: Config,
        plumbing_rules: PlumbingRules,
        mode: EditorMode,
        log_buffer: LogBuffer,
        system: S,
    ) -> Self {
        let cwd = match env::current_dir() {
            Ok(cwd) => cwd,
            Err(e) => die!("Unable to determine working directory: {e}"),
        };
        let (tx_events, rx_events) = channel();
        let (tx_fsys, rx_fsys) = channel();

        let show_splash = config.show_splash;
        let lsp_manager = Arc::new(LspManager::spawn(
            config.filetypes.clone(),
            tx_events.clone(),
            config.lsp_autostart,
        ));

        let modes = modes(&config.keys);
        let config = Arc::new(RwLock::new(config));

        let ui = Ui::new(mode, config.clone());
        let mut layout = Layout::new(100, 100, lsp_manager.clone(), config.clone());
        if show_splash && layout.is_empty_squirrel() {
            layout
                .active_buffer_mut_ignoring_scratch()
                .txt
                .insert_str(0, WELCOME_SQUIRREL);
        }

        Self {
            config,
            system,
            ui,
            cwd,
            running: true,
            modes,
            pending_keys: Vec::new(),
            layout,
            lsp_manager,
            tx_events,
            rx_events,
            tx_fsys,
            rx_fsys: Some(rx_fsys),
            log_buffer,
            plumbing_rules,
            held_click: None,
            last_click_was_left: false,
            last_click_time: Instant::now(),
        }
    }

    /// Construct a new [Editor] using the provided config and then open a list of initial
    /// files based on their file paths relative to the working directory of the editor.
    ///
    /// The config and plumbing rules results are used to display parse errors to the user
    /// in virtual buffers. In the case that Err is passed for either argument, the default
    /// for that type will be used to start the editor so the parser errors can be shown.
    pub fn new_with_system_and_initial_files(
        config_res: Result<Config, String>,
        plumbing_rules_res: Result<PlumbingRules, String>,
        mode: EditorMode,
        log_buffer: LogBuffer,
        system: S,
        file_paths: &[impl AsRef<Path>],
    ) -> Self {
        let (config, config_err) = match config_res {
            Ok(config) => (config, None),
            Err(e) => (Config::default(), Some(e)),
        };
        let (plumbing_rules, plumb_err) = match plumbing_rules_res {
            Ok(rules) => (rules, None),
            Err(e) => (PlumbingRules::default(), Some(e)),
        };

        let mut e = Self::new_with_system(config, plumbing_rules, mode, log_buffer, system);

        for path in file_paths.iter() {
            e.open_file_relative_to_cwd(path, false);
        }

        if let Some(err) = config_err {
            e.open_virtual(
                "+config-error",
                format!("Unable to load config file:\n{err}"),
                true,
            );
        }
        if let Some(err) = plumb_err {
            e.open_virtual(
                "+plumbing-error",
                format!("Unable to load plumbing rules:\n{err}"),
                true,
            );
        }

        e
    }

    /// The id of the currently active buffer
    #[inline]
    pub fn active_buffer_id(&self) -> usize {
        self.layout.active_buffer_ignoring_scratch().id
    }

    #[inline]
    pub fn active_buffer_name(&self) -> &str {
        self.layout.active_buffer_ignoring_scratch().full_name()
    }

    pub fn buffer_list(&self) -> Vec<String> {
        self.layout.as_buffer_list()
    }

    pub fn buffer_content(&self, id: BufferId) -> Option<String> {
        self.layout.buffer_with_id(id).map(|b| b.str_contents())
    }

    pub fn buffer_dot(&self, id: BufferId) -> Option<String> {
        self.layout.buffer_with_id(id).map(|b| b.dot_contents())
    }

    pub fn layout_ids(&self) -> Vec<Vec<BufferId>> {
        self.layout.ids()
    }

    /// The effective directory of the editor at any point is the directory containing the
    /// file backing the active buffer, or if the active buffer can not define a containing
    /// directory, self.cwd.
    #[inline]
    pub fn effective_directory(&self) -> &Path {
        self.layout
            .active_buffer_ignoring_scratch()
            .dir()
            .unwrap_or(&self.cwd)
    }

    /// Update the stored window size, accounting for the status and message bars
    /// This will panic if the available screen rows are 0 or 1
    pub(crate) fn update_window_size(&mut self, screen_rows: usize, screen_cols: usize) {
        trace!("window size updated: rows={screen_rows} cols={screen_cols}");
        self.layout.update_screen_size(screen_rows - 2, screen_cols);
    }

    /// Ensure that opening without any files initialises the fsys state correctly
    fn ensure_correct_fsys_state(&self) {
        if self.layout.is_empty_squirrel() {
            _ = self.tx_fsys.send(LogEvent::Open(0));
            _ = self.tx_fsys.send(LogEvent::Focus(0));
        }
    }

    /// Initialise any UI state required for our [EditorMode] and run the main event loop using a
    /// custom path for our fsys socket.
    pub fn run_with_explicit_fsys_path(&mut self, socket_path: PathBuf) {
        self.run_event_loop(Some(socket_path));
    }

    /// Initialise any UI state required for our [EditorMode] and run the main event loop.
    pub fn run(&mut self) {
        self.run_event_loop(None);
    }

    fn run_event_loop(&mut self, socket_path: Option<PathBuf>) {
        let (fs_enabled, auto_mount) = {
            let cfg = config_handle!(self);
            (cfg.filesystem.enabled, cfg.filesystem.auto_mount)
        };

        let handle = if fs_enabled {
            let rx_fsys = self.rx_fsys.take().expect("to have fsys channels");
            let fs = AdFs::new(self.tx_events.clone(), rx_fsys, auto_mount);
            let handle = fs.run_threaded(socket_path);
            self.ensure_correct_fsys_state();

            Some(handle)
        } else {
            None
        };

        let tx = self.tx_events.clone();
        let (screen_rows, screen_cols) = self.ui.init(tx);
        self.update_window_size(screen_rows, screen_cols);
        self.ui.set_cursor_shape(self.current_cursor_shape());

        while self.running {
            self.refresh_screen_w_minibuffer(None);

            match self.rx_events.recv() {
                Ok(next_event) => self.handle_event(next_event),
                _ => break,
            }
        }

        self.ui.shutdown();

        if let Some(handle) = handle {
            handle.remove_socket();
        }
    }

    #[inline]
    pub fn handle_event(&mut self, event: Event) {
        match event {
            Event::Action(a) => self.handle_action(a, Source::Fsys),
            Event::Actions(a) => self.handle_actions(a, Source::Fsys),
            Event::BracketedPaste(s) => {
                self.handle_action(Action::InsertString { s }, Source::Fsys)
            }
            Event::Input(i) => self.handle_input(i),
            Event::Message(msg) => self.handle_message(msg),
            Event::StatusMessage(msg) => self.set_status_message(msg),
            Event::WinsizeChanged { rows, cols } => self.update_window_size(rows, cols),
        }
    }

    pub fn refresh_screen_w_minibuffer(&mut self, mb: Option<MiniBufferState<'_>>) {
        self.layout.clamp_scroll();
        self.ui.refresh(
            &self.modes[0].name,
            &mut self.layout,
            self.system.n_running_children(),
            &self.pending_keys,
            self.held_click.as_ref(),
            mb,
        );
    }

    /// Update the status line to contain the given message.
    pub fn set_status_message(&mut self, msg: impl Into<String>) {
        self.ui
            .state_change(StateChange::StatusMessage { msg: msg.into() });
    }

    pub(crate) fn current_cursor_shape(&self) -> CurShape {
        self.modes[0].cur_shape
    }

    pub(crate) fn block_for_input(&mut self) -> Vec<Input> {
        while self.running {
            match self.rx_events.recv().unwrap() {
                Event::Input(i) => return vec![i],
                Event::BracketedPaste(s) => return s.chars().map(Input::Char).collect(),
                Event::Action(a) => self.handle_action(a, Source::Fsys),
                Event::Actions(a) => self.handle_actions(a, Source::Fsys),
                Event::Message(msg) => self.handle_message(msg),
                Event::StatusMessage(msg) => self.set_status_message(msg),
                Event::WinsizeChanged { rows, cols } => self.update_window_size(rows, cols),
            }
        }

        Vec::new()
    }

    fn send_buffer_resp(
        &self,
        id: usize,
        tx: Sender<Result<String, String>>,
        f: fn(&Buffer) -> String,
    ) {
        if id == SCRATCH_ID {
            _ = tx.send(Ok((f)(self.layout.scratch.b.buffer())));
            return;
        }

        match self.layout.buffer_with_id(id) {
            Some(b) => _ = tx.send(Ok((f)(b))),
            None => {
                _ = tx.send(Err("unknown buffer".to_string()));
                _ = self.tx_fsys.send(LogEvent::Close(id));
            }
        }
    }

    fn handle_buffer_mutation<F: FnOnce(&mut Buffer, String)>(
        &mut self,
        id: usize,
        tx: Sender<Result<String, String>>,
        s: String,
        f: F,
    ) {
        if id == SCRATCH_ID {
            (f)(self.layout.scratch.b.buffer_mut(), s);
            _ = tx.send(Ok("handled".to_string()));
            return;
        }

        match self.layout.buffer_with_id_mut(id) {
            Some(b) => {
                (f)(b, s);
                _ = tx.send(Ok("handled".to_string()))
            }

            None => {
                _ = tx.send(Err("unknown buffer".to_string()));
                _ = self.tx_fsys.send(LogEvent::Close(id));
            }
        }
    }

    fn handle_message(&mut self, Message { req, tx }: Message) {
        use Req::*;

        debug!("received fys message: {req:?}");
        let default_handled = || _ = tx.send(Ok("handled".to_string()));

        match req {
            ControlMessage { msg } => {
                self.execute_command(&msg);
                default_handled();
            }

            MinibufferSelect { prompt, lines, tx } => {
                self.fsys_minibuffer(prompt, lines, tx);
                default_handled();
            }

            ReadBufferName { id } => {
                self.send_buffer_resp(id, tx, |b| format!("{}\n", b.full_name()))
            }
            ReadBufferAddr { id } => self.send_buffer_resp(id, tx, |b| format!("{}\n", b.addr())),
            ReadBufferDot { id } => self.send_buffer_resp(id, tx, |b| b.dot_contents()),
            ReadBufferXAddr { id } => self.send_buffer_resp(id, tx, |b| format!("{}\n", b.xaddr())),
            ReadBufferXDot { id } => self.send_buffer_resp(id, tx, |b| b.xdot_contents()),
            ReadBufferBody { id } => self.send_buffer_resp(id, tx, |b| b.str_contents()),
            ReadBufferFtype { id } => self.send_buffer_resp(id, tx, |b| {
                format!(
                    "{}\n",
                    b.configured_filetype()
                        .unwrap_or_else(|| "unknown".to_string())
                )
            }),

            SetBufferName { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.set_filename(s.trim());
            }),

            SetBufferAddr { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                if let Ok(addr) = Addr::parse(s.trim_end()) {
                    b.dot = b.map_addr(&addr);
                };
            }),
            SetBufferDot { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.handle_action(Action::InsertString { s }, Source::Fsys);
            }),
            SetBufferXAddr { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                if let Ok(addr) = Addr::parse(s.trim_end()) {
                    b.xdot = b.map_addr(&addr);
                };
            }),
            SetBufferXDot { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.insert_xdot(s);
            }),

            ClearBufferBody { id } => self.handle_buffer_mutation(id, tx, String::new(), |b, _| {
                b.clear();
            }),

            AppendBufferBody { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.append(s, Source::Fsys);
            }),

            AppendOutput { id, s } => {
                self.layout.write_output_for_buffer(id, s, &self.cwd);
                default_handled();
            }

            AddInputEventFilter { id, filter } => {
                let resp = if self.layout.try_set_input_filter(id, filter) {
                    Ok("handled".to_string())
                } else {
                    Err("filter already in place".to_string())
                };
                _ = tx.send(resp);
            }

            RemoveInputEventFilter { id } => {
                self.layout.clear_input_filter(id);
                default_handled();
            }

            LoadInBuffer { id, txt } => {
                self.load_string_in_buffer(id, txt, false);
                default_handled();
            }

            ExecuteInBuffer { id, txt } => {
                self.execute_explicit_string(id, &txt, Source::Fsys);
                default_handled();
            }
        }
    }

    pub fn handle_input(&mut self, input: Input) {
        self.pending_keys.push(input);
        let maybe_actions = self.modes[0].handle_keys(&mut self.pending_keys);

        if let Some(actions) = maybe_actions {
            self.handle_actions(actions, Source::Keyboard);
        }
    }

    fn handle_explicit_inputs(&mut self, inputs: Vec<Input>) {
        for i in inputs.into_iter() {
            self.handle_input(i);
        }
    }

    fn handle_actions(&mut self, actions: Actions, source: Source) {
        match actions {
            Actions::Single(action) => self.handle_action(action, source),
            Actions::Multi(actions) => {
                for action in actions.into_iter() {
                    self.handle_action(action, source);
                    if !self.running {
                        break;
                    };
                }
            }
        }
    }

    /// Process a single action and update editor state accordingly
    pub fn handle_action(&mut self, action: Action, source: Source) {
        use Action::*;

        match action {
            Noop => (),

            AppendToOutputBuffer { bufid, content } => self
                .layout
                .write_output_for_buffer(bufid, content, &self.cwd),
            BalanceActiveColumn => self.layout.balance_active_column(),
            BalanceAll => self.layout.balance_all(),
            BalanceColumns => self.layout.balance_columns(),
            BalanceWindows => self.layout.balance_windows(),
            ChangeDirectory { path } => self.change_directory(path),
            CleanupChild { id } => self.system.cleanup_child(id),
            ClearScratch => self.layout.scratch.b.clear(),
            CommandMode => self.command_mode(),
            DeleteBuffer { force } => self.delete_buffer(self.active_buffer_id(), force),
            DeleteColumn { force } => self.delete_active_column(force),
            DeleteWindow { force } => self.delete_active_window(force),
            DragWindow {
                direction: Arrow::Up,
            } => self.layout.drag_up(),
            DragWindow {
                direction: Arrow::Down,
            } => self.layout.drag_down(),
            DragWindow {
                direction: Arrow::Left,
            } => self.layout.drag_left(),
            DragWindow {
                direction: Arrow::Right,
            } => self.layout.drag_right(),
            EditCommand { cmd } => self.execute_edit_command(&cmd),
            EnsureFileIsOpen { path } => self.layout.ensure_file_is_open(&path),
            ExecuteDot => self.default_execute_dot(None, source),
            ExecuteString { s } => {
                self.execute_explicit_string(self.active_buffer_id(), &s, source)
            }
            Exit { force } => self.exit(force),
            ExpandDot => self.expand_current_dot(),
            FindFile { new_window } => self.find_file(new_window),
            FindRepoFile { new_window } => self.find_repo_file(new_window),
            FocusBuffer { id } => self.focus_buffer(id, false), // allow focusing another window
            JumpListForward => self.jump_forward(),
            JumpListBack => self.jump_backward(),
            KillRunningChild => self.kill_running_child(),
            LoadDot { new_window } => self.default_load_dot(source, new_window),
            LspShowCapabilities => {
                if let Some((name, txt)) = self
                    .lsp_manager
                    .show_server_capabilities(self.layout.active_buffer_ignoring_scratch())
                {
                    self.layout.open_virtual(name, txt, true)
                }
            }
            LspShowDiagnostics => {
                let action = self
                    .lsp_manager
                    .show_diagnostics(self.layout.active_buffer_ignoring_scratch());
                self.handle_action(action, Source::Fsys);
            }
            LspStart => {
                if let Some(msg) = self.lsp_manager.start_client(self.layout.buffers()) {
                    self.set_status_message(msg);
                }
            }
            LspStop => self
                .lsp_manager
                .stop_client(self.layout.active_buffer_ignoring_scratch()),
            LspCompletion => self
                .lsp_manager
                .completion(self.layout.active_buffer_ignoring_scratch()),
            LspFormat => self
                .lsp_manager
                .format(self.layout.active_buffer_ignoring_scratch()),
            LspGotoDeclaration => self
                .lsp_manager
                .goto_declaration(self.layout.active_buffer_ignoring_scratch()),
            LspGotoDefinition => self
                .lsp_manager
                .goto_definition(self.layout.active_buffer_ignoring_scratch()),
            LspGotoTypeDefinition => self
                .lsp_manager
                .goto_type_definition(self.layout.active_buffer_ignoring_scratch()),
            LspHover => self
                .lsp_manager
                .hover(self.layout.active_buffer_ignoring_scratch()),
            LspReferences => self
                .lsp_manager
                .find_references(self.layout.active_buffer_ignoring_scratch()),
            LspRename => self.lsp_rename(),
            LspRenamePrepare => self.prepare_lsp_rename(),
            MarkClean { bufid } => self.mark_clean(bufid),
            MbSelect(selector) => selector.run(self),
            NewEditLogTransaction => self.layout.active_buffer_mut().new_edit_log_transaction(),
            NewColumn => self.layout.new_column(),
            NewWindow => self.layout.new_window(),
            NextBuffer => {
                let id = self.layout.focus_next_buffer();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            NextColumn => {
                self.layout.next_column();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            NextWindowInColumn => {
                self.layout.next_window_in_column();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            OpenFile { path } => self.open_file_relative_to_effective_directory(&path, false),
            OpenFileInNewWindow { path } => {
                self.open_file_relative_to_effective_directory(&path, true)
            }
            OpenTransientScratch { name, txt } => self.layout.open_transient_scratch(name, txt),
            OpenVirtualFile { name, txt } => self.layout.open_virtual(name, txt, true),
            Paste => self.paste_from_clipboard(source),
            Plumb { txt, new_window } => self.plumb(txt, new_window),
            PreviousBuffer => {
                let id = self.layout.focus_previous_buffer();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            PreviousColumn => {
                self.layout.prev_column();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            PreviousWindowInColumn => {
                self.layout.prev_window_in_column();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            ReloadActiveBuffer => self.reload_active_buffer(),
            ReloadBuffer { id } => self.reload_buffer(id),
            ReloadConfig => self.reload_config(),
            ResizeActiveColumn { delta } => self.layout.resize_active_column(delta),
            ResizeActiveWindow { delta } => self.layout.resize_active_window(delta),
            RunMode => self.run_mode(),
            SamMode => self.sam_mode(),
            SaveBuffer { force } => self.save_current_buffer(None, force),
            SaveBufferAll { force } => self.save_all_buffers(force),
            SaveBufferAs { path, force } => self.save_current_buffer(Some(path), force),
            SearchInCurrentBuffer => self.search_in_current_buffer(),
            SendKeys { ks } => self.handle_explicit_inputs(ks),
            SelectBuffer => self.select_buffer(),
            SetMode { m } => self.set_mode(m),
            SetStatusMessage { message } => self.set_status_message(&message),
            SetViewPort(vp) => self.layout.set_viewport(vp),
            ShellPipe { cmd } => self.pipe_dot_through_shell_cmd(&cmd),
            ShellReplace { cmd } => self.replace_dot_with_shell_cmd(&cmd),
            ShellRun { cmd } => self.run_shell_cmd(&cmd),
            ShowHelp => self.show_help(),
            ToggleScratch => self.layout.toggle_scratch(),
            TsShowTree => self.show_active_ts_tree(),
            ViewLogs => self.view_logs(),
            Yank => self.set_clipboard(self.layout.active_buffer().dot_contents()),

            DebugBufferContents => self.debug_buffer_contents(),
            DebugEditLog => self.debug_edit_log(),

            RawInput { i } if i == Input::PageUp || i == Input::PageDown => {
                let arr = if i == Input::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                self.forward_action_to_active_buffer(
                    DotSet(TextObject::Arr(arr), self.layout.active_window_rows()),
                    Source::Keyboard,
                );
            }
            RawInput {
                i: Input::Mouse(evt),
            } => self.handle_mouse_event(evt),

            a => self.forward_action_to_active_buffer(a, source),
        }
    }

    fn jump_forward(&mut self) {
        if let Some(id) = self.layout.jump_forward() {
            _ = self.tx_fsys.send(LogEvent::Focus(id));
        }
    }

    fn jump_backward(&mut self) {
        if let Some(id) = self.layout.jump_backward() {
            _ = self.tx_fsys.send(LogEvent::Focus(id));
        }
    }

    pub(super) fn forward_action_to_active_buffer(&mut self, a: Action, source: Source) {
        if let Some(o) = self.layout.active_buffer_mut().handle_action(a, source) {
            match o {
                ActionOutcome::SetStatusMessage(msg) => self.set_status_message(&msg),
                ActionOutcome::SetClipboard(s) => self.set_clipboard(s),
            }
        }
    }

    pub(super) fn forward_action_to_active_buffer_ignoring_scratch(
        &mut self,
        a: Action,
        source: Source,
    ) {
        if let Some(o) = self
            .layout
            .active_buffer_mut_ignoring_scratch()
            .handle_action(a, source)
        {
            match o {
                ActionOutcome::SetStatusMessage(msg) => self.set_status_message(&msg),
                ActionOutcome::SetClipboard(s) => self.set_clipboard(s),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::system::DefaultSystem;
    use std::{thread::sleep, time::Duration};

    // We need access to the internals of Editor for this but really this is a test of the
    // behaviour of the System trait and DefaultSystem.
    #[test]
    fn process_control_works() {
        let mut ed = Editor::new_with_system(
            Config::default(),
            PlumbingRules::default(),
            EditorMode::Headless,
            LogBuffer::default(),
            DefaultSystem::without_clipboard_provider(),
        );

        ed.update_window_size(100, 80);
        ed.open_file(ed.cwd.join("test"), false);
        ed.handle_action(
            Action::ShellRun {
                cmd: "yes".to_string(),
            },
            Source::Keyboard,
        );

        // Allow the script to write to the output buffer
        let evt = ed.rx_events.recv().unwrap();
        ed.handle_event(evt);

        // Should have the test file and now the output buffer
        assert_eq!(ed.layout.buffers().len(), 2);
        assert_eq!(ed.system.running_children().len(), 1);

        ed.system.kill_child(0);
        assert_eq!(ed.system.running_children().len(), 0);

        // hack: avoid race between the child thread pushing lines to the output buffer and the
        // attempt to drain the event queue below
        sleep(Duration::from_millis(100));

        // drain any pending writes from the script
        while let Ok(evt) = ed.rx_events.try_recv() {
            match evt {
                Event::Action(Action::AppendToOutputBuffer { .. }) => (),
                Event::Action(Action::CleanupChild { .. }) => (),
                _ => panic!("expected AppendToOutputBuffer or CleanupChild but got {evt:?}"),
            }
        }

        ed.layout.close_buffer(1);
        assert_eq!(ed.layout.buffers().len(), 1);

        match ed.rx_events.try_recv() {
            Err(_) => (),
            Ok(Event::Action(Action::CleanupChild { .. })) => (),
            Ok(evt) => panic!("expected no events or CleanupChild, got {evt:?}"),
        }
    }
}
