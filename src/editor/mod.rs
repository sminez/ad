//! The main control flow and functionality of the `ad` editor.
use crate::{
    buffer::{ActionOutcome, Buffer, Buffers},
    config::Config,
    die,
    dot::TextObject,
    exec::{Addr, Address},
    fsys::{AdFs, InputFilter, LogEvent, Message, Req},
    input::Event,
    key::{Arrow, Input},
    mode::{modes, Mode},
    plumb::PlumbingRules,
    set_config,
    system::{DefaultSystem, System},
    term::CurShape,
    ui::{StateChange, Ui, UserInterface, Windows},
    LogBuffer,
};
use ad_event::Source;
use std::{
    env, panic,
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
    time::Instant,
};
use tracing::{debug, trace, warn};

mod actions;
mod built_in_commands;
mod commands;
mod minibuffer;
mod mouse;

pub(crate) use actions::{Action, Actions, ViewPort};
pub(crate) use built_in_commands::built_in_commands;
pub(crate) use minibuffer::{MiniBufferSelection, MiniBufferState};
pub(crate) use mouse::Click;

/// The mode that the [Editor] will run in following a call to [Editor::run].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EditorMode {
    /// Run as a TUI
    Terminal,
    /// Run without a user interface
    Headless,
}

/// The main editor state.
#[derive(Debug)]
pub struct Editor<S>
where
    S: System,
{
    system: S,
    ui: Ui,
    cwd: PathBuf,
    running: bool,
    modes: Vec<Mode>,
    pending_keys: Vec<Input>,
    buffers: Buffers,
    windows: Windows,
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
        Self::new_with_system(cfg, plumbing_rules, mode, log_buffer, DefaultSystem)
    }
}

impl<S> Editor<S>
where
    S: System,
{
    /// Construct a new [Editor] with the provided config and System.
    pub fn new_with_system(
        cfg: Config,
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

        set_config(cfg);

        Self {
            system,
            ui: mode.into(),
            cwd,
            running: true,
            modes: modes(),
            pending_keys: Vec::new(),
            buffers: Buffers::new(),
            windows: Windows::new(0, 0, 0),
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

    /// The id of the currently active buffer
    pub fn active_buffer_id(&self) -> usize {
        self.buffers.active().id
    }

    /// Update the stored window size, accounting for the status and message bars
    /// This will panic if the available screen rows are 0 or 1
    pub(crate) fn update_window_size(&mut self, screen_rows: usize, screen_cols: usize) {
        trace!("window size updated: rows={screen_rows} cols={screen_cols}");
        self.windows
            .update_screen_size(screen_rows - 2, screen_cols);
    }

    /// Ensure that opening without any files initialises the fsys state correctly
    fn ensure_correct_fsys_state(&self) {
        if self.buffers.is_empty_scratch() {
            _ = self.tx_fsys.send(LogEvent::Open(0));
            _ = self.tx_fsys.send(LogEvent::Focus(0));
        }
    }

    /// Initialise any UI state required for our [EditorMode] and run the main event loop.
    pub fn run(mut self) {
        let rx_fsys = self.rx_fsys.take().expect("to have fsys channels");
        AdFs::new(self.tx_events.clone(), rx_fsys).run_threaded();
        self.ensure_correct_fsys_state();
        self.run_event_loop();
    }

    #[inline]
    fn handle_event(&mut self, event: Event) {
        match event {
            Event::Input(i) => self.handle_input(i),
            Event::Action(a) => self.handle_action(a, Source::Fsys),
            Event::Message(msg) => self.handle_message(msg),
            Event::WinsizeChanged { rows, cols } => self.update_window_size(rows, cols),
        }
    }

    pub(super) fn refresh_screen_w_minibuffer(&mut self, mb: Option<MiniBufferState<'_>>) {
        self.windows.clamp_scroll(self.buffers.active_mut());
        self.ui.refresh(
            &self.modes[0].name,
            &self.buffers,
            &self.windows,
            &self.pending_keys,
            self.held_click.as_ref(),
            mb,
        );
    }

    fn run_event_loop(mut self) {
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
    }

    /// Update the status line to contain the given message.
    pub fn set_status_message(&mut self, msg: &str) {
        self.ui.state_change(StateChange::StatusMessage {
            msg: msg.to_string(),
        });
    }

    pub(crate) fn current_cursor_shape(&self) -> CurShape {
        self.modes[0].cur_shape
    }

    pub(crate) fn block_for_input(&mut self) -> Input {
        loop {
            match self.rx_events.recv().unwrap() {
                Event::Input(k) => return k,
                Event::Action(a) => self.handle_action(a, Source::Fsys),
                Event::Message(msg) => self.handle_message(msg),
                Event::WinsizeChanged { rows, cols } => self.update_window_size(rows, cols),
            }
        }
    }

    /// Open a new virtual buffer which will be removed from state when it loses focus.
    pub(crate) fn open_virtual(&mut self, name: impl Into<String>, content: impl Into<String>) {
        self.buffers.open_virtual(name.into(), content.into());
    }

    fn send_buffer_resp(
        &self,
        id: usize,
        tx: Sender<Result<String, String>>,
        f: fn(&Buffer) -> String,
    ) {
        match self.buffers.with_id(id) {
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
        match self.buffers.with_id_mut(id) {
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

            ReadBufferName { id } => self.send_buffer_resp(id, tx, |b| b.full_name().to_string()),
            ReadBufferAddr { id } => self.send_buffer_resp(id, tx, |b| b.addr()),
            ReadBufferDot { id } => self.send_buffer_resp(id, tx, |b| b.dot_contents()),
            ReadBufferXAddr { id } => self.send_buffer_resp(id, tx, |b| b.xaddr()),
            ReadBufferXDot { id } => self.send_buffer_resp(id, tx, |b| b.xdot_contents()),
            ReadBufferBody { id } => self.send_buffer_resp(id, tx, |b| b.str_contents()),

            SetBufferAddr { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                if let Ok(mut expr) = Addr::parse(&mut s.trim_end().chars().peekable()) {
                    b.dot = b.map_addr(&mut expr);
                };
            }),
            SetBufferDot { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.handle_action(Action::InsertString { s }, Source::Fsys);
            }),
            SetBufferXAddr { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                if let Ok(mut expr) = Addr::parse(&mut s.trim_end().chars().peekable()) {
                    b.xdot = b.map_addr(&mut expr);
                };
            }),
            SetBufferXDot { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                let dot = b.dot;
                b.dot = b.xdot;
                b.handle_action(Action::InsertString { s }, Source::Fsys);
                (b.xdot, b.dot) = (b.dot, dot);
                b.dot.clamp_idx(b.txt.len_chars()); // xdot already clamped as part of the insert
            }),

            ClearBufferBody { id } => self.handle_buffer_mutation(id, tx, String::new(), |b, _| {
                b.handle_action(Action::DotSet(TextObject::BufferStart, 1), Source::Fsys);
                b.handle_action(
                    Action::DotExtendForward(TextObject::BufferEnd, 1),
                    Source::Fsys,
                );
                b.handle_action(Action::Delete, Source::Fsys);
            }),

            AppendBufferBody { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.append(s, Source::Fsys);
            }),

            AppendOutput { id, s } => {
                self.buffers.write_output_for_buffer(id, s, &self.cwd);
                default_handled();
            }

            AddInputEventFilter { id, filter } => {
                let resp = if self.try_set_input_filter(id, filter) {
                    Ok("handled".to_string())
                } else {
                    Err("filter already in place".to_string())
                };
                _ = tx.send(resp);
            }

            RemoveInputEventFilter { id } => {
                self.clear_input_filter(id);
                default_handled();
            }

            LoadInBuffer { id, txt } => {
                self.load_explicit_string(id, txt);
                default_handled();
            }

            ExecuteInBuffer { id, txt } => {
                self.execute_explicit_string(id, txt, Source::Fsys);
                default_handled();
            }
        }
    }

    fn handle_input(&mut self, input: Input) {
        self.pending_keys.push(input);

        if let Some(actions) = self.modes[0].handle_keys(&mut self.pending_keys) {
            self.handle_actions(actions, Source::Keyboard);
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

    fn handle_action(&mut self, action: Action, source: Source) {
        use Action::*;

        match action {
            AppendToOutputBuffer { bufid, content } => self
                .buffers
                .write_output_for_buffer(bufid, content, &self.cwd),
            ChangeDirectory { path } => self.change_directory(path),
            CommandMode => self.command_mode(),
            DeleteBuffer { force } => self.delete_buffer(self.buffers.active().id, force),
            EditCommand { cmd } => self.execute_edit_command(&cmd),
            ExecuteDot => self.default_execute_dot(None, source),
            Exit { force } => self.exit(force),
            ExpandDot => self.expand_current_dot(),
            FindFile => self.find_file(),
            FindRepoFile => self.find_repo_file(),
            FocusBuffer { id } => self.focus_buffer(id),
            JumpListForward => self.jump_forward(),
            JumpListBack => self.jump_backward(),
            LoadDot => self.default_load_dot(source),
            MarkClean { bufid } => self.mark_clean(bufid),
            NewEditLogTransaction => self.buffers.active_mut().new_edit_log_transaction(),
            NextBuffer => {
                self.buffers.next();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            OpenFile { path } => self.open_file_relative_to_cwd(&path),
            Paste => self.paste_from_clipboard(source),
            PreviousBuffer => {
                self.buffers.previous();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            ReloadActiveBuffer => self.reload_active_buffer(),
            ReloadBuffer { id } => self.reload_buffer(id),
            ReloadConfig => self.reload_config(),
            RunMode => self.run_mode(),
            SamMode => self.sam_mode(),
            SaveBufferAs { path, force } => self.save_current_buffer(Some(path), force),
            SaveBuffer { force } => self.save_current_buffer(None, force),
            SearchInCurrentBuffer => self.search_in_current_buffer(),
            SelectBuffer => self.select_buffer(),
            SetMode { m } => self.set_mode(m),
            SetStatusMessage { message } => self.set_status_message(&message),
            SetViewPort(vp) => self.windows.set_viewport(self.buffers.active_mut(), vp),
            ShellPipe { cmd } => self.pipe_dot_through_shell_cmd(&cmd),
            ShellReplace { cmd } => self.replace_dot_with_shell_cmd(&cmd),
            ShellRun { cmd } => self.run_shell_cmd(&cmd),
            ShowHelp => self.show_help(),
            UpdateConfig { input } => self.update_config(&input),
            ViewLogs => self.view_logs(),
            Yank => self.set_clipboard(self.buffers.active().dot_contents()),

            DebugBufferContents => self.debug_buffer_contents(),
            DebugEditLog => self.debug_edit_log(),

            RawInput { i } if i == Input::PageUp || i == Input::PageDown => {
                let arr = if i == Input::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                self.forward_action_to_active_buffer(
                    DotSet(TextObject::Arr(arr), self.windows.active_window_rows()),
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
        let maybe_ids = self.buffers.jump_list_forward();
        if let Some((prev_id, new_id)) = maybe_ids {
            if let Some(b) = self.buffers.with_id_mut(new_id) {
                self.windows.set_viewport(b, ViewPort::Center);
                self.windows.focus_buffer_in_active_window(b);
            }
            if new_id != prev_id {
                _ = self.tx_fsys.send(LogEvent::Focus(new_id));
            }
        }
    }

    fn jump_backward(&mut self) {
        let maybe_ids = self.buffers.jump_list_backward();
        if let Some((prev_id, new_id)) = maybe_ids {
            if let Some(b) = self.buffers.with_id_mut(new_id) {
                self.windows.set_viewport(b, ViewPort::Center);
                self.windows.focus_buffer_in_active_window(b);
            }
            if new_id != prev_id {
                _ = self.tx_fsys.send(LogEvent::Focus(new_id));
            }
        }
    }

    fn forward_action_to_active_buffer(&mut self, a: Action, source: Source) {
        if let Some(o) = self.buffers.active_mut().handle_action(a, source) {
            match o {
                ActionOutcome::SetStatusMessage(msg) => self.set_status_message(&msg),
                ActionOutcome::SetClipboard(s) => self.set_clipboard(s),
            }
        }
    }

    /// Returns `true` if the filter was successfully set, false if there was already one in place.
    pub(crate) fn try_set_input_filter(&mut self, bufid: usize, filter: InputFilter) -> bool {
        let b = match self.buffers.with_id_mut(bufid) {
            Some(b) => b,
            None => return false,
        };

        if b.input_filter.is_some() {
            warn!("attempt to set an input filter when one is already in place. id={bufid:?}");
            return false;
        }

        b.input_filter = Some(filter);

        true
    }

    /// Remove the input filter for the given scope if one exists.
    pub(crate) fn clear_input_filter(&mut self, bufid: usize) {
        if let Some(b) = self.buffers.with_id_mut(bufid) {
            b.input_filter = None;
        }
    }
}
