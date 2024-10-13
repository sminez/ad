//! The main control flow and functionality of the `ad` editor.
use crate::{
    buffer::{ActionOutcome, Buffer, Buffers},
    config::Config,
    die,
    dot::{Dot, Range, TextObject},
    exec::{Addr, Address},
    fsys::{AdFs, InputFilter, LogEvent, Message, Req},
    input::{Event, StdinInput},
    key::{Arrow, Input, MouseButton, MouseEvent},
    mode::{modes, Mode},
    plumb::PlumbingRules,
    restore_terminal_state, set_config,
    term::{
        clear_screen, enable_alternate_screen, enable_mouse_support, enable_raw_mode, get_termios,
        get_termsize, register_signal_handler,
    },
    system::{System, DefaultSystem},
    LogBuffer, ORIGINAL_TERMIOS,
};
use std::{
    env,
    io::{self, Stdout, Write},
    panic,
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
    time::Instant,
};
use tracing::{debug, trace, warn};

mod actions;
mod built_in_commands;
mod commands;
mod minibuffer;
mod render;

pub(crate) use actions::{Action, Actions, ViewPort};
pub(crate) use minibuffer::{MiniBufferSelection, MiniBufferState};

/// The mode that the [Editor] will run in following a call to [Editor::run].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EditorMode {
    /// Run as a TUI
    Terminal,
    /// Run without a user interface
    Headless,
}

/// Transient state that we hold to track the last mouse click we saw while
/// we wait for it to be released or if the buffer changes.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Click {
    /// The button being held down
    btn: MouseButton,
    /// The current state of the dot associated with this click. This is updated
    /// by Hold events and matches the buffer Dot for Left clicks. For Right and
    /// Middle clicks this is a separate selection that is used on release.
    selection: Range,
}

/// The main editor state.
#[derive(Debug)]
pub struct Editor<S>
where
    S: System
{
    system: S,
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    cwd: PathBuf,
    running: bool,
    status_message: String,
    status_time: Instant,
    modes: Vec<Mode>,
    pending_keys: Vec<Input>,
    buffers: Buffers,
    tx_events: Sender<Event>,
    rx_events: Receiver<Event>,
    tx_fsys: Sender<LogEvent>,
    rx_fsys: Option<Receiver<LogEvent>>,
    mode: EditorMode,
    log_buffer: LogBuffer,
    plumbing_rules: PlumbingRules,
    held_click: Option<Click>,
}

impl<S> Drop for Editor<S>
where
    S: System
{
    fn drop(&mut self) {
        if self.mode == EditorMode::Terminal {
            restore_terminal_state(&mut self.stdout);
        }
    }
}

impl Editor<DefaultSystem> {
    /// Construct a new [Editor] with the provided config.
    pub fn new(
        cfg: Config,
        plumbing_rules: PlumbingRules,
        mode: EditorMode,
        log_buffer: LogBuffer,
    ) -> Self {
        let cwd = match env::current_dir() {
            Ok(cwd) => cwd,
            Err(e) => die!("Unable to determine working directory: {e}"),
        };
        let stdout = io::stdout();
        let (tx_events, rx_events) = channel();
        let (tx_fsys, rx_fsys) = channel();

        set_config(cfg);

        Self {
            system: DefaultSystem,
            screen_rows: 0,
            screen_cols: 0,
            stdout,
            cwd,
            running: true,
            status_message: String::new(),
            status_time: Instant::now(),
            modes: modes(),
            pending_keys: Vec::new(),
            buffers: Buffers::new(),
            tx_events,
            rx_events,
            tx_fsys,
            rx_fsys: Some(rx_fsys),
            mode,
            log_buffer,
            plumbing_rules,
            held_click: None,
        }
    }
}

impl<S> Editor<S>
where
    S: System
{
    /// The id of the currently active buffer
    pub fn active_buffer_id(&self) -> usize {
        self.buffers.active().id
    }

    /// Update the stored window size, accounting for the status and message bars
    /// This will panic if the available screen rows are 0 or 1
    fn update_window_size(&mut self) {
        let (screen_rows, screen_cols) = get_termsize();
        trace!("window size updated: rows={screen_rows} cols={screen_cols}");

        self.screen_rows = screen_rows - 2;
        self.screen_cols = screen_cols;
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

        match self.mode {
            EditorMode::Terminal => self.run_event_loop_with_screen_refresh(self.tx_events.clone()),
            EditorMode::Headless => self.run_event_loop(),
        }
    }

    #[inline]
    fn handle_event(&mut self, event: Event) {
        match event {
            Event::Input(i) => self.handle_input(i),
            Event::Action(a) => self.handle_action(a),
            Event::Message(msg) => self.handle_message(msg),
            Event::WinsizeChanged => self.update_window_size(),
        }
    }

    fn run_event_loop(&mut self) {
        while self.running {
            match self.rx_events.recv() {
                Ok(next_event) => self.handle_event(next_event),
                _ => break,
            }
        }
    }

    fn run_event_loop_with_screen_refresh(&mut self, tx: Sender<Event>) {
        self.init_tui(tx);

        while self.running {
            self.refresh_screen();
            match self.rx_events.recv() {
                Ok(next_event) => self.handle_event(next_event),
                _ => break,
            }
        }

        clear_screen(&mut self.stdout);
    }

    fn init_tui(&mut self, tx: Sender<Event>) {
        let original_termios = get_termios();
        enable_raw_mode(original_termios);
        _ = ORIGINAL_TERMIOS.set(original_termios);

        panic::set_hook(Box::new(|panic_info| {
            let mut stdout = io::stdout();
            restore_terminal_state(&mut stdout);
            _ = stdout.flush();
            println!("{panic_info}");
            _ = stdout.flush();
        }));

        enable_mouse_support(&mut self.stdout);
        enable_alternate_screen(&mut self.stdout);
        // SAFETY: we only register our signal handler once
        unsafe { register_signal_handler() };
        self.update_window_size();
        StdinInput::new(tx).run_threaded();
    }

    /// Update the status line to contain the given message.
    pub fn set_status_message(&mut self, msg: &str) {
        self.status_message.clear();
        self.status_message.push_str(msg);
        self.status_time = Instant::now();
    }

    pub(crate) fn block_for_input(&mut self) -> Input {
        loop {
            match self.rx_events.recv().unwrap() {
                Event::Input(k) => return k,
                Event::Action(a) => self.handle_action(a),
                Event::Message(msg) => self.handle_message(msg),
                Event::WinsizeChanged => self.update_window_size(),
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
                b.handle_action(Action::InsertString { s });
            }),
            SetBufferXAddr { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                if let Ok(mut expr) = Addr::parse(&mut s.trim_end().chars().peekable()) {
                    b.xdot = b.map_addr(&mut expr);
                };
            }),
            SetBufferXDot { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                let dot = b.dot;
                b.dot = b.xdot;
                b.handle_action(Action::InsertString { s });
                (b.xdot, b.dot) = (b.dot, dot);
                b.dot.clamp_idx(b.txt.len_chars());
                b.xdot.clamp_idx(b.txt.len_chars());
            }),

            ClearBufferBody { id } => self.handle_buffer_mutation(id, tx, String::new(), |b, _| {
                b.handle_action(Action::DotSet(TextObject::BufferStart, 1));
                b.handle_action(Action::DotExtendForward(TextObject::BufferEnd, 1));
                b.handle_action(Action::Delete);
            }),

            AppendBufferBody { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.append(s);
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
                self.execute_explicit_string(id, txt);
                default_handled();
            }
        }
    }

    fn handle_input(&mut self, input: Input) {
        self.pending_keys.push(input);

        if let Some(actions) = self.modes[0].handle_keys(&mut self.pending_keys) {
            self.handle_actions(actions);
        }
    }

    fn handle_actions(&mut self, actions: Actions) {
        match actions {
            Actions::Single(action) => self.handle_action(action),
            Actions::Multi(actions) => {
                for action in actions.into_iter() {
                    self.handle_action(action);
                    if !self.running {
                        break;
                    };
                }
            }
        }
    }

    fn handle_action(&mut self, action: Action) {
        use Action::*;

        match action {
            AppendToOutputBuffer { bufid, content } => self
                .buffers
                .write_output_for_buffer(bufid, content, &self.cwd),
            ChangeDirectory { path } => self.change_directory(path),
            CommandMode => self.command_mode(),
            DeleteBuffer { force } => self.delete_buffer(self.buffers.active().id, force),
            EditCommand { cmd } => self.execute_edit_command(&cmd),
            ExecuteDot => self.default_execute_dot(None),
            Exit { force } => self.exit(force),
            ExpandDot => self.expand_current_dot(),
            FindFile => self.find_file(),
            FindRepoFile => self.find_repo_file(),
            FocusBuffer { id } => self.focus_buffer(id),
            JumpListForward => self.jump_forward(),
            JumpListBack => self.jump_backward(),
            LoadDot => self.default_load_dot(),
            MarkClean { bufid } => self.mark_clean(bufid),
            NewEditLogTransaction => self.buffers.active_mut().new_edit_log_transaction(),
            NextBuffer => {
                self.buffers.next();
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Focus(id));
            }
            OpenFile { path } => self.open_file_relative_to_cwd(&path),
            Paste => self.paste_from_clipboard(),
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
            SetViewPort(vp) => {
                self.buffers
                    .active_mut()
                    .set_view_port(vp, self.screen_rows, self.screen_cols)
            }
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

                self.forward_action_to_active_buffer(DotSet(
                    TextObject::Arr(arr),
                    self.screen_rows,
                ));
            }
            RawInput {
                i: Input::Mouse(evt),
            } => self.handle_mouse_event(evt),

            a => self.forward_action_to_active_buffer(a),
        }
    }

    fn jump_forward(&mut self) {
        let maybe_id = self
            .buffers
            .jump_list_forward(self.screen_rows, self.screen_cols);
        if let Some(id) = maybe_id {
            _ = self.tx_fsys.send(LogEvent::Focus(id));
        }
    }

    fn jump_backward(&mut self) {
        let maybe_id = self
            .buffers
            .jump_list_backward(self.screen_rows, self.screen_cols);
        if let Some(id) = maybe_id {
            _ = self.tx_fsys.send(LogEvent::Focus(id));
        }
    }

    fn forward_action_to_active_buffer(&mut self, a: Action) {
        if let Some(o) = self.buffers.active_mut().handle_action(a) {
            match o {
                ActionOutcome::SetStatusMessage(msg) => self.set_status_message(&msg),
                ActionOutcome::SetClipboard(s) => self.set_clipboard(s),
            }
        }
    }

    #[inline]
    fn click_from_button(&mut self, btn: MouseButton, x: usize, y: usize) -> Click {
        let cur = self.buffers.active_mut().cur_from_screen_coords(x, y);

        Click {
            btn,
            selection: Range::from_cursors(cur, cur, false),
        }
    }

    #[inline]
    fn handle_right_or_middle_click(&mut self, is_right: bool, x: usize, y: usize) {
        use MouseButton::*;

        match self.held_click {
            Some(click) => {
                // Mouse chords execute on the click of the second button rather than the release
                if click.btn == Left {
                    if is_right {
                        self.paste_from_clipboard();
                    } else {
                        self.forward_action_to_active_buffer(Action::Delete);
                    }
                } else if (is_right && click.btn == Middle) || (!is_right && click.btn == Right) {
                    self.held_click = None;
                }
            }

            None => {
                let btn = if is_right { Right } else { Middle };
                self.held_click = Some(self.click_from_button(btn, x, y));
            }
        };
    }

    #[inline]
    fn handle_right_or_middle_release(&mut self, is_right: bool, click: Click) {
        if click.selection.start != click.selection.end {
            // In the case where the click selection is a range we Load/Execute it directly.
            // For Middle clicks, if there is also a range dot in the buffer then that is
            // used as an argument to the command being executed.
            if is_right {
                self.buffers.active_mut().dot = Dot::from(click.selection);
                self.default_load_dot();
            } else {
                let dot = self.buffers.active().dot;
                self.buffers.active_mut().dot = Dot::from(click.selection);

                if dot.is_range() {
                    // Execute as if the click selection was dot then reset dot
                    let arg = dot.content(self.buffers.active()).trim().to_string();
                    self.default_execute_dot(Some((dot.as_range(), arg)));
                    self.buffers.active_mut().dot = dot;
                } else {
                    self.default_execute_dot(None);
                }
            }
        } else {
            // In the case where the click selection was a Cur rather than a Range we
            // set the buffer dot to the click location if it is outside of the current buffer
            // dot (and allow smart expand to handle generating the selection) before we Load/Execute
            if !self.buffers.active().dot.contains(&click.selection.start) {
                self.buffers.active_mut().dot = Dot::from(click.selection.start);
            }

            if is_right {
                self.default_load_dot();
            } else {
                self.default_execute_dot(None);
            }
        }
    }

    /// The outcome of a mouse event depends on any prior mouse state that is being held:
    ///   - Left   (click+release):      set Cur Dot at the location of the click
    ///   - Left   (click+hold+release): set Range Dot from click->release with click active
    ///   - Right  (click+release):      Load Dot (expanding current dot from Cur if needed)
    ///   - Right  (click+hold+release): Load click->release
    ///   - Middle (click+release):      Execute Dot (expanding current dot from Cur if needed)
    ///   - Middle (click+hold+release): Execute click->release
    ///
    ///   -> For Execute actions, if the current Dot is a Range then it is used as an
    ///      argument to the command being executed
    ///
    /// The chording behaviour we implement follows that of acme: http://acme.cat-v.org/mouse
    ///   - Hold Left + click Right:    Paste into selection
    ///   - Hold Left + click Middle:   Delete selection (cut)
    ///   - Hold Right + click either:  cancel Load
    ///   - Hold Middle + click either: cancel Execute
    fn handle_mouse_event(&mut self, evt: MouseEvent) {
        use MouseButton::*;

        match evt {
            MouseEvent::Press { b: Left, x, y } => {
                // Left clicking while Right or Middle is held is always a cancel
                if self.held_click.is_some() {
                    self.held_click = None;
                    return;
                }

                let b = self.buffers.active_mut();
                b.set_dot_from_screen_coords(x, y);
                self.held_click = Some(Click {
                    btn: Left,
                    selection: b.dot.as_range(),
                });
            }

            MouseEvent::Press { b: Right, x, y } => self.handle_right_or_middle_click(true, x, y),
            MouseEvent::Press { b: Middle, x, y } => self.handle_right_or_middle_click(false, x, y),

            MouseEvent::Hold { x, y, .. } => {
                if let Some(click) = &mut self.held_click {
                    let cur = self.buffers.active_mut().cur_from_screen_coords(x, y);
                    click.selection.set_active_cursor(cur);

                    if click.btn == Left {
                        self.buffers.active_mut().dot = Dot::from(click.selection);
                    }
                }
            }

            MouseEvent::Press { b: WheelUp, .. } => {
                self.buffers.active_mut().scroll_up(self.screen_rows);
            }

            MouseEvent::Press { b: WheelDown, .. } => {
                self.buffers.active_mut().scroll_down();
            }

            MouseEvent::Release { b, x, y } => {
                if let Some(click) = self.held_click {
                    if click.btn == Left && (b == Right || b == Middle) {
                        return; // paste and cut are handled on click
                    }
                }

                let mut click = match self.held_click.take() {
                    Some(click) => click,
                    None => return,
                };

                let cur = self.buffers.active_mut().cur_from_screen_coords(x, y);
                click.selection.set_active_cursor(cur);

                match click.btn {
                    Right | Middle => {
                        self.handle_right_or_middle_release(click.btn == Right, click)
                    }
                    Left | WheelUp | WheelDown => (),
                }
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
