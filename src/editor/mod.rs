use crate::{
    buffer::{parse_dot::DotExpression, ActionOutcome, Buffer, Buffers, Cur, Dot, TextObject},
    config::Config,
    die,
    exec::IterableStream,
    fsys::{AdFs, BufId, Message, Req},
    key::{Arrow, Key, MouseButton, MouseEvent},
    mode::{modes, Mode},
    restore_terminal_state, set_config,
    term::{
        clear_screen, enable_alternate_screen, enable_mouse_support, enable_raw_mode, get_termios,
        get_termsize, register_signal_handler,
    },
    ORIGINAL_TERMIOS,
};
use std::{
    env,
    io::{self, Stdout},
    path::PathBuf,
    sync::mpsc::{channel, Receiver, Sender},
    time::Instant,
};

mod actions;
mod commands;
mod input;
mod render;

pub use actions::{Action, Actions, ViewPort};
use input::Input;
pub use input::InputEvent;

pub struct Editor {
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    cwd: PathBuf,
    running: bool,
    status_message: String,
    status_time: Instant,
    modes: Vec<Mode>,
    pending_keys: Vec<Key>,
    buffers: Buffers,
    rx: Receiver<InputEvent>,
    btx: Sender<BufId>,
    fs_chans: Option<(Sender<InputEvent>, Receiver<BufId>)>,
}

impl Drop for Editor {
    fn drop(&mut self) {
        restore_terminal_state(&mut self.stdout);
    }
}

impl Editor {
    #[allow(clippy::new_without_default)]
    pub fn new(cfg: Config) -> Self {
        let original_termios = get_termios();
        let cwd = match env::current_dir() {
            Ok(cwd) => cwd,
            Err(e) => die!("Unable to determine working directory: {e}"),
        };

        enable_raw_mode(original_termios);
        _ = ORIGINAL_TERMIOS.set(original_termios);

        let mut stdout = io::stdout();
        enable_mouse_support(&mut stdout);
        enable_alternate_screen(&mut stdout);

        let (tx, rx) = channel();
        let (btx, brx) = channel();

        set_config(cfg);

        Self {
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
            rx,
            btx,
            fs_chans: Some((tx, brx)),
        }
    }

    /// Update the stored window size, accounting for the status and message bars
    /// This will panic if the available screen rows are 0 or 1
    fn update_window_size(&mut self) {
        let (screen_rows, screen_cols) = get_termsize();
        self.screen_rows = screen_rows - 2;
        self.screen_cols = screen_cols;
    }

    pub fn run(mut self) {
        register_signal_handler();
        self.update_window_size();

        let (tx, brx) = self.fs_chans.take().expect("to have fsys channels");
        Input::new(tx.clone()).run_threaded();
        AdFs::new(tx, brx).run_threaded();

        self.run_event_loop();
        clear_screen(&mut self.stdout);
    }

    fn run_event_loop(&mut self) {
        while self.running {
            self.refresh_screen();

            match self.rx.recv().unwrap() {
                InputEvent::KeyPress(k) => self.handle_keypress(k),
                InputEvent::Message(msg) => self.handle_message(msg),
                InputEvent::WinsizeChanged => self.update_window_size(),
            }
        }
    }

    pub(crate) fn screen_rowcol(&self) -> (usize, usize) {
        (self.screen_rows, self.screen_cols)
    }

    pub fn set_status_message(&mut self, msg: &str) {
        self.status_message.clear();
        self.status_message.push_str(msg);
        self.status_time = Instant::now();
    }

    pub(crate) fn block_for_key(&mut self) -> Key {
        loop {
            match self.rx.recv().unwrap() {
                InputEvent::KeyPress(k) => return k,
                InputEvent::Message(msg) => self.handle_message(msg),
                InputEvent::WinsizeChanged => self.update_window_size(),
            }
        }
    }

    fn send_buffer_resp(
        &self,
        id: usize,
        tx: Sender<Result<String, String>>,
        f: fn(&Buffer) -> String,
    ) {
        match self.buffers.with_id(id) {
            Some(b) => tx.send(Ok((f)(b))).unwrap(),
            None => {
                tx.send(Err("unknown buffer".to_string())).unwrap();
                self.btx.send(BufId::Remove(id)).unwrap();
            }
        }
    }
    fn handle_buffer_mutation<F: FnOnce(&mut Buffer, String) -> Option<ActionOutcome>>(
        &mut self,
        id: usize,
        tx: Sender<Result<String, String>>,
        s: String,
        f: F,
    ) {
        match self.buffers.with_id_mut(id) {
            Some(b) => {
                if let Some(o) = (f)(b, s) {
                    match o {
                        ActionOutcome::SetStatusMessag(msg) => self.set_status_message(&msg),
                        ActionOutcome::SetClipboard(s) => self.set_clipboard(s),
                    }
                };

                tx.send(Ok("handled".to_string())).unwrap()
            }

            None => {
                tx.send(Err("unknown buffer".to_string())).unwrap();
                self.btx.send(BufId::Remove(id)).unwrap();
            }
        }
    }

    fn handle_message(&mut self, Message { req, tx }: Message) {
        use Req::*;

        match req {
            ControlMessage { msg } => {
                self.execute_command(&msg);
                tx.send(Ok("handled".to_string())).unwrap();
            }

            ReadBufferName { id } => self.send_buffer_resp(id, tx, |b| b.full_name().to_string()),
            ReadBufferDot { id } => self.send_buffer_resp(id, tx, |b| b.dot_contents()),
            ReadBufferAddr { id } => self.send_buffer_resp(id, tx, |b| b.addr()),
            ReadBufferBody { id } => self.send_buffer_resp(id, tx, |b| b.str_contents()),

            SetBufferDot { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                b.handle_action(Action::InsertString { s })
            }),

            SetBufferAddr { id, s } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                if let Ok(mut expr) = DotExpression::parse(&mut s.trim_end().chars().peekable()) {
                    b.dot = b.map_dot_expr(&mut expr);
                };

                None
            }),

            ClearBufferBody { id } => self.handle_buffer_mutation(id, tx, String::new(), |b, _| {
                b.handle_action(Action::DotSet(TextObject::BufferStart, 1));
                b.handle_action(Action::DotExtendForward(TextObject::BufferEnd, 1));
                b.handle_action(Action::Delete)
            }),

            InsertBufferBody { id, s, offset } => self.handle_buffer_mutation(id, tx, s, |b, s| {
                let idx = b.txt.byte_to_char(offset);
                b.dot = Dot::Cur { c: Cur { idx } };
                b.handle_action(Action::InsertString { s })
            }),
        }
    }

    fn handle_keypress(&mut self, k: Key) {
        self.pending_keys.push(k);

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
        match action {
            Action::SetViewPort(vp) => {
                self.buffers
                    .active_mut()
                    .view_port(vp, self.screen_rows, self.screen_cols)
            }
            Action::ChangeDirectory { path } => self.change_directory(path),
            Action::CommandMode => self.command_mode(),
            Action::DeleteBuffer { force } => self.delete_current_buffer(force),
            Action::EditCommand { cmd } => self.execute_edit_command(&cmd),
            Action::Exit { force } => self.exit(force),
            Action::NextBuffer => self.buffers.next(),
            Action::OpenFile { path } => self.open_file(&path),
            Action::Paste => self.paste_from_clipboard(),
            Action::PreviousBuffer => self.buffers.previous(),
            Action::ReloadActiveBuffer => self.reload_active_buffer(),
            Action::ReloadBuffer { id } => self.reload_buffer(id),
            Action::ReloadConfig => self.reload_config(),
            Action::SamMode => self.sam_mode(),
            Action::SaveBufferAs { path } => self.save_current_buffer(Some(path)),
            Action::SaveBuffer => self.save_current_buffer(None),
            Action::SearchInCurrentBuffer => self.search_in_current_buffer(),
            Action::SelectBuffer => self.select_buffer(),
            Action::SetConfigProp { input } => self.set_config_prop(&input),
            Action::SetMode { m } => self.set_mode(m),
            Action::ShellPipe { cmd } => self.pipe_dot_through_shell_cmd(&cmd),
            Action::ShellReplace { cmd } => self.replace_dot_with_shell_cmd(&cmd),
            Action::Yank => self.set_clipboard(self.buffers.active().dot_contents()),

            Action::DebugBufferContents => self.debug_buffer_contents(),
            Action::DebugEditLog => self.debug_edit_log(),

            Action::RawKey { k } if k == Key::PageUp || k == Key::PageDown => {
                let arr = if k == Key::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                self.forward_action_to_active_buffer(Action::DotSet(
                    TextObject::Arr(arr),
                    self.screen_rows,
                ));
            }

            Action::RawKey { k: Key::Mouse(evt) } => self.handle_mouse_event(evt),

            a => self.forward_action_to_active_buffer(a),
        }
    }

    fn forward_action_to_active_buffer(&mut self, a: Action) {
        if let Some(o) = self.buffers.active_mut().handle_action(a) {
            match o {
                ActionOutcome::SetStatusMessag(msg) => self.set_status_message(&msg),
                ActionOutcome::SetClipboard(s) => self.set_clipboard(s),
            }
        }
    }

    fn handle_mouse_event(&mut self, evt: MouseEvent) {
        use MouseButton::*;

        match evt {
            MouseEvent::Press { b: Left, x, y } => {
                self.buffers
                    .active_mut()
                    .set_dot_from_screen_coords(x, y, self.screen_rows);
            }

            MouseEvent::Press { b: Right, x, y } => {
                self.buffers
                    .active_mut()
                    .set_dot_from_screen_coords(x, y, self.screen_rows);
                self.buffers.active_mut().handle_action(Action::LoadDot);
            }

            MouseEvent::Hold { x, y } => {
                self.buffers
                    .active_mut()
                    .extend_dot_to_screen_coords(x, y, self.screen_rows);
            }

            MouseEvent::Press { b: WheelUp, .. } => {
                self.buffers.active_mut().scroll_up(self.screen_rows);
            }
            MouseEvent::Press { b: WheelDown, .. } => {
                self.buffers.active_mut().scroll_down();
            }

            _ => (),
        }
    }
}
