use crate::{
    buffer::{ActionOutcome, Buffers, TextObject},
    die,
    key::{Arrow, Key, MouseButton, MouseEvent},
    mode::{modes, Mode},
    term::{
        clear_screen, disable_mouse_support, enable_mouse_support, enable_raw_mode, get_termios,
        get_termsize, register_signal_handler, set_termios, win_size_changed,
    },
};
use libc::termios as Termios;
use std::{
    env,
    io::{self, Stdin, Stdout},
    path::PathBuf,
    time::Instant,
};

mod actions;
mod commands;
mod input;
mod render;

pub use actions::{Action, Actions};

pub struct Editor {
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    stdin: Stdin,
    original_termios: Termios,
    cwd: PathBuf,
    running: bool,
    status_message: String,
    status_time: Instant,
    modes: Vec<Mode>,
    pending_keys: Vec<Key>,
    buffers: Buffers,
}

impl Drop for Editor {
    fn drop(&mut self) {
        disable_mouse_support(&mut self.stdout);
        set_termios(self.original_termios)
    }
}

impl Editor {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let original_termios = get_termios();
        let cwd = match env::current_dir() {
            Ok(cwd) => cwd,
            Err(e) => die!("Unable to determine working directory: {e}"),
        };

        enable_raw_mode(original_termios);

        let mut stdout = io::stdout();
        enable_mouse_support(&mut stdout);

        Self {
            screen_rows: 0,
            screen_cols: 0,
            stdout,
            stdin: io::stdin(),
            original_termios,
            cwd,
            running: true,
            status_message: String::new(),
            status_time: Instant::now(),
            modes: modes(),
            pending_keys: Vec::new(),
            buffers: Buffers::new(),
        }
    }

    /// Update the stored window size, accounting for the status and message bars
    /// This will panic if the available screen rows are 0 or 1
    fn update_window_size(&mut self) {
        let (screen_rows, screen_cols) = get_termsize();
        self.screen_rows = screen_rows - 2;
        self.screen_cols = screen_cols;
    }

    pub fn run(&mut self) {
        register_signal_handler();
        self.update_window_size();

        while self.running {
            self.refresh_screen();
            if let Some(k) = self.try_read_key() {
                self.handle_keypress(k);
            }

            if win_size_changed() {
                self.update_window_size();
            }
        }

        clear_screen(&mut self.stdout);
    }

    pub(crate) fn screen_rowcol(&self) -> (usize, usize) {
        (self.screen_rows, self.screen_cols)
    }

    pub fn set_status_message(&mut self, msg: &str) {
        self.status_message.clear();
        self.status_message.push_str(msg);
        self.status_time = Instant::now();
    }

    pub fn handle_keypress(&mut self, k: Key) {
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
            Action::ChangeDirectory { path } => self.change_directory(path),
            Action::CommandMode => self.command_mode(),
            Action::DeleteBuffer { force } => self.delete_current_buffer(force),
            Action::Exit { force } => self.exit(force),
            Action::NextBuffer => self.buffers.next(),
            Action::OpenFile { path } => self.open_file(&path),
            Action::Paste => self.paste_from_clipboard(),
            Action::PreviousBuffer => self.buffers.previous(),
            Action::SamMode => self.sam_mode(),
            Action::SaveBufferAs { path } => self.save_current_buffer(Some(path)),
            Action::SaveBuffer => self.save_current_buffer(None),
            Action::SearchInCurrentBuffer => self.search_in_current_buffer(),
            Action::SelectBuffer => self.select_buffer(),
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
