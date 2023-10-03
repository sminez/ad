use crate::{
    buffer::Buffers,
    die,
    key::Key,
    mode::{modes, Mode},
    term::{
        clear_screen, enable_raw_mode, get_termios, get_termsize, register_signal_handler,
        set_termios, win_size_changed,
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

        Self {
            screen_rows: 0,
            screen_cols: 0,
            stdout: io::stdout(),
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

    #[inline]
    fn handle_action(&mut self, action: Action) {
        match action {
            Action::ChangeDirectory { path } => self.change_directory(path),
            Action::CommandMode => self.command_mode(),
            Action::DeleteBuffer { force } => self.delete_current_buffer(force),
            Action::Exit { force } => self.exit(force),
            Action::NextBuffer => self.buffers.next(),
            Action::OpenFile { path } => self.open_file(&path),
            Action::Paste => self.paste(),
            Action::PreviousBuffer => self.buffers.previous(),
            Action::SaveBufferAs { path } => self.save_current_buffer(Some(path)),
            Action::SaveBuffer => self.save_current_buffer(None),
            Action::SearchInCurrentBuffer => self.search_in_current_buffer(),
            Action::SelectBuffer => self.select_buffer(),
            Action::SetMode { m } => self.set_mode(m),
            Action::ShellPipe { cmd } => self.pipe_dot_through_shell_cmd(&cmd),
            Action::ShellReplace { cmd } => self.replace_dot_with_shell_cmd(&cmd),
            Action::Yank => self.yank(),

            Action::DebugBufferContents => self.debug_buffer_contents(),

            a => self.buffers.active_mut().handle_action(a, self.screen_rows),
        }
    }
}
