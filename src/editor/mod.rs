use crate::{
    buffer::Buffers,
    key::Key,
    mode::{modes, Mode},
    term::{enable_raw_mode, get_termios, get_termsize, set_termios},
};
use libc::termios as Termios;
use std::{
    io::{self, Stdin, Stdout},
    time::Instant,
};

mod actions;
mod input;
mod render;

pub use actions::Action;

pub struct Editor {
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    stdin: Stdin,
    original_termios: Termios,
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
        let (screen_rows, screen_cols) = get_termsize();
        let original_termios = get_termios();

        enable_raw_mode(original_termios);

        Self {
            screen_rows: screen_rows - 2, // stats+msg bars
            screen_cols,
            stdout: io::stdout(),
            stdin: io::stdin(),
            original_termios,
            running: true,
            status_message: String::new(),
            status_time: Instant::now(),
            modes: modes(),
            pending_keys: Vec::new(),
            buffers: Buffers::new(),
        }
    }

    #[inline]
    pub fn running(&self) -> bool {
        self.running
    }

    pub(crate) fn screen_rowcol(&self) -> (usize, usize) {
        (self.screen_rows, self.screen_cols)
    }

    pub fn set_status_message(&mut self, msg: &str) {
        self.status_message.clear();
        self.status_message.push_str(msg);
        self.status_time = Instant::now();
    }

    pub fn handle_keypress(&mut self, k: Key) -> io::Result<()> {
        self.pending_keys.push(k);

        match self.modes[0].handle_keys(&mut self.pending_keys) {
            Some(actions) => self.handle_actions(actions),
            None => Ok(()),
        }
    }

    fn handle_actions(&mut self, actions: Vec<Action>) -> io::Result<()> {
        for action in actions.into_iter() {
            match action {
                Action::SaveBuffer => self.save_current_buffer()?,
                Action::SetMode(name) => self.set_mode(name)?,
                Action::Exit => self.exit(false)?,
                Action::ForceExit => self.exit(true)?,

                a => self
                    .buffers
                    .active_mut()
                    .handle_action(a, self.screen_rows)?,
            }

            if !self.running {
                break;
            };
        }

        Ok(())
    }
}
