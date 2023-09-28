//! Fetching and parsing input from the user
use crate::{
    die,
    editor::Editor,
    key::{Arrow, Key},
};
use std::io::{ErrorKind, Read};

impl Editor {
    #[inline]
    fn read_char(&mut self) -> char {
        let mut buf: [u8; 1] = [0; 1];
        loop {
            match self.stdin.read_exact(&mut buf) {
                Ok(_) => break,
                Err(e) if e.kind() == ErrorKind::UnexpectedEof => continue,
                Err(e) => die!("read: {e}"),
            }
        }

        buf[0] as char
    }

    #[inline]
    fn try_read_char(&mut self) -> Option<char> {
        let mut buf: [u8; 1] = [0; 1];
        let res = self.stdin.read_exact(&mut buf);
        if res.is_ok() {
            Some(buf[0] as char)
        } else {
            None
        }
    }

    pub fn read_key(&mut self) -> Key {
        let c = self.read_char();

        match Key::from_char(c) {
            Key::Esc => (),
            key => return key,
        }

        let c2 = match self.try_read_char() {
            Some(c2) => c2,
            None => return Key::Esc,
        };
        let c3 = match self.try_read_char() {
            Some(c3) => c3,
            None => return Key::try_from_seq2(c, c2).unwrap_or(Key::Esc),
        };

        if let Some(key) = Key::try_from_seq2(c2, c3) {
            return key;
        }

        if c2 == '[' && c3.is_ascii_digit() {
            if let Some('~') = self.try_read_char() {
                if let Some(key) = Key::try_from_bracket_tilde(c3) {
                    return key;
                }
            }
        }

        Key::Esc
    }

    /// Prompt the user for input supporting cancellation
    pub(super) fn prompt(&mut self, prompt: &str) -> Option<String> {
        let mut input = String::new();
        let mut x = 0;
        let offset = prompt.len();

        loop {
            self.set_status_message(&format!("{prompt}{input}"));
            self.buffers.active_mut().rx = x;
            self.refresh_screen_w_cursor(Some((x + offset, self.screen_rows + 1)));

            match self.read_key() {
                Key::Char(c) => {
                    input.insert(x, c);
                    x += 1;
                }
                Key::Ctrl('h') | Key::Backspace | Key::Del => {
                    if x <= input.len() {
                        input.remove(x - 1);
                        x = x.saturating_sub(1);
                    }
                }
                Key::Return => {
                    self.set_status_message("");
                    return Some(input);
                }
                Key::Esc => {
                    self.set_status_message("");
                    return None;
                }
                Key::Arrow(Arrow::Right) => {
                    if x < input.len() {
                        x += 1;
                    }
                }
                Key::Arrow(Arrow::Left) => {
                    x = x.saturating_sub(1);
                }

                _ => (),
            }
        }
    }
}
