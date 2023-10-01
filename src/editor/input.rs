//! Fetching and parsing input from the user
use crate::{editor::Editor, key::Key, term::win_size_changed};
use std::io::Read;

impl Editor {
    #[inline]
    pub fn block_for_key(&mut self) -> Key {
        loop {
            // We need to check for size updates inside of this loop so that we don't have
            // to wait for user input to respond to a resize event
            if win_size_changed() {
                self.update_window_size();
                self.refresh_screen();
            }

            if let Some(key) = self.try_read_key() {
                return key;
            }
        }
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

    pub fn try_read_key(&mut self) -> Option<Key> {
        let c = self.try_read_char()?;

        match Key::from_char(c) {
            Key::Esc => (),
            key => return Some(key),
        }

        let c2 = match self.try_read_char() {
            Some(c2) => c2,
            None => return Some(Key::Esc),
        };
        let c3 = match self.try_read_char() {
            Some(c3) => c3,
            None => return Some(Key::try_from_seq2(c, c2).unwrap_or(Key::Esc)),
        };

        if let Some(key) = Key::try_from_seq2(c2, c3) {
            return Some(key);
        }

        if c2 == '[' && c3.is_ascii_digit() {
            if let Some('~') = self.try_read_char() {
                if let Some(key) = Key::try_from_bracket_tilde(c3) {
                    return Some(key);
                }
            }
        }

        Some(Key::Esc)
    }
}
