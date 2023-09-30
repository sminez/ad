//! Fetching and parsing input from the user
use crate::{die, editor::Editor, key::Key};
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
}
