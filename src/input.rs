//! Fetching and parsing input from the user
use crate::{
    editor::Action,
    fsys::Message,
    key::{Input, MouseEvent},
    term::win_size_changed,
};
use std::{
    io::{stdin, Read, Stdin},
    sync::mpsc::Sender,
    thread::{spawn, JoinHandle},
};

/// An input event that can be processed by the editor event loop
#[derive(Debug)]
pub enum Event {
    /// A [Message] received from the virtual filesystem interface
    Message(Message),
    /// An [Input] from the user
    Input(Input),
    /// An [Action] for the event loop to handle
    Action(Action),
    /// A signal that our window size has changed
    WinsizeChanged,
}

/// A tui input handle that parses stdin and emits [Event]s to the main editor event loop.
pub(super) struct StdinInput {
    stdin: Stdin,
    tx: Sender<Event>,
}

impl StdinInput {
    pub(super) fn new(tx: Sender<Event>) -> Self {
        Self { stdin: stdin(), tx }
    }

    pub fn run_threaded(mut self) -> JoinHandle<()> {
        spawn(move || loop {
            if let Some(key) = self.try_read_input() {
                _ = self.tx.send(Event::Input(key));
            } else if win_size_changed() {
                _ = self.tx.send(Event::WinsizeChanged);
            }
        })
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

    pub fn try_read_input(&mut self) -> Option<Input> {
        let c = self.try_read_char()?;

        // Normal key press
        match Input::from_char(c) {
            Input::Esc => (),
            key => return Some(key),
        }

        let c2 = match self.try_read_char() {
            Some(c2) => c2,
            None => return Some(Input::Esc),
        };
        let c3 = match self.try_read_char() {
            Some(c3) => c3,
            None => return Some(Input::try_from_seq2(c, c2).unwrap_or(Input::Esc)),
        };

        if let Some(key) = Input::try_from_seq2(c2, c3) {
            return Some(key);
        }

        if c2 == '[' && c3.is_ascii_digit() {
            if let Some('~') = self.try_read_char() {
                if let Some(key) = Input::try_from_bracket_tilde(c3) {
                    return Some(key);
                }
            }
        }

        // xterm mouse encoding: "^[< Cb;Cx;Cy(;) (M or m) "
        if c2 == '[' && c3 == '<' {
            let mut buf = Vec::new();
            let m;

            loop {
                match self.try_read_char() {
                    Some(c @ 'm' | c @ 'M') => {
                        m = c;
                        break;
                    }
                    Some(c) => buf.push(c as u8),
                    None => return None,
                };
            }
            let s = String::from_utf8(buf).unwrap();
            let nums: Vec<usize> = s.split(';').map(|s| s.parse::<usize>().unwrap()).collect();
            let (b, x, y) = (nums[0], nums[1], nums[2]);

            return MouseEvent::try_from_raw(b, x, y, m).map(Input::Mouse);
        }

        Some(Input::Esc)
    }
}
