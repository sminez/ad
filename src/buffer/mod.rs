use crate::{
    editor::Action,
    key::{Arrow, Key},
    MAX_NAME_LEN, TAB_STOP, UNNAMED_BUFFER,
};
use std::{
    cmp::{min, Ordering},
    fs,
    io::{self, ErrorKind},
    path::{Path, PathBuf},
};

mod buffers;
mod line;
mod minibuffer;

pub(crate) use buffers::Buffers;
pub(crate) use line::Line;
pub(crate) use minibuffer::{MiniBuffer, MiniBufferSelection, MiniBufferState};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BufferKind {
    File(PathBuf),
    Virtual(String),
    Unnamed,
    MiniBuffer,
}

impl Default for BufferKind {
    fn default() -> Self {
        Self::Unnamed
    }
}

impl BufferKind {
    fn display_name(&self) -> &str {
        match self {
            BufferKind::File(p) => p
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or(UNNAMED_BUFFER),
            BufferKind::Virtual(s) => s.as_str(),
            BufferKind::Unnamed => UNNAMED_BUFFER,
            BufferKind::MiniBuffer => "",
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Buffer {
    pub(crate) kind: BufferKind,
    pub(crate) lines: Vec<Line>,
    pub(crate) cx: usize,
    pub(crate) cy: usize,
    pub(crate) rx: usize,
    pub(crate) row_off: usize,
    pub(crate) col_off: usize,
    pub(crate) dirty: bool,
}

impl Buffer {
    pub fn new_from_file(path: &str) -> io::Result<Self> {
        let raw = match fs::read_to_string(path) {
            Ok(contents) => contents,
            Err(e) if e.kind() == ErrorKind::NotFound => String::new(),
            Err(e) => return Err(e),
        };

        let lines: Vec<Line> = raw.lines().map(|s| Line::new(s.to_string())).collect();

        Ok(Self {
            kind: BufferKind::File(PathBuf::from(path)),
            lines,
            cx: 0,
            cy: 0,
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
        })
    }

    pub fn new_virtual(name: String) -> Self {
        Self {
            kind: BufferKind::Virtual(name),
            lines: Vec::new(),
            cx: 0,
            cy: 0,
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
        }
    }

    pub fn display_name(&self) -> &str {
        let s = self.kind.display_name();

        &s[0..min(MAX_NAME_LEN, s.len())]
    }

    pub fn file_path(&self) -> Option<&Path> {
        match &self.kind {
            BufferKind::File(p) => Some(p),
            _ => None,
        }
    }

    pub fn is_unnamed(&self) -> bool {
        self.kind == BufferKind::Unnamed
    }

    pub fn contents(&self) -> String {
        let mut s = String::new();
        for line in self.lines.iter() {
            s.push_str(&line.raw);
            s.push('\n');
        }

        s
    }

    #[inline]
    pub fn len_lines(&self) -> usize {
        self.lines.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    pub fn clamp_scroll(&mut self, screen_rows: usize, screen_cols: usize) {
        self.rx = 0;
        if self.cy < self.lines.len() {
            self.update_rx();
        }

        if self.cy < self.row_off {
            self.row_off = self.cy;
        }

        if self.cy >= self.row_off + screen_rows {
            self.row_off = self.cy - screen_rows + 1;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols {
            self.col_off = self.rx - screen_cols + 1;
        }
    }

    fn update_rx(&mut self) {
        let mut rx = 0;

        for c in self.lines[self.cy].raw.chars().take(self.cx) {
            if c == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;
        }

        self.rx = rx;
    }

    fn set_cx_from_rx(&mut self, cur_rx: usize) {
        if self.lines.is_empty() {
            self.cx = 0;
            return;
        }

        let mut rx = 0;
        let mut cx = 0;

        for c in self.lines[self.cy].raw.chars() {
            if c == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;

            if rx > cur_rx {
                break;
            }
            cx += 1;
        }

        self.cx = cx;
    }

    pub fn clamp_cx(&mut self) {
        let len = if self.cy >= self.len_lines() {
            0
        } else {
            self.lines[self.cy].len()
        };

        if self.cx > len {
            self.cx = len;
        }
    }

    pub fn current_line(&self) -> Option<&Line> {
        if self.cy >= self.len_lines() {
            None
        } else {
            Some(&self.lines[self.cy])
        }
    }

    pub fn handle_action(&mut self, a: Action, screen_rows: usize) -> io::Result<()> {
        match a {
            Action::Move { d, n } => self.move_cursor(d, n),
            Action::DeleteChar => self.delete_char(),
            Action::InsertLine => self.insert_line(self.cy + 1, "".to_string()),
            Action::RawKey { k } => self.handle_raw_key(k, screen_rows)?,

            _ => (),
        }

        Ok(())
    }

    fn handle_raw_key(&mut self, k: Key, screen_rows: usize) -> io::Result<()> {
        match k {
            Key::Arrow(arr) => self.move_cursor(arr, 1),
            Key::Home => self.cx = 0,
            Key::End => {
                if self.cy < self.lines.len() {
                    self.cx = self.lines[self.cy].len();
                }
            }
            Key::PageUp | Key::PageDown => {
                let arr = if k == Key::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                self.move_cursor(arr, screen_rows);
            }
            Key::Return => self.insert_newline(),
            Key::Tab => self.insert_char('\t'),
            Key::Char(c) => self.insert_char(c),

            _ => (),
        }

        Ok(())
    }

    fn move_cursor(&mut self, arr: Arrow, count: usize) {
        for _ in 0..count {
            match arr {
                Arrow::Up => {
                    if self.cy != 0 {
                        self.cy -= 1;
                        self.set_cx_from_rx(self.rx);
                    }
                }
                Arrow::Down => {
                    if !self.lines.is_empty() && self.cy < self.lines.len() - 1 {
                        self.cy += 1;
                        self.set_cx_from_rx(self.rx);
                    }
                }
                Arrow::Left => {
                    if self.cx != 0 {
                        self.cx -= 1;
                    } else if self.cy > 0 {
                        // Allow <- to move to the end of the previous line
                        self.cy -= 1;
                        self.cx = self.lines[self.cy].len();
                    }
                }
                Arrow::Right => {
                    if let Some(line) = self.current_line() {
                        match self.cx.cmp(&line.len()) {
                            Ordering::Less => self.cx += 1,
                            Ordering::Equal => {
                                // Allow -> to move to the start of the next line
                                self.cy += 1;
                                self.cx = 0;
                            }
                            _ => (),
                        }
                    }
                }
            }
        }

        self.clamp_cx();
    }

    fn insert_char(&mut self, c: char) {
        if self.cy == self.lines.len() {
            self.insert_line(self.lines.len(), String::new());
        }

        self.lines[self.cy].modify(|s| s.insert(self.cx, c));
        self.cx += 1;
        self.dirty = true;
    }

    fn insert_line(&mut self, at: usize, line: String) {
        if at <= self.len_lines() {
            self.lines.insert(at, Line::new(line));
            self.dirty = true;
        }
    }

    fn insert_newline(&mut self) {
        if self.cx == 0 {
            self.insert_line(self.cy, String::new());
        } else {
            let (cur, nxt) = self.lines[self.cy].raw.split_at(self.cx);
            let (cur, nxt) = (cur.to_string(), nxt.to_string());
            self.lines[self.cy].modify(|s| *s = cur.clone());
            self.insert_line(self.cy + 1, nxt);
        }

        self.cy += 1;
        self.cx = 0;
        self.dirty = true;
    }

    fn delete_char(&mut self) {
        if self.cy == self.len_lines() || (self.cx == 0 && self.cy == 0) {
            return;
        }

        if self.cx > 0 {
            self.lines[self.cy].modify(|s| {
                s.remove(self.cx - 1);
            });
            self.cx -= 1;
        } else {
            self.cx = self.lines[self.cy - 1].len();
            let line = self.lines.remove(self.cy);
            self.lines[self.cy - 1].modify(|s| s.push_str(&line.raw));
            self.cy -= 1;
        }

        self.dirty = true;
    }
}
