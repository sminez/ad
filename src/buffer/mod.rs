use crate::{
    editor::Action,
    key::{Arrow, Key},
    term::{Color, Style},
    util::relative_path_from,
    MAX_NAME_LEN, TAB_STOP, UNNAMED_BUFFER,
};
use std::{
    cmp::{max, min},
    fs,
    io::{self, ErrorKind},
    path::{Path, PathBuf},
};

mod buffers;
mod dot;
mod line;
mod minibuffer;

pub(crate) use buffers::Buffers;
pub(crate) use dot::{Cur, Dot, LineRange, Range, TextObject, UpdateDot};
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
    fn display_name(&self, cwd: &Path) -> String {
        match self {
            BufferKind::File(p) => relative_path_from(cwd, p).display().to_string(),
            BufferKind::Virtual(s) => s.clone(),
            BufferKind::Unnamed => UNNAMED_BUFFER.to_string(),
            BufferKind::MiniBuffer => "".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffer {
    id: usize,
    pub(crate) kind: BufferKind,
    pub(crate) dot: Dot,
    pub(crate) lines: Vec<Line>,
    pub(crate) rx: usize,
    pub(crate) row_off: usize,
    pub(crate) col_off: usize,
    pub(crate) dirty: bool,
}

impl Buffer {
    /// As the name implies, this method MUST be called with the full cannonical file path
    pub(super) fn new_from_canonical_file_path(id: usize, path: PathBuf) -> io::Result<Self> {
        let raw = match fs::read_to_string(&path) {
            Ok(contents) => contents,
            Err(e) if e.kind() == ErrorKind::NotFound => String::new(),
            Err(e) => return Err(e),
        };

        let lines: Vec<Line> = raw.lines().map(|s| Line::new(s.to_string())).collect();

        Ok(Self {
            id,
            kind: BufferKind::File(path),
            dot: Dot::default(),
            lines,
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
        })
    }

    pub fn new_unnamed(id: usize) -> Self {
        Self {
            id,
            kind: BufferKind::Unnamed,
            dot: Dot::default(),
            lines: Vec::new(),
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
        }
    }

    pub fn new_virtual(id: usize, name: String) -> Self {
        Self {
            id,
            kind: BufferKind::Virtual(name),
            dot: Dot::default(),
            lines: Vec::new(),
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
        }
    }

    /// Short name for displaying in the status line
    pub fn display_name(&self, cwd: &Path) -> String {
        let s = self.kind.display_name(cwd);

        s[0..min(MAX_NAME_LEN, s.len())].to_string()
    }

    /// Absolute path of full name of a virtual buffer
    pub fn full_name(&self) -> &str {
        match &self.kind {
            BufferKind::File(p) => p.to_str().expect("valid unicode"),
            BufferKind::Virtual(s) => s,
            BufferKind::Unnamed => UNNAMED_BUFFER,
            BufferKind::MiniBuffer => "*mini-buffer*",
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
        let Cur { x, y } = self.dot.active_cur();
        self.rx = self.rx_from_x(y, x);

        if y < self.row_off {
            self.row_off = y;
        }

        if y >= self.row_off + screen_rows {
            self.row_off = y - screen_rows + 1;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols {
            self.col_off = self.rx - screen_cols + 1;
        }
    }

    pub(crate) fn rx_from_x(&self, y: usize, x: usize) -> usize {
        if y >= self.lines.len() {
            return 0;
        }

        let mut rx = 0;
        for c in self.lines[y].raw.chars().take(x) {
            if c == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;
        }

        rx
    }

    pub(crate) fn x_from_rx(&self, y: usize) -> usize {
        if self.lines.is_empty() {
            return 0;
        }

        let mut rx = 0;
        let mut cx = 0;

        for c in self.lines[y].raw.chars() {
            if c == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;

            if rx > self.rx {
                break;
            }
            cx += 1;
        }

        cx
    }

    pub fn line(&self, y: usize) -> Option<&Line> {
        if y >= self.len_lines() {
            None
        } else {
            Some(&self.lines[y])
        }
    }

    /// The render representation of a given line, truncated to fit within the
    /// available screen space.
    /// This includes tab expansion and any styling that might be applied but not
    /// trailing \r\n or screen clearing escape codes.
    pub(crate) fn styled_rline_unchecked(
        &self,
        y: usize,
        lpad: usize,
        screen_cols: usize,
        bg_dot: Color,
    ) -> String {
        // Truncate to available screen width
        let mut rline = self.lines[y].render.clone();
        let mut len = max(0, rline.len() - self.col_off);
        len = min(screen_cols - lpad, len);
        rline = rline[self.col_off..min(screen_cols - lpad, len)].to_string();

        // Apply highlight if included in current Dot
        if let Some(lr) = self.dot.line_range(y) {
            let (start, end) = match lr {
                // LineRange is an inclusive range so we need to insert after `end` if its
                // not the end of the line
                LineRange::Partial { start, end, .. } => (start, min(end + 1, rline.len())),
                LineRange::FromStart { end, .. } => (0, min(end + 1, rline.len())),
                LineRange::ToEnd { start, .. } => (start, rline.len()),
                LineRange::Full { .. } => (0, rline.len()),
            };
            rline.insert_str(end, &Style::Reset.to_string());
            rline.insert_str(start, &Style::Bg(bg_dot).to_string());
        }

        rline
    }

    pub fn handle_action(&mut self, a: Action, screen_rows: usize) {
        match a {
            Action::Move { d } => self.dot = d.set_dot(self),
            Action::Delete => self.delete(),
            Action::InsertChar { c } => self.insert_char(c),
            Action::RawKey { k } => self.handle_raw_key(k, screen_rows),
            Action::DotCollapseFirst => self.dot = self.dot.collapse_to_first_cur(),
            Action::DotCollapseLast => self.dot = self.dot.collapse_to_last_cur(),
            Action::DotFlip => self.dot.flip(),
            Action::DotSet(tobj) => self.dot = tobj.set_dot(self),
            Action::DotExtendForward(tobj) => self.dot = tobj.extend_dot_forward(self),
            Action::DotExtendBackward(tobj) => self.dot = tobj.extend_dot_backward(self),

            _ => (),
        }
    }

    fn handle_raw_key(&mut self, k: Key, screen_rows: usize) {
        match k {
            Key::Return => self.insert_char('\n'),
            Key::Tab => self.insert_char('\t'),
            Key::Char(c) => self.insert_char(c),

            Key::Arrow(arr) => self.dot = arr.set_dot(self),
            Key::PageUp | Key::PageDown => {
                let arr = if k == Key::PageUp {
                    Arrow::Up
                } else {
                    Arrow::Down
                };

                for _ in 0..screen_rows {
                    self.dot = arr.set_dot(self);
                }
            }

            _ => (),
        }
    }

    /// ch is inserted based on the current dot
    fn insert_char(&mut self, ch: char) {
        if self.dot.last_cur().y == self.lines.len() {
            self.insert_line(self.lines.len(), String::new());
        }

        let c = match self.dot {
            Dot::Cur { c } => self.insert_char_handling_newline(c, ch),
            Dot::Range { r } => {
                let c = self.delete_range(r);
                self.insert_char_handling_newline(c, ch)
            }
        };

        self.dot = Dot::Cur { c };
        self.dirty = true;
    }

    fn insert_char_handling_newline(&mut self, mut cur: Cur, ch: char) -> Cur {
        if ch == '\n' {
            if cur.x == 0 {
                self.insert_line(cur.y, String::new());
            } else {
                let (l1, l2) = self.lines[cur.y].raw.split_at(cur.x);
                let (l1, l2) = (l1.to_string(), l2.to_string());
                self.lines[cur.y].modify(|s| *s = l1.clone());
                self.insert_line(cur.y + 1, l2);
            }

            cur.y += 1;
            cur.x = 0;
        } else {
            self.lines[cur.y].modify(|s| s.insert(cur.x, ch));
            cur.x += 1;
        }

        cur
    }

    fn insert_line(&mut self, at: usize, line: String) {
        if at <= self.len_lines() {
            self.lines.insert(at, Line::new(line));
            self.dirty = true;
        }
    }

    fn delete(&mut self) {
        let c = match self.dot {
            Dot::Cur { c } => self.delete_cur(c),
            Dot::Range { r } => self.delete_range(r),
        };

        self.dot = Dot::Cur { c };
        self.dirty = true;
    }

    fn delete_cur(&mut self, cur: Cur) -> Cur {
        if cur.y == self.len_lines() || (cur.x == 0 && cur.y == 0) {
            return cur;
        }

        if cur.x < self.lines[cur.y].len() {
            self.lines[cur.y].modify(|s| {
                s.remove(cur.x);
            });
        } else if cur.y < self.lines.len() - 1 {
            // Deleting the newline char at the end of this line
            let line = self.lines.remove(cur.y + 1);
            self.lines[cur.y].modify(|s| s.push_str(&line.raw));
        }

        self.dirty = true;
        cur
    }

    /// Delete all LineRanges from the given range in reverse order so we
    /// don't invalidate line offsets
    fn delete_range(&mut self, r: Range) -> Cur {
        let mut had_trailing_chars = false;

        for lr in r.line_ranges().into_iter().rev() {
            match lr {
                lr if lr.is_full_line(self) => {
                    self.lines.remove(lr.y());
                }
                LineRange::Full { y } => {
                    self.lines.remove(y);
                }
                LineRange::ToEnd { y, start } => self.lines[y].modify(|s| s.truncate(start)),
                LineRange::FromStart { y, end } => {
                    self.lines[y].modify(|s| {
                        let _: String = s.drain(..=end).collect();
                    });
                    had_trailing_chars = true;
                }
                LineRange::Partial { y, start, end } => {
                    self.lines[y].modify(|s| {
                        let _: String = s.drain(start..=end).collect();
                    });
                }
            }
        }

        if had_trailing_chars {
            let line = self.lines.remove(r.start.y + 1);
            self.lines[r.start.y].modify(|s| s.push_str(&line.raw));
        }

        r.start
    }
}
