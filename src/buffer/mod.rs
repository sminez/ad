use crate::{
    editor::Action,
    key::{Arrow, Key},
    term::{Color, Style},
    util::{relative_path_from, set_clipboard},
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
mod edit;
mod line;
mod minibuffer;

use edit::{Edit, EditLog, Kind, Txt};

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
    edit_log: EditLog,
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
            edit_log: EditLog::default(),
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
            edit_log: EditLog::default(),
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
            edit_log: EditLog::default(),
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

    pub fn dot_contents(&self) -> String {
        self.dot.content(self)
    }

    #[inline]
    pub fn len_lines(&self) -> usize {
        self.lines.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    pub(crate) fn debug_edit_log(&self) -> Vec<Line> {
        self.edit_log
            .debug_edits()
            .into_iter()
            .map(Line::new)
            .collect()
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

    /// The error result of this function is an error string that should be displayed to the user
    pub fn handle_action(&mut self, a: Action, screen_rows: usize) -> Result<(), String> {
        match a {
            Action::Delete => {
                self.dot = Dot::Cur {
                    c: self.delete_dot(self.dot),
                }
            }
            Action::InsertChar { c } => {
                self.dot = Dot::Cur {
                    c: self.insert_char(self.dot, c),
                }
            }
            Action::InsertString { s } => {
                self.dot = Dot::Cur {
                    c: self.insert_string(self.dot, s),
                }
            }

            Action::Redo => self.redo()?,
            Action::Undo => self.undo()?,

            Action::DotCollapseFirst => self.dot = self.dot.collapse_to_first_cur(),
            Action::DotCollapseLast => self.dot = self.dot.collapse_to_last_cur(),
            Action::DotExtendBackward(tobj) => self.dot = tobj.extend_dot_backward(self),
            Action::DotExtendForward(tobj) => self.dot = tobj.extend_dot_forward(self),
            Action::DotFlip => self.dot.flip(),
            Action::DotSet(tobj) => self.dot = tobj.set_dot(self),

            Action::RawKey { k } => self.handle_raw_key(k, screen_rows),

            _ => (),
        }

        Ok(())
    }

    fn handle_raw_key(&mut self, k: Key, screen_rows: usize) {
        match k {
            Key::Return => {
                self.dot = Dot::Cur {
                    c: self.insert_char(self.dot, '\n'),
                }
            }
            Key::Tab => {
                self.dot = Dot::Cur {
                    c: self.insert_char(self.dot, '\t'),
                }
            }
            Key::Char(c) => {
                self.dot = Dot::Cur {
                    c: self.insert_char(self.dot, c),
                }
            }

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

    fn undo(&mut self) -> Result<(), String> {
        match self.edit_log.undo() {
            Some(edit) => {
                self.edit_log.paused = true;
                self.apply_edit(edit);
                self.edit_log.paused = false;
                self.dirty = !self.edit_log.is_empty();
                Ok(())
            }
            None => Err("Nothing to undo".to_string()),
        }
    }

    fn redo(&mut self) -> Result<(), String> {
        match self.edit_log.redo() {
            Some(edit) => {
                self.edit_log.paused = true;
                self.apply_edit(edit);
                self.edit_log.paused = false;
                Ok(())
            }
            None => Err("Nothing to redo".to_string()),
        }
    }

    fn apply_edit(&mut self, Edit { kind, cur, txt }: Edit) {
        let new_cur = match (kind, txt) {
            (Kind::Insert, Txt::Char(c)) => self.insert_char(Dot::Cur { c: cur }, c),
            (Kind::Insert, Txt::String(s)) => self.insert_string(Dot::Cur { c: cur }, s),
            (Kind::Delete, Txt::Char(_)) => self.delete_dot(Dot::Cur { c: cur }),
            (Kind::Delete, Txt::String(s)) => {
                let (dy, last_line) = s.lines().enumerate().last().unwrap();
                let mut end = cur;
                end.x += last_line.len();
                end.y += dy;
                self.delete_dot(Dot::Range {
                    r: Range::from_cursors(cur, end, true),
                })
            }
        };

        self.dot = Dot::Cur { c: new_cur };
    }

    fn insert_char(&mut self, dot: Dot, ch: char) -> Cur {
        if dot.last_cur().y == self.lines.len() {
            self.insert_line(self.lines.len(), String::new());
        }

        let c = match dot {
            Dot::Cur { c } => {
                self.edit_log.insert_char(c, ch);
                self.insert_char_handling_newline(c, ch)
            }
            Dot::Range { r } => {
                let (c, deleted) = self.delete_range(r);
                let _ = set_clipboard(&deleted);
                self.edit_log.insert_char(c, ch);
                self.insert_char_handling_newline(c, ch)
            }
        };

        self.dirty = true;
        c
    }

    fn insert_string(&mut self, dot: Dot, s: String) -> Cur {
        if dot.last_cur().y == self.lines.len() {
            self.insert_line(self.lines.len(), String::new());
        }

        self.edit_log.insert_string(dot.first_cur(), s.clone());

        let mut cur = match dot {
            Dot::Cur { c } => c,
            Dot::Range { r } => {
                let (cur, deleted) = self.delete_range(r);
                let _ = set_clipboard(&deleted);
                cur
            }
        };

        self.edit_log.paused = true;
        for ch in s.chars() {
            cur = self.insert_char_handling_newline(cur, ch);
        }
        self.edit_log.paused = false;
        self.dirty = true;

        cur
    }

    fn delete_dot(&mut self, dot: Dot) -> Cur {
        let (cur, deleted) = match dot {
            Dot::Cur { c } => self.delete_cur(c),
            Dot::Range { r } => {
                let (cur, deleted) = self.delete_range(r);
                (cur, Some(deleted))
            }
        };

        // NOTE: Ignoring errors in setting the system clipboard
        if let Some(deleted) = deleted {
            let _ = set_clipboard(&deleted);
        }

        self.dirty = true;

        cur
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
        } else if cur.y == self.lines.len() {
            self.lines.push(Line::new(ch.into()));
            cur.x += 1;
        } else {
            self.lines[cur.y].modify(|s| s.insert(cur.x, ch));
            cur.x += 1;
        }

        cur
    }

    fn insert_line(&mut self, at: usize, line: String) {
        if at <= self.len_lines() {
            let cur = Cur { y: at, x: 0 };
            self.edit_log.insert_string(cur, line.clone());
            self.lines.insert(at, Line::new(line));
            self.dirty = true;
        }
    }

    fn delete_cur(&mut self, cur: Cur) -> (Cur, Option<String>) {
        if cur.y == self.len_lines() {
            return (cur, None);
        }

        let deleted = if cur.x < self.lines[cur.y].len() {
            let c = self.lines[cur.y].raw.remove(cur.x);
            self.lines[cur.y].update_render();
            self.edit_log.delete_char(cur, c);
            Some(c.to_string())
        } else if cur.y < self.lines.len() - 1 {
            // Deleting the newline char at the end of this line
            let line = self.lines.remove(cur.y + 1);
            self.lines[cur.y].modify(|s| s.push_str(&line.raw));
            self.edit_log.delete_char(cur, '\n');
            Some("\n".to_string())
        } else if cur.x == 0 && self.lines[cur.y].raw.is_empty() {
            // Deleting an empty line
            self.lines.remove(cur.y);
            self.edit_log.delete_char(cur, '\n');
            Some("\n".to_string())
        } else {
            None
        };

        self.dirty = deleted.is_some();

        (cur, deleted)
    }

    /// Delete all LineRanges from the given range in reverse order so we
    /// don't invalidate line offsets
    fn delete_range(&mut self, r: Range) -> (Cur, String) {
        let line_ranges = r.line_ranges();
        let mut single_line_had_newline = false;
        let mut single_line_y = 0;
        let mut deleted_lines = Vec::with_capacity(line_ranges.len());

        for lr in line_ranges.into_iter().rev() {
            match lr {
                lr if lr.y() == self.lines.len() => {
                    continue;
                }
                lr if lr.is_full_line(self) => {
                    deleted_lines.push(self.lines.remove(lr.y()).raw);
                    single_line_had_newline = true;
                    single_line_y = lr.y();
                }
                LineRange::Full { y } => {
                    deleted_lines.push(self.lines.remove(y).raw);
                }
                LineRange::ToEnd { y, start } => {
                    let (left, deleted) = self.lines[y].raw.split_at(start);
                    deleted_lines.push(deleted.to_string());
                    self.lines[y].raw = left.to_string();
                    self.lines[y].update_render();
                }
                LineRange::FromStart { y, end } => {
                    deleted_lines.push(self.lines[y].raw.drain(..=end).collect());
                    self.lines[y].update_render();
                }
                LineRange::Partial { y, start, end } => {
                    deleted_lines.push(self.lines[y].raw.drain(start..=end).collect());
                    self.lines[y].update_render();
                }
            }
        }

        let deleted = if deleted_lines.len() == 1
            && single_line_had_newline
            && single_line_y != self.lines.len()
        {
            deleted_lines[0].push('\n');
            deleted_lines.remove(0)
        } else {
            deleted_lines.reverse();
            deleted_lines.join("\n")
        };

        self.edit_log.delete_string(r.start, deleted.clone());

        (r.start, deleted)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use edit::tests::{del_s, in_c, in_s};

    fn simple_initial_buffer() -> Buffer {
        let mut b = Buffer::new_unnamed(0);
        let s = "This is a test\ninvolving multiple lines";

        for c in s.chars() {
            let res = b.handle_action(Action::InsertChar { c }, 80);
            assert!(res.is_ok());
        }

        b
    }

    #[test]
    fn simple_insert_works() {
        let b = simple_initial_buffer();
        let c = Cur {
            y: b.lines.len() - 1,
            x: b.lines[1].len(),
        };

        assert_eq!(b.lines.len(), 2);
        assert_eq!(b.lines[0].raw, "This is a test");
        assert_eq!(b.lines[1].raw, "involving multiple lines");
        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(
            b.edit_log.edits,
            vec![
                in_s(0, 0, "This is a test\n"),
                in_s(1, 0, "involving multiple lines")
            ]
        );
    }

    #[test]
    fn insert_char_w_range_dot_works() -> Result<(), String> {
        let mut b = simple_initial_buffer();
        b.handle_action(Action::DotSet(TextObject::Line), 80)?;
        b.handle_action(Action::InsertChar { c: 'x' }, 80)?;
        let c = Cur {
            y: b.lines.len() - 1,
            x: b.lines[1].len(),
        };

        assert_eq!(b.lines.len(), 2);
        assert_eq!(b.lines[0].raw, "This is a test");
        assert_eq!(b.lines[1].raw, "x");
        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(
            b.edit_log.edits,
            vec![
                in_s(0, 0, "This is a test\n"),
                in_s(1, 0, "involving multiple lines"),
                del_s(1, 0, "involving multiple lines"),
                in_c(1, 0, 'x'),
            ]
        );

        Ok(())
    }
}
