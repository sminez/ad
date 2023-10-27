use crate::{
    editor::Action,
    key::Key,
    term::{Color, Style},
    util::relative_path_from,
    MAX_NAME_LEN, TAB_STOP, UNNAMED_BUFFER,
};
use ropey::{Rope, RopeSlice};
use std::{
    cmp::min,
    fs,
    io::{self, ErrorKind},
    path::{Path, PathBuf},
};

mod buffers;
mod dot;
mod edit;
mod minibuffer;

use edit::{Edit, EditLog, Kind, Txt};

pub(crate) use buffers::Buffers;
pub(crate) use dot::{Cur, Dot, LineRange, Matcher, Range, TextObject, UpdateDot};
pub(crate) use minibuffer::{MiniBuffer, MiniBufferSelection, MiniBufferState};

// Used to inform the editor that further action needs to be taken by it after a Buffer has
// finished processing a given Action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ActionOutcome {
    SetClipboard(String),
    SetStatusMessag(String),
}

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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Buffer {
    id: usize,
    pub(crate) kind: BufferKind,
    pub(crate) dot: Dot,
    pub(crate) txt: Rope,
    pub(crate) rx: usize,
    pub(crate) row_off: usize,
    pub(crate) col_off: usize,
    pub(crate) dirty: bool,
    edit_log: EditLog,
    has_trailing_newline: bool,
}

impl Buffer {
    /// As the name implies, this method MUST be called with the full cannonical file path
    pub(super) fn new_from_canonical_file_path(id: usize, path: PathBuf) -> io::Result<Self> {
        let mut raw = match fs::read_to_string(&path) {
            Ok(contents) => contents,
            Err(e) if e.kind() == ErrorKind::NotFound => String::new(),
            Err(e) => return Err(e),
        };

        let has_trailing_newline = raw.ends_with('\n');
        if has_trailing_newline {
            raw.pop();
        }

        Ok(Self {
            id,
            kind: BufferKind::File(path),
            dot: Dot::default(),
            txt: Rope::from_str(&raw),
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
            edit_log: EditLog::default(),
            has_trailing_newline,
        })
    }

    pub fn new_unnamed(id: usize, content: &str) -> Self {
        Self {
            id,
            kind: BufferKind::Unnamed,
            dot: Dot::default(),
            txt: Rope::from_str(content),
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
            edit_log: EditLog::default(),
            has_trailing_newline: true,
        }
    }

    pub fn new_virtual(id: usize, name: String, mut content: String) -> Self {
        let has_trailing_newline = content.ends_with('\n');
        if has_trailing_newline {
            content.pop();
        }

        Self {
            id,
            kind: BufferKind::Virtual(name),
            dot: Dot::default(),
            txt: Rope::from_str(&content),
            rx: 0,
            row_off: 0,
            col_off: 0,
            dirty: false,
            edit_log: EditLog::default(),
            has_trailing_newline,
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

    pub fn contents(&self) -> Vec<u8> {
        let mut contents: Vec<u8> = self.txt.bytes().collect();
        if self.has_trailing_newline {
            contents.push(b'\n');
        }

        contents
    }

    pub(crate) fn string_lines(&self) -> Vec<String> {
        self.txt
            .lines()
            .map(|l| l.to_string().trim_end_matches('\n').to_string())
            .collect()
    }

    pub fn dot_contents(&self) -> String {
        self.dot.content(self)
    }

    #[inline]
    pub fn len_lines(&self) -> usize {
        self.txt.len_lines()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.txt.len_chars() == 0
    }

    pub(crate) fn debug_edit_log(&self) -> Vec<String> {
        self.edit_log.debug_edits().into_iter().collect()
    }

    pub fn clamp_scroll(&mut self, screen_rows: usize, screen_cols: usize) {
        let Cur { x, y } = self.dot.active_cur();
        self.rx = self.rx_from_x(y, x);

        if y < self.row_off {
            self.dot.set_active_cur(Cur { y: self.row_off, x });
        }

        if y >= self.row_off + screen_rows {
            self.dot.set_active_cur(Cur {
                y: self.row_off + screen_rows - 1,
                x,
            });
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols {
            self.col_off = self.rx - screen_cols + 1;
        }
    }

    pub(crate) fn rx_from_x(&self, y: usize, x: usize) -> usize {
        if y >= self.len_lines() {
            return 0;
        }

        let mut rx = 0;
        for c in self.txt.line(y).chars().take(x) {
            if c == '\t' {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;
        }

        rx
    }

    pub(crate) fn x_from_rx(&self, y: usize) -> usize {
        if self.is_empty() {
            return 0;
        }

        let mut rx = 0;
        let mut cx = 0;

        for c in self.txt.line(y).chars() {
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

    pub fn line(&self, y: usize) -> Option<RopeSlice> {
        if y >= self.len_lines() {
            None
        } else {
            Some(self.txt.line(y))
        }
    }

    /// The render representation of a given line, truncated to fit within the
    /// available screen space.
    /// This includes tab expansion but not any styling that might be applied,
    /// trailing \r\n or screen clearing escape codes.
    pub(crate) fn raw_rline_unchecked(&self, y: usize, lpad: usize, screen_cols: usize) -> String {
        // We need to know if there are any leading tab characters that are padding
        // the screen so we read the full line up to its max end point for the current
        // window size and then trim off the column offset once we have expanded tabs.
        let chars_to_check = self.col_off + screen_cols - lpad;
        let mut rline = String::with_capacity(chars_to_check);
        let mut it = self.txt.line(y).chars();

        while rline.len() <= chars_to_check {
            match it.next() {
                Some('\n') | None => break,
                Some('\t') => rline.push_str(&" ".repeat(TAB_STOP)),
                Some(c) => rline.push(c),
            }
        }

        if rline.len() > self.col_off {
            rline = rline.split_off(self.col_off);
        }

        rline
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
        let mut rline = self.raw_rline_unchecked(y, lpad, screen_cols);

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

    pub(crate) fn sign_col_dims(&self, screen_rows: usize) -> (usize, usize) {
        let n_lines = self.len_lines();
        let max_linum = min(n_lines, screen_rows + self.row_off);
        let w_lnum = n_digits(max_linum);
        let w_sgncol = w_lnum + 2;

        (w_lnum, w_sgncol)
    }

    pub(crate) fn set_dot_from_screen_coords(&mut self, x: usize, y: usize, screen_rows: usize) {
        let (_, w_sgncol) = self.sign_col_dims(screen_rows);
        self.rx = x - 1 - w_sgncol;
        let y = min(y - 1 + self.row_off, self.len_lines() - 1);

        let mut dot = Dot::Cur {
            c: Cur {
                y,
                x: self.x_from_rx(y),
            },
        };
        dot.clamp_for(self);
        self.dot = dot;
    }

    pub(crate) fn extend_dot_to_screen_coords(&mut self, x: usize, y: usize, screen_rows: usize) {
        let (_, w_sgncol) = self.sign_col_dims(screen_rows);
        self.rx = x - 1 - w_sgncol;
        let y = min(y - 1 + self.row_off, self.len_lines() - 1);

        let mut r = self.dot.as_range();
        let c = Cur {
            y,
            x: self.x_from_rx(y),
        };
        r.set_active_cursor(c);

        let mut dot = Dot::Range { r };
        dot.clamp_for(self);
        self.dot = dot;
    }

    pub(crate) fn scroll_up(&mut self) {
        self.row_off = self.row_off.saturating_sub(1);
    }

    pub(crate) fn scroll_down(&mut self) {
        self.row_off += 1;
    }

    /// The error result of this function is an error string that should be displayed to the user
    pub(crate) fn handle_action(&mut self, a: Action) -> Option<ActionOutcome> {
        match a {
            Action::Delete => {
                let (c, deleted) = self.delete_dot(self.dot);
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }
            Action::InsertChar { c } => {
                let (c, deleted) = self.insert_char(self.dot, c);
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }
            Action::InsertString { s } => {
                let (c, deleted) = self.insert_string(self.dot, s);
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }

            Action::Redo => return self.redo(),
            Action::Undo => return self.undo(),

            Action::DotCollapseFirst => self.dot = self.dot.collapse_to_first_cur(),
            Action::DotCollapseLast => self.dot = self.dot.collapse_to_last_cur(),
            Action::DotExtendBackward(tobj, count) => {
                self.dot = tobj.extend_dot_backward_n(self.dot, count, self)
            }
            Action::DotExtendForward(tobj, count) => {
                self.dot = tobj.extend_dot_forward_n(self.dot, count, self)
            }
            Action::DotFlip => self.dot.flip(),
            Action::DotSet(tobj, count) => self.dot = tobj.set_dot_n(self.dot, count, self),

            Action::LoadDot => {
                if let Dot::Cur { .. } = self.dot {
                    self.dot = TextObject::Word.set_dot(self.dot, self);
                }
                let s = self.dot.content(self);
                self.find_forward(s);
            }

            Action::RawKey { k } => return self.handle_raw_key(k),

            _ => (),
        }

        None
    }

    fn handle_raw_key(&mut self, k: Key) -> Option<ActionOutcome> {
        match k {
            Key::Return => {
                let (c, deleted) = self.insert_char(self.dot, '\n');
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }
            Key::Tab => {
                let (c, deleted) = self.insert_char(self.dot, '\t');
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }
            Key::Char(ch) => {
                let (c, deleted) = self.insert_char(self.dot, ch);
                self.dot = Dot::Cur { c };
                return deleted.map(ActionOutcome::SetClipboard);
            }

            Key::Arrow(arr) => self.dot = arr.set_dot(self.dot, self),

            _ => (),
        }

        None
    }

    fn undo(&mut self) -> Option<ActionOutcome> {
        match self.edit_log.undo() {
            Some(edit) => {
                self.edit_log.paused = true;
                self.apply_edit(edit);
                self.edit_log.paused = false;
                self.dirty = !self.edit_log.is_empty();
                None
            }
            None => Some(ActionOutcome::SetStatusMessag(
                "Nothing to undo".to_string(),
            )),
        }
    }

    fn redo(&mut self) -> Option<ActionOutcome> {
        match self.edit_log.redo() {
            Some(edit) => {
                self.edit_log.paused = true;
                self.apply_edit(edit);
                self.edit_log.paused = false;
                None
            }
            None => Some(ActionOutcome::SetStatusMessag(
                "Nothing to redo".to_string(),
            )),
        }
    }

    fn apply_edit(&mut self, Edit { kind, cur, txt }: Edit) {
        let new_cur = match (kind, txt) {
            (Kind::Insert, Txt::Char(c)) => self.insert_char(Dot::Cur { c: cur }, c).0,
            (Kind::Insert, Txt::String(s)) => self.insert_string(Dot::Cur { c: cur }, s).0,
            (Kind::Delete, Txt::Char(_)) => self.delete_dot(Dot::Cur { c: cur }).0,
            (Kind::Delete, Txt::String(s)) => {
                let (dy, last_line) = s.lines().enumerate().last().unwrap();
                let mut end = cur;
                end.x += last_line.len();
                end.y += dy;
                self.delete_dot(Dot::Range {
                    r: Range::from_cursors(cur, end, true),
                })
                .0
            }
        };

        self.dot = Dot::Cur { c: new_cur };
    }

    fn insert_char(&mut self, dot: Dot, ch: char) -> (Cur, Option<String>) {
        let (cur, deleted) = match dot {
            Dot::Cur { c } => (c, None),
            Dot::Range { r } => self.delete_range(r),
        };

        let idx = cur.as_char_idx(self);
        self.txt.insert_char(idx, ch);
        self.edit_log.insert_char(cur, ch);
        self.dirty = true;

        (Cur::from_char_idx(idx + 1, self), deleted)
    }

    fn insert_string(&mut self, dot: Dot, s: String) -> (Cur, Option<String>) {
        let (cur, deleted) = match dot {
            Dot::Cur { c } => (c, None),
            Dot::Range { r } => self.delete_range(r),
        };

        let idx = cur.as_char_idx(self);
        self.txt.insert(idx, &s);
        self.edit_log.insert_string(cur, s);
        self.dirty = true;

        (cur, deleted)
    }

    fn delete_dot(&mut self, dot: Dot) -> (Cur, Option<String>) {
        match dot {
            Dot::Cur { c } => (self.delete_cur(c), None),
            Dot::Range { r } => self.delete_range(r),
        }
    }

    fn delete_cur(&mut self, cur: Cur) -> Cur {
        let idx = cur.as_char_idx(self);
        if idx < self.txt.len_chars() {
            let ch = self.txt.char(idx);
            self.txt.remove(idx..(idx + 1));
            self.edit_log.delete_char(cur, ch);
            self.dirty = true;
        }

        cur
    }

    fn delete_range(&mut self, r: Range) -> (Cur, Option<String>) {
        let rng = match r.as_inclusive_char_range(self) {
            Some(rng) => rng,
            None => return (r.start, None),
        };

        let s = self.txt.slice(rng.clone()).to_string();
        self.txt.remove(rng);
        self.edit_log.delete_string(r.start, s.clone());
        self.dirty = true;

        (r.start, Some(s))
    }

    fn find_forward<M: Matcher>(&mut self, m: M) {
        if let Some(dot) = m.match_forward_from_wrapping(self.dot.active_cur(), self) {
            self.dot = dot;
        }
    }

    // fn find_backward<M: Matcher>(&mut self, m: M) {
    //     if let Some(dot) = m.match_backward_from_wrapping(self.dot.active_cur(), self) {
    //         self.dot = dot;
    //     }
    // }
}

fn n_digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut digits = 0;
    while n != 0 {
        digits += 1;
        n /= 10;
    }

    digits
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::Arrow;
    use edit::tests::{del_c, del_s, in_c, in_s};
    use simple_test_case::test_case;

    #[test_case(0, 1; "n0")]
    #[test_case(5, 1; "n5")]
    #[test_case(10, 2; "n10")]
    #[test_case(13, 2; "n13")]
    #[test_case(731, 3; "n731")]
    #[test_case(930, 3; "n930")]
    #[test]
    fn n_digits_works(n: usize, digits: usize) {
        assert_eq!(n_digits(n), digits);
    }

    const LINE_1: &str = "This is a test";
    const LINE_2: &str = "involving multiple lines";

    pub fn buffer_from_lines(lines: &[&str]) -> Buffer {
        let mut b = Buffer::new_unnamed(0, "");
        let s = lines.join("\n");

        for c in s.chars() {
            b.handle_action(Action::InsertChar { c });
        }

        b
    }

    fn simple_initial_buffer() -> Buffer {
        buffer_from_lines(&[LINE_1, LINE_2])
    }

    #[test]
    fn simple_insert_works() {
        let b = simple_initial_buffer();
        let c = Cur {
            y: 1,
            x: LINE_2.len(),
        };
        let lines = b.string_lines();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], LINE_2);
        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(
            b.edit_log.edits,
            vec![in_s(0, 0, &format!("{LINE_1}\n")), in_s(1, 0, LINE_2)]
        );
    }

    #[test]
    fn insert_char_w_range_dot_works() {
        let mut b = simple_initial_buffer();
        b.handle_action(Action::DotSet(TextObject::Line, 1));
        b.handle_action(Action::InsertChar { c: 'x' });
        let c = Cur { y: 1, x: 1 };
        let lines = b.string_lines();

        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], "x");
        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(
            b.edit_log.edits,
            vec![
                in_s(0, 0, &format!("{LINE_1}\n")),
                in_s(1, 0, LINE_2),
                del_s(1, 0, LINE_2),
                in_c(1, 0, 'x'),
            ]
        );
    }

    #[test]
    fn delete_in_empty_buffer_is_fine() {
        let mut b = Buffer::new_unnamed(0, "");
        b.handle_action(Action::Delete);
        let c = Cur { y: 0, x: 0 };
        let lines = b.string_lines();

        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(lines.len(), 1);
        assert_eq!(lines[0], "");
        assert_eq!(b.edit_log.edits, vec![]);
    }

    #[test]
    fn simple_delete_works() {
        let mut b = simple_initial_buffer();
        b.handle_action(Action::DotSet(TextObject::Arr(Arrow::Left), 1));
        b.handle_action(Action::Delete);

        let c = Cur {
            y: 1,
            x: LINE_2.len() - 1,
        };
        let lines = b.string_lines();

        assert_eq!(b.dot, Dot::Cur { c });
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], LINE_1);
        assert_eq!(lines[1], "involving multiple line");
        assert_eq!(
            b.edit_log.edits,
            vec![
                in_s(0, 0, "This is a test\n"),
                in_s(1, 0, "involving multiple lines"),
                del_c(1, 23, 's')
            ]
        );
    }
}
