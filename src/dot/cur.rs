use crate::{
    buffer::{Buffer, CharIdx, CharOffset, LineIdx},
    key::Arrow,
};
use std::cmp::min;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cur {
    pub idx: CharIdx,
}

impl Cur {
    pub const fn new(idx: CharIdx) -> Self {
        Self { idx }
    }

    pub const fn buffer_start() -> Self {
        Cur { idx: CharIdx(0) }
    }

    pub const fn buffer_end(b: &Buffer) -> Self {
        Cur {
            idx: CharIdx(b.txt.len_chars()),
        }
    }

    pub fn as_string_addr(&self, b: &Buffer) -> String {
        let (y, x) = self.as_yx(b);
        format!("{}:{}", y.uadd(1), x + CharOffset(1))
    }

    pub(crate) fn as_yx(&self, b: &Buffer) -> (LineIdx, CharOffset) {
        let y = b.txt.char_to_line(self.idx);
        let x = self.idx.offset_from(b.txt.line_to_char(y));

        (y, x)
    }

    pub(crate) fn from_yx(y_idx: LineIdx, x_idx: CharOffset, b: &Buffer) -> Self {
        let line_start = b.txt.line_to_char(y_idx);
        let mut x_max = b.txt.line_len_chars(y_idx);
        if y_idx.0 < b.len_lines() - 1 {
            x_max -= 1;
        }

        Self {
            idx: line_start + min(x_idx, CharOffset(x_max as isize)),
        }
    }

    pub(super) fn arr_w_count(&self, arr: Arrow, count: usize, b: &Buffer) -> Self {
        let mut cur = *self;

        for _ in 0..count {
            cur = cur.arr(arr, b);
        }

        cur.clamp_idx(CharIdx(b.txt.len_chars()));
        cur
    }

    pub(super) fn arr(&self, arr: Arrow, b: &Buffer) -> Self {
        let mut cur = *self;

        match arr {
            Arrow::Up => {
                let (mut y, _) = self.as_yx(b);
                if y != LineIdx(0) {
                    y = y.usub(1);
                    cur.idx = b.txt.line_to_char(y) + b.x_from_rx(y);
                }
            }
            Arrow::Down => {
                let (mut y, _) = self.as_yx(b);
                if !b.is_empty() && y.0 < b.len_lines() - 1 {
                    y = y.uadd(1);
                    cur.idx = b.txt.line_to_char(y) + b.x_from_rx(y);
                }
            }
            Arrow::Left => cur.idx = cur.idx.saturating_usub(1),
            Arrow::Right => cur.idx = min(cur.idx.uadd(1), CharIdx(b.txt.len_chars())),
        }

        cur
    }

    pub(crate) fn clamp_idx(&mut self, max_idx: CharIdx) {
        self.idx = min(self.idx, max_idx);
    }

    #[must_use]
    pub(super) fn move_to_line_start(mut self, b: &Buffer) -> Self {
        let y = b.txt.char_to_line(self.idx);
        self.idx = b.txt.line_to_char(y);
        self
    }

    #[must_use]
    pub(super) fn move_to_line_end(mut self, b: &Buffer) -> Self {
        let y = b.txt.char_to_line(self.idx);
        self.idx = b.txt.line_to_char(y).uadd(b.txt.line_len_chars(y));

        // On the last line of the file we don't have a trailing newline
        // so we really do want the final character
        if y.0 < b.len_lines().saturating_sub(1) {
            self.idx = self.idx.saturating_usub(1);
        }

        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::tests::buffer_from_lines;

    #[test]
    fn from_yx_focuses_eol() {
        let lines = &["a", "ab", "abc", "abcd"];
        let b = buffer_from_lines(lines);

        for (n, s) in lines.iter().enumerate() {
            let c = Cur::from_yx(LineIdx(n), CharOffset(100), &b);

            // Should be focused on the newline character at the end of the line
            // In the case of the last line we should be focused on the index after
            // the end of the string so that we are appending to the buffer.
            assert_eq!(
                c.as_yx(&b),
                (LineIdx(n), CharOffset::from(*s)),
                "line='{s}'"
            );
        }
    }

    #[test]
    fn arr_right_at_eof_focuses_eof() {
        let lines = &["a", "ab", "abc", "abcd"];
        let b = buffer_from_lines(lines);
        let c = Cur::buffer_end(&b);
        assert_eq!(c.as_yx(&b), (LineIdx(3), CharOffset(4)));

        let final_c = c.arr(Arrow::Right, &b);
        assert_eq!(final_c.as_yx(&b), (LineIdx(3), CharOffset(4)));
    }

    #[test]
    fn arr_right_at_last_char_focuses_eof() {
        let lines = &["a", "ab", "abc", "abcd"];
        let b = buffer_from_lines(lines);
        let mut c = Cur::buffer_end(&b);
        c.idx = c.idx.usub(1);

        assert_eq!(c.as_yx(&b), (LineIdx(3), CharOffset(3)));

        let mut final_c = c.arr(Arrow::Right, &b);
        final_c.clamp_idx(CharIdx(b.txt.len_chars()));
        assert_eq!(final_c.as_yx(&b), (LineIdx(3), CharOffset(4)));
    }
}
