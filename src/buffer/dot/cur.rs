use crate::{buffer::Buffer, key::Arrow};
use std::cmp::min;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cur {
    pub idx: usize,
}

impl Cur {
    pub fn buffer_start() -> Self {
        Cur { idx: 0 }
    }

    pub fn buffer_end(b: &Buffer) -> Self {
        Cur {
            idx: b.txt.len_chars(),
        }
    }

    pub fn as_string_addr(&self, b: &Buffer) -> String {
        let (y, x) = self.as_yx(b);
        format!("{}:{}", y + 1, x + 1)
    }

    pub(crate) fn as_yx(&self, b: &Buffer) -> (usize, usize) {
        let y = b.txt.char_to_line(self.idx);
        let x = self.idx - b.txt.line_to_char(y);

        (y, x)
    }

    pub(crate) fn from_yx(y_idx: usize, x_idx: usize, b: &Buffer) -> Self {
        let line_start = b.txt.line_to_char(y_idx);
        let mut x_max = b.txt.line(y_idx).len_chars();
        if y_idx < b.len_lines() - 1 {
            x_max -= 1;
        }

        Self {
            idx: line_start + min(x_idx, x_max),
        }
    }

    pub(super) fn arr_w_count(&self, arr: Arrow, count: usize, b: &Buffer) -> Self {
        let mut cur = *self;

        for _ in 0..count {
            cur = cur.arr(arr, b);
        }

        cur.clamp_idx(b.txt.len_chars());
        cur
    }

    fn arr(&self, arr: Arrow, b: &Buffer) -> Self {
        let mut cur = *self;

        match arr {
            Arrow::Up => {
                let (mut y, _) = self.as_yx(b);
                if y != 0 {
                    y -= 1;
                    cur.idx = b.txt.line_to_char(y) + b.x_from_rx(y);
                }
            }
            Arrow::Down => {
                let (mut y, _) = self.as_yx(b);
                if !b.is_empty() && y < b.len_lines() - 1 {
                    y += 1;
                    cur.idx = b.txt.line_to_char(y) + b.x_from_rx(y);
                }
            }
            Arrow::Left => cur.idx = cur.idx.saturating_sub(1),
            Arrow::Right => cur.idx = min(cur.idx + 1, b.txt.len_chars()),
        }

        cur
    }

    pub(super) fn clamp_idx(&mut self, max_idx: usize) {
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

        self.idx = b.txt.line_to_char(y) + b.txt.line(y).len_chars();
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
            let c = Cur::from_yx(n, 100, &b);

            // Should be focused on the newline character at the end of the line
            // In the case of the last line we should be focused on the index after
            // the end of the string so that we are appending to the buffer.
            assert_eq!(c.as_yx(&b), (n, s.len()), "line='{s}'");
        }
    }

    #[test]
    fn arr_right_at_eof_focuses_eof() {
        let lines = &["a", "ab", "abc", "abcd"];
        let b = buffer_from_lines(lines);
        let c = Cur::buffer_end(&b);
        assert_eq!(c.as_yx(&b), (3, 4));

        let final_c = c.arr(Arrow::Right, &b);
        assert_eq!(final_c.as_yx(&b), (3, 4));
    }

    #[test]
    fn arr_right_at_last_char_focuses_eof() {
        let lines = &["a", "ab", "abc", "abcd"];
        let b = buffer_from_lines(lines);
        let mut c = Cur::buffer_end(&b);
        c.idx -= 1;

        assert_eq!(c.as_yx(&b), (3, 3));

        let mut final_c = c.arr(Arrow::Right, &b);
        final_c.clamp_idx(b.txt.len_chars());
        assert_eq!(final_c.as_yx(&b), (3, 4));
    }
}
