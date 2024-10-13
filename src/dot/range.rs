use crate::{buffer::Buffer, dot::Cur};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range {
    pub start: Cur,
    pub end: Cur,
    pub(super) start_active: bool,
}

impl Range {
    pub(crate) fn from_cursors(c1: Cur, c2: Cur, c1_was_active: bool) -> Self {
        let (start, end, start_active) = if c1 <= c2 {
            (c1, c2, c1_was_active)
        } else if c1_was_active {
            (c2, c1, false)
        } else {
            (c2, c1, true)
        };

        Self {
            start,
            end,
            start_active,
        }
    }

    pub fn as_string_addr(&self, b: &Buffer) -> String {
        format!(
            "{},{}",
            self.start.as_string_addr(b),
            self.end.as_string_addr(b)
        )
    }

    pub(crate) fn contains(&self, cur: &Cur) -> bool {
        cur.idx >= self.start.idx && cur.idx <= self.end.idx
    }

    /// Extends the STARTING cursor to its line start
    #[must_use]
    pub(super) fn extend_to_line_start(mut self, b: &Buffer) -> Self {
        self.start = self.start.move_to_line_start(b);
        self
    }

    /// Extends the ENDING cursor to its line start
    #[must_use]
    pub(super) fn extend_to_line_end(mut self, b: &Buffer) -> Self {
        self.end = self.end.move_to_line_end(b);
        self
    }

    pub fn flip(&mut self) {
        self.start_active = !self.start_active;
    }

    pub fn active_cursor(&self) -> Cur {
        if self.start_active {
            self.start
        } else {
            self.end
        }
    }

    pub fn set_active_cursor(&mut self, c: Cur) {
        if c == self.start && c == self.end {
            return;
        }

        if self.start_active {
            if c >= self.end {
                self.start = self.end;
                self.end = c;
                self.start_active = false;
            } else {
                self.start = c;
            }
        } else if c <= self.start {
            self.end = self.start;
            self.start = c;
            self.start_active = true;
        } else {
            self.end = c;
        }
    }

    pub(crate) fn line_range(&self, y: usize, b: &Buffer) -> Option<LineRange> {
        let (y_start, x_start) = self.start.as_yx(b);
        let (y_end, x_end) = self.end.as_yx(b);

        if y == y_start {
            if y_start == y_end {
                Some(LineRange::Partial {
                    y: y_start,
                    start: x_start,
                    end: x_end,
                })
            } else {
                Some(LineRange::ToEnd {
                    y: y_start,
                    start: x_start,
                })
            }
        } else if y > y_start && y < y_end {
            Some(LineRange::Full { y })
        } else if y == y_end {
            Some(LineRange::FromStart {
                y: y_end,
                end: x_end,
            })
        } else {
            None
        }
    }
}

/// A an inclusive range of characters within a single line
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LineRange {
    Full { y: usize },
    ToEnd { y: usize, start: usize },
    FromStart { y: usize, end: usize },
    Partial { y: usize, start: usize, end: usize },
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    fn c(idx: usize) -> Cur {
        Cur { idx }
    }

    fn r(start: usize, end: usize, start_active: bool) -> Range {
        Range {
            start: c(start),
            end: c(end),
            start_active,
        }
    }

    #[test_case(r(2, 2, true), c(0), r(0, 2, true); "null range start active new before")]
    #[test_case(r(2, 2, true), c(2), r(2, 2, true); "null range start active new equal")]
    #[test_case(r(2, 2, true), c(5), r(2, 5, false); "null range start active new after")]
    #[test_case(r(2, 2, false), c(0), r(0, 2, true); "null range end active new before")]
    #[test_case(r(2, 2, false), c(2), r(2, 2, false); "null range end active new equal")]
    #[test_case(r(2, 2, false), c(5), r(2, 5, false); "null range end active new after")]
    #[test_case(r(3, 5, true), c(1), r(1, 5, true); "start active new before")]
    #[test_case(r(3, 5, true), c(3), r(3, 5, true); "start active new at start")]
    #[test_case(r(3, 5, true), c(4), r(4, 5, true); "start active new in between")]
    #[test_case(r(3, 5, true), c(5), r(5, 5, false); "start active new at end")]
    #[test_case(r(3, 5, true), c(7), r(5, 7, false); "start active new after")]
    #[test_case(r(3, 5, false), c(1), r(1, 3, true); "end active new before")]
    #[test_case(r(3, 5, false), c(3), r(3, 3, true); "end active new at start")]
    #[test_case(r(3, 5, false), c(4), r(3, 4, false); "end active new in between")]
    #[test_case(r(3, 5, false), c(5), r(3, 5, false); "end active new at end")]
    #[test_case(r(3, 5, false), c(7), r(3, 7, false); "end active new after")]
    #[test]
    fn set_active_cursor_works(mut rng: Range, cur: Cur, expected: Range) {
        rng.set_active_cursor(cur);
        assert_eq!(rng, expected);
    }
}
