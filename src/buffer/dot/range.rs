use crate::buffer::{dot::Cur, Buffer};
use ropey::RopeSlice;
use std::{cmp::min, fmt, ops::RangeInclusive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Range {
    pub start: Cur,
    pub end: Cur,
    pub(super) start_active: bool,
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{},{}", self.start, self.end)
    }
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

    pub(crate) fn as_inclusive_char_range(&self, b: &Buffer) -> Option<RangeInclusive<usize>> {
        let start = self.start.as_char_idx(b);
        let end = self.end.as_char_idx(b);

        assert!(start != end, "null range that should have been collapsed");

        if b.is_empty() {
            None
        } else {
            Some(start..=min(end, b.txt.len_chars().saturating_sub(1)))
        }
    }

    /// Extends the STARTING cursor to its line start
    #[must_use]
    pub(super) fn extend_to_line_start(mut self) -> Self {
        self.start = self.start.move_to_line_start();
        self
    }

    /// Extends the ENDING cursor to its line start
    #[must_use]
    pub(super) fn extend_to_line_end(mut self, b: &Buffer) -> Self {
        self.end = self.end.move_to_line_end(b);
        self
    }

    /// Extend end back until cond returns an x position in the given line or we bottom
    /// out at the end of the buffer
    #[must_use]
    pub(super) fn extend_to(mut self, b: &Buffer, cond: fn(RopeSlice) -> Option<usize>) -> Self {
        self.end = self.end.move_to(b, cond);
        self
    }

    /// Extend start back until cond returns an x position in the given line or we bottom
    /// out at the start of the buffer
    #[must_use]
    pub(super) fn extend_back_to(
        mut self,
        b: &Buffer,
        cond: fn(RopeSlice) -> Option<usize>,
    ) -> Self {
        self.start = self.start.move_back_to(b, cond);
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

    pub(crate) fn line_range(&self, y: usize) -> Option<LineRange> {
        if y == self.start.y {
            if self.start.y == self.end.y {
                Some(LineRange::Partial {
                    y: self.start.y,
                    start: self.start.x,
                    end: self.end.x,
                })
            } else {
                Some(LineRange::ToEnd {
                    y: self.start.y,
                    start: self.start.x,
                })
            }
        } else if y > self.start.y && y < self.end.y {
            Some(LineRange::Full { y })
        } else if y == self.end.y {
            Some(LineRange::FromStart {
                y: self.end.y,
                end: self.end.x,
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
