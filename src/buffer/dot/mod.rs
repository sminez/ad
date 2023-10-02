//! Sam style dot manipulation
//!
//! See http://sam.cat-v.org/ for details on how sam works, particularly
//! http://doc.cat-v.org/plan_9/4th_edition/papers/sam/ which is the original paper
//! where Rob Pike lays out how the editor works.
//!
//! All indexing is 0-based when working with the contents of a specific buffer.
//! Converting to 1-based indices for the terminal is exclusively handled in the
//! rendering logic.
use crate::{buffer::Buffer, key::Arrow};

mod cur;
mod range;

pub(crate) use cur::Cur;
pub(crate) use range::{LineRange, Range};

/// A Dot represents the currently selected contents of a Buffer.
///
/// Most of the editing commands available in ad which manipulate the buffer contents
/// do so via setting and manipulating the current dot. The name comes from the fact
/// that the representation of the current Dot in the editing language is `.`
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dot {
    Cur { c: Cur },
    Range { r: Range },
}

impl Default for Dot {
    fn default() -> Self {
        Self::Cur { c: Cur::default() }
    }
}

impl Dot {
    #[inline]
    pub fn active_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.active_cursor(),
        }
    }

    #[inline]
    pub fn first_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.start,
        }
    }

    #[inline]
    pub fn last_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.end,
        }
    }

    #[inline]
    fn as_range(&self) -> Range {
        match self {
            Self::Cur { c } => Range {
                start: *c,
                end: *c,
                start_active: true,
            },
            Self::Range { r } => *r,
        }
    }

    #[inline]
    fn start_active(&self) -> bool {
        match self {
            Self::Cur { .. } => true,
            Self::Range { r } => r.start_active,
        }
    }

    #[inline]
    pub fn collapse_to_first_cur(&self) -> Self {
        Dot::Cur {
            c: self.first_cur(),
        }
    }

    #[inline]
    pub fn collapse_to_last_cur(&self) -> Self {
        Dot::Cur { c: self.last_cur() }
    }

    #[inline]
    pub fn flip(&mut self) {
        if let Dot::Range { r } = self {
            r.flip();
        }
    }

    pub(crate) fn line_range(&self, y: usize) -> Option<LineRange> {
        match self {
            Dot::Cur { .. } => None,
            Dot::Range { r } => r.line_range(y),
        }
    }

    /// If both ends of a Range match then replace with a single Cur
    fn collapse_null_range(self) -> Self {
        match self {
            Dot::Range {
                r: Range { start, end, .. },
            } if start == end => Dot::Cur { c: start },
            _ => self,
        }
    }
}

/// Something that can be applied to an existing Dot to update it to a new state
pub trait UpdateDot {
    #[must_use]
    fn set_dot(&self, b: &Buffer) -> Dot;
    #[must_use]
    fn extend_dot_forward(&self, b: &Buffer) -> Dot;
    #[must_use]
    fn extend_dot_backward(&self, b: &Buffer) -> Dot;
}

impl UpdateDot for Arrow {
    fn set_dot(&self, b: &Buffer) -> Dot {
        Dot::Cur {
            c: b.dot.active_cur().arr_w_count(*self, 1, b),
        }
    }

    fn extend_dot_forward(&self, b: &Buffer) -> Dot {
        match b.dot {
            Dot::Cur { c } => Dot::Range {
                r: Range::from_cursors(c, c.arr_w_count(*self, 1, b), b.dot.start_active()),
            },
            Dot::Range { r } => Dot::Range {
                r: Range::from_cursors(
                    r.start,
                    r.end.arr_w_count(*self, 1, b),
                    b.dot.start_active(),
                ),
            }
            .collapse_null_range(),
        }
    }

    fn extend_dot_backward(&self, b: &Buffer) -> Dot {
        match b.dot {
            Dot::Cur { .. } => self.flip().extend_dot_forward(b),
            Dot::Range { r } => Dot::Range {
                r: Range::from_cursors(
                    r.start.arr_w_count(self.flip(), 1, b),
                    r.end,
                    b.dot.start_active(),
                ),
            }
            .collapse_null_range(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TextObject {
    // Arr(Arrow),
    Buffer,
    Character,
    // Delimited(char, char),
    // FindChar(char),
    Line,
    LineBoundary,
    // Paragraph,
    // Word,
}

impl UpdateDot for TextObject {
    fn set_dot(&self, b: &Buffer) -> Dot {
        match self {
            TextObject::Buffer => Dot::Range {
                r: Range {
                    start: Cur::buffer_start(),
                    end: Cur::buffer_end(b),
                    start_active: b.dot.start_active(),
                },
            },
            TextObject::Character => Dot::Cur {
                c: b.dot.active_cur().arr_w_count(Arrow::Right, 1, b),
            },
            // For setting dot, line and line boundary operate the same as we are selecting
            // "the whole line" or "between line boundaries" which are equivalent
            TextObject::Line | TextObject::LineBoundary => {
                let mut start = b.dot.first_cur();
                let mut end = b.dot.last_cur();
                start.x = 0;
                end.x = b.lines.get(end.y).map(|l| l.len()).unwrap_or_default();

                Dot::Range {
                    r: Range {
                        start,
                        end,
                        start_active: b.dot.start_active(),
                    },
                }
            }
        }
    }

    fn extend_dot_forward(&self, b: &Buffer) -> Dot {
        let Range {
            mut start,
            mut end,
            start_active,
        } = b.dot.as_range();

        (start, end) = match (self, start_active) {
            (TextObject::Buffer, true) => (end, Cur::buffer_end(b)),
            (TextObject::Buffer, false) => (start, Cur::buffer_end(b)),
            (TextObject::Character, true) => (start.arr_w_count(Arrow::Right, 1, b), end),
            (TextObject::Character, false) => (start, end.arr_w_count(Arrow::Right, 1, b)),
            (TextObject::Line, true) => (start.arr_w_count(Arrow::Down, 1, b), end),
            (TextObject::Line, false) => (start, end.arr_w_count(Arrow::Down, 1, b)),
            (TextObject::LineBoundary, true) => {
                start.x = b.lines.get(start.y).map(|l| l.len()).unwrap_or_default();
                (start, end)
            }
            (TextObject::LineBoundary, false) => {
                end.x = b.lines.get(end.y).map(|l| l.len()).unwrap_or_default();
                (start, end)
            }
        };

        Dot::Range {
            r: Range::from_cursors(start, end, start_active),
        }
    }

    fn extend_dot_backward(&self, b: &Buffer) -> Dot {
        let Range {
            mut start,
            mut end,
            start_active,
        } = b.dot.as_range();

        (start, end) = match (self, start_active) {
            (TextObject::Buffer, true) => (Cur::buffer_start(), end),
            (TextObject::Buffer, false) => (Cur::buffer_start(), start),
            (TextObject::Character, true) => (start.arr_w_count(Arrow::Left, 1, b), end),
            (TextObject::Character, false) => (start, end.arr_w_count(Arrow::Left, 1, b)),
            (TextObject::Line, true) => (start.arr_w_count(Arrow::Up, 1, b), end),
            (TextObject::Line, false) => (start, end.arr_w_count(Arrow::Up, 1, b)),
            (TextObject::LineBoundary, true) => {
                start.x = 0;
                (start, end)
            }
            (TextObject::LineBoundary, false) => {
                end.x = 0;
                (start, end)
            }
        };

        Dot::Range {
            r: Range::from_cursors(start, end, start_active),
        }
    }
}
