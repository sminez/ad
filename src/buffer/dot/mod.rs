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
    pub fn first_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.start,
        }
    }

    pub fn last_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.end,
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

    /// If both ends of a Range match then replace with a single Cur
    fn collapse_null_range(self) -> Self {
        match self {
            Dot::Range {
                r: Range { start, end },
            } if start == end => Dot::Cur { c: start },
            _ => self,
        }
    }
}

/// Something that can be applied to an existing Dot to update it to a new state
pub trait UpdateDot {
    fn set_dot(&self, b: &Buffer) -> Dot;
    fn extend_dot_forward(&self, b: &Buffer) -> Dot;
    fn extend_dot_backward(&self, b: &Buffer) -> Dot;
}

impl UpdateDot for Arrow {
    fn set_dot(&self, b: &Buffer) -> Dot {
        match b.dot {
            Dot::Cur { c } => Dot::Cur {
                c: c.arr_w_count(*self, 1, b),
            },
            Dot::Range { r } => Dot::Cur {
                c: r.end.arr_w_count(*self, 1, b),
            },
        }
    }

    fn extend_dot_forward(&self, b: &Buffer) -> Dot {
        match b.dot {
            Dot::Cur { c } => Dot::Range {
                r: Range::from_cursors(c, c.arr_w_count(*self, 1, b)),
            },
            Dot::Range { r } => Dot::Range {
                r: Range::from_cursors(r.start, r.end.arr_w_count(*self, 1, b)),
            }
            .collapse_null_range(),
        }
    }

    fn extend_dot_backward(&self, b: &Buffer) -> Dot {
        match b.dot {
            Dot::Cur { .. } => self.flip().extend_dot_forward(b),
            Dot::Range { r } => Dot::Range {
                r: Range::from_cursors(r.start.arr_w_count(self.flip(), 1, b), r.end),
            }
            .collapse_null_range(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TextObject {
    Buffer,
    // Character(char),
    // Delimited(char, char),
    Line,
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
                },
            },
            TextObject::Line => {
                let mut start = b.dot.first_cur();
                let mut end = b.dot.last_cur();
                start.x = 0;
                end.x = b.lines.get(end.y).map(|l| l.len()).unwrap_or_default();

                Dot::Range {
                    r: Range { start, end },
                }
            }
        }
    }

    fn extend_dot_forward(&self, b: &Buffer) -> Dot {
        let mut start = b.dot.first_cur();
        let mut end = b.dot.last_cur();

        (start, end) = match self {
            TextObject::Buffer => (start, Cur::buffer_end(b)),
            TextObject::Line => {
                end.x = b.lines.get(end.y).map(|l| l.len()).unwrap_or_default();
                (start, end)
            }
        };

        Dot::Range {
            r: Range { start, end },
        }
    }

    fn extend_dot_backward(&self, b: &Buffer) -> Dot {
        let mut start = b.dot.first_cur();
        let mut end = b.dot.last_cur();

        (start, end) = match self {
            TextObject::Buffer => (Cur::buffer_start(), end),
            TextObject::Line => {
                start.x = 0;
                (start, end)
            }
        };

        Dot::Range {
            r: Range { start, end },
        }
    }
}
