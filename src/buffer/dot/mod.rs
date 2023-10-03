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
    pub fn content(&self, b: &Buffer) -> String {
        match self {
            Self::Cur { c } => b.lines[c.y].raw[c.x..(c.x + 1)].to_string(),
            Self::Range { r } => {
                let lrs = r.line_ranges();
                let mut lines: Vec<&str> = Vec::with_capacity(lrs.len());

                for lr in lrs.into_iter() {
                    match lr {
                        LineRange::Full { y } => lines.push(&b.lines[y].raw),
                        LineRange::ToEnd { y, start } => {
                            lines.push(b.lines[y].raw.split_at(start).1)
                        }
                        LineRange::FromStart { y, end } => {
                            lines.push(b.lines[y].raw.split_at(end + 1).0)
                        }
                        LineRange::Partial { y, start, end } => {
                            lines.push(&b.lines[y].raw[start..end])
                        }
                    }
                }

                if lines.len() == 1 {
                    format!("{}\n", lines[0])
                } else {
                    lines.join("\n")
                }
            }
        }
    }

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
                start_active: false,
            },
            Self::Range { r } => *r,
        }
    }

    #[inline]
    fn start_active(&self) -> bool {
        match self {
            Self::Cur { .. } => false,
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
    Arr(Arrow),
    BufferEnd,
    BufferStart,
    Character,
    // Delimited(char, char),
    // FindChar(char),
    Line,
    LineEnd,
    LineStart,
    // Paragraph,
    // Word,
}

impl UpdateDot for TextObject {
    fn set_dot(&self, b: &Buffer) -> Dot {
        match self {
            TextObject::Arr(arr) => arr.set_dot(b),
            TextObject::BufferEnd => Dot::Cur {
                c: Cur::buffer_end(b),
            },
            TextObject::BufferStart => Dot::Cur {
                c: Cur::buffer_start(),
            },
            TextObject::Character => Dot::Cur {
                c: b.dot.active_cur().arr_w_count(Arrow::Right, 1, b),
            },
            TextObject::Line => Dot::Range {
                r: b.dot
                    .as_range()
                    .extend_to_line_start()
                    .extend_to_line_end(b),
            },
            TextObject::LineEnd => Dot::Cur {
                c: b.dot.active_cur().move_to_line_end(b),
            },
            TextObject::LineStart => Dot::Cur {
                c: b.dot.active_cur().move_to_line_start(),
            },
        }
    }

    fn extend_dot_forward(&self, b: &Buffer) -> Dot {
        let Range {
            mut start,
            mut end,
            start_active,
        } = b.dot.as_range();

        (start, end) = match (self, start_active) {
            (TextObject::Arr(arr), _) => return arr.extend_dot_forward(b),
            (TextObject::BufferEnd, true) => (end, Cur::buffer_end(b)),
            (TextObject::BufferEnd, false) => (start, Cur::buffer_end(b)),
            (TextObject::BufferStart, _) => return b.dot.clone(), // Can't move forward to the buffer start
            (TextObject::Character, true) => (start.arr_w_count(Arrow::Right, 1, b), end),
            (TextObject::Character, false) => (start, end.arr_w_count(Arrow::Right, 1, b)),
            (TextObject::Line, true) => (start.arr_w_count(Arrow::Down, 1, b), end),
            (TextObject::Line, false) => (start, end.arr_w_count(Arrow::Down, 1, b)),
            (TextObject::LineEnd, true) => {
                // start can only be EOL if start==end
                if start != end {
                    (start.move_to_line_end(b), end)
                } else {
                    let new_end = start.arr_w_count(Arrow::Down, 1, b).move_to_line_end(b);
                    (end, new_end)
                }
            }
            (TextObject::LineEnd, false) => {
                let mut new_end = end.move_to_line_end(b);
                if new_end == end {
                    // already EOL so move to the next
                    new_end = new_end.arr_w_count(Arrow::Down, 1, b).move_to_line_end(b);
                }
                (start, new_end)
            }
            (TextObject::LineStart, true) => (
                start.arr_w_count(Arrow::Down, 1, b).move_to_line_start(),
                end,
            ),
            (TextObject::LineStart, false) => (
                start,
                end.arr_w_count(Arrow::Down, 1, b).move_to_line_start(),
            ),
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
            (TextObject::Arr(arr), _) => return arr.extend_dot_backward(b),
            (TextObject::BufferEnd, _) => return b.dot.clone(), // Can't move back to the buffer end
            (TextObject::BufferStart, true) => (Cur::buffer_start(), end),
            (TextObject::BufferStart, false) => (Cur::buffer_start(), start),
            (TextObject::Character, true) => (start.arr_w_count(Arrow::Left, 1, b), end),
            (TextObject::Character, false) => (start, end.arr_w_count(Arrow::Left, 1, b)),
            (TextObject::Line, true) => (start.arr_w_count(Arrow::Up, 1, b), end),
            (TextObject::Line, false) => (start, end.arr_w_count(Arrow::Up, 1, b)),
            (TextObject::LineEnd, true) => {
                (start.arr_w_count(Arrow::Up, 1, b).move_to_line_end(b), end)
            }
            (TextObject::LineEnd, false) => {
                (start, end.arr_w_count(Arrow::Up, 1, b).move_to_line_end(b))
            }
            (TextObject::LineStart, true) => {
                if start.x == 0 {
                    (start.arr_w_count(Arrow::Up, 1, b), end)
                } else {
                    start.x = 0;
                    (start, end)
                }
            }
            (TextObject::LineStart, false) => {
                end.x = 0;
                (start, end)
            }
        };

        Dot::Range {
            r: Range::from_cursors(start, end, start_active),
        }
    }
}
