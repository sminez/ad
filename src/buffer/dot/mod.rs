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
use std::fmt;

mod cur;
mod range;
mod util;

pub(crate) use cur::Cur;
pub(crate) use range::{LineRange, Range};

use util::{
    cond::{alphanumeric, blank_line, non_alphanumeric, non_blank_line},
    consumer::{consume_on_boundary, consume_until, consume_while},
};

/// A Dot represents the currently selected contents of a Buffer.
///
/// Most of the editing commands available in ad which manipulate the buffer contents
/// do so via setting and manipulating the current dot. The name comes from the fact
/// that the representation of the current Dot in the editing language is `.`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dot {
    Cur { c: Cur },
    Range { r: Range },
}

impl Default for Dot {
    fn default() -> Self {
        Self::Cur { c: Cur::default() }
    }
}

impl fmt::Display for Dot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Cur { c } => write!(f, "{c}"),
            Self::Range { r } => write!(f, "{r}"),
        }
    }
}

impl Dot {
    /// The address representation of this dot in the form that is enterable by the user.
    /// Indices are 1-based rather than their internal 0-based representation.
    pub fn addr(&self) -> String {
        self.to_string()
    }

    pub fn content(&self, b: &Buffer) -> String {
        match self {
            Self::Cur { c } => b.txt.line(c.y).slice(c.x..(c.x + 1)).to_string(),
            Self::Range { r } => match r.as_inclusive_char_range(b) {
                Some(rng) => b.txt.slice(rng).to_string(),
                None => String::new(),
            },
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
    fn set_dot(&self, dot: Dot, b: &Buffer) -> Dot;

    #[must_use]
    fn set_dot_n(&self, mut dot: Dot, n: usize, b: &Buffer) -> Dot {
        for _ in 0..n {
            dot = self.set_dot(dot, b);
        }

        dot
    }

    #[must_use]
    fn extend_dot_forward(&self, dot: Dot, b: &Buffer) -> Dot;

    #[must_use]
    fn extend_dot_forward_n(&self, mut dot: Dot, n: usize, b: &Buffer) -> Dot {
        for _ in 0..n {
            dot = self.extend_dot_forward(dot, b);
        }

        dot
    }

    #[must_use]
    fn extend_dot_backward(&self, dot: Dot, b: &Buffer) -> Dot;

    #[must_use]
    fn extend_dot_backward_n(&self, mut dot: Dot, n: usize, b: &Buffer) -> Dot {
        for _ in 0..n {
            dot = self.extend_dot_backward(dot, b);
        }

        dot
    }
}

impl UpdateDot for Arrow {
    fn set_dot(&self, dot: Dot, b: &Buffer) -> Dot {
        Dot::Cur {
            c: dot.active_cur().arr_w_count(*self, 1, b),
        }
    }

    fn set_dot_n(&self, dot: Dot, n: usize, b: &Buffer) -> Dot {
        Dot::Cur {
            c: dot.active_cur().arr_w_count(*self, n, b),
        }
    }

    fn extend_dot_forward(&self, dot: Dot, b: &Buffer) -> Dot {
        self.extend_dot_forward_n(dot, 1, b)
    }

    fn extend_dot_forward_n(&self, dot: Dot, n: usize, b: &Buffer) -> Dot {
        match dot {
            Dot::Cur { c } => Dot::Range {
                r: Range::from_cursors(c, c.arr_w_count(*self, n, b), dot.start_active()),
            },
            Dot::Range { r } => Dot::Range {
                r: Range::from_cursors(r.start, r.end.arr_w_count(*self, n, b), dot.start_active()),
            }
            .collapse_null_range(),
        }
    }

    fn extend_dot_backward(&self, dot: Dot, b: &Buffer) -> Dot {
        self.extend_dot_backward_n(dot, 1, b)
    }

    fn extend_dot_backward_n(&self, dot: Dot, n: usize, b: &Buffer) -> Dot {
        match dot {
            Dot::Cur { .. } => self.flip().extend_dot_forward_n(dot, n, b),
            Dot::Range { r } => Dot::Range {
                r: Range::from_cursors(
                    r.start.arr_w_count(self.flip(), n, b),
                    r.end,
                    dot.start_active(),
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
    Paragraph,
    Word,
}

// This is working alright for now but not behaving quite right if the cursor is on the last
// line of a paragraph.
macro_rules! paragraph {
    (@fwd) => {
        [
            (consume_on_boundary, blank_line),
            (consume_until, non_blank_line),
        ]
    };

    (@bwd) => {
        [
            (consume_on_boundary, blank_line),
            (consume_on_boundary, non_blank_line),
            (consume_on_boundary, blank_line),
        ]
    };
}

// Again, _broadly_ working but not consistent with vim or kakoune in terms of what is treated
// as the start and end of a word. This is largely down to not being conditional on the previous
// character (which we would need to move correctly between whitespace, alphanumeric and "other"
// characters.
//
// -> vim and kakoune actually have different behaviours around what they treat as the target of
//    the "word" and "backwards word" text objects so there isn't even a consistent definition
//    to align with it seems?
macro_rules! word {
    (@fwd) => {
        [
            (consume_until, non_alphanumeric),
            (consume_until, alphanumeric),
        ]
    };
    (@bwd) => {
        [
            (consume_until, non_alphanumeric),
            (consume_until, alphanumeric),
            (consume_while, alphanumeric),
        ]
    };
}

impl UpdateDot for TextObject {
    fn set_dot(&self, dot: Dot, b: &Buffer) -> Dot {
        match self {
            TextObject::Arr(arr) => arr.set_dot(dot, b),

            TextObject::BufferEnd => Dot::Cur {
                c: Cur::buffer_end(b),
            },
            TextObject::BufferStart => Dot::Cur {
                c: Cur::buffer_start(),
            },
            TextObject::Character => Dot::Cur {
                c: dot.active_cur().arr_w_count(Arrow::Right, 1, b),
            },
            TextObject::LineEnd => Dot::Cur {
                c: dot.active_cur().move_to_line_end(b),
            },
            TextObject::LineStart => Dot::Cur {
                c: dot.active_cur().move_to_line_start(),
            },

            TextObject::Line => Dot::Range {
                r: dot.as_range().extend_to_line_start().extend_to_line_end(b),
            }
            .collapse_null_range(),

            TextObject::Paragraph => Dot::Range {
                r: dot
                    .as_range()
                    .extend_bwd_lines(b, paragraph!(@bwd))
                    .extend_fwd_lines(b, paragraph!(@fwd)),
            }
            .collapse_null_range(),
            TextObject::Word => Dot::Range {
                r: dot
                    .as_range()
                    .extend_bwd_chars(b, word!(@bwd))
                    .extend_fwd_chars(b, word!(@fwd)),
            }
            .collapse_null_range(),
        }
    }

    fn extend_dot_forward(&self, dot: Dot, b: &Buffer) -> Dot {
        let Range {
            mut start,
            mut end,
            start_active,
        } = dot.as_range();

        (start, end) = match (self, start_active) {
            (TextObject::Arr(arr), _) => return arr.extend_dot_forward(dot, b),
            (TextObject::BufferEnd, true) => (end, Cur::buffer_end(b)),
            (TextObject::BufferEnd, false) => (start, Cur::buffer_end(b)),
            (TextObject::BufferStart, _) => return dot, // Can't move forward to the buffer start
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
            (TextObject::Paragraph, true) => (start.fwd_lines(b, paragraph!(@fwd)), end),
            (TextObject::Paragraph, false) => (start, end.fwd_lines(b, paragraph!(@fwd))),
            (TextObject::Word, true) => (start.fwd_chars(b, word!(@fwd)), end),
            (TextObject::Word, false) => (start, end.fwd_chars(b, word!(@fwd))),
        };

        Dot::Range {
            r: Range::from_cursors(start, end, start_active),
        }
        .collapse_null_range()
    }

    fn extend_dot_backward(&self, dot: Dot, b: &Buffer) -> Dot {
        let Range {
            mut start,
            mut end,
            start_active,
        } = dot.as_range();

        (start, end) = match (self, start_active) {
            (TextObject::Arr(arr), _) => return arr.extend_dot_backward(dot, b),
            (TextObject::BufferEnd, _) => return dot, // Can't move back to the buffer end
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
            (TextObject::Paragraph, true) => (start.bwd_lines(b, paragraph!(@fwd)), end),
            (TextObject::Paragraph, false) => (start, end.bwd_lines(b, paragraph!(@bwd))),
            (TextObject::Word, true) => (start.bwd_chars(b, word!(@bwd)), end),
            (TextObject::Word, false) => (start, end.bwd_chars(b, word!(@bwd))),
        };

        Dot::Range {
            r: Range::from_cursors(start, end, start_active),
        }
        .collapse_null_range()
    }
}

#[cfg(test)]
mod tests {
    use super::{util::iter::IdxChars, TextObject::*, *};
    use simple_test_case::test_case;

    const EXAMPLE_TEXT: &str = "\
This is the first line of the file. Followed
by the second line. Some of the sentences are split
over multiple lines.
Others are not.

There is a second paragraph as well. But it
is quite short when compared to the first.

The third paragraph is even shorter.";

    fn c(y: usize, x: usize) -> Dot {
        Dot::Cur { c: Cur { y, x } }
    }

    fn r(y: usize, x: usize, y2: usize, x2: usize) -> Dot {
        Dot::Range {
            r: Range {
                start: Cur { y, x },
                end: Cur { y: y2, x: x2 },
                start_active: false,
            },
        }
    }

    #[test]
    fn idx_chars_works() {
        let b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        let from_it: Vec<(usize, char)> = IdxChars::new(Cur::buffer_start(), &b).collect();
        let expected: Vec<(usize, char)> = EXAMPLE_TEXT.chars().enumerate().collect();

        assert_eq!(from_it, expected);
    }

    #[test_case(BufferStart, c(0, 0); "buffer start")]
    #[test_case(BufferEnd, c(8, 36); "buffer end")]
    #[test_case(Character, c(5, 2); "character")]
    #[test_case(Line, r(5, 0, 5, 44); "line")]
    #[test_case(LineEnd, c(5, 44); "line end")]
    #[test_case(LineStart, c(5, 0); "line start")]
    #[test_case(Paragraph, r(5, 0, 6, 0); "paragraph")]
    #[test]
    fn set_dot_works(to: TextObject, expected: Dot) {
        let mut b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        b.dot = c(5, 1); // Start of paragraph 2
        let dot = to.set_dot(b.dot, &b);

        assert_eq!(dot, expected);
    }
}
