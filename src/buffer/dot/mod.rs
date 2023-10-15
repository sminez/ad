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
use ropey::RopeSlice;
use std::{fmt, iter::Peekable};

mod cur;
mod range;
mod util;

pub(crate) use cur::Cur;
pub(crate) use range::{LineRange, Range};

use util::{
    cond::{blank_line, non_blank_line, Cond},
    consumer::consume_while,
    iter::{IdxChars, IdxLines, RevIdxChars, RevIdxLines},
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

    pub fn set_active_cur(&mut self, cur: Cur) {
        match self {
            Self::Cur { c } => *c = cur,
            Self::Range { r } => r.set_active_cursor(cur),
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
    pub fn as_range(&self) -> Range {
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

            TextObject::Paragraph => {
                let Range {
                    start,
                    end,
                    start_active,
                } = dot.as_range();

                Dot::Range {
                    r: Range {
                        start: start.to_prev_paragraph_start(b),
                        end: end.to_next_paragraph_end(b),
                        start_active,
                    },
                }
            }
            .collapse_null_range(),

            TextObject::Word => {
                let Range {
                    start,
                    end,
                    start_active,
                } = dot.as_range();

                Dot::Range {
                    r: Range {
                        start: start.to_prev_word_start(b),
                        end: end.to_next_word_end(b),
                        start_active,
                    },
                }
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
            (TextObject::Paragraph, true) => (start.to_next_paragraph_end(b), end),
            (TextObject::Paragraph, false) => (start, end.to_next_paragraph_end(b)),
            (TextObject::Word, true) => (start.to_next_word_end(b), end),
            (TextObject::Word, false) => (start, end.to_next_word_end(b)),
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
            (TextObject::Paragraph, true) => (start.to_prev_paragraph_start(b), end),
            (TextObject::Paragraph, false) => (start, end.to_prev_paragraph_start(b)),
            (TextObject::Word, true) => (start.to_prev_word_start(b), end),
            (TextObject::Word, false) => (start, end.to_prev_word_start(b)),
        };

        Dot::Range {
            r: Range::from_cursors(start, end, start_active),
        }
        .collapse_null_range()
    }
}

#[derive(Copy, Clone, PartialEq)]
enum CharKind {
    Whitespace,
    Alnum,
    NonAlnum,
}

impl From<char> for CharKind {
    fn from(ch: char) -> Self {
        if ch.is_whitespace() {
            Self::Whitespace
        } else if ch.is_alphanumeric() {
            Self::Alnum
        } else {
            Self::NonAlnum
        }
    }
}

fn next_word_boundary<I>(b: &Buffer, it: &mut Peekable<I>) -> Option<Cur>
where
    I: Iterator<Item = (usize, char)>,
{
    let (mut idx, k): (usize, CharKind) = match (it.next(), it.peek()) {
        // If we are on whitespace or the next character is whitespace then we need to
        // advance until we find the next word and start from there
        (Some((_, c1)), Some((_, c2))) if c1.is_whitespace() || c2.is_whitespace() => {
            consume_while(|c| c.is_whitespace(), it);
            match it.next() {
                Some((i, k)) => (i, k.into()),
                None => return None,
            }
        }

        // If the next kind doesn't match the current one then we're on a boundary and we
        // need to start from the following character not this one.
        (Some((_, c1)), Some((_, c2))) if CharKind::from(c1) != CharKind::from(*c2) => {
            let (i, c) = it.next().unwrap();
            (i, c.into())
        }

        // We're currently within a word
        (Some((i, c)), Some(_)) => (i, c.into()),

        // The next char is the end of our input
        _ => return None,
    };

    for (j, c) in it {
        let k2 = CharKind::from(c);
        if k2 != k {
            return Some(Cur::from_char_idx(idx, b));
        }
        idx = j;
    }

    None
}

fn next_paragraph_boundary<'a, I>(
    cond1: Cond<RopeSlice<'a>>,
    cond2: Cond<RopeSlice<'a>>,
    it: &mut Peekable<I>,
) -> Option<Cur>
where
    I: Iterator<Item = (usize, RopeSlice<'a>)>,
{
    match (it.next(), it.peek()) {
        (Some((_, current)), Some((_, next))) if (cond1)(&current) => {
            if (cond2)(next) {
                // On a boundary already so find the next one
                it.next();
                consume_while(cond2, it);
                if let Some(y) = consume_while(cond1, it) {
                    return Some(Cur { y, x: 0 });
                }
            } else if let Some(y) = consume_while(cond1, it) {
                return Some(Cur { y, x: 0 });
            }
        }

        (Some(_), Some(_)) => {
            consume_while(cond2, it);
            if let Some(y) = consume_while(cond1, it) {
                return Some(Cur { y, x: 0 });
            }
        }

        // Out of input
        _ => (),
    }

    None
}

impl Cur {
    // The following marks are where we should be moving to on successive
    // applications of this function.
    //
    // "  fn foo(bar: usize) -> anyhow::Result<()>  "
    //     ^   ^^  ^^     ^^  ^      ^ ^     ^   ^ ^
    fn to_next_word_end(self, b: &Buffer) -> Cur {
        next_word_boundary(b, &mut IdxChars::new(self, b)).unwrap_or_else(|| Cur::buffer_end(b))
    }

    // The following marks are where we should be moving to on successive
    // applications of this function.
    //
    // "  fn foo(bar: usize) -> anyhow::Result<()>  "
    //  ^ ^  ^  ^^  ^ ^    ^ ^  ^     ^ ^     ^
    fn to_prev_word_start(self, b: &Buffer) -> Cur {
        next_word_boundary(b, &mut RevIdxChars::new(self, b)).unwrap_or_else(Cur::buffer_start)
    }

    /// Advance the given cursor to the next "paragraph end".
    ///
    /// Following the behaviour of kakoune, this is defined as the last blank line before the
    /// next paragraph start.
    fn to_next_paragraph_end(self, b: &Buffer) -> Cur {
        next_paragraph_boundary(blank_line, non_blank_line, &mut IdxLines::new(self, b))
            .unwrap_or_else(|| Cur::buffer_end(b))
    }

    /// Reverse the given cursor to the previous "paragraph start".
    ///
    /// Following the behaviour of kakoune, this is defined as the first character of the
    /// first non-blank line of the previous paragraph.
    fn to_prev_paragraph_start(self, b: &Buffer) -> Cur {
        next_paragraph_boundary(non_blank_line, blank_line, &mut RevIdxLines::new(self, b))
            .unwrap_or_else(Cur::buffer_start)
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
    #[test_case(BufferEnd, c(9, 36); "buffer end")]
    #[test_case(Character, c(5, 2); "character")]
    #[test_case(Line, r(5, 0, 5, 43); "line")]
    #[test_case(LineEnd, c(5, 43); "line end")]
    #[test_case(LineStart, c(5, 0); "line start")]
    #[test]
    fn set_dot_works(to: TextObject, expected: Dot) {
        let mut b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        b.dot = c(5, 1); // Start of paragraph 2
        let dot = to.set_dot(b.dot, &b);

        assert_eq!(dot, expected);
    }

    fn cur(y: usize, x: usize) -> Cur {
        Cur { y, x }
    }

    #[test_case(cur(4, 0), cur(8, 0); "on paragraph start")]
    #[test_case(cur(1, 12), cur(4, 0); "in paragraph")]
    #[test_case(cur(3, 15), cur(4, 0); "on paragraph end")]
    #[test_case(cur(4, 0), cur(8, 0); "on single blank line")]
    #[test_case(cur(7, 0), cur(8, 0); "in multi blank line")]
    #[test_case(cur(9, 3), cur(9, 36); "in last paragraph")]
    #[test]
    fn to_next_paragraph_end_works(c: Cur, expected: Cur) {
        let b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        let pstart = c.to_next_paragraph_end(&b);
        assert_eq!(pstart, expected);
    }

    #[test_case(cur(9, 0), cur(5, 0); "on paragraph start")]
    #[test_case(cur(6, 12), cur(5, 0); "in paragraph")]
    #[test_case(cur(6, 42), cur(5, 0); "on paragraph end")]
    #[test_case(cur(7, 0), cur(5, 0); "on single blank line")]
    #[test_case(cur(8, 0), cur(5, 0); "in multi blank line")]
    #[test_case(cur(3, 5), cur(0, 0); "in first paragraph")]
    #[test]
    fn to_prev_paragraph_start_works(c: Cur, expected: Cur) {
        let b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        let pstart = c.to_prev_paragraph_start(&b);
        assert_eq!(pstart, expected);
    }
}
