//! Searching through the contents of a buffer for a target patter
use crate::buffer::{
    dot::{
        util::iter::{IdxChars, RevIdxChars},
        Cur, Dot, Range,
    },
    Buffer,
};

/// A Matcher is able to locate its next occurance within an indexed character stream and return
/// an optional pair of start/end indicies denoting the location of the next match.
pub trait Matcher {
    /// The type of the matcher being used when running backwards through a buffer
    type Reversed: Matcher;

    /// Attempt to locate the start/end indices of the next match of this Matcher within a
    /// character stream. Single character matchers should be returned with start==end.
    fn try_match<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>;

    /// Construct the reversed version of this matcher for searching backwards through a buffer
    fn reversed(&self) -> Self::Reversed;

    /// Located the next match in a buffer from a given cursor position to the end of the buffer.
    fn match_forward_from(&self, cur: Cur, b: &Buffer) -> Option<Dot> {
        match self.try_match(IdxChars::new(cur, b)) {
            Some((start, end)) if start == end => Some(Dot::Cur {
                c: Cur::from_char_idx(start, b),
            }),
            Some((start, end)) => Some(Dot::Range {
                r: Range::from_cursors(
                    Cur::from_char_idx(start, b),
                    Cur::from_char_idx(end, b),
                    false,
                ),
            }),
            None => None,
        }
    }

    /// Located the next match in a buffer from a given cursor position to the end of
    /// the buffer, wrapping to the start of the buffer if no match is found.
    fn match_forward_from_wrapping(&self, cur: Cur, b: &Buffer) -> Option<Dot> {
        self.match_forward_from(cur, b)
            .or_else(|| self.match_forward_from(Cur::buffer_start(), b))
    }

    /// Located the next match backwards in a buffer from a given cursor position to
    /// the start of the buffer.
    fn match_backward_from(&self, cur: Cur, b: &Buffer) -> Option<Dot> {
        match self.reversed().try_match(RevIdxChars::new(cur, b)) {
            Some((start, end)) if start == end => Some(Dot::Cur {
                c: Cur::from_char_idx(start, b),
            }),
            Some((start, end)) => Some(Dot::Range {
                r: Range::from_cursors(
                    Cur::from_char_idx(start, b),
                    Cur::from_char_idx(end, b),
                    true,
                ),
            }),
            None => None,
        }
    }

    /// Located the next match backwards in a buffer from a given cursor position to
    /// the start of the buffer, wrapping to the start of the buffer if no match is found.
    fn match_backward_from_wrapping(&self, cur: Cur, b: &Buffer) -> Option<Dot> {
        self.match_backward_from(cur, b)
            .or_else(|| self.match_backward_from(Cur::buffer_end(b), b))
    }
}

// Chars just need to locate themselves.
impl Matcher for char {
    type Reversed = char;

    fn try_match<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        for (i, ch) in it {
            if ch == *self {
                return Some((i, i));
            }
        }

        None
    }

    fn reversed(&self) -> Self::Reversed {
        *self
    }
}

// Strings need to locate themselves character by character.
impl<'a> Matcher for &'a str {
    type Reversed = String;

    fn try_match<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        let chars: Vec<char> = self.chars().collect();
        let last = chars.len() - 1;
        let mut cix = 0;
        let mut start = 0;

        for (i, ch) in it {
            if ch != chars[cix] {
                start = 0;
                cix = 0;
                continue;
            }

            if cix == 0 {
                start = i;
            }

            if cix == last {
                return Some((start, i));
            }

            cix += 1;
        }

        None
    }

    fn reversed(&self) -> Self::Reversed {
        self.chars().rev().collect()
    }
}

// Deferring to &str for the impl
impl Matcher for String {
    type Reversed = String;

    fn try_match<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        self.as_str().try_match(it)
    }

    fn reversed(&self) -> Self::Reversed {
        self.chars().rev().collect()
    }
}
