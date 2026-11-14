//! Searching through the contents of a buffer for simple target patterns
//!
//! For more complex patterns, Regex should be used instead
use crate::{
    buffer::{Buffer, CharIdx},
    dot::{Cur, Dot},
};

/// A Find is able to locate its next occurrence within an indexed character stream and return
/// an optional pair of start/end indices denoting the location of the next match.
///
/// For more complex patterns, Regex should be used instead
pub trait Find {
    /// The type of the Find being used when running backwards through a buffer
    type Reversed: Find;

    /// Construct the reversed version of this matcher for searching backwards through a buffer
    fn reversed(&self) -> Self::Reversed;

    /// Attempt to locate the start/end indices of the next match of this Matcher within a
    /// character stream. Single character matches should be returned with start==end.
    fn try_find<I>(&self, it: I) -> Option<(CharIdx, CharIdx)>
    where
        I: Iterator<Item = (CharIdx, char)>;
}

pub fn find_forward<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Option<Dot> {
    find_between(f, cur.idx, CharIdx(b.txt.len_chars()), b)
}

pub fn find_forward_end<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Cur {
    find_forward(f, cur, b)
        .unwrap_or_else(|| Cur::buffer_end(b).into())
        .last_cur()
}

pub fn find_backward<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Option<Dot> {
    rev_find_between(f, cur.idx, CharIdx(0), b)
}

pub fn find_backward_start<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Cur {
    find_backward(f, cur, b).unwrap_or_default().first_cur()
}

fn match_to_dot(m: Option<(CharIdx, CharIdx)>) -> Option<Dot> {
    match m {
        Some((start, end)) if start == end => Some(Cur { idx: start }.into()),
        Some((start, end)) => Some(Dot::from_char_indices(start, end)),
        None => None,
    }
}

fn find_between<F: Find>(f: &F, from: CharIdx, to: CharIdx, b: &Buffer) -> Option<Dot> {
    match_to_dot(f.try_find(b.iter_between_chars(from, to)))
}

fn rev_find_between<F: Find>(f: &F, from: CharIdx, to: CharIdx, b: &Buffer) -> Option<Dot> {
    match_to_dot(f.reversed().try_find(b.rev_iter_between_chars(from, to)))
}

// Functions that check a single character
impl<F> Find for F
where
    F: Fn(char) -> bool + Copy,
{
    type Reversed = F;

    fn try_find<I>(&self, it: I) -> Option<(CharIdx, CharIdx)>
    where
        I: Iterator<Item = (CharIdx, char)>,
    {
        for (i, ch) in it {
            if (self)(ch) {
                return Some((i, i));
            }
        }

        None
    }

    fn reversed(&self) -> Self::Reversed {
        *self
    }
}

// Chars just need to locate themselves.
impl Find for char {
    type Reversed = char;

    fn try_find<I>(&self, it: I) -> Option<(CharIdx, CharIdx)>
    where
        I: Iterator<Item = (CharIdx, char)>,
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
