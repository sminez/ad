//! Searching through the contents of a buffer for simple target patterns
//!
//! For more complex patterns, Regex should be used instead
use crate::{
    buffer::Buffer,
    dot::{Cur, Dot, Range},
    exec::IterBoundedChars,
};

/// A Find is able to locate its next occurance within an indexed character stream and return
/// an optional pair of start/end indicies denoting the location of the next match.
///
/// For more complex patterns, Regex should be used instead
pub trait Find {
    /// The type of the Find being used when running backwards through a buffer
    type Reversed: Find;

    /// Construct the reversed version of this matcher for searching backwards through a buffer
    fn reversed(&self) -> Self::Reversed;

    /// Attempt to locate the start/end indices of the next match of this Matcher within a
    /// character stream. Single character matches should be returned with start==end.
    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>;

    fn expand(&self, dot: Dot, b: &Buffer) -> Dot
    where
        Self: Sized,
    {
        let Range {
            mut start,
            mut end,
            start_active,
        } = dot.as_range();

        start = find_backward_start(self, start, b);
        end = find_forward_end(self, end, b);

        Dot::from(Range::from_cursors(start, end, start_active)).collapse_null_range()
    }
}

pub fn find_forward<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Option<Dot> {
    find_between(f, cur.idx, b.txt.len_chars(), b)
}

pub fn find_forward_end<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Cur {
    find_forward(f, cur, b)
        .unwrap_or_else(|| Cur::buffer_end(b).into())
        .last_cur()
}

pub fn find_forward_wrapping<F: Find>(f: &F, b: &Buffer) -> Option<Dot> {
    find_between(f, b.dot.last_cur().idx, b.txt.len_chars(), b)
        .or_else(|| find_between(f, 0, b.dot.last_cur().idx, b))
}

pub fn find_backward<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Option<Dot> {
    rev_find_between(f, cur.idx, 0, b)
}

pub fn find_backward_start<F: Find>(f: &F, cur: Cur, b: &Buffer) -> Cur {
    find_backward(f, cur, b).unwrap_or_default().first_cur()
}

// pub fn find_backward_wrapping<F: Find>(f: &F, b: &Buffer) -> Option<Dot> {
//     rev_find_between(f, b.dot.first_cur().idx, 0, b)
//         .or_else(|| rev_find_between(f, b.txt.len_chars(), b.dot.first_cur().idx, b))
// }

fn match_to_dot(m: Option<(usize, usize)>) -> Option<Dot> {
    match m {
        Some((start, end)) if start == end => Some(Cur { idx: start }.into()),
        Some((start, end)) => Some(Dot::from_char_indices(start, end)),
        None => None,
    }
}

fn find_between<F: Find>(f: &F, from: usize, to: usize, b: &Buffer) -> Option<Dot> {
    match_to_dot(f.try_find(b.iter_between(from, to)))
}

fn rev_find_between<F: Find>(f: &F, from: usize, to: usize, b: &Buffer) -> Option<Dot> {
    match_to_dot(f.reversed().try_find(b.rev_iter_between(from, to)))
}

// Functions that check a single character
impl Find for fn(char) -> bool {
    type Reversed = fn(char) -> bool;

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
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

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
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
impl<'a> Find for &'a str {
    type Reversed = String;

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
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
                return Some((start, i + 1));
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
impl Find for String {
    type Reversed = String;

    fn try_find<I>(&self, it: I) -> Option<(usize, usize)>
    where
        I: Iterator<Item = (usize, char)>,
    {
        self.as_str().try_find(it)
    }

    fn reversed(&self) -> Self::Reversed {
        self.chars().rev().collect()
    }
}
