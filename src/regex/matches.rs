use super::vm::Regex;
use ropey::{iter::Chars, Rope, RopeSlice};
use std::{iter::Skip, str::CharIndices};

/// The match location of a Regex against a given input.
///
/// The sub-match indices are relative to the input used to run the original match.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Match {
    pub(super) sub_matches: [usize; 20],
}

impl Match {
    pub fn str_match_text<'a>(&self, s: &'a str) -> &'a str {
        let (a, b) = self.loc();
        &s[a..b]
    }

    pub fn str_submatch_text<'a>(&self, n: usize, s: &'a str) -> Option<&'a str> {
        let (a, b) = self.sub_loc(n)?;
        Some(&s[a..b])
    }

    pub fn rope_match_text<'a>(&self, r: &'a Rope) -> RopeSlice<'a> {
        let (a, b) = self.loc();
        r.slice(a..b)
    }

    pub fn rope_submatch_text<'a>(&self, n: usize, r: &'a Rope) -> Option<RopeSlice<'a>> {
        let (a, b) = self.sub_loc(n)?;
        Some(r.slice(a..b))
    }

    pub fn loc(&self) -> (usize, usize) {
        (self.sub_matches[0], self.sub_matches[1])
    }

    pub fn sub_loc(&self, n: usize) -> Option<(usize, usize)> {
        if n > 9 {
            return None;
        }
        let (start, end) = (self.sub_matches[2 * n], self.sub_matches[2 * n + 1]);
        if start == end {
            return None;
        }

        Some((start, end))
    }
}

pub trait IndexedChars {
    type I: Iterator<Item = (usize, char)>;
    fn iter_from(&self, from: usize) -> Self::I;
}

impl<'a> IndexedChars for &'a str {
    type I = Skip<CharIndices<'a>>;

    fn iter_from(&self, from: usize) -> Self::I {
        self.char_indices().skip(from)
    }
}

pub struct IdxRopeChars<'a> {
    inner: Chars<'a>,
    from: usize,
}

impl<'a> Iterator for IdxRopeChars<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|c| {
            let res = (self.from, c);
            self.from += 1;

            res
        })
    }
}

impl<'a> IndexedChars for &'a Rope {
    type I = IdxRopeChars<'a>;

    fn iter_from(&self, from: usize) -> Self::I {
        IdxRopeChars {
            inner: self.chars_at(from),
            from,
        }
    }
}

/// An iterator over sequential, non overlapping matches of a Regex
/// against a given input
pub struct MatchIter<'a, I>
where
    I: IndexedChars,
{
    pub(super) it: I,
    pub(super) r: &'a mut Regex,
    pub(super) from: usize,
}

impl<'a, I> Iterator for MatchIter<'a, I>
where
    I: IndexedChars,
{
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        let m = self.r.match_iter(&mut self.it.iter_from(self.from))?;
        (_, self.from) = m.loc();

        Some(m)
    }
}
