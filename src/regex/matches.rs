use super::vm::Regex;
use crate::buffer::{GapBuffer, IdxChars};
use std::{
    iter::{Enumerate, Skip},
    str::Chars,
};

/// The match location of a Regex against a given input.
///
/// The sub-match indices are relative to the input used to run the original match.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Match {
    pub(super) sub_matches: [usize; 20],
}

impl Match {
    pub(crate) fn synthetic(from: usize, to: usize) -> Self {
        let mut sub_matches = [0; 20];
        sub_matches[0] = from;
        sub_matches[1] = to;
        Self { sub_matches }
    }

    pub(crate) fn apply_offset(&mut self, offset: isize) {
        for i in 0..20 {
            if i > 0 && self.sub_matches[i] == 0 {
                continue;
            }
            self.sub_matches[i] = (self.sub_matches[i] as isize + offset) as usize;
        }
    }

    pub fn str_match_text(&self, s: &str) -> String {
        let (a, b) = self.loc();
        s.chars().skip(a).take(b - a).collect()
    }

    pub fn str_match_text_ref<'a>(&self, s: &'a str) -> &'a str {
        let (a, b) = self.loc();
        let mut it = s.char_indices().skip(a);
        let (first, _) = it.next().unwrap();
        let (last, _) = it.take(b - a - 1).last().unwrap_or((first, ' '));

        &s[first..=last]
    }

    pub fn str_submatch_text(&self, n: usize, s: &str) -> Option<String> {
        let (a, b) = self.sub_loc(n)?;
        Some(s.chars().skip(a).take(b - a).collect())
    }

    pub fn loc(&self) -> (usize, usize) {
        let (start, end) = (self.sub_matches[0], self.sub_matches[1]);

        assert!(
            start <= end,
            "invalid match: {start} > {end}: {:?}",
            self.sub_matches
        );

        (start, end)
    }

    pub fn sub_loc(&self, n: usize) -> Option<(usize, usize)> {
        if n > 9 {
            return None;
        }
        let (start, end) = (self.sub_matches[2 * n], self.sub_matches[2 * n + 1]);
        if n > 0 && start == 0 && end == 0 {
            return None;
        }

        assert!(
            start <= end,
            "invalid match: {start} > {end}: {:?}",
            self.sub_matches
        );

        Some((start, end))
    }
}

pub trait IndexedChars {
    type I: Iterator<Item = (usize, char)>;
    fn iter_from(&self, from: usize) -> Option<Self::I>;
}

impl<'a> IndexedChars for &'a str {
    type I = Skip<Enumerate<Chars<'a>>>;

    fn iter_from(&self, from: usize) -> Option<Self::I> {
        // This is not at all efficient but we only really make use of strings in test cases where
        // the length of the string is small. For the "real" impls using GapBuffers, checking the number
        // of chars in the buffer is O(1) as we cache it.
        if from >= self.chars().count() {
            None
        } else {
            Some(self.chars().enumerate().skip(from))
        }
    }
}

impl<'a> IndexedChars for &'a GapBuffer {
    type I = IdxChars<'a>;

    fn iter_from(&self, from: usize) -> Option<Self::I> {
        if from >= self.len_chars() {
            None
        } else {
            Some(
                self.slice(from, self.len_chars())
                    .indexed_chars(from, false),
            )
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
        let m = self
            .r
            .match_iter(&mut self.it.iter_from(self.from)?, self.from)?;

        let (_, from) = m.loc();
        if from == self.from {
            self.from += 1;
        } else {
            self.from = from;
        }

        Some(m)
    }
}
