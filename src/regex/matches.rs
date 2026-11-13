//! Representations of the matches reported by a [Regex].
use crate::{
    buffer::{ByteIdx, ByteOffset},
    regex::{Haystack, Regex},
};
use std::{borrow::Cow, sync::Arc};

/// The match location of a Regex against a given input.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Match {
    pub pattern_id: usize,
    pub from: ByteIdx,
    pub to: ByteIdx,
}

impl Match {
    /// Extract this match from the given haystack
    pub fn match_text<'a, H>(&self, haystack: &'a H) -> Cow<'a, str>
    where
        H: Haystack,
    {
        haystack.substr(self.from, self.to)
    }
}

/// The match location of a Regex against a given input along with all capture groups.
///
/// The submatch indices are relative to the input used to run the original search.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Captures {
    pub(super) pattern_id: usize,
    pub(super) n_submatches: usize,
    pub(super) sub_matches: Vec<ByteIdx>,
    pub(super) submatch_names: Arc<[Option<Arc<str>>]>,
}

impl Captures {
    /// Extract this match from the given haystack
    pub fn match_text<'a, H>(&self, haystack: &'a H) -> Cow<'a, str>
    where
        H: Haystack,
    {
        let (from, to) = self.loc();

        haystack.substr(from, to)
    }

    /// Extract the given submatch by index if it exists
    pub fn submatch_text<'a, H>(&self, n: usize, haystack: &'a H) -> Option<Cow<'a, str>>
    where
        H: Haystack,
    {
        let (from, to) = self.sub_loc(n)?;

        Some(haystack.substr(from, to))
    }

    /// The names of all submatches.
    pub fn submatch_names(&self) -> &[Option<Arc<str>>] {
        &self.submatch_names
    }

    /// Extract the given submatch by name if it exists
    pub fn submatch_text_by_name<'a, H>(&self, name: &str, haystack: &'a H) -> Option<Cow<'a, str>>
    where
        H: Haystack,
    {
        let (from, to) = self.sub_loc_by_name(name)?;

        Some(haystack.substr(from, to))
    }

    /// The start and end of this match in terms of byte offsets
    pub fn loc(&self) -> (ByteIdx, ByteIdx) {
        let (start, end) = (self.sub_matches[0], self.sub_matches[1]);

        assert!(
            start <= end,
            "invalid match: {start} > {end}: {:?}",
            self.sub_matches
        );

        (start, end)
    }

    fn sub_loc_by_name(&self, name: &str) -> Option<(ByteIdx, ByteIdx)> {
        let n = self
            .submatch_names
            .iter()
            .position(|s| s.as_ref().map(|s| s.as_ref()) == Some(name))?;

        self.sub_loc(n)
    }

    pub(crate) fn sub_loc(&self, n: usize) -> Option<(ByteIdx, ByteIdx)> {
        if 2 * n + 1 >= self.sub_matches.len() {
            return None;
        }
        let (start, end) = (self.sub_matches[2 * n], self.sub_matches[2 * n + 1]);
        if n > 0 && start == ByteIdx(0) && end == ByteIdx(0) {
            return None;
        }

        assert!(
            start <= end,
            "invalid match: {start} > {end}: {:?}",
            self.sub_matches
        );

        Some((start, end))
    }

    pub fn iter_locs(&self) -> impl Iterator<Item = Option<(ByteIdx, ByteIdx)>> {
        let mut n = 0;

        std::iter::from_fn(move || {
            if n > self.n_submatches {
                None
            } else {
                let loc = self.sub_loc(n);
                n += 1;

                Some(loc)
            }
        })
    }

    /// The same as [Captures::iter_locs] but returning the raw usize offsets for use with APIs that
    /// do not make use of [ByteIdx].
    pub fn iter_raw_locs(&self) -> impl Iterator<Item = Option<(usize, usize)>> {
        self.iter_locs()
            .map(|loc| loc.map(|(from, to)| (from.0, to.0)))
    }
}

/// An iterator over sequential, non overlapping [Captures] of a Regex against a given input.
#[derive(Debug)]
pub struct CapturesIter<'a, H>
where
    H: Haystack,
{
    pub(super) haystack: &'a H,
    pub(super) re: &'a mut Regex,
    pub(super) from: ByteIdx,
}

impl<'a, H> Iterator for CapturesIter<'a, H>
where
    H: Haystack,
{
    type Item = Captures;

    fn next(&mut self) -> Option<Self::Item> {
        let mut m = self.re.captures_from(self.haystack, self.from)?;
        let (_, from) = m.loc();

        // If we had a zero length match we need to bump self.from along by a character so we don't
        // end up stuck repeating the same match each iteration.
        if from == self.from {
            let ch = self.haystack.substr_from(from)?.chars().next()?;
            self.from += ByteOffset::from(ch);

            // We start with self.from=0, so we allow a match at 0 to be emitted, but anything
            // after that is a repeat which we need to drop and move on to the next match.
            if from > ByteIdx(0) {
                m = self.re.captures_from(self.haystack, self.from)?;
                let (_, from) = m.loc();
                self.from = from;
            }
        } else {
            self.from = from;
        }

        Some(m)
    }
}
