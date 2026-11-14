//! Representations of the matches reported by a [Regex].
use crate::{
    buffer::{ByteIdx, ByteOffset},
    regex::{Haystack, Regex},
};
use regex_automata::util::captures as ra_caps;
use std::borrow::Cow;

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
#[derive(Debug, Clone)]
pub struct Captures {
    pub(super) inner: ra_caps::Captures,
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
        let span = self.inner.get_match().unwrap().span();

        (ByteIdx(span.start), ByteIdx(span.end))
    }

    fn sub_loc_by_name(&self, name: &str) -> Option<(ByteIdx, ByteIdx)> {
        let span = self.inner.get_group_by_name(name)?;

        Some((ByteIdx(span.start), ByteIdx(span.end)))
    }

    pub(crate) fn sub_loc(&self, n: usize) -> Option<(ByteIdx, ByteIdx)> {
        let span = self.inner.get_group(n)?;

        Some((ByteIdx(span.start), ByteIdx(span.end)))
    }

    pub fn iter_locs(&self) -> impl Iterator<Item = Option<(ByteIdx, ByteIdx)>> {
        let mut n = 0;

        std::iter::from_fn(move || {
            if n > self.inner.group_len() {
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
