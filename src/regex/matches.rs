use crate::regex::{Haystack, Regex, vm::N_SLOTS};
use std::{borrow::Cow, sync::Arc};

/// The match location of a Regex against a given input.
///
/// The sub-match indices are relative to the input used to run the original match.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Match {
    pub(super) n_submatches: usize,
    pub(super) sub_matches: [usize; N_SLOTS],
    pub(super) submatch_names: Arc<[String]>,
}

impl Match {
    pub(crate) fn synthetic(from: usize, to: usize) -> Self {
        let mut sub_matches = [0; N_SLOTS];
        sub_matches[0] = from;
        sub_matches[1] = to;

        Self {
            n_submatches: 0,
            sub_matches,
            submatch_names: Arc::new([]),
        }
    }

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

    /// The names of each submatch
    pub fn named_matches(&self) -> Vec<&str> {
        let mut matches = Vec::new();
        for name in self.submatch_names.iter() {
            if self.sub_loc_by_name(name).is_some() {
                matches.push(name.as_str());
            }
        }

        matches
    }

    /// The start and end of this match in terms of byte offsets
    pub fn loc(&self) -> (usize, usize) {
        let (start, end) = (self.sub_matches[0], self.sub_matches[1]);

        assert!(
            start <= end,
            "invalid match: {start} > {end}: {:?}",
            self.sub_matches
        );

        (start, end)
    }

    fn sub_loc_by_name(&self, name: &str) -> Option<(usize, usize)> {
        let n = self.submatch_names.iter().position(|s| s == name)?;
        self.sub_loc(n + 1)
    }

    pub(crate) fn sub_loc(&self, n: usize) -> Option<(usize, usize)> {
        if 2 * n + 1 >= N_SLOTS {
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

    pub fn iter_locs(&self) -> impl Iterator<Item = Option<(usize, usize)>> {
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
}

/// An iterator over sequential, non overlapping matches of a Regex
/// against a given input
#[derive(Debug)]
pub struct MatchIter<'a, H>
where
    H: Haystack,
{
    pub(super) haystack: &'a H,
    pub(super) r: &'a mut Regex,
    pub(super) from: usize,
}

impl<'a, H> Iterator for MatchIter<'a, H>
where
    H: Haystack,
{
    type Item = Match;

    fn next(&mut self) -> Option<Self::Item> {
        let m = self.r.find_from(self.haystack, self.from)?;
        let (_, from) = m.loc();
        if from == self.from {
            self.from += self
                .haystack
                .substr_from(from)
                .unwrap()
                .chars()
                .next()
                .unwrap()
                .len_utf8();
        } else {
            self.from = from;
        }

        Some(m)
    }
}
