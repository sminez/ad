use crate::regex::{
    Haystack,
    vm::{N_SLOTS, Regex},
};
use std::rc::Rc;

/// The match location of a Regex against a given input.
///
/// The sub-match indices are relative to the input used to run the original match.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Match {
    pub(super) sub_matches: [usize; N_SLOTS],
    pub(super) submatch_names: Rc<[String]>,
}

impl Match {
    pub(crate) fn synthetic(from: usize, to: usize) -> Self {
        let mut sub_matches = [0; N_SLOTS];
        sub_matches[0] = from;
        sub_matches[1] = to;
        Self {
            sub_matches,
            submatch_names: Rc::new([]),
        }
    }

    pub(crate) fn apply_offset(&mut self, offset: isize) {
        for i in 0..N_SLOTS {
            if i > 0 && self.sub_matches[i] == 0 {
                continue;
            }
            self.sub_matches[i] = (self.sub_matches[i] as isize + offset) as usize;
        }
    }

    /// Extract this match from the given string
    pub fn str_match_text(&self, s: &str) -> String {
        let (a, b) = self.loc();
        s.chars().skip(a).take(b - a).collect()
    }

    /// The start and end of this match in terms of byte offsets
    ///
    /// use loc for character offsets
    #[inline]
    pub fn str_loc_bytes(&self, s: &str) -> (usize, usize) {
        let (a, b) = self.loc();
        let mut it = s.char_indices().skip(a);
        let (first, _) = it.next().unwrap();
        let (last, _) = it.take(b - a - 1).last().unwrap_or((first, ' '));

        (first, last)
    }

    /// The start and end of the nth submatch in terms of byte offsets
    #[inline]
    pub fn str_sub_loc_bytes(&self, n: usize, s: &str) -> Option<(usize, usize)> {
        let (a, b) = self.sub_loc(n)?;
        let mut it = s.char_indices().skip(a);
        let (first, _) = it.next().unwrap();
        let (last, _) = it.take(b - a - 1).last().unwrap_or((first, ' '));

        Some((first, last))
    }

    // FIXME: this is a terrible way to do this but used for testing at the moment

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

    /// The start and end of a named submatch in terms of byte offsets
    #[inline]
    pub fn str_sub_loc_bytes_by_name(&self, name: &str, s: &str) -> Option<(usize, usize)> {
        let (a, b) = self.sub_loc_by_name(name)?;
        let mut it = s.char_indices().skip(a);
        let (first, _) = it.next().unwrap();
        let (last, _) = it.take(b - a - 1).last().unwrap_or((first, ' '));

        Some((first, last))
    }

    /// The contents of a named submatch
    pub fn str_sub_loc_text_ref_by_name<'a>(&self, name: &str, s: &'a str) -> Option<&'a str> {
        let (first, last) = self.str_sub_loc_bytes_by_name(name, s)?;

        Some(&s[first..=last])
    }

    /// The full match as applied to s
    pub fn str_match_text_ref<'a>(&self, s: &'a str) -> &'a str {
        let (first, last) = self.str_loc_bytes(s);

        &s[first..=last]
    }

    /// The numbered submatch match as applied to s
    pub fn str_submatch_text(&self, n: usize, s: &str) -> Option<String> {
        let (a, b) = self.sub_loc(n)?;
        Some(s.chars().skip(a).take(b - a).collect())
    }

    /// The start and end of this match in terms of character offsets
    ///
    /// use str_loc_bytes for byte offsets
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
            self.from += 1;
        } else {
            self.from = from;
        }

        Some(m)
    }
}
