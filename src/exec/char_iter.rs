//! A helper trait for bounded iteration over characters in an arbitrary
//! piece of text.
use crate::{
    buffer::Buffer,
    exec::cached_stdin::{CachedStdin, CachedStdinIter},
    util::IdxRopeChars,
};
use ropey::Rope;

/// Something that can yield characters between two offsets from within
/// a piece of text.
///
/// To avoid boxing, the return of the methods provided by this trait
/// are an enum rather than a trait object.
pub trait IterBoundedChars {
    /// Iterate forward: from -> to
    ///
    /// This should be an inclusive range from..=to
    fn iter_between(&self, from: usize, to: usize) -> CharIter<'_>;

    /// Iterate backward: from -> to
    ///
    /// This should be an inclusive range from..=to
    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter<'_>;
}

impl IterBoundedChars for Rope {
    fn iter_between(&self, from: usize, to: usize) -> CharIter {
        CharIter::Rope(IdxRopeChars::new(self, from, to))
    }

    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter {
        CharIter::Rope(IdxRopeChars::new_reversed(self, from, to))
    }
}

impl IterBoundedChars for Buffer {
    fn iter_between(&self, from: usize, to: usize) -> CharIter {
        CharIter::Rope(IdxRopeChars::new(&self.txt, from, to))
    }

    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter {
        CharIter::Rope(IdxRopeChars::new_reversed(&self.txt, from, to))
    }
}

/// Supported iterator types that can be returned by an InterBoundedChars
pub enum CharIter<'a> {
    Rope(IdxRopeChars<'a>),
    StdIn(CachedStdinIter<'a>),
}

impl<'a> Iterator for CharIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Rope(it) => it.next(),
            Self::StdIn(it) => it.next(),
        }
    }
}

impl IterBoundedChars for CachedStdin {
    fn iter_between(&self, from: usize, to: usize) -> CharIter {
        CharIter::StdIn(CachedStdinIter {
            inner: self,
            from,
            to,
        })
    }

    /// This will always return None
    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter {
        CharIter::StdIn(CachedStdinIter {
            inner: self,
            from,
            to,
        })
    }
}
