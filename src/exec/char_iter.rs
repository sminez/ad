//! A helper trait for bounded iteration over characters in an arbitrary
//! piece of text.
use crate::{
    buffer::{Buffer, GapBuffer, IdxChars},
    exec::cached_stdin::{CachedStdin, CachedStdinIter},
};

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

impl IterBoundedChars for GapBuffer {
    fn iter_between(&self, from: usize, to: usize) -> CharIter<'_> {
        CharIter::Slice(self.slice(from, to).indexed_chars(from, false))
    }

    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter<'_> {
        CharIter::Slice(self.slice(to, from).indexed_chars(to, true))
    }
}

impl IterBoundedChars for Buffer {
    fn iter_between(&self, from: usize, to: usize) -> CharIter<'_> {
        CharIter::Slice(self.txt.slice(from, to).indexed_chars(from, false))
    }

    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter<'_> {
        CharIter::Slice(self.txt.slice(to, from).indexed_chars(to, true))
    }
}

/// Supported iterator types that can be returned by an InterBoundedChars
#[derive(Debug)]
pub enum CharIter<'a> {
    Slice(IdxChars<'a>),
    StdIn(CachedStdinIter<'a>),
}

impl<'a> Iterator for CharIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Slice(it) => it.next(),
            Self::StdIn(it) => it.next(),
        }
    }
}

impl IterBoundedChars for CachedStdin {
    fn iter_between(&self, from: usize, to: usize) -> CharIter<'_> {
        CharIter::StdIn(CachedStdinIter {
            inner: self,
            from,
            to,
        })
    }

    /// This will always return None
    fn rev_iter_between(&self, from: usize, to: usize) -> CharIter<'_> {
        CharIter::StdIn(CachedStdinIter {
            inner: self,
            from,
            to,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case(0, 4; "first word")]
    #[test_case(5, 7; "second word")]
    #[test_case(0, 14; "full buffer")]
    #[test]
    fn buffer_rev_iter_between_covers_same_range_as_iter_between(from: usize, to: usize) {
        let b = Buffer::new_virtual(0, "test", "this is a test");
        let forward: Vec<char> = b.iter_between(from, to).map(|(_, c)| c).collect();
        let mut backward: Vec<char> = b.rev_iter_between(to, from).map(|(_, c)| c).collect();
        backward.reverse();

        assert_eq!(forward, backward);
    }
}
