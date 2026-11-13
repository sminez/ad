//! Reference types and iterators for working with discontiguous views on a [GapBuffer].
use crate::buffer::internal::{
    ByteIdx, CharIdx, GapBuffer, RawIdx,
    utf8::{count_chars, decode_char_at, decode_char_ending_at},
};
use std::{borrow::Cow, cmp::min, fmt};

/// A view on a region of a [GapBuffer].
///
/// [Slice] makes no guarantees about whether or not the underlying memory allocation being pointed
/// to is contiguous. As such, you can always construct a [Cow<'a, str>] from a Slice but not a
/// `&str`. If you absolutely _must_ work with a single `&str` without cloning the underlying
/// data, use a [View][crate::buffer::internal::View] instead.
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Slice<'a> {
    /// The logical byte offset that this slice was taken from
    pub(super) from: ByteIdx,
    pub(super) left: &'a [u8],
    pub(super) right: &'a [u8],
}

impl<'a> From<Slice<'a>> for Cow<'a, str> {
    fn from(s: Slice<'a>) -> Self {
        if s.left.is_empty() {
            // SAFETY: we know that we have valid utf8 data internally
            Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(s.right) })
        } else if s.right.is_empty() {
            // SAFETY: we know that we have valid utf8 data internally
            Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(s.left) })
        } else {
            Cow::Owned(s.to_string())
        }
    }
}

impl<'a> Slice<'a> {
    pub(super) const NULL: Slice<'a> = Slice {
        from: ByteIdx(0),
        left: &[],
        right: &[],
    };

    pub fn is_contiguous(&self) -> bool {
        self.left.is_empty() || self.right.is_empty()
    }

    /// The logical byte offset that this slice was taken from.
    pub fn from(&self) -> ByteIdx {
        self.from
    }

    #[inline]
    pub(super) fn from_raw_offsets(from: RawIdx, to: RawIdx, gb: &'a GapBuffer) -> Slice<'a> {
        let to = min(to, RawIdx(gb.data.len()));
        let logical_from = gb.raw_byte_to_byte(from);

        if to <= gb.gap_start || from >= gb.gap_end {
            return Slice {
                from: logical_from,
                left: gb.raw_slice(from..to),
                right: &[],
            };
        }

        debug_assert!(from <= gb.gap_start, "line offset sits in gap");

        Slice {
            from: logical_from,
            left: gb.raw_slice(from..gb.gap_start),
            right: gb.raw_slice(gb.gap_end..to),
        }
    }

    pub fn subslice(&self, from: ByteIdx, to: ByteIdx) -> Slice<'_> {
        if from == to {
            return Slice::NULL;
        }

        let to = min(to, ByteIdx(self.len()));
        let logical_from = self.from + from;

        if to.0 <= self.left.len() {
            Slice {
                from: logical_from,
                left: &self.left[from.0..to.0],
                right: &[],
            }
        } else if from.0 >= self.left.len() {
            Slice {
                from: logical_from,
                left: &[],
                right: &self.right[from.0..to.0],
            }
        } else {
            Slice {
                from: logical_from,
                left: &self.left[from.0..],
                right: &self.right[..to.0],
            }
        }
    }

    pub fn subslice_from(&self, from: ByteIdx) -> Slice<'_> {
        self.subslice(from, ByteIdx(self.len()))
    }

    /// The number of utf-8 characters within this slice.
    ///
    /// Calculating involves parsing the entire slice as utf-8.
    pub fn len_utf8(&self) -> usize {
        self.chars().count()
    }

    pub fn len(&self) -> usize {
        self.left.len() + self.right.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// The two sides of this slice as &str references
    pub fn as_strs(&self) -> (&str, &str) {
        // SAFETY: we know that we have valid utf8 data internally
        unsafe {
            (
                std::str::from_utf8_unchecked(self.left),
                std::str::from_utf8_unchecked(self.right),
            )
        }
    }

    /// The two sides of this slice as &[u8] slices
    pub fn as_slices(&self) -> (&[u8], &[u8]) {
        (self.left, self.right)
    }

    /// Iterate over the contiguous &[u8] regions within this slice
    pub fn slice_iter(self) -> SliceIter<'a> {
        SliceIter {
            inner: self,
            pos: Some(false),
        }
    }

    /// Iterate over the characters in this slice
    pub fn chars(self) -> Chars<'a> {
        Chars { s: self, cur: 0 }
    }

    /// Iterate over the characters in this slice in reverse
    pub fn rev_chars(self) -> RevChars<'a> {
        RevChars {
            s: self,
            cur: self.left.len() + self.right.len(),
        }
    }

    /// Iterate over the characters in this slice with their corresponding character indices
    pub fn indexed_chars(self, from: CharIdx, rev: bool) -> IdxChars<'a> {
        let (cur, idx) = if rev {
            (
                self.left.len() + self.right.len(),
                from.uadd(count_chars(self.left) + count_chars(self.right)),
            )
        } else {
            (0, from)
        };

        IdxChars {
            s: self,
            cur,
            idx,
            rev,
        }
    }

    fn cur_and_data(&self, cur: usize) -> (usize, &[u8]) {
        if cur < self.left.len() {
            (cur, self.left)
        } else {
            (cur - self.left.len(), self.right)
        }
    }
}

impl fmt::Display for Slice<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut v = Vec::with_capacity(self.left.len() + self.right.len());
        v.extend_from_slice(self.left);
        v.extend_from_slice(self.right);

        match String::from_utf8(v) {
            Ok(s) => write!(f, "{s}"),
            Err(_) => Err(fmt::Error),
        }
    }
}

impl<'a> PartialEq<&'a str> for Slice<'_> {
    fn eq(&self, other: &&'a str) -> bool {
        let b = other.as_bytes();
        if b.len() != self.left.len() + self.right.len() {
            return false;
        }

        &b[..self.left.len()] == self.left && &b[self.left.len()..] == self.right
    }
}

impl PartialEq<String> for Slice<'_> {
    fn eq(&self, other: &String) -> bool {
        *self == other.as_str()
    }
}

#[derive(Debug)]
pub struct SliceIter<'a> {
    inner: Slice<'a>,
    pos: Option<bool>,
}

impl<'a> Iterator for SliceIter<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<Self::Item> {
        match self.pos? {
            false => {
                self.pos = Some(true);
                Some(self.inner.left)
            }

            true => {
                self.pos = None;
                Some(self.inner.right)
            }
        }
    }
}

/// An iterator of characters from a [Slice]
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Chars<'a> {
    pub(super) s: Slice<'a>,
    pub(super) cur: usize,
}

impl Iterator for Chars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur >= self.s.left.len() + self.s.right.len() {
            return None;
        }

        let (cur, data) = self.s.cur_and_data(self.cur);
        // SAFETY: we know we are in bounds and that we contain valid utf-8 data
        let ch = unsafe { decode_char_at(cur, data) };
        self.cur += ch.len_utf8();

        Some(ch)
    }
}

/// An iterator of characters from a [Slice]
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct RevChars<'a> {
    pub(super) s: Slice<'a>,
    pub(super) cur: usize,
}

impl Iterator for RevChars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur == 0 {
            return None;
        }

        let (cur, data) = self.s.cur_and_data(self.cur - 1);
        // SAFETY: we know we are in bounds and that we contain valid utf-8 data
        let ch = unsafe { decode_char_ending_at(cur, data) };
        self.cur -= ch.len_utf8();

        Some(ch)
    }
}

/// An iterator of characters and their indices from a [Slice]
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct IdxChars<'a> {
    s: Slice<'a>,
    cur: usize,
    idx: CharIdx,
    rev: bool,
}

impl Iterator for IdxChars<'_> {
    type Item = (CharIdx, char);

    fn next(&mut self) -> Option<Self::Item> {
        if (!self.rev && self.cur >= self.s.left.len() + self.s.right.len())
            || (self.rev && self.cur == 0)
        {
            return None;
        }

        if self.rev {
            let (cur, data) = self.s.cur_and_data(self.cur - 1);
            // SAFETY: we know we are in bounds and that we contain valid utf-8 data
            let ch = unsafe { decode_char_ending_at(cur, data) };
            let len = ch.len_utf8();
            self.idx = self.idx.usub(1);
            self.cur -= len;
            Some((self.idx, ch))
        } else {
            let (cur, data) = self.s.cur_and_data(self.cur);
            // SAFETY: we know we are in bounds and that we contain valid utf-8 data
            let ch = unsafe { decode_char_at(cur, data) };
            let len = ch.len_utf8();
            let res = Some((self.idx, ch));
            self.cur += len;
            self.idx = self.idx.uadd(1);
            res
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::internal::{LineIdx, raw_debug_buffer_content};
    use simple_test_case::test_case;

    #[test_case(0, 0, "hello, world!\n"; "first line cur at BOF")]
    #[test_case(0, 1, "how are you?"; "second line cur at BOF")]
    #[test_case(26, 0, "hello, world!\n"; "first line cur at EOF")]
    #[test_case(26, 1, "how are you?"; "second line cur at EOF")]
    #[test_case(10, 0, "hello, world!\n"; "first line cur in line")]
    #[test_case(10, 1, "how are you?"; "second line cur in line")]
    #[test]
    fn slice_to_string_works(cur: usize, line: usize, expected: &str) {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");
        gb.move_gap_to(ByteIdx(cur));
        gb.shred_gap();

        assert_eq!(gb.line(LineIdx(line)).to_string(), expected);
    }

    #[test]
    fn chars_work() {
        let s1 = "hello, world!\n";
        let s2 = "how are you?";
        let gb = GapBuffer::from(format!("{s1}{s2}"));

        let l1_chars: String = gb.line(LineIdx(0)).chars().collect();
        assert_eq!(l1_chars, s1);

        let l2_chars: String = gb.line(LineIdx(1)).chars().collect();
        assert_eq!(l2_chars, s2);
    }

    #[test_case(
        "hello, 世界!", false,
        &[(0, 'h'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o'),
          (5, ','), (6, ' '), (7, '世'), (8, '界'), (9, '!')];
        "multi-byte block forward"
    )]
    #[test_case(
        "hello, 世界!", true,
        &[(9, '!'), (8, '界'), (7, '世'), (6, ' '), (5, ','),
          (4, 'o'), (3, 'l'), (2, 'l'), (1, 'e'), (0, 'h')];
        "multi-byte block reversed"
    )]
    #[test_case(
        "foo│foo│foo", false,
        &[(0, 'f'), (1, 'o'), (2, 'o'), (3, '│'), (4, 'f'), (5, 'o'),
          (6, 'o'), (7, '│'), (8, 'f'), (9, 'o'), (10, 'o')];
        "interleaved forward"
    )]
    #[test_case(
        "foo│foo│foo", true,
        &[(10, 'o'), (9, 'o'), (8, 'f'), (7, '│'), (6, 'o'), (5, 'o'),
          (4, 'f'), (3, '│'), (2, 'o'), (1, 'o'), (0, 'f')];
        "interleaved reversed"
    )]
    #[test]
    fn indexed_chars_works(s: &str, rev: bool, expected: &[(usize, char)]) {
        let mut gb = GapBuffer::from(s);
        let v: Vec<(usize, char)> = gb
            .line(LineIdx(0))
            .indexed_chars(CharIdx(0), rev)
            .map(|(i, ch)| (i.0, ch))
            .collect();
        assert_eq!(&v, expected);

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(CharIdx(i));
            gb.move_gap_to(idx);
            gb.shred_gap();

            let v: Vec<(usize, char)> = gb
                .line(LineIdx(0))
                .indexed_chars(CharIdx(0), rev)
                .map(|(i, ch)| (i.0, ch))
                .collect();
            assert_eq!(&v, expected, "idx={idx}");
        }
    }

    #[test_case("foo│foo│foo"; "interleaved multibyte and ascii")]
    #[test_case("hello, 世界!"; "blocks of multibyte and ascii")]
    #[test]
    fn chars_works(s: &str) {
        let mut gb = GapBuffer::from(s);
        let chars: String = gb.line(LineIdx(0)).chars().collect();
        assert_eq!(chars, s);

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(CharIdx(i));
            gb.move_gap_to(idx);
            gb.shred_gap();

            let chars: String = gb.line(LineIdx(0)).chars().collect();
            assert_eq!(chars, s, "idx={idx}");
        }
    }

    #[test]
    fn slice_works() {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");
        let slice = Slice::from_raw_offsets(RawIdx(0), RawIdx(gb.cap), &gb);
        let (s1, s2) = slice.as_strs();
        assert_eq!(s1, "");
        assert_eq!(s2, "hello, world!\nhow are you?");

        let slice = gb.slice(CharIdx(6), CharIdx(17));
        let (s1, s2) = slice.as_strs();
        assert_eq!(s1, " world!\nhow");
        assert_eq!(s2, "");

        gb.move_gap_to(ByteIdx(12));
        gb.shred_gap();
        println!("after move:  {:?}", raw_debug_buffer_content(&gb));

        let slice = gb.slice(CharIdx(6), CharIdx(17));
        let (s1, s2) = slice.as_strs();
        assert_eq!(s1, " world");
        assert_eq!(s2, "!\nhow");
    }

    #[test]
    fn null_slice_is_empty() {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(CharIdx(i));
            gb.move_gap_to(idx);
            gb.shred_gap();

            let slice = gb.slice(CharIdx(0), CharIdx(0));

            assert!(slice.left.is_empty(), "{i} slice left: {:?}", slice.left);
            assert!(slice.right.is_empty(), "{i} slice right: {:?}", slice.right);
        }
    }

    #[test]
    fn slice_eq_str_works() {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");
        gb.move_gap_to(ByteIdx(3));
        gb.shred_gap();
        let slice = gb.slice(CharIdx(0), CharIdx(5));
        assert_eq!(slice, "hello");
    }
}
