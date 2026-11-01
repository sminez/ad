//! A haystack is something that can be searched over by a Regex
use crate::buffer::{Buffer, ByteIdx, ByteOffset, CharIdx, GapBuffer};
use std::borrow::Cow;

/// Something that can be searched over by a [Regex][0].
///
/// The interface exposed by this trait supports searching over streaming data if needed but at the
/// cost of reduced performance.
///
/// [0]: crate::regex::Regex
#[expect(clippy::len_without_is_empty)]
pub trait Haystack {
    fn try_make_contiguous(&mut self);
    fn is_contiguous(&self) -> bool;
    fn len(&self) -> usize;
    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>>;
    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str>;

    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx>;
    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx>;

    fn iter_from(&self, from: ByteIdx) -> Option<impl Iterator<Item = (ByteIdx, char)>>;
    fn iter_between(&self, from: ByteIdx, to: ByteIdx) -> impl Iterator<Item = (ByteIdx, char)>;
    fn rev_iter_between(&self, from: ByteIdx, to: ByteIdx)
    -> impl Iterator<Item = (ByteIdx, char)>;
}

impl Haystack for &str {
    fn try_make_contiguous(&mut self) {}

    fn is_contiguous(&self) -> bool {
        true
    }

    fn len(&self) -> usize {
        str::len(self)
    }

    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>> {
        if offset.0 > self.len() {
            None
        } else {
            let raw = &self.as_bytes()[offset.0..];
            // SAFETY: assumes a valid byte offset
            Some(Cow::Borrowed(unsafe { std::str::from_utf8_unchecked(raw) }))
        }
    }

    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str> {
        Cow::Borrowed(&self[from.0..to.0])
    }

    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx> {
        Some(CharIdx(
            self.char_indices()
                .take_while(|&(idx, _)| idx < byte_idx.0)
                .count(),
        ))
    }

    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx> {
        self.char_indices()
            .nth(char_idx.0)
            .map(|(idx, _)| ByteIdx(idx))
    }

    fn iter_from(&self, from: ByteIdx) -> Option<impl Iterator<Item = (ByteIdx, char)>> {
        if from.0 >= self.len() {
            None
        } else {
            Some(
                self[from.0..]
                    .char_indices()
                    .map(move |(i, ch)| (from + ByteOffset(i as isize), ch)),
            )
        }
    }

    fn iter_between(&self, from: ByteIdx, to: ByteIdx) -> impl Iterator<Item = (ByteIdx, char)> {
        self[from.0..to.0]
            .char_indices()
            .map(move |(i, ch)| (from + ByteOffset(i as isize), ch))
    }

    fn rev_iter_between(
        &self,
        from: ByteIdx,
        to: ByteIdx,
    ) -> impl Iterator<Item = (ByteIdx, char)> {
        self[from.0..to.0]
            .char_indices()
            .map(move |(i, ch)| (from + ByteOffset(i as isize), ch))
            .rev()
    }
}

impl Haystack for GapBuffer {
    fn try_make_contiguous(&mut self) {
        self.make_contiguous();
    }

    fn is_contiguous(&self) -> bool {
        self.is_contiguous()
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>> {
        if offset.0 > self.len() {
            None
        } else {
            // SAFETY: assumes make_contiguous was called first
            Some(Cow::Borrowed(unsafe { self.substr_from(offset) }))
        }
    }

    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str> {
        self.slice_from_byte_offsets(from, to).into()
    }

    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx> {
        if byte_idx.0 > self.len() {
            None
        } else {
            Some(self.byte_to_char(byte_idx))
        }
    }

    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx> {
        if char_idx.0 > self.len_chars() {
            None
        } else {
            Some(self.char_to_byte(char_idx))
        }
    }

    fn iter_from(&self, from: ByteIdx) -> Option<impl Iterator<Item = (ByteIdx, char)>> {
        if from.0 >= self.len() {
            None
        } else {
            let mut acc = from;
            Some(
                self.slice_from_byte_offsets(from, ByteIdx(self.len()))
                    .chars()
                    .map(move |ch| {
                        let item = (acc, ch);
                        acc += ByteOffset::from(ch);
                        item
                    }),
            )
        }
    }

    fn iter_between(&self, from: ByteIdx, to: ByteIdx) -> impl Iterator<Item = (ByteIdx, char)> {
        let mut acc = from;

        self.slice_from_byte_offsets(from, to)
            .chars()
            .map(move |ch| {
                let item = (acc, ch);
                acc += ByteOffset::from(ch);
                item
            })
    }

    fn rev_iter_between(
        &self,
        from: ByteIdx,
        to: ByteIdx,
    ) -> impl Iterator<Item = (ByteIdx, char)> {
        let mut acc = to;

        self.slice_from_byte_offsets(from, to)
            .rev_chars()
            .map(move |ch| {
                acc -= ByteOffset::from(ch);
                (acc, ch)
            })
    }
}

impl Haystack for Buffer {
    fn try_make_contiguous(&mut self) {
        self.txt.make_contiguous();
    }

    fn is_contiguous(&self) -> bool {
        self.txt.is_contiguous()
    }

    fn len(&self) -> usize {
        self.txt.len()
    }

    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>> {
        if offset.0 > self.txt.len() {
            None
        } else {
            // SAFETY: assumes make_contiguous was called first
            Some(Cow::Borrowed(unsafe { self.txt.substr_from(offset) }))
        }
    }

    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str> {
        self.txt.slice_from_byte_offsets(from, to).into()
    }

    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx> {
        if byte_idx.0 > self.txt.len() {
            None
        } else {
            Some(self.txt.byte_to_char(byte_idx))
        }
    }

    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx> {
        if char_idx.0 > self.txt.len_chars() {
            None
        } else {
            Some(self.txt.char_to_byte(char_idx))
        }
    }

    fn iter_from(&self, from: ByteIdx) -> Option<impl Iterator<Item = (ByteIdx, char)>> {
        if from.0 >= self.len() {
            None
        } else {
            let mut acc = from;
            Some(
                self.txt
                    .slice_from_byte_offsets(from, ByteIdx(self.len()))
                    .chars()
                    .map(move |ch| {
                        let item = (acc, ch);
                        acc += ByteOffset::from(ch);
                        item
                    }),
            )
        }
    }

    fn iter_between(&self, from: ByteIdx, to: ByteIdx) -> impl Iterator<Item = (ByteIdx, char)> {
        let mut acc = from;

        self.txt
            .slice_from_byte_offsets(from, to)
            .chars()
            .map(move |ch| {
                let item = (acc, ch);
                acc += ByteOffset::from(ch);
                item
            })
    }

    fn rev_iter_between(
        &self,
        from: ByteIdx,
        to: ByteIdx,
    ) -> impl Iterator<Item = (ByteIdx, char)> {
        let mut acc = to;

        self.txt
            .slice_from_byte_offsets(from, to)
            .rev_chars()
            .map(move |ch| {
                acc -= ByteOffset::from(ch);
                (acc, ch)
            })
    }
}
