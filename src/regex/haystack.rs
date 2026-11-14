//! A haystack is something that can be searched over by a Regex
use crate::buffer::{Buffer, ByteIdx, CharIdx, GapBuffer, Slice, View};
use std::borrow::Cow;

/// Something that can be searched over by a [Regex][0].
///
/// [0]: crate::regex::Regex
pub trait Haystack {
    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>>;
    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str>;
    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx>;
    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx>;
}

impl Haystack for &str {
    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>> {
        if offset.0 > self.len() {
            None
        } else {
            Some(Cow::Borrowed(&self[offset.0..]))
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
}

impl Haystack for GapBuffer {
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
}

impl Haystack for Buffer {
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
}

impl<'gb> Haystack for View<'gb> {
    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>> {
        if offset.0 > self.len() {
            None
        } else {
            Some(Cow::Borrowed(&self.as_str()[offset.0..]))
        }
    }

    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str> {
        Cow::Borrowed(&self.as_str()[from.0..to.0])
    }

    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx> {
        if byte_idx.0 > self.len() {
            None
        } else {
            Some(self.map_byte_index(byte_idx))
        }
    }

    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx> {
        if char_idx.0 > self.len_chars() {
            None
        } else {
            Some(self.map_char_index(char_idx))
        }
    }
}

impl<'gb> Haystack for Slice<'gb> {
    fn substr_from<'a>(&'a self, offset: ByteIdx) -> Option<Cow<'a, str>> {
        if offset.0 > self.len() {
            None
        } else {
            Some(self.subslice_from(offset).into())
        }
    }

    fn substr<'a>(&'a self, from: ByteIdx, to: ByteIdx) -> Cow<'a, str> {
        self.subslice(from, to).into()
    }

    fn byte_to_char(&self, byte_idx: ByteIdx) -> Option<CharIdx> {
        Some(CharIdx(
            Cow::from(*self)
                .char_indices()
                .take_while(|&(idx, _)| idx < byte_idx.0)
                .count(),
        ))
    }

    fn char_to_byte(&self, char_idx: CharIdx) -> Option<ByteIdx> {
        Cow::from(*self)
            .char_indices()
            .nth(char_idx.0)
            .map(|(idx, _)| ByteIdx(idx))
    }
}
