//! A haystack is something that can be searched over by a Regex
use crate::buffer::{Buffer, GapBuffer};

/// Something that can be searched over by a [Regex][0].
///
/// The interface exposed by this trait supports searching over streaming data if needed but at the
/// cost of reduced performance.
///
/// [0]: crate::regex::Regex
pub trait Haystack {
    fn try_make_contiguous(&mut self);
    fn is_contiguous(&self) -> bool;
    fn substr_from(&self, byte_offset: usize) -> Option<&str>;

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize>;
    fn char_to_byte(&self, char_idx: usize) -> Option<usize>;

    fn iter_from(&self, char_from: usize) -> Option<impl Iterator<Item = (usize, char)>>;
    fn iter_between(&self, char_from: usize, char_to: usize)
    -> impl Iterator<Item = (usize, char)>;
    fn rev_iter_between(
        &self,
        char_from: usize,
        char_to: usize,
    ) -> impl Iterator<Item = (usize, char)>;
}

impl Haystack for &str {
    fn try_make_contiguous(&mut self) {}

    fn is_contiguous(&self) -> bool {
        true
    }

    fn substr_from(&self, byte_offset: usize) -> Option<&str> {
        if byte_offset > self.len() {
            None
        } else {
            let raw = &self.as_bytes()[byte_offset..];
            // SAFETY: assumes a valid byte offset
            Some(unsafe { std::str::from_utf8_unchecked(raw) })
        }
    }

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        Some(
            self.char_indices()
                .take_while(|&(idx, _)| idx < byte_idx)
                .count(),
        )
    }

    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        self.char_indices().nth(char_idx).map(|(idx, _)| idx)
    }

    fn iter_from(&self, char_from: usize) -> Option<impl Iterator<Item = (usize, char)>> {
        // This is not at all efficient but we only really make use of strings in test cases where
        // the length of the string is small. For the "real" impls using GapBuffers, checking the number
        // of chars in the buffer is O(1) as we cache it.
        if char_from >= self.chars().count() {
            None
        } else {
            Some(self.chars().enumerate().skip(char_from))
        }
    }

    fn iter_between(
        &self,
        char_from: usize,
        char_to: usize,
    ) -> impl Iterator<Item = (usize, char)> {
        // This is not at all efficient but we only really make use of strings in test cases where
        // the length of the string is small. For the "real" impls using GapBuffers, checking the number
        // of chars in the buffer is O(1) as we cache it.
        self.chars()
            .enumerate()
            .skip(char_from)
            .take(char_to.saturating_sub(char_from))
    }

    fn rev_iter_between(
        &self,
        _char_from: usize,
        _char_to: usize,
    ) -> impl Iterator<Item = (usize, char)> {
        std::iter::empty()
    }
}

impl Haystack for GapBuffer {
    fn try_make_contiguous(&mut self) {
        self.make_contiguous();
    }

    fn is_contiguous(&self) -> bool {
        self.is_contiguous()
    }

    fn substr_from(&self, byte_offset: usize) -> Option<&str> {
        if byte_offset > self.len() {
            None
        } else {
            // SAFETY: assumes make_contiguous was called first
            Some(unsafe { self.substr_from(byte_offset) })
        }
    }

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        if byte_idx > self.len() {
            None
        } else {
            Some(self.byte_to_char(byte_idx))
        }
    }

    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        if char_idx > self.len_chars() {
            None
        } else {
            Some(self.char_to_byte(char_idx))
        }
    }

    fn iter_from(&self, char_from: usize) -> Option<impl Iterator<Item = (usize, char)>> {
        if char_from >= self.len_chars() {
            None
        } else {
            Some(
                self.slice(char_from, self.len_chars())
                    .indexed_chars(char_from, false),
            )
        }
    }

    fn iter_between(
        &self,
        char_from: usize,
        char_to: usize,
    ) -> impl Iterator<Item = (usize, char)> {
        self.slice(char_from, char_to)
            .indexed_chars(char_from, false)
    }

    fn rev_iter_between(
        &self,
        char_from: usize,
        char_to: usize,
    ) -> impl Iterator<Item = (usize, char)> {
        self.slice(char_to, char_from).indexed_chars(char_to, true)
    }
}

impl Haystack for Buffer {
    fn try_make_contiguous(&mut self) {
        self.txt.make_contiguous();
    }

    fn is_contiguous(&self) -> bool {
        self.txt.is_contiguous()
    }

    fn substr_from(&self, byte_offset: usize) -> Option<&str> {
        if byte_offset > self.txt.len() {
            None
        } else {
            // SAFETY: assumes make_contiguous was called first
            Some(unsafe { self.txt.substr_from(byte_offset) })
        }
    }

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        if byte_idx > self.txt.len() {
            None
        } else {
            Some(self.txt.byte_to_char(byte_idx))
        }
    }

    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        if char_idx > self.txt.len_chars() {
            None
        } else {
            Some(self.txt.char_to_byte(char_idx))
        }
    }

    fn iter_from(&self, char_from: usize) -> Option<impl Iterator<Item = (usize, char)>> {
        if char_from >= self.txt.len_chars() {
            None
        } else {
            Some(
                self.txt
                    .slice(char_from, self.len_chars())
                    .indexed_chars(char_from, false),
            )
        }
    }

    fn iter_between(
        &self,
        char_from: usize,
        char_to: usize,
    ) -> impl Iterator<Item = (usize, char)> {
        self.txt
            .slice(char_from, char_to)
            .indexed_chars(char_from, false)
    }

    fn rev_iter_between(
        &self,
        char_from: usize,
        char_to: usize,
    ) -> impl Iterator<Item = (usize, char)> {
        self.txt
            .slice(char_to, char_from)
            .indexed_chars(char_to, true)
    }
}
