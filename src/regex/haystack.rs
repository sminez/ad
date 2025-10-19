//! A haystack is something that can be searched over by a Regex
use crate::buffer::{Buffer, GapBuffer, IdxChars};
use std::{
    iter::{Enumerate, Skip},
    str::Chars,
};

/// Something that can be searched over by a [Regex].
///
/// The interface exposed by this trait supports searching over streaming data if needed but at the
/// cost of reduced performance.
///
/// [0]: crate::regex::Regex
pub trait Haystack<'a> {
    type Iter: Iterator<Item = (usize, char)>;

    fn try_make_contiguous(&mut self) -> bool;
    fn substr_from(&'a self, byte_offset: usize) -> &'a str;
    fn iter_from(&'a self, char_from: usize) -> Option<Self::Iter>;
}

impl<'a> Haystack<'a> for &'a str {
    type Iter = Skip<Enumerate<Chars<'a>>>;

    fn try_make_contiguous(&mut self) -> bool {
        true
    }

    fn substr_from(&'a self, byte_offset: usize) -> &'a str {
        let raw = &self.as_bytes()[byte_offset..];

        // SAFETY: assumes a valid byte offset
        unsafe { std::str::from_utf8_unchecked(raw) }
    }

    fn iter_from(&self, char_from: usize) -> Option<Self::Iter> {
        // This is not at all efficient but we only really make use of strings in test cases where
        // the length of the string is small. For the "real" impls using GapBuffers, checking the number
        // of chars in the buffer is O(1) as we cache it.
        if char_from >= self.chars().count() {
            None
        } else {
            Some(self.chars().enumerate().skip(char_from))
        }
    }
}

impl<'a> Haystack<'a> for GapBuffer {
    type Iter = IdxChars<'a>;

    fn try_make_contiguous(&mut self) -> bool {
        self.make_contiguous();
        true
    }

    fn substr_from(&'a self, byte_offset: usize) -> &'a str {
        // SAFETY: assumes make_contiguous was called first
        unsafe { self.substr_from(byte_offset) }
    }

    fn iter_from(&'a self, char_from: usize) -> Option<Self::Iter> {
        if char_from >= self.len_chars() {
            None
        } else {
            Some(
                self.slice(char_from, self.len_chars())
                    .indexed_chars(char_from, false),
            )
        }
    }
}

impl<'a> Haystack<'a> for Buffer {
    type Iter = IdxChars<'a>;

    fn try_make_contiguous(&mut self) -> bool {
        self.txt.make_contiguous();
        true
    }

    fn substr_from(&'a self, byte_offset: usize) -> &'a str {
        // SAFETY: assumes make_contiguous was called first
        unsafe { self.txt.substr_from(byte_offset) }
    }

    fn iter_from(&'a self, char_from: usize) -> Option<Self::Iter> {
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
}
