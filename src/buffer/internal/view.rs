//! A contiguous view onto a GapBuffer
//!
//! Unlike a Slice (which makes no guarantees about the underlying GapBuffer state), a View ensures
//! that the underlying range of the GapBuffer which it exposes is a single contiguous region of
//! memory. For most purposes, a Slice is what you want -- it is cheaper to construct and provides
//! a broader API for working with the underlying data -- but when you _need_ a contiguous memory
//! allocation to work with, use a View.
use crate::buffer::{
    CharIdx,
    internal::{ByteIdx, GapBuffer, index::RawIdx},
};

/// A reference to a contiguous region from a [GapBuffer].
///
/// A [View] guarantees that the range of the gap buffer it exposes is contiguous. In order to do
/// this, constructing a View requires mutable access to the underlying [GapBuffer] in order to
/// move the gap out of the region you are taking a reference to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct View<'a> {
    gb: &'a GapBuffer,
    from: ByteIdx,
    to: ByteIdx,
    raw_from: RawIdx,
    raw_to: RawIdx,
    ch_from: CharIdx,
    ch_to: CharIdx,
}

impl<'a> View<'a> {
    /// Construct a new View pointing to the full underlying buffer content.
    ///
    /// Calls [GapBuffer::make_contiguous].
    pub fn new(gb: &'a mut GapBuffer) -> Self {
        Self::new_for_range(gb, ByteIdx(0), ByteIdx(gb.len()))
    }

    pub fn new_for_range(gb: &'a mut GapBuffer, from: ByteIdx, to: ByteIdx) -> Self {
        gb.make_contiguous();

        let raw_from = gb.byte_to_raw_byte(from);
        let raw_to = gb.byte_to_raw_byte(to);
        let ch_from = gb.byte_to_char(from);
        let ch_to = gb.byte_to_char(from);

        Self {
            gb,
            from,
            to,
            raw_from,
            raw_to,
            ch_from,
            ch_to,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.to.0 - self.from.0
    }

    #[inline]
    pub fn len_chars(&self) -> usize {
        self.ch_to.0 - self.ch_from.0
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.from == self.to
    }

    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        // We are safe to use raw indices here as we ensure that the region we point to is
        // contiguous on construction and as we hold a reference to the GapBuffer, nothing else can
        // move the gap into the region we point to.
        self.gb.raw_slice(self.raw_from..self.raw_to)
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // SAFETY: we ensure that we point to a contiguous live buffer region on construction
        unsafe { std::str::from_utf8_unchecked(self.as_bytes()) }
    }

    pub fn map_byte_index(&self, byte_idx: ByteIdx) -> CharIdx {
        self.gb.byte_to_char(byte_idx) - self.ch_from
    }

    pub fn map_char_index(&self, char_idx: CharIdx) -> ByteIdx {
        self.gb.char_to_byte(char_idx) - self.from
    }
}

impl<'a> AsRef<str> for View<'a> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
