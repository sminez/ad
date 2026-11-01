//! Type-safe indexing into GapBuffers.
//!
//! Mixing up character and byte indices when interacting with the GapBuffer has been one of the
//! main sources of logic bugs within ad. The GapBuffer itself is highly defensive about this but
//! previous versions of the public GapBuffer API made use of bare usize parameters for indexing
//! which allowed callers to mess things up (particularly when multi-byte utf-8 characters were
//! involved).
//!
//! # Indexing into a GapBuffer
//!
//! There are four distinct ways to index into a GapBuffer depending on the operation in question.
//! The first three offer different ways of performing arbitrary indexing into the contents of the
//! buffer:
//!
//! 1. [RawIdx] - A raw byte offset into the memory allocation held by the GapBuffer. This is only
//!    permitted within the `buffer::internal` module as an implementation detail of the GapBuffer
//!    and Slice types. Indexing in this way is never guaranteed to return valid utf-8 data so the
//!    caller must ensure that indexes are calculated correctly and that the data read is valid.
//! 2. [ByteIdx] - A byte offset within the logical contents of the GapBuffer. This is used
//!    primarily by the regex and treesitter APIs where a GapBuffer is being treated as a byte
//!    slice. The standard pitfalls of indexing into a utf-8 encoded string all apply (i.e. care
//!    must be taked when multi-byte characters are present) and it is _not_ guaranteed that an
//!    arbitrary index of this type lies on a character boundary. However, this type _does_
//!    guarantee that indexing in this way always returns bytes from the logical contents of the
//!    buffer and not the gap region.
//! 3. [CharIdx] - A character offset within the logical contents of the GapBuffer. This is the
//!    most expensive index type to work with but it guarantees that indexing in this way always
//!    points to a valid utf-8 character boundary. This is used when reading and writing utf-8 data
//!    and is efficiently mapped to a [RawIdx] when inserting / deleting at the start of the gap
//!    region.
//!
//! The fourth index type is [LineIdx] which is used to quickly jump to the beginning of newline
//! delimited lines within the logical buffer state. GapBuffer maintains a cache of line ending
//! positions within the buffer in order to make this an `O(1)` operation.
//!
//! # Shared API for index types
//!
//! All four index types behave the same way and implement a set of shared helper methods / traits
//! to make them look as much like a bare `usize` as possible while still forcing the user to be
//! explicit about how they are indexing into a GapBuffer. Indicies can be freely added and
//! subtracted from one another but offsetting by a `usize` requires calling the `uadd` and `usub`
//! methods in order make it easy to audit for logic errors around mixing indices.
//!
//! The underlying `usize` value of the index is freely accessible as a public field, but
//! conversion between index types should be handled via the methods provided by GapBuffer rather
//! than ad-hoc manual conversion or external helper functions.
use std::{
    fmt,
    ops::{Add, AddAssign, Sub, SubAssign},
};

/// A byte offset within the logical buffer state (i.e. ignoring the position of the gap).
pub type ByteIdx = usize;
/// A character offset within the logical buffer state.
pub type CharIdx = usize;

macro_rules! impl_index_type {
    (
        $(#[$doc_str:meta])*
        $name:ident
    ) => {
        $(#[$doc_str])*
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub usize);

        impl $name {
            pub const fn saturating_sub(self, rhs: Self) -> Self {
                Self(self.0.saturating_sub(rhs.0))
            }

            pub const fn uadd(self, rhs: usize) -> Self {
                Self(self.0 + rhs)
            }

            pub const fn usub(self, rhs: usize) -> Self {
                Self(self.0 - rhs)
            }

            pub fn to(self, end: Self) -> impl Iterator<Item = Self> {
                (self.0..end.0).map(Self)
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl Add for $name {
            type Output = Self;

            fn add(self, rhs: Self) -> Self {
                Self(self.0 + rhs.0)
            }
        }

        impl Sub for $name {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self {
                Self(self.0 - rhs.0)
            }
        }

        impl AddAssign for $name {
            fn add_assign(&mut self, rhs: Self) {
                self.0 += rhs.0;
            }
        }

        impl SubAssign for $name {
            fn sub_assign(&mut self, rhs: Self) {
                self.0 -= rhs.0;
            }
        }
    };
}

impl_index_type!(
    /// A byte offset within the raw buffer state (i.e. accounting for the position of the gap).
    RawIdx
);
impl_index_type!(
    /// A line index within the logical buffer state.
    LineIdx
);
