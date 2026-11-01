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
//!    must be taken when multi-byte characters are present) and it is _not_ guaranteed that an
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
//! explicit about how they are indexing into a GapBuffer. Indices can be freely added and
//! subtracted from one another but offsetting by a `usize` requires calling the `uadd` and `usub`
//! methods in order make it easy to audit for logic errors around mixing indices.
//!
//! The underlying `usize` value of the index is freely accessible as a public field, but
//! conversion between index types should be handled via the methods provided by GapBuffer rather
//! than ad-hoc manual conversion or external helper functions.
use std::{
    fmt,
    ops::{Add, AddAssign, Neg, Sub, SubAssign},
};

macro_rules! impl_index_types {
    (
        $( $(#[$doc_str:meta])* $name:ident; )*
    ) => {
        $(
            $(#[$doc_str])*
            #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(pub usize);

            impl $name {
                #[allow(dead_code)]
                /// The largest value that can be represented by this index type.
                pub const MAX: Self = Self(usize::MAX);

                pub const fn saturating_sub(self, rhs: Self) -> Self {
                    Self(self.0.saturating_sub(rhs.0))
                }

                pub const fn uadd(self, rhs: usize) -> Self {
                    Self(self.0 + rhs)
                }

                pub const fn usub(self, rhs: usize) -> Self {
                    Self(self.0 - rhs)
                }

                #[allow(dead_code)]
                pub const fn saturating_usub(self, rhs: usize) -> Self {
                    Self(self.0.saturating_sub(rhs))
                }

                #[allow(dead_code)]
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
        )*
    };
}

impl_index_types!(
    /// A byte offset within the logical buffer state (ignoring the position of the gap).
    ByteIdx;
    /// A character offset within the logical buffer state.
    CharIdx;
    /// A byte offset within the raw buffer state (accounting for the position of the gap).
    RawIdx;
    /// A line index within the logical buffer state.
    LineIdx;
);

macro_rules! impl_offset_types {
    (
        $( $(#[$doc_str:meta])* $name:ident => $idx:ident; )*
    ) => {
        $(
            $(#[$doc_str])*
            #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(pub isize);

            impl $idx {
                pub const fn offset_from(self, origin: Self) -> $name {
                    $name(self.0 as isize - origin.0 as isize)
                }

                pub const fn saturating_add(self, rhs: $name) -> Self {
                    Self(self.0.saturating_add_signed(rhs.0))
                }
            }

            impl From<$idx> for $name {
                fn from(idx: $idx) -> Self {
                    Self(idx.0 as isize)
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

            impl Neg for $name {
                type Output = Self;

                fn neg(self) -> Self {
                    Self(-self.0)
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

            impl Add<$name> for $idx {
                type Output = Self;

                fn add(self, rhs: $name) -> Self {
                    match self.0.checked_add_signed(rhs.0) {
                        Some(idx) => Self(idx),
                        None => panic!("index overflow: {} + {}", self.0, rhs.0),
                    }
                }
            }

            impl Sub<$name> for $idx {
                type Output = Self;

                fn sub(self, rhs: $name) -> Self {
                    self + -rhs
                }
            }

            impl AddAssign<$name> for $idx {
                fn add_assign(&mut self, rhs: $name) {
                    *self = *self + rhs;
                }
            }

            impl SubAssign<$name> for $idx {
                fn sub_assign(&mut self, rhs: $name) {
                    *self = *self - rhs;
                }
            }
        )*
    };
}

impl_offset_types!(
    /// A signed offset between two [ByteIdx] values.
    ByteOffset => ByteIdx;
    /// A signed offset between two [CharIdx] values.
    CharOffset => CharIdx;
    /// A signed offset between two [LineIdx] values.
    LineOffset => LineIdx;
);

impl From<&str> for ByteOffset {
    fn from(s: &str) -> Self {
        // Casting a length to isize is lossless as Rust guarantees that allocations are never
        // larger than isize::MAX bytes.
        Self(s.len() as isize)
    }
}

impl From<char> for ByteOffset {
    fn from(ch: char) -> Self {
        Self(ch.len_utf8() as isize)
    }
}

impl From<&str> for CharOffset {
    fn from(s: &str) -> Self {
        Self(s.chars().count() as isize)
    }
}

impl From<char> for CharOffset {
    fn from(_: char) -> Self {
        Self(1)
    }
}
