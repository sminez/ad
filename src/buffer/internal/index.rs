//! Type-safe indexing into GapBuffers.
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
