//! A simple 9p Protocol implementation for serving filesystem interfaces
#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::style,
    future_incompatible,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    rustdoc::all,
    clippy::undocumented_unsafe_blocks
)]

pub mod fs;
pub mod sansio;
pub mod sync;
pub mod tokio;

/// A simple result type for errors returned from this crate
pub type Result<T> = std::result::Result<T, String>;
