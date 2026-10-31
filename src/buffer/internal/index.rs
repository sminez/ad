//! Type-safe indexing into GapBuffers.

/// A byte offset within the logical buffer state (i.e. ignoring the position of the gap).
pub type ByteIdx = usize;
/// A character offset within the logical buffer state.
pub type CharIdx = usize;
/// A byte offset within the raw buffer state (i.e. accounting for the position of the gap).
pub type RawIdx = usize;
/// A line index within the logical buffer state.
pub type LineIdx = usize;
