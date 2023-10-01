use crate::buffer::Buffer;
use std::collections::VecDeque;

/// A non-empty vec of buffers where the active buffer is accessible and default
/// buffers are inserted where needed to maintain invariants
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffers(VecDeque<Buffer>);

impl Default for Buffers {
    fn default() -> Self {
        Self::new()
    }
}

impl Buffers {
    pub fn new() -> Self {
        Self(vec![Buffer::default()].into())
    }

    pub fn insert(&mut self, b: Buffer) {
        // Remove an empty scratch buffer if the user has now opened a file
        if self.0.len() == 1 && self.0[0].is_unnamed() && !self.0[0].dirty {
            self.0.remove(0);
        }

        self.0.insert(0, b);
    }

    pub fn dirty_buffers(&self) -> Vec<&str> {
        self.0
            .iter()
            .filter(|b| b.dirty)
            .flat_map(|b| b.full_name())
            .collect()
    }

    #[inline]
    pub fn active(&self) -> &Buffer {
        &self.0[0]
    }

    #[inline]
    pub fn active_mut(&mut self) -> &mut Buffer {
        &mut self.0[0]
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    pub fn is_empty_scratch(&self) -> bool {
        self.0.len() == 1 && self.0[0].is_unnamed() && !self.0[0].dirty
    }

    pub fn next(&mut self) {
        self.0.rotate_right(1)
    }

    pub fn previous(&mut self) {
        self.0.rotate_left(1)
    }

    pub fn close_active(&mut self) {
        self.0.remove(0);
        if self.0.is_empty() {
            self.0.push_back(Buffer::default());
        }
    }
}
