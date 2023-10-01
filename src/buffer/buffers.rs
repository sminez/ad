use crate::buffer::{Buffer, BufferKind, Line};
use std::{collections::VecDeque, io, path::Path};

/// A non-empty vec of buffers where the active buffer is accessible and default
/// buffers are inserted where needed to maintain invariants
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffers {
    next_id: usize,
    inner: VecDeque<Buffer>,
}

impl Default for Buffers {
    fn default() -> Self {
        Self::new()
    }
}

impl Buffers {
    pub fn new() -> Self {
        Self {
            next_id: 1,
            inner: vec![Buffer::new_unnamed(0)].into(),
        }
    }

    pub fn open_or_focus<P: AsRef<Path>>(&mut self, path: P) -> io::Result<()> {
        let path = path.as_ref().canonicalize()?;
        let idx = self.inner.iter().position(|b| match &b.kind {
            BufferKind::File(p) => p == &path,
            _ => false,
        });

        if let Some(idx) = idx {
            self.inner.swap(0, idx);
            return Ok(());
        }

        // Remove an empty scratch buffer if the user has now opened a file
        if self.inner.len() == 1 && self.inner[0].is_unnamed() && !self.inner[0].dirty {
            self.inner.remove(0);
        }

        let b = Buffer::new_from_canonical_file_path(self.next_id, path)?;
        self.inner.insert(0, b);
        self.next_id += 1;

        Ok(())
    }

    /// Used to seed the buffer selection mini-buffer
    pub(crate) fn as_buf_list(&self) -> Vec<Line> {
        let focused = self.inner[0].id;
        self.inner
            .iter()
            .map(|b| {
                Line::new(format!(
                    "{:<4} {} {}",
                    b.id,
                    if b.id == focused { '*' } else { ' ' },
                    b.full_name()
                ))
            })
            .collect()
    }

    pub(crate) fn focus_id(&mut self, id: usize) {
        if let Some(idx) = self.inner.iter().position(|b| b.id == id) {
            self.inner.swap(0, idx);
        }
    }

    pub fn dirty_buffers(&self) -> Vec<&str> {
        self.inner
            .iter()
            .filter(|b| b.dirty)
            .map(|b| b.full_name())
            .collect()
    }

    #[inline]
    pub fn active(&self) -> &Buffer {
        &self.inner[0]
    }

    #[inline]
    pub fn active_mut(&mut self) -> &mut Buffer {
        &mut self.inner[0]
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty_scratch(&self) -> bool {
        self.inner.len() == 1 && self.inner[0].is_unnamed() && !self.inner[0].dirty
    }

    pub fn next(&mut self) {
        self.inner.rotate_right(1)
    }

    pub fn previous(&mut self) {
        self.inner.rotate_left(1)
    }

    pub fn close_active(&mut self) {
        self.inner.remove(0);
        if self.inner.is_empty() {
            self.inner.push_back(Buffer::new_unnamed(self.next_id));
            self.next_id += 1;
        }
    }
}
