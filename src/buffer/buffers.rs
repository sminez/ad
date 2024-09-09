use crate::{
    buffer::{Buffer, BufferKind, Cur},
    editor::ViewPort,
};
use std::{
    collections::VecDeque,
    io::{self, ErrorKind},
    path::Path,
};

const MAX_JUMPS: usize = 100;

type BufferId = usize;

/// A non-empty vec of buffers where the active buffer is accessible and default
/// buffers are inserted where needed to maintain invariants
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Buffers {
    next_id: BufferId,
    inner: VecDeque<Buffer>,
    jump_list: JumpList,
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
            inner: vec![Buffer::new_unnamed(0, "")].into(),
            jump_list: JumpList::default(),
        }
    }

    /// Returns the id of a newly created buffer, None if the buffer already existed
    pub fn open_or_focus<P: AsRef<Path>>(&mut self, path: P) -> io::Result<Option<BufferId>> {
        let path = match path.as_ref().canonicalize() {
            Ok(p) => p,
            Err(e) if e.kind() == ErrorKind::NotFound => path.as_ref().to_path_buf(),
            Err(e) => return Err(e),
        };
        let idx = self.inner.iter().position(|b| match &b.kind {
            BufferKind::File(p) => p == &path,
            _ => false,
        });

        if let Some(idx) = idx {
            self.record_jump_position();
            self.inner.swap(0, idx);
            return Ok(None);
        }

        // Remove an empty scratch buffer if the user has now opened a file
        if self.is_empty_scratch() {
            self.inner.remove(0);
        } else {
            self.record_jump_position();
        }

        let id = self.next_id;
        self.next_id += 1;
        let b = Buffer::new_from_canonical_file_path(id, path)?;
        self.inner.insert(0, b);

        Ok(Some(id))
    }

    /// Used to seed the buffer selection mini-buffer
    pub(crate) fn as_buf_list(&self) -> Vec<String> {
        let focused = self.inner[0].id;
        self.inner
            .iter()
            .map(|b| {
                format!(
                    "{:<4} {} {}",
                    b.id,
                    if b.id == focused { '*' } else { ' ' },
                    b.full_name()
                )
            })
            .collect()
    }

    pub(crate) fn focus_id(&mut self, id: BufferId) {
        if let Some(idx) = self.inner.iter().position(|b| b.id == id) {
            self.record_jump_position();
            self.inner.swap(0, idx);
        }
    }

    pub(crate) fn with_id(&self, id: BufferId) -> Option<&Buffer> {
        self.inner.iter().find(|b| b.id == id)
    }

    pub(crate) fn with_id_mut(&mut self, id: BufferId) -> Option<&mut Buffer> {
        self.inner.iter_mut().find(|b| b.id == id)
    }

    pub fn dirty_buffers(&self) -> Vec<String> {
        self.inner
            .iter()
            .filter(|b| b.dirty)
            .map(|b| b.full_name().to_string())
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

    pub fn record_jump_position(&mut self) {
        self.jump_list
            .push(self.inner[0].id, self.inner[0].dot.active_cur());
    }

    fn jump(&mut self, bufid: BufferId, cur: Cur, screen_rows: usize, screen_cols: usize) {
        if let Some(idx) = self.inner.iter().position(|b| b.id == bufid) {
            self.inner.swap(0, idx);
            self.inner[0].dot = cur.into();
            self.inner[0].view_port(ViewPort::Center, screen_rows, screen_cols);
        }
    }

    pub fn jump_list_forward(&mut self, screen_rows: usize, screen_cols: usize) {
        if let Some((bufid, cur)) = self.jump_list.forward() {
            self.jump(bufid, cur, screen_rows, screen_cols);
        }
    }

    pub fn jump_list_backward(&mut self, screen_rows: usize, screen_cols: usize) {
        let (bufid, cur) = (self.inner[0].id, self.inner[0].dot.active_cur());
        if let Some((bufid, cur)) = self.jump_list.backward(bufid, cur) {
            self.jump(bufid, cur, screen_rows, screen_cols);
        }
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
            self.inner.push_back(Buffer::new_unnamed(self.next_id, ""));
            self.next_id += 1;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct JumpList {
    idx: usize,
    jumps: VecDeque<(BufferId, Cur)>,
}

impl Default for JumpList {
    fn default() -> Self {
        Self {
            idx: 0,
            jumps: VecDeque::with_capacity(MAX_JUMPS),
        }
    }
}

impl JumpList {
    fn push(&mut self, id: BufferId, cur: Cur) {
        self.jumps.truncate(self.idx);
        let jump = (id, cur);

        if self.jumps.back() == Some(&jump) {
            return;
        }
        if self.jumps.len() == MAX_JUMPS {
            self.jumps.pop_front();
        }

        self.jumps.push_back(jump);
        self.idx = self.jumps.len();
    }

    fn forward(&mut self) -> Option<(BufferId, Cur)> {
        if self.idx + 1 >= self.jumps.len() {
            return None;
        }

        self.idx += 1;
        self.jumps.get(self.idx).copied()
    }

    fn backward(&mut self, id: BufferId, cur: Cur) -> Option<(BufferId, Cur)> {
        if self.idx == 0 {
            return None;
        }

        if self.idx == self.jumps.len() {
            self.push(id, cur);
        }

        self.idx -= 1;
        self.jumps.get(self.idx).copied()
    }
}
