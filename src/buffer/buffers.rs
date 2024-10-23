use crate::{
    buffer::{Buffer, BufferKind, Cur},
    dot::TextObject,
    ziplist,
    ziplist::ZipList,
};
use ad_event::Source;
use std::{
    collections::VecDeque,
    io::{self, ErrorKind},
    mem,
    path::Path,
};

const MAX_JUMPS: usize = 100;

/// An ID for a known Buffer
pub type BufferId = usize;

/// A non-empty vec of buffers where the active buffer is accessible and default
/// buffers are inserted where needed to maintain invariants
#[derive(Debug)]
pub struct Buffers {
    next_id: BufferId,
    inner: ZipList<Buffer>,
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
            inner: ziplist![Buffer::new_unnamed(0, "")],
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

        if self.active().kind.is_dir() && path.metadata().map(|m| m.is_dir()).unwrap_or_default() {
            let b = self.active_mut();
            b.kind = BufferKind::Directory(path);
            b.reload_from_disk();
            b.set_dot(TextObject::BufferStart, 1);
            return Ok(None);
        }

        let existing_id = self
            .inner
            .iter()
            .find(|(_, b)| match &b.kind {
                BufferKind::File(p) | BufferKind::Directory(p) => p == &path,
                _ => false,
            })
            .map(|(_, b)| b.id);

        if let Some(existing_id) = existing_id {
            self.record_jump_position();
            self.inner.focus_element_by(|b| b.id == existing_id);
            return Ok(None);
        }

        let id = self.next_id;
        self.next_id += 1;
        let mut b = Buffer::new_from_canonical_file_path(id, path)?;

        // Remove an empty scratch buffer if the user has now opened a file
        if self.is_empty_scratch() {
            mem::swap(&mut self.inner.focus, &mut b);
        } else {
            self.record_jump_position();
            self.push_buffer(b);
        }

        Ok(Some(id))
    }

    pub fn next(&mut self) {
        self.inner.focus_down();
    }

    pub fn previous(&mut self) {
        self.inner.focus_up();
    }

    pub fn close_buffer(&mut self, id: BufferId) {
        let removed = self
            .inner
            .remove_where_with_default(|b| b.id == id, || Buffer::new_unnamed(self.next_id, ""));
        self.jump_list.clear_for_buffer(id);

        if removed.is_some() {
            self.next_id += 1;
        }
    }

    fn push_buffer(&mut self, buf: Buffer) {
        self.inner.insert(buf);
    }

    pub(crate) fn open_virtual(&mut self, name: String, content: String) {
        let existing_id = self
            .inner
            .iter()
            .find(|(_, b)| match &b.kind {
                BufferKind::Virtual(s) => s == &name,
                _ => false,
            })
            .map(|(_, b)| b.id);

        if let Some(id) = existing_id {
            self.focus_id(id);
            self.inner.focus.txt = content.into();
            let n = self.inner.focus.txt.len_chars();
            self.inner.focus.dot.clamp_idx(n);
            self.inner.focus.xdot.clamp_idx(n);
            return;
        }

        let buf = Buffer::new_virtual(self.next_id, name, content);
        self.record_jump_position();
        self.push_buffer(buf);
        self.next_id += 1;
    }

    /// Used to seed the buffer selection mini-buffer
    pub(crate) fn as_buf_list(&self) -> Vec<String> {
        let mut entries: Vec<String> = self
            .inner
            .iter()
            .map(|(focused, b)| {
                format!(
                    "{:<4} {} {}",
                    b.id,
                    if focused { '*' } else { ' ' },
                    b.full_name()
                )
            })
            .collect();
        entries.sort();

        entries
    }

    fn contains_bufid(&self, id: BufferId) -> bool {
        self.inner.iter().any(|(_, b)| b.id == id)
    }

    pub(crate) fn focus_id(&mut self, id: BufferId) {
        if !self.contains_bufid(id) {
            return;
        }
        self.record_jump_position();
        self.inner.focus_element_by(|b| b.id == id);
    }

    /// Focus the given buffer ID without touching the jump list
    pub(crate) fn focus_id_silent(&mut self, id: BufferId) {
        self.inner.focus_element_by(|b| b.id == id);
    }

    pub(crate) fn with_id(&self, id: BufferId) -> Option<&Buffer> {
        self.inner.iter().find(|(_, b)| b.id == id).map(|(_, b)| b)
    }

    pub(crate) fn with_id_mut(&mut self, id: BufferId) -> Option<&mut Buffer> {
        self.inner
            .iter_mut()
            .find(|(_, b)| b.id == id)
            .map(|(_, b)| b)
    }

    pub fn dirty_buffers(&self) -> Vec<String> {
        self.inner
            .iter()
            .filter(|(_, b)| b.dirty && b.kind.is_file())
            .map(|(_, b)| b.full_name().to_string())
            .collect()
    }

    #[inline]
    pub fn active(&self) -> &Buffer {
        &self.inner.focus
    }

    #[inline]
    pub fn active_mut(&mut self) -> &mut Buffer {
        &mut self.inner.focus
    }

    pub fn record_jump_position(&mut self) {
        self.jump_list
            .push(self.inner.focus.id, self.inner.focus.dot.active_cur());
    }

    fn jump(&mut self, bufid: BufferId, cur: Cur) -> (BufferId, BufferId) {
        let prev_id = self.inner.focus.id;
        self.inner.focus_element_by(|b| b.id == bufid);
        let new_id = self.inner.focus.id;
        if new_id == bufid {
            self.inner.focus.dot = cur.into();
        }

        (prev_id, new_id)
    }

    pub fn jump_list_forward(&mut self) -> Option<(BufferId, BufferId)> {
        if let Some((bufid, cur)) = self.jump_list.forward() {
            Some(self.jump(bufid, cur))
        } else {
            None
        }
    }

    pub fn jump_list_backward(&mut self) -> Option<(BufferId, BufferId)> {
        let (bufid, cur) = (self.inner.focus.id, self.inner.focus.dot.active_cur());
        if let Some((bufid, cur)) = self.jump_list.backward(bufid, cur) {
            Some(self.jump(bufid, cur))
        } else {
            None
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty_scratch(&self) -> bool {
        self.inner.len() == 1 && self.inner.focus.is_unnamed() && self.inner.focus.txt.is_empty()
    }

    /// Append to the +output buffer assigned to the buffer with provided id.
    pub(crate) fn write_output_for_buffer(&mut self, id: usize, s: String, cwd: &Path) {
        let key = match self.with_id(id) {
            Some(b) => b.output_file_key(cwd),
            None => format!("{}/DEFAULT_OUTPUT_BUFFER", cwd.display()),
        };

        let id = match self
            .inner
            .iter_mut()
            .find(|(_, b)| b.kind == BufferKind::Output(key.clone()))
        {
            Some((_, b)) => {
                b.append(s, Source::Fsys);

                b.id
            }

            None => {
                let id = self.next_id;
                self.next_id += 1;
                let b = Buffer::new_output(id, key, s);
                self.record_jump_position();
                self.inner.insert(b);

                id
            }
        };

        self.focus_id(id);
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

        // Mark our current position so we can jump forward to it later.
        // We need to move self.idx back after the push so we don't get stuck
        // and no-op jump imediately after
        if self.idx == self.jumps.len() {
            self.push(id, cur);
            self.idx -= 1;
        }

        self.idx -= 1;
        self.jumps.get(self.idx).copied()
    }

    // FIXME: this needs to be smarter about modifying self.idx based on which entries are removed
    fn clear_for_buffer(&mut self, id: BufferId) {
        self.jumps.retain(|j| j.0 != id);
        self.idx = self.jumps.len();
    }
}
