use crate::{
    buffer::{Buffer, BufferKind, Cur, WELCOME_SQUIRREL},
    config::Config,
    dot::TextObject,
    lsp::LspManagerHandle,
    ziplist,
    ziplist::{Position, ZipList},
};
use ad_event::Source;
use std::{
    collections::VecDeque,
    io::{self, ErrorKind},
    mem,
    path::Path,
    sync::{Arc, RwLock},
};

#[cfg(test)]
use crate::lsp::Req;
#[cfg(test)]
use std::sync::mpsc::Sender;

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
    lsp_handle: Arc<LspManagerHandle>,
    config: Arc<RwLock<Config>>,
}

impl Buffers {
    pub fn new(lsp_handle: Arc<LspManagerHandle>, config: Arc<RwLock<Config>>) -> Self {
        Self {
            next_id: 1,
            inner: ziplist![Buffer::new_unnamed(0, "", config.clone())],
            jump_list: JumpList::default(),
            lsp_handle,
            config,
        }
    }

    #[cfg(test)]
    pub(crate) fn new_with_raw_sender(tx_req: Sender<Req>, config: Arc<RwLock<Config>>) -> Self {
        Self {
            next_id: 1,
            inner: ziplist![Buffer::new_unnamed(0, "", config.clone())],
            jump_list: JumpList::default(),
            lsp_handle: Arc::new(LspManagerHandle::new_stubbed(tx_req)),
            config,
        }
    }

    #[cfg(test)]
    pub(crate) fn new_stubbed(
        ids: &[usize],
        tx_req: Sender<Req>,
        config: Arc<RwLock<Config>>,
    ) -> Self {
        Self {
            next_id: ids.last().unwrap() + 1,
            inner: ZipList::try_from_iter(
                ids.iter()
                    .map(|i| Buffer::new_virtual(*i, "", "", config.clone())),
            )
            .unwrap(),
            jump_list: JumpList::default(),
            lsp_handle: Arc::new(LspManagerHandle::new_stubbed(tx_req)),
            config,
        }
    }

    /// Returns the id of a newly created buffer, None if the buffer already existed
    pub fn open_or_focus<P: AsRef<Path>>(
        &mut self,
        path: P,
        retain_empty_unnamed: bool,
    ) -> io::Result<Option<BufferId>> {
        let path = match path.as_ref().canonicalize() {
            Ok(p) => p,
            Err(e) if e.kind() == ErrorKind::NotFound => path.as_ref().to_path_buf(),
            Err(e) => return Err(e),
        };

        // Opening a directory from an existing directory buffer replaces the existing content
        // rather than opening a new buffer in order to prevent the issue in Acme where drilling
        // down into subdirectories results in having multiple.
        if self.active().kind.is_dir() && path.metadata().map(|m| m.is_dir()).unwrap_or_default() {
            let b = self.active_mut();
            b.kind = BufferKind::Directory(path);
            b.reload_from_disk();
            b.set_dot(TextObject::BufferStart, 1);
            return Ok(None);
        }

        // If we already have the requested path open then we focus that buffer instead of opening
        // a duplicate.
        let existing_id = self.with_path(&path).map(|b| b.id);
        if let Some(existing_id) = existing_id {
            self.notify_lsp_changes_if_dirty();
            self.record_jump_position();
            self.inner.focus_element_by(|b| b.id == existing_id);
            return Ok(None);
        }

        // Otherwise we load the file and add it to the buffer list.
        let id = self.next_id;
        self.next_id += 1;
        let mut b = Buffer::new_from_canonical_file_path(id, path, self.config.clone())?;

        // Remove an empty unnamed buffer if the user has now opened a file and we have not
        // been told to retain it (typically because we have an open window showing it)
        if !retain_empty_unnamed && self.is_empty_squirrel() {
            mem::swap(&mut self.inner.focus, &mut b);
        } else {
            self.record_jump_position();
            self.push_buffer(b);
        }

        self.lsp_handle.document_opened(self);

        Ok(Some(id))
    }

    pub fn ensure_file_is_open<P: AsRef<Path>>(&mut self, path: P) {
        let p = match path.as_ref().canonicalize() {
            Ok(p) => p,
            Err(_) => return,
        };

        if self.with_path(&p).is_some() {
            return;
        }

        let id = self.next_id;
        self.next_id += 1;

        if let Ok(b) = Buffer::new_from_canonical_file_path(id, p, self.config.clone()) {
            self.lsp_handle.document_opened(self);
            self.inner.insert_at(Position::Tail, b);
        }
    }

    /// This is a pretty hacky way to handle document sync but given that we are not wanting
    /// the standard IDE behaviour of everything updating as you type, notifying changes when
    /// we switch focus to another buffer (or within ../lsp/mod.rs before sending requests)
    /// is sufficient for maintaining document sync.
    #[inline]
    fn notify_lsp_changes_if_dirty(&self) {
        if self.inner.focus.dirty {
            self.lsp_handle.document_changed(&self.inner.focus);
        }
    }

    pub fn next(&mut self) {
        self.notify_lsp_changes_if_dirty();
        self.inner.focus_down();
    }

    pub fn previous(&mut self) {
        self.notify_lsp_changes_if_dirty();
        self.inner.focus_up();
    }

    pub fn iter(&self) -> impl Iterator<Item = &Buffer> {
        self.inner.iter().map(|(_, b)| b)
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Buffer> {
        self.inner.iter_mut().map(|(_, b)| b)
    }

    pub fn close_buffer(&mut self, id: BufferId) {
        let removed = self.inner.remove_where_with_default(
            |b| b.id == id,
            || Buffer::new_unnamed(self.next_id, "", self.config.clone()),
        );
        self.jump_list.clear_for_buffer(id);

        if let Some(b) = removed {
            self.lsp_handle.document_closed(&b);
            self.next_id += 1; // for the newly added unnamed buffer
        }
    }

    fn push_buffer(&mut self, buf: Buffer) {
        self.inner.insert(buf);
    }

    pub(crate) fn open_virtual(&mut self, name: String, content: String) -> BufferId {
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
            return id;
        }

        let id = self.next_id;
        let buf = Buffer::new_virtual(id, name, content, self.config.clone());
        self.record_jump_position();
        self.push_buffer(buf);
        self.next_id += 1;

        id
    }

    /// Used to seed the buffer selection mini-buffer
    pub(crate) fn as_buffer_list(&self) -> Vec<String> {
        let mut entries: Vec<(usize, String)> = self
            .inner
            .iter()
            .map(|(focused, b)| {
                (
                    b.id,
                    format!(
                        "{:<4} {} {}",
                        b.id,
                        if focused { '*' } else { ' ' },
                        b.full_name()
                    ),
                )
            })
            .collect();
        entries.sort_by_key(|e| e.0);

        entries.into_iter().map(|(_, s)| s).collect()
    }

    pub(crate) fn contains_bufid(&self, id: BufferId) -> bool {
        self.inner.iter().any(|(_, b)| b.id == id)
    }

    pub(crate) fn focus_id(&mut self, id: BufferId) -> Option<BufferId> {
        if !self.contains_bufid(id) || self.active().id == id {
            return None;
        }
        self.notify_lsp_changes_if_dirty();
        self.record_jump_position();
        self.inner.focus_element_by(|b| b.id == id);

        Some(id)
    }

    /// Focus the given buffer ID without touching the jump list
    pub(crate) fn focus_id_silent(&mut self, id: BufferId) {
        self.notify_lsp_changes_if_dirty();
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

    pub(crate) fn with_path<P: AsRef<Path>>(&self, path: P) -> Option<&Buffer> {
        let path = path.as_ref();
        self.inner
            .iter()
            .find(|(_, b)| match &b.kind {
                BufferKind::File(p) | BufferKind::Directory(p) => p == path,
                _ => false,
            })
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

    fn jump(&mut self, bufid: BufferId, mut cur: Cur) -> (BufferId, BufferId) {
        self.notify_lsp_changes_if_dirty();
        let prev_id = self.inner.focus.id;
        self.inner.focus_element_by(|b| b.id == bufid);
        let new_id = self.inner.focus.id;
        if new_id == bufid {
            // Clamp the cursor to the buffer's valid range before setting it
            // in case the buffer was edited since this position was recorded
            cur.clamp_idx(self.inner.focus.txt.len_chars());
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

    /// Whether or not the only buffer we currently have open is the default squirrel buffer rather
    /// than a "real" buffer that has been opened by the user.
    #[inline]
    pub fn is_empty_squirrel(&self) -> bool {
        self.inner.len() == 1
            && self.inner.focus.is_unnamed()
            && (self.inner.focus.txt.is_empty() || self.inner.focus.txt == WELCOME_SQUIRREL)
    }

    /// Append to the +output buffer assigned to the buffer with provided id.
    pub(crate) fn write_output_for_buffer(&mut self, id: usize, s: String, cwd: &Path) -> BufferId {
        let key = match self.with_id(id) {
            Some(b) => b.output_file_key(cwd),
            None => format!("{}/DEFAULT_OUTPUT_BUFFER", cwd.display()),
        };

        let k = BufferKind::Output(key.clone());
        match self.inner.iter_mut().find(|(_, b)| b.kind == k) {
            Some((_, b)) => {
                b.append(s, Source::Fsys);

                b.id
            }

            None => {
                let id = self.next_id;
                self.next_id += 1;
                let b = Buffer::new_output(id, key, s, self.config.clone());
                self.record_jump_position();
                self.inner.insert(b);

                id
            }
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

        // Mark our current position so we can jump forward to it later.
        // We need to move self.idx back after the push so we don't get stuck
        // and no-op jump immediately after
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jump_backward_clamps_stale_cursor_positions() {
        let (tx, _rx) = std::sync::mpsc::channel();
        let config = Arc::new(RwLock::new(Config::default()));
        let mut buffers = Buffers::new_with_raw_sender(tx, config.clone());

        let initial_content = "This is a test buffer with enough content to demonstrate the bug.";
        buffers.inner.focus.txt = initial_content.into();

        buffers.inner.focus.set_dot_from_cursor(60);
        assert_eq!(buffers.inner.focus.dot.active_cur().idx, 60);

        buffers.record_jump_position();

        assert_eq!(buffers.inner.focus.txt.len_chars(), 65);
        let new_len = 30;
        buffers
            .inner
            .focus
            .txt
            .remove_range(new_len, buffers.inner.focus.txt.len_chars());
        assert_eq!(buffers.inner.focus.txt.len_chars(), new_len);

        buffers.inner.focus.set_dot_from_cursor(0);
        let result = buffers.jump_list_backward();

        assert!(result.is_some());

        let cur = buffers.inner.focus.dot.active_cur();
        let (_y, _x) = cur.as_yx(&buffers.inner.focus);

        assert!(
            cur.idx <= buffers.inner.focus.txt.len_chars(),
            "cursor index {} exceeds buffer length {}",
            cur.idx,
            buffers.inner.focus.txt.len_chars()
        );
    }
}
