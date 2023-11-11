//! Buffer state for the fuse filesystem
use super::{
    empty_dir_attrs, empty_file_attrs, Ino, Message, Req, BLOCK_SIZE, BUFFERS_INO, INO_OFFSET,
};
use fuser::FileAttr;
use std::{
    collections::BTreeMap,
    sync::mpsc::{Receiver, Sender},
};

pub(super) const BUFFER_FILES: [(Ino, &str); INO_OFFSET as usize - 1] = [
    (1, "filename"),
    (2, "dot"),
    (3, "addr"),
    (4, "body"),
    (5, "event"),
];

/// Not guaranteed to be a valid parent node
fn parent_and_fname(ino: Ino) -> (Ino, &'static str) {
    let parent = (ino - BUFFERS_INO) / INO_OFFSET;
    let fname = BUFFER_FILES[((ino - BUFFERS_INO) % INO_OFFSET) as usize].1;

    (parent, fname)
}

/// A message sent by the main editor thread to notify the fs thread that
/// the current buffer list has changed.
pub enum BufId {
    Add(usize),
    Remove(usize),
}

#[derive(Debug)]
pub(super) struct BufferNodes {
    known: BTreeMap<Ino, BufferNode>,
    next_ino: Ino,
    attrs: FileAttr,
    mtx: Sender<Message>,
    brx: Receiver<BufId>,
}

impl BufferNodes {
    pub(super) fn new(mtx: Sender<Message>, brx: Receiver<BufId>) -> Self {
        Self {
            known: BTreeMap::default(),
            next_ino: BUFFERS_INO + 1,
            attrs: empty_dir_attrs(BUFFERS_INO),
            mtx,
            brx,
        }
    }

    pub(super) fn attrs(&self) -> FileAttr {
        // self.attrs.atime = SystemTime::now();

        self.attrs
    }

    pub(super) fn is_known_buffer_ino(&self, ino: Ino) -> bool {
        self.known.contains_key(&ino)
    }

    pub(super) fn known_buffer_ids(&self) -> Vec<(Ino, &str)> {
        self.known
            .values()
            .map(|b| (b.ino, b.str_id.as_str()))
            .collect()
    }

    pub(super) fn lookup_file_attrs(&mut self, parent: Ino, name: &str) -> Option<FileAttr> {
        self.known.get(&parent)?.file_attrs.get(&name).copied()
    }

    pub(super) fn get_attr_for_inode(&mut self, ino: Ino) -> Option<FileAttr> {
        // If this is a known directory then we return the attrs for it
        if ino == BUFFERS_INO {
            return Some(self.attrs());
        } else if let Some(b) = self.known.get(&ino) {
            return Some(b.attrs());
        }

        // Otherwise we see if this is known buffer file
        let (parent, fname) = parent_and_fname(ino);
        self.known
            .get_mut(&parent)?
            .refreshed_file_attrs(fname, &self.mtx)
    }

    pub(super) fn get_file_content(&mut self, ino: Ino) -> Option<String> {
        let (parent, fname) = parent_and_fname(ino);
        self.known
            .get(&parent)?
            .current_file_content(fname, &self.mtx)
    }

    /// Process any pending updates from the main thread for changes to the buffer set
    pub(super) fn update(&mut self) {
        for bid in self.brx.try_iter() {
            match bid {
                BufId::Add(id) => {
                    let ino = self.next_ino;
                    self.next_ino += INO_OFFSET;
                    self.known.insert(ino, BufferNode::new(id, ino));
                }

                BufId::Remove(id) => self.known.retain(|_, v| v.id != id),
            };
        }
    }
}

/// A BufferNode in the filesystem is a directory containing a fixed
/// set of control files
///
/// The inodes generated for each of the control files are based on offsets
/// from the inode of the buffer directory itself (see INO_OFFSET above).
#[derive(Debug)]
pub(super) struct BufferNode {
    id: usize,
    ino: Ino,
    str_id: String,
    attrs: FileAttr,
    file_attrs: BTreeMap<&'static str, FileAttr>,
}

impl BufferNode {
    fn new(id: usize, ino: Ino) -> Self {
        Self {
            id,
            ino,
            str_id: id.to_string(),
            attrs: empty_dir_attrs(ino),
            file_attrs: stub_file_attrs(ino),
        }
    }

    fn attrs(&self) -> FileAttr {
        // self.attrs.atime = SystemTime::now();

        self.attrs
    }

    fn refreshed_file_attrs(&mut self, fname: &str, mtx: &Sender<Message>) -> Option<FileAttr> {
        let content = self.current_file_content(fname, mtx)?;
        let attrs = self.file_attrs.get_mut(fname)?;
        attrs.size = content.as_bytes().len() as u64;
        attrs.blocks = (attrs.size + BLOCK_SIZE - 1) / BLOCK_SIZE;

        Some(*attrs)
    }

    fn current_file_content(&self, fname: &str, mtx: &Sender<Message>) -> Option<String> {
        match fname {
            "filename" => Some(Message::send(Req::ReadBufferName { id: self.id }, mtx)),
            "dot" => Some(Message::send(Req::ReadBufferDot { id: self.id }, mtx)),
            "addr" => Some(Message::send(Req::ReadBufferAddr { id: self.id }, mtx)),
            "body" => Some(Message::send(Req::ReadBufferBody { id: self.id }, mtx)),
            "event" => None, // TODO: sort out the event file
            _ => None,
        }
    }
}

fn stub_file_attrs(ino: Ino) -> BTreeMap<&'static str, FileAttr> {
    let mut m = BTreeMap::new();

    for (offset, name) in BUFFER_FILES.into_iter() {
        m.insert(name, empty_file_attrs(ino + offset));
    }

    m
}
