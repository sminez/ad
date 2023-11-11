//! An Acme style filesystem interface for ad
//!
//!
//! ## Mount Point
//! https://www.pathname.com/fhs/pub/fhs-2.3.html#VARLIBLTEDITORGTEDITORBACKUPFILESAN
//!
//! `/var/lib/ad` feels like it would be the "correct" place to mount the filesystem but
//! this would need to be created with something like:
//! ```sh
//! $ sudo mkdir /var/lib/ad
//! $ sudo chown $(whoami) /var/lib/ad
//! ```
//!
//! So instead we mount to `$HOME/.ad/mnt/`
//!
//! ## Filesystem contents
//! ```text
//! $HOME/.ad/mnt/
//!   log
//!   ctrl
//!   buffers/
//!     [n]/
//!       filename
//!       dot
//!       addr
//!       body
//!       event
//! ```
use crate::editor::InputEvent;
use fuser::{
    FileAttr, FileType, Filesystem, MountOption, ReplyAttr, ReplyData, ReplyDirectory, ReplyEmpty,
    ReplyEntry, ReplyOpen, Request,
};
// See the following for an overview of libc error codes:
//   https://www.gnu.org/software/libc/manual/html_node/Error-Codes.html
use libc::{EINVAL, EIO, ENOENT};
use std::{
    env,
    ffi::OsStr,
    fs::create_dir_all,
    sync::{
        atomic::{AtomicU64, Ordering},
        mpsc::{Receiver, Sender},
    },
    thread::{spawn, JoinHandle},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

mod buffer;
mod message;

pub use buffer::BufId;
pub use message::{Message, Req};

use buffer::{BufferNodes, BUFFER_FILES};

const TTL: Duration = Duration::from_secs(1);
const MOUNT_DIR: &str = ".ad/mnt";
const FS_NAME: &str = "ad";
const BLOCK_SIZE: u64 = 512;

/// Inode number
type Ino = u64;

// Fixed inodes inside of '$HOME/.ad/mnt/buffers':
///   1. $HOME/.ad/mnt  -> The directory we mount to
const MOUNT_ROOT_INO: Ino = 1;
///   2.   ctrl         -> control file for issuing commands
const CONTROL_FILE_INO: Ino = 2;
const CONTROL_FILE: &str = "ctrl";
///   3    buffers/     -> parent directory for buffers
const BUFFERS_INO: Ino = 3;
const BUFFERS_DIR: &str = "buffers";

/// The number of inodes required to serve both the directory and contents
/// of a buffer node (used to generate Ino values for buffers):
///
///   1. $id            -> The buffer directory
///   2.   filename     -> The current filename for the buffer
///   3.   dot          -> The text currently held in dot
///   4.   addr         -> The address value of dot
///   5.   body         -> The full body of the buffer
///   6.   event        -> Contol file for intercepting input events for the buffer
const INO_OFFSET: Ino = 6;

pub struct AdFs {
    mount_path: String,
    next_fh: AtomicU64,
    tx: Sender<InputEvent>,
    buffer_nodes: BufferNodes,
    // Root level files and directories
    mount_dir_attrs: FileAttr,
    control_file_attrs: FileAttr,
}

impl AdFs {
    pub fn new(tx: Sender<InputEvent>, brx: Receiver<BufId>) -> Self {
        let home = env::var("HOME").expect("$HOME to be set");
        let mount_path = format!("{home}/{MOUNT_DIR}");

        create_dir_all(&mount_path).expect("to be able to create our mount point");
        let buffer_nodes = BufferNodes::new(tx.clone(), brx);

        Self {
            mount_path,
            next_fh: AtomicU64::new(1),
            tx,
            buffer_nodes,
            mount_dir_attrs: empty_dir_attrs(MOUNT_ROOT_INO),
            control_file_attrs: empty_file_attrs(CONTROL_FILE_INO),
        }
    }

    pub fn mount_path(&self) -> &str {
        &self.mount_path
    }

    pub fn next_fh(&mut self) -> u64 {
        self.next_fh.fetch_add(1, Ordering::SeqCst)
    }

    /// Spawn a thread for running this filesystem and return a handle to it
    pub fn run_threaded(self) -> JoinHandle<()> {
        let options = [
            MountOption::FSName(FS_NAME.to_string()),
            MountOption::AutoUnmount,
            MountOption::AllowRoot,
            MountOption::RW,
        ];
        let path = self.mount_path.clone();

        spawn(move || fuser::mount2(self, path, &options).unwrap())
    }
}

impl Filesystem for AdFs {
    fn lookup(&mut self, _req: &Request, parent: u64, name: &OsStr, reply: ReplyEntry) {
        self.buffer_nodes.update();

        let str_name = match name.to_str() {
            Some(s) => s,
            None => {
                reply.error(ENOENT);
                return;
            }
        };

        match parent {
            MOUNT_ROOT_INO => match str_name {
                CONTROL_FILE => reply.entry(&TTL, &self.control_file_attrs, 0),
                BUFFERS_DIR => reply.entry(&TTL, &self.buffer_nodes.attrs(), 0),
                _ => match self.buffer_nodes.lookup_file_attrs(parent, str_name) {
                    Some(attrs) => reply.entry(&TTL, &attrs, 0),
                    None => reply.error(ENOENT),
                },
            },

            ino if ino == BUFFERS_INO || self.buffer_nodes.is_known_buffer_ino(ino) => {
                match self.buffer_nodes.lookup_file_attrs(ino, str_name) {
                    Some(attrs) => reply.entry(&TTL, &attrs, 0),
                    None => reply.error(ENOENT),
                }
            }

            _ => reply.error(ENOENT),
        }
    }

    fn getattr(&mut self, _req: &Request, ino: u64, reply: ReplyAttr) {
        self.buffer_nodes.update();

        match ino {
            MOUNT_ROOT_INO => reply.attr(&TTL, &self.mount_dir_attrs),
            CONTROL_FILE_INO => reply.attr(&TTL, &self.control_file_attrs),

            ino => match self.buffer_nodes.get_attr_for_inode(ino) {
                Some(attrs) => reply.attr(&TTL, &attrs),
                None => reply.error(ENOENT),
            },
        }
    }

    fn read(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        _size: u32,
        _flags: i32,
        _lock: Option<u64>,
        reply: ReplyData,
    ) {
        self.buffer_nodes.update();

        match ino {
            CONTROL_FILE_INO => reply.data(&[]),

            ino => match self.buffer_nodes.get_file_content(ino) {
                Some(content) => reply.data(&content.as_bytes()[offset as usize..]),
                None => reply.error(ENOENT),
            },
        }
    }

    fn readdir(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        offset: i64,
        mut reply: ReplyDirectory,
    ) {
        self.buffer_nodes.update();

        let entries = match ino {
            MOUNT_ROOT_INO => vec![
                (1, FileType::Directory, "."),
                (1, FileType::Directory, ".."),
                (CONTROL_FILE_INO, FileType::RegularFile, CONTROL_FILE),
                (BUFFERS_INO, FileType::Directory, BUFFERS_DIR),
            ],

            BUFFERS_INO => {
                let mut entries = vec![
                    (BUFFERS_INO, FileType::Directory, "."),
                    (BUFFERS_INO, FileType::Directory, ".."),
                ];

                for (ino, id) in self.buffer_nodes.known_buffer_ids() {
                    entries.push((ino, FileType::Directory, id));
                }

                entries
            }

            ino if self.buffer_nodes.is_known_buffer_ino(ino) => {
                let mut entries = vec![
                    (ino, FileType::Directory, "."),
                    (ino, FileType::Directory, ".."),
                ];

                for (offset, fname) in BUFFER_FILES.into_iter() {
                    entries.push((ino + offset, FileType::RegularFile, fname));
                }

                entries
            }

            _ => return reply.error(ENOENT),
        };

        for (i, entry) in entries.into_iter().enumerate().skip(offset as usize) {
            // i + 1 means the index of the next entry
            if reply.add(entry.0, (i + 1) as i64, entry.1, entry.2) {
                break;
            }
        }

        reply.ok();
    }

    // TODO: restrict by filehandle for the events file
    fn write(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        data: &[u8],
        _write_flags: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: fuser::ReplyWrite,
    ) {
        assert!(offset >= 0, "negative read offset");
        self.buffer_nodes.update();

        let n_bytes = data.len() as u32;
        let msg = match String::from_utf8(data.to_vec()) {
            Ok(s) => s,
            Err(_) => {
                reply.error(EINVAL);
                return;
            }
        };

        match ino {
            CONTROL_FILE_INO => {
                self.control_file_attrs.mtime = SystemTime::now();
                match Message::send(Req::ControlMessage { msg }, &self.tx) {
                    Ok(_) => reply.written(n_bytes),
                    Err(_) => reply.error(EIO),
                }
            }

            ino => match self.buffer_nodes.req_for_write(ino, msg, offset as usize) {
                Some(req) => match Message::send(req, &self.tx) {
                    Ok(_) => reply.written(n_bytes),
                    Err(_) => reply.error(EIO),
                },
                None => reply.error(ENOENT),
            },
        }
    }

    fn access(&mut self, _req: &Request<'_>, _ino: u64, _mask: i32, reply: ReplyEmpty) {
        reply.ok()
    }

    fn open(&mut self, _req: &Request<'_>, _ino: u64, _flags: i32, reply: ReplyOpen) {
        let flags = 0;
        let fh = self.next_fh();

        reply.opened(fh, flags)
    }

    fn setattr(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _mode: Option<u32>,
        _uid: Option<u32>,
        _gid: Option<u32>,
        size: Option<u64>,
        _atime: Option<fuser::TimeOrNow>,
        _mtime: Option<fuser::TimeOrNow>,
        _ctime: Option<SystemTime>,
        _fh: Option<u64>,
        _crtime: Option<SystemTime>,
        _chgtime: Option<SystemTime>,
        _bkuptime: Option<SystemTime>,
        _flags: Option<u32>,
        reply: ReplyAttr,
    ) {
        // Truncation of a file
        if let Some(0) = size {
            match ino {
                MOUNT_ROOT_INO => self.mount_dir_attrs.size = 0,
                CONTROL_FILE_INO => self.control_file_attrs.size = 0,
                ino => self.buffer_nodes.truncate(ino),
            }
        }

        let attrs = match ino {
            MOUNT_ROOT_INO => self.mount_dir_attrs,
            CONTROL_FILE_INO => self.control_file_attrs,
            ino => match self.buffer_nodes.get_attr_for_inode(ino) {
                Some(attrs) => attrs,
                None => {
                    reply.error(ENOENT);
                    return;
                }
            },
        };

        reply.attr(&TTL, &attrs);
    }
}

fn empty_dir_attrs(ino: Ino) -> FileAttr {
    FileAttr {
        ino,
        size: 0,
        blocks: 0,
        atime: SystemTime::now(),
        mtime: SystemTime::now(),
        ctime: SystemTime::now(),
        crtime: UNIX_EPOCH,
        kind: FileType::Directory,
        perm: 0o755,
        nlink: 2,
        uid: 501,
        gid: 20,
        rdev: 0,
        flags: 0,
        blksize: BLOCK_SIZE as u32,
    }
}

fn empty_file_attrs(ino: Ino) -> FileAttr {
    FileAttr {
        ino,
        size: 0,
        blocks: 0,
        atime: SystemTime::now(),
        mtime: SystemTime::now(),
        ctime: SystemTime::now(),
        crtime: UNIX_EPOCH,
        kind: FileType::RegularFile,
        perm: 0o644,
        nlink: 1,
        uid: 501,
        gid: 20,
        rdev: 0,
        flags: 0,
        blksize: BLOCK_SIZE as u32,
    }
}
