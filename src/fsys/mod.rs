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
//!   ctl
//!   minibuffer
//!   log
//!   buffers/
//!     [n]/
//!       filename
//!       dot
//!       addr
//!       body
//!       event
//! ```
use crate::{config_handle, input::Event};
use ninep::{
    fs::{FileMeta, IoUnit, Mode, Perm, Stat},
    server::{socket_path, ClientId, ReadOutcome, Serve9p, Server},
    Result,
};
use std::{
    collections::HashMap,
    env,
    fs::create_dir_all,
    mem::take,
    path::Path,
    process::Command,
    sync::mpsc::{channel, Receiver, Sender},
    thread::{spawn, JoinHandle},
    time::SystemTime,
};
use tracing::{error, trace};

mod buffer;
mod event;
mod log;
mod message;

pub(crate) use event::InputFilter;
pub(crate) use log::LogEvent;
pub(crate) use message::{Message, Req};

use buffer::{BufferNodes, QidCheck};
use log::spawn_log_listener;

const DEFAULT_SOCKET_NAME: &str = "ad";
const MOUNT_DIR: &str = ".ad/mnt";
const IO_UNIT: u32 = 8168;

// Fixed qids inside of '$HOME/.ad/mnt/buffers':
///   0. $HOME/.ad/mnt  -> The directory we mount to
const MOUNT_ROOT_QID: u64 = 0;
///   1.   /ctl         -> control file for issuing commands
const CONTROL_FILE_QID: u64 = 1;
const CONTROL_FILE: &str = "ctl";
///   2.   /log         -> read only log of events in the editor
const LOG_FILE_QID: u64 = 2;
const LOG_FILE: &str = "log";
///   3    /minibuffer  -> control file for selecting text using the minibuffer
const MINIBUFFER_QID: u64 = 3;
const MINIBUFFER: &str = "minibuffer";
///   4    /buffers/    -> parent directory for buffers
const BUFFERS_QID: u64 = 4;
const BUFFERS_DIR: &str = "buffers";
//    5      /index     -> a listing of all of the currently open buffers
const INDEX_BUFFER_QID: u64 = 5;
const INDEX_BUFFER: &str = "index";
//    6      /current   -> the fsys filename of the current buffer
const CURRENT_BUFFER_QID: u64 = 6;
const CURRENT_BUFFER: &str = "current";

/// The number of qids required to serve both the directory and contents
/// of a buffer node (used to generate qid values for buffers):
///
///   1. $id            -> The buffer directory
///   2.   filename     -> The current filename for the buffer
///   3.   dot          -> The text currently held in dot
///   4.   addr         -> The address value of dot
///   5.   xdot         -> The text currently held in xdot (a virtual dot not affecting real dot)
///   6.   xaddr        -> The address value of xdot
///   7.   body         -> The full body of the buffer
///   8.   event        -> Contol file for intercepting input events for the buffer
///   9.   output       -> Write only output connected to stdout/err of commands run within the buffer
const QID_OFFSET: u64 = 9;

const TOP_LEVEL_QIDS: [u64; 7] = [
    MOUNT_ROOT_QID,
    CONTROL_FILE_QID,
    MINIBUFFER_QID,
    LOG_FILE_QID,
    BUFFERS_QID,
    INDEX_BUFFER_QID,
    CURRENT_BUFFER_QID,
];

const E_UNKNOWN_FILE: &str = "unknown file";
const E_NOT_ALLOWED: &str = "not allowed";

enum InternalRead {
    Immediate(Vec<u8>),
    Blocked(Receiver<Vec<u8>>),
    Unknown,
}

#[derive(Debug, Default)]
struct Cids {
    cids: Vec<ClientId>,
    read_locked: Option<ClientId>,
}

/// A join handle for the filesystem thread
#[derive(Debug)]
pub struct FsHandle(JoinHandle<()>);

impl FsHandle {
    /// Join on the filesystem thread
    pub fn join(self) {
        _ = self.0.join();
    }
}

#[derive(Debug)]
enum MiniBufferContent {
    Buffering(Vec<u8>),
    Data(Vec<u8>),
    Pending(Sender<Sender<Vec<u8>>>, Receiver<Vec<u8>>),
}

/// The filesystem interface for ad
#[derive(Debug)]
pub(crate) struct AdFs {
    tx: Sender<Event>,
    buffer_nodes: BufferNodes,
    minibuffer_content: MiniBufferContent,
    /// map of qids to client IDs with that qid open
    open_cids: HashMap<u64, Cids>,
    // Root level files and directories
    mount_dir_stat: Stat,
    control_file_stat: Stat,
    minibuffer_stat: Stat,
    log_file_stat: Stat,
    mount_path: String,
    auto_mount: bool,
}

impl Drop for AdFs {
    fn drop(&mut self) {
        if self.auto_mount {
            let res = Command::new("fusermount")
                .args(["-u", &self.mount_path])
                .spawn();

            if let Ok(mut child) = res {
                _ = child.wait();
            }
        }
    }
}

impl AdFs {
    /// Construct a new filesystem interface using channels held by the editor.
    pub fn new(tx: Sender<Event>, brx: Receiver<LogEvent>) -> Self {
        let home = env::var("HOME").expect("$HOME to be set");
        let mount_path = format!("{home}/{MOUNT_DIR}");

        if !Path::new(&mount_path).exists() {
            create_dir_all(&mount_path).expect("to be able to create our mount point");
        }

        let (log_tx, log_rx) = channel();
        let (listener_tx, listener_rx) = channel();
        spawn_log_listener(brx, listener_tx, log_rx);

        let buffer_nodes = BufferNodes::new(tx.clone(), listener_rx, log_tx);
        let auto_mount = config_handle!().auto_mount;

        Self {
            tx,
            buffer_nodes,
            open_cids: HashMap::new(),
            minibuffer_content: MiniBufferContent::Data(Vec::new()),
            mount_dir_stat: empty_dir_stat(MOUNT_ROOT_QID, "/"),
            control_file_stat: empty_file_stat(CONTROL_FILE_QID, CONTROL_FILE),
            minibuffer_stat: empty_file_stat(MINIBUFFER_QID, MINIBUFFER),
            log_file_stat: empty_file_stat(LOG_FILE_QID, LOG_FILE),
            mount_path,
            auto_mount,
        }
    }

    /// Spawn a thread for running this filesystem and return a handle to it
    pub fn run_threaded(self) -> FsHandle {
        let auto_mount = self.auto_mount;
        let mount_path = self.mount_path.clone();
        let socket_path = socket_path(DEFAULT_SOCKET_NAME);

        let s = Server::new(self);
        let handle = FsHandle(s.serve_socket(DEFAULT_SOCKET_NAME.to_string()));

        if auto_mount {
            let res = Command::new("9pfuse")
                .args([socket_path, mount_path])
                .spawn();

            if let Ok(mut child) = res {
                _ = child.wait();
            }
        }

        handle
    }

    fn add_open_cid(&mut self, qid: u64, cid: ClientId) {
        self.open_cids.entry(qid).or_default().cids.push(cid);
    }

    fn remove_open_cid(&mut self, qid: u64, cid: ClientId) {
        self.open_cids.entry(qid).and_modify(|cids| {
            cids.cids.retain(|&id| id != cid);
            if cids.read_locked == Some(cid) {
                cids.read_locked = None;
            }
        });
    }

    fn lock_qid_for_reading(&mut self, qid: u64, cid: ClientId) -> Result<()> {
        trace!("locking qid for reading qid={qid} cid={cid:?}");
        match self.open_cids.get_mut(&qid) {
            Some(cids) => cids.read_locked = Some(cid),
            None => return Err(E_UNKNOWN_FILE.to_string()),
        }

        Ok(())
    }

    fn readlocked_cid(&self, qid: u64) -> Option<ClientId> {
        self.open_cids.get(&qid).and_then(|cids| cids.read_locked)
    }

    /// Writing data to the minibuffer causes fsys to buffer the writes internally until the client
    /// is done. When a client then attempts to read back the selection the full buffer is sent to
    /// the editor for rendering and the reads block until the user makes a selection.
    fn minibuffer_write(&mut self, lines: String) -> Result<usize> {
        let n_bytes = lines.len();
        match &mut self.minibuffer_content {
            MiniBufferContent::Buffering(buffer) => buffer.extend_from_slice(lines.as_bytes()),
            _ => self.minibuffer_content = MiniBufferContent::Buffering(lines.into_bytes()),
        }

        Ok(n_bytes)
    }

    fn minibuffer_read(&mut self, offset: usize, count: usize) -> ReadOutcome {
        match &mut self.minibuffer_content {
            MiniBufferContent::Buffering(lines_bytes) => {
                let lines = match String::from_utf8(take(lines_bytes)) {
                    Ok(s) => s,
                    Err(e) => {
                        error!("invalid minibuffer data: {e}");
                        self.minibuffer_content = MiniBufferContent::Buffering(Vec::new());
                        return ReadOutcome::Immediate(Vec::new());
                    }
                };

                let (data_tx, data_rx) = channel();
                let (fsys_tx, fsys_rx) = channel();
                let (sub_tx, sub_rx) = channel();

                self.minibuffer_stat.n_bytes = 0;
                self.minibuffer_stat.last_modified = SystemTime::now();
                spawn_minibuffer_listener(data_rx, fsys_tx, sub_rx);

                let (tx, rx) = channel();
                _ = sub_tx.send(tx);
                self.minibuffer_content = MiniBufferContent::Pending(sub_tx, fsys_rx);

                match Message::send(Req::MinibufferSelect { lines, tx: data_tx }, &self.tx) {
                    Ok(_) => ReadOutcome::Blocked(rx),
                    Err(e) => {
                        error!("unable to open minibuffer: {e}");
                        self.minibuffer_content = MiniBufferContent::Buffering(Vec::new());
                        ReadOutcome::Immediate(Vec::new())
                    }
                }
            }

            MiniBufferContent::Data(data) => {
                ReadOutcome::Immediate(apply_offset(data, offset, count))
            }

            MiniBufferContent::Pending(sub_tx, fsys_rx) => match fsys_rx.try_recv() {
                Ok(data) => {
                    self.minibuffer_stat.n_bytes = data.len() as u64;
                    self.minibuffer_content = MiniBufferContent::Data(data.clone());
                    ReadOutcome::Immediate(apply_offset(&data, offset, count))
                }
                _ => {
                    let (tx, rx) = channel();
                    _ = sub_tx.send(tx);
                    ReadOutcome::Blocked(rx)
                }
            },
        }
    }
}

/// Spawn a listener to wait for a reply from the editor for our minibuffer selection
fn spawn_minibuffer_listener(
    data_rx: Receiver<String>,
    fsys_tx: Sender<Vec<u8>>,
    sub_rx: Receiver<Sender<Vec<u8>>>,
) {
    spawn(move || {
        let data = match data_rx.recv() {
            Ok(s) => s.into_bytes(),
            Err(e) => {
                error!("unable to read minibuffer output: {e}");
                Vec::new()
            }
        };

        // Reply to fsys first so the data is ready for incoming reads
        _ = fsys_tx.send(data.clone());

        // Any client currently blocked on a read then gets their own reply
        for tx in sub_rx.try_iter() {
            _ = tx.send(data.clone());
        }
    });
}

impl Serve9p for AdFs {
    fn stat(&mut self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat> {
        trace!(?cid, %qid, %uname, "handling stat request");
        self.buffer_nodes.update();

        match qid {
            MOUNT_ROOT_QID => Ok(self.mount_dir_stat.clone()),
            CONTROL_FILE_QID => Ok(self.control_file_stat.clone()),
            MINIBUFFER_QID => Ok(self.minibuffer_stat.clone()),
            LOG_FILE_QID => Ok(self.log_file_stat.clone()),
            BUFFERS_QID => Ok(self.buffer_nodes.stat().clone()),
            qid => match self.buffer_nodes.get_stat_for_qid(qid) {
                Some(stat) => Ok(stat.clone()),
                None => Err(E_UNKNOWN_FILE.to_string()),
            },
        }
    }

    fn write_stat(&mut self, cid: ClientId, qid: u64, stat: Stat, uname: &str) -> Result<()> {
        trace!(?cid, %qid, %uname, "handling write stat request");
        self.buffer_nodes.update();

        if stat.n_bytes == 0 {
            trace!(%qid, %uname, "stat n_bytes=0, truncating file");
            match qid {
                MOUNT_ROOT_QID | CONTROL_FILE_QID | MINIBUFFER_QID | LOG_FILE_QID => (),
                qid => self.buffer_nodes.truncate(qid),
            }
        }

        Ok(())
    }

    fn walk(
        &mut self,
        cid: ClientId,
        parent_qid: u64,
        child: &str,
        uname: &str,
    ) -> Result<FileMeta> {
        trace!(?cid, %parent_qid, %child, %uname, "handling walk request");
        self.buffer_nodes.update();

        match parent_qid {
            MOUNT_ROOT_QID => match child {
                CONTROL_FILE => Ok(self.control_file_stat.fm.clone()),
                MINIBUFFER => Ok(self.minibuffer_stat.fm.clone()),
                LOG_FILE => Ok(self.log_file_stat.fm.clone()),
                BUFFERS_DIR => Ok(self.buffer_nodes.stat().fm.clone()),
                _ => match self.buffer_nodes.lookup_file_stat(parent_qid, child) {
                    Some(stat) => Ok(stat.fm.clone()),
                    None => Err(format!("{E_UNKNOWN_FILE}: {parent_qid} {child}")),
                },
            },

            qid if qid == BUFFERS_QID || self.buffer_nodes.is_known_buffer_qid(qid) => {
                match self.buffer_nodes.lookup_file_stat(qid, child) {
                    Some(stat) => Ok(stat.fm.clone()),
                    None => Err(format!("{E_UNKNOWN_FILE}: {parent_qid} {child}")),
                }
            }

            _ => Err(format!("{E_UNKNOWN_FILE}: {parent_qid} {child}")),
        }
    }

    fn open(&mut self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        trace!(?cid, %qid, %uname, ?mode, "handling open request");
        self.buffer_nodes.update();

        if qid == LOG_FILE_QID {
            self.buffer_nodes.log.add_client(cid);
        } else if !TOP_LEVEL_QIDS.contains(&qid) {
            if let QidCheck::Unknown = self.buffer_nodes.check_if_known_qid(qid) {
                return Err(format!("{E_UNKNOWN_FILE}: {qid}"));
            }
        }

        self.add_open_cid(qid, cid);

        Ok(IO_UNIT)
    }

    fn clunk(&mut self, cid: ClientId, qid: u64) {
        trace!(?cid, %qid, "handling clunk request");

        if qid == LOG_FILE_QID {
            self.buffer_nodes.log.remove_client(cid);
        } else if let QidCheck::EventFile { buf_qid } = self.buffer_nodes.check_if_known_qid(qid) {
            if self.readlocked_cid(qid) == Some(cid) {
                self.buffer_nodes.clear_input_filter(buf_qid);
            }
        }
        self.remove_open_cid(qid, cid); // also handles clearing the read lock
    }

    fn read(
        &mut self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome> {
        trace!(?cid, %qid, %offset, %count, %uname, "handling read request");
        self.buffer_nodes.update();

        if qid == CONTROL_FILE_QID {
            return Ok(ReadOutcome::Immediate(Vec::new()));
        } else if qid == MINIBUFFER_QID {
            return Ok(self.minibuffer_read(offset, count));
        } else if qid == LOG_FILE_QID {
            return Ok(self.buffer_nodes.log.events_since_last_read(cid));
        }

        if let QidCheck::EventFile { buf_qid } = self.buffer_nodes.check_if_known_qid(qid) {
            match self.readlocked_cid(qid) {
                Some(id) if id == cid => (),
                Some(_) => return Ok(ReadOutcome::Immediate(Vec::new())),
                None => {
                    trace!("attaching filter qid={qid} cid={cid:?}");
                    self.buffer_nodes.attach_input_filter(buf_qid)?;
                    self.lock_qid_for_reading(qid, cid)?;
                }
            }
        }

        match self.buffer_nodes.get_file_content(qid, offset, count) {
            InternalRead::Unknown => Err(format!("{E_UNKNOWN_FILE}: {qid}")),
            InternalRead::Immediate(content) => Ok(ReadOutcome::Immediate(content)),
            InternalRead::Blocked(tx) => Ok(ReadOutcome::Blocked(tx)),
        }
    }

    fn read_dir(&mut self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        trace!(?cid, %qid, %uname, "handling read dir request");
        self.buffer_nodes.update();

        match qid {
            MOUNT_ROOT_QID => Ok(vec![
                self.log_file_stat.clone(),
                self.minibuffer_stat.clone(),
                self.control_file_stat.clone(),
                self.buffer_nodes.stat().clone(),
            ]),
            BUFFERS_QID => Ok(self.buffer_nodes.top_level_stats()),
            qid => self
                .buffer_nodes
                .buffer_level_stats(qid)
                .ok_or_else(|| E_UNKNOWN_FILE.to_string()),
        }
    }

    fn write(
        &mut self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize> {
        trace!(?cid, %qid, %offset, n_bytes=%data.len(), %uname, "handling write request");
        self.buffer_nodes.update();

        let n_bytes = data.len();
        let s = match String::from_utf8(data.to_vec()) {
            Ok(s) => s,
            Err(e) => return Err(format!("Invalid data: {e}")),
        };

        match qid {
            CONTROL_FILE_QID => {
                self.control_file_stat.last_modified = SystemTime::now();
                match Message::send(Req::ControlMessage { msg: s }, &self.tx) {
                    Ok(_) => Ok(n_bytes),
                    Err(e) => Err(format!("unable to execute control message: {e}")),
                }
            }

            MINIBUFFER_QID => self.minibuffer_write(s),

            CURRENT_BUFFER_QID | LOG_FILE_QID | INDEX_BUFFER_QID => Err(E_NOT_ALLOWED.to_string()),

            qid => self.buffer_nodes.write(qid, s, offset),
        }
    }

    // TODO: allow remove of a buffer to close the buffer
    fn remove(&mut self, cid: ClientId, qid: u64, uname: &str) -> Result<()> {
        trace!(?cid, %qid, %uname, "handling remove request");
        Err("remove not allowed".to_string())
    }

    fn create(
        &mut self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(FileMeta, IoUnit)> {
        trace!(?cid, %parent, %name, ?perm, ?mode, %uname, "handling create request");
        Err("create not allowed".to_string())
    }
}

fn apply_offset(data: &[u8], offset: usize, count: usize) -> Vec<u8> {
    data.iter()
        .skip(offset)
        .take(count)
        .copied()
        .collect::<Vec<u8>>()
}

fn empty_dir_stat(qid: u64, name: &str) -> Stat {
    Stat {
        fm: FileMeta::dir(name, qid),
        perms: Perm::OWNER_READ | Perm::OWNER_EXEC,
        n_bytes: 0,
        last_accesses: SystemTime::now(),
        last_modified: SystemTime::now(),
        owner: "ad".into(),
        group: "ad".into(),
        last_modified_by: "ad".into(),
    }
}

fn empty_file_stat(qid: u64, name: &str) -> Stat {
    Stat {
        fm: FileMeta::file(name, qid),
        perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
        n_bytes: 0,
        last_accesses: SystemTime::now(),
        last_modified: SystemTime::now(),
        owner: "ad".into(),
        group: "ad".into(),
        last_modified_by: "ad".into(),
    }
}
