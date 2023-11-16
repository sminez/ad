//! Types for describing files in a 9p virtual filesystem
use super::protocol::{Format9p, Qid, RawStat};
use std::{
    mem::size_of,
    path::PathBuf,
    time::{SystemTime, UNIX_EPOCH},
};

// const NO_FID: u32 = u32::MAX;
pub const QID_ROOT: u64 = 0;

// File "mode" values for use in Qids
pub const QTDIR: u8 = 0x80;
pub const QTFILE: u8 = 0x00;
// const QTAPPEND: u8 = 0x40;
// const QTEXCL: u8 = 0x20;
// const QTMOUNT: u8 = 0x10;
// const QTAUTH: u8 = 0x08;
// const QTTMP: u8 = 0x04;
// const QTSYMLINK: u8 = 0x02;

pub const DMDIR: u32 = 0x80000000;
// const DMAPPEND: u32 = 0x40000000;
// const DMEXCL: u32 = 0x20000000;
// const DMMOUNT: u32 = 0x10000000;
// const DMAUTH: u32 = 0x08000000;
// const DMTMP: u32 = 0x04000000;
// const DMSYMLINK: u32 = 0x02000000;
// const DMDEVICE: u32 = 0x00800000;
// const DMNAMEDPIPE: u32 = 0x00200000;
// const DMSOCKET: u32 = 0x00100000;
// const DMSETUID: u32 = 0x00080000;
// const DMSETGID: u32 = 0x00040000;

pub type Mode = u8;

/// http://p9f.org/magic/man2html/2/iounit
///
/// Reads and writes of files are transmitted using the 9P protocol (see intro(5)) and in general,
/// operations involving large amounts of data must be broken into smaller pieces by the operating
/// system. The `I/O unit' associated with each file descriptor records the maximum size, in bytes,
/// that may be read or written without breaking up the transfer.
pub type IoUnit = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stat {
    pub qid: u64,
    pub name: String,
    pub ty: FileType,
    pub perms: u32,
    pub n_bytes: u64,
    pub last_accesses: SystemTime,
    pub last_modified: SystemTime,
    pub owner: String,
    pub group: String,
    pub last_modified_by: String,
}

impl From<(Qid, Stat)> for RawStat {
    fn from((qid, s): (Qid, Stat)) -> Self {
        let size = (size_of::<u16>()
            + size_of::<u32>() * 4
            + qid.n_bytes()
            + s.n_bytes.n_bytes()
            + s.name.n_bytes()
            + s.owner.n_bytes()
            + s.group.n_bytes()
            + s.last_modified_by.n_bytes()) as u16;

        RawStat {
            size,
            ty: 0,
            dev: 0,
            qid,
            mode: u32::from(s.ty) | s.perms,
            atime: systime_as_u32(s.last_accesses),
            mtime: systime_as_u32(s.last_modified),
            length: s.n_bytes,
            name: s.name,
            uid: s.owner,
            gid: s.group,
            muid: s.last_modified_by,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Regular,
    Directory,
}

impl From<FileType> for u8 {
    fn from(value: FileType) -> Self {
        match value {
            FileType::Directory => QTDIR,
            FileType::Regular => QTFILE,
        }
    }
}

impl From<FileType> for u32 {
    fn from(value: FileType) -> Self {
        match value {
            FileType::Directory => DMDIR,
            FileType::Regular => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct FileMeta {
    pub(super) path: PathBuf,
    pub(super) ty: FileType,
    pub(super) qid: u64,
}

fn systime_as_u32(t: SystemTime) -> u32 {
    match t.duration_since(UNIX_EPOCH) {
        Ok(d) => d.as_secs() as u32,
        Err(_) => 0,
    }
}
