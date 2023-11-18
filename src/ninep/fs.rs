//! Types for describing files in a 9p virtual filesystem
use super::protocol::{Format9p, Qid, RawStat};
use std::{
    mem::size_of,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

pub const QID_ROOT: u64 = 0;

/// The file mode contains some additional attributes besides the permissions. If bit 31 (DMDIR) is
/// set, the file is a directory; if bit 30 (DMAPPEND) is set, the file is append-only (offset is
/// ignored in writes); if bit 29 (DMEXCL) is set, the file is exclusive-use (only one client may
/// have it open at a time); if bit 27 (DMAUTH) is set, the file is an authentication file
/// established by auth messages; if bit 26 (DMTMP) is set, the contents of the file (or directory)
/// are not included in nightly archives. (Bit 28 is skipped for historical reasons.) These bits
/// are reproduced, from the top bit down, in the type byte of the Qid: QTDIR, QTAPPEND, QTEXCL,
/// (skipping one bit) QTAUTH, and QTTMP. The name QTFILE, defined to be zero, identifies the value
/// of the type for a plain file.
pub type Mode = u8;

pub mod mode {
    pub const QTDIR: u8 = 0x80;
    pub const QTAPPEND: u8 = 0x40;
    pub const QTEXCL: u8 = 0x20;
    pub const QTFILE: u8 = 0x00;
    // const QTMOUNT: u8 = 0x10;
    // const QTAUTH: u8 = 0x08;
    // const QTTMP: u8 = 0x04;
    // const QTSYMLINK: u8 = 0x02;
}

/// Each file has an associated owner and group id and three sets of permissions: those of the owner,
/// those of the group, and those of “other” users. When the owner attempts to do something to a file,
/// the owner, group, and other permissions are consulted, and if any of them grant the requested
/// permission, the operation is allowed. For someone who is not the owner, but is a member of the
/// file’s group, the group and other permissions are consulted. For everyone else, the other
/// permissions are used. Each set of permissions says whether reading is allowed, whether writing is
/// allowed, and whether executing is allowed.
///
/// A walk in a directory is regarded as executing the directory, not reading it.
///
/// Permissions are kept in the low-order bits of the file mode:
///   - owner read/write/execute permission represented as 1 in bits 8, 7, and 6 respectively
///     (using 0 to number the low order).
///   - The group permissions are in bits 5, 4, and 3,
///   - and the other permissions are in bits 2, 1, and 0.
pub type Perm = u32;

pub mod perm {
    pub const DMDIR: u32 = 0x80000000;
    pub const DMAPPEND: u32 = 0x40000000;
    pub const DMEXCL: u32 = 0x20000000;
    pub const DMFILE: u32 = 0x00000000;
    // const DMMOUNT: u32 = 0x10000000;
    // const DMAUTH: u32 = 0x08000000;
    // const DMTMP: u32 = 0x04000000;
    // const DMSYMLINK: u32 = 0x02000000;
    // const DMDEVICE: u32 = 0x00800000;
    // const DMNAMEDPIPE: u32 = 0x00200000;
    // const DMSOCKET: u32 = 0x00100000;
    // const DMSETUID: u32 = 0x00080000;
    // const DMSETGID: u32 = 0x00040000;
}

/// http://p9f.org/magic/man2html/2/iounit
///
/// Reads and writes of files are transmitted using the 9P protocol (see intro(5)) and in general,
/// operations involving large amounts of data must be broken into smaller pieces by the operating
/// system. The `I/O unit' associated with each file descriptor records the maximum size, in bytes,
/// that may be read or written without breaking up the transfer.
pub type IoUnit = u32;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stat {
    pub fm: FileMeta,
    pub perms: u32,
    pub n_bytes: u64,
    pub last_accesses: SystemTime,
    pub last_modified: SystemTime,
    pub owner: String,
    pub group: String,
    pub last_modified_by: String,
}

impl From<Stat> for RawStat {
    fn from(s: Stat) -> Self {
        let qid = Qid {
            ty: s.fm.ty.into(),
            version: 0,
            path: s.fm.qid,
        };

        let size = (size_of::<u16>()
            + size_of::<u32>() * 4
            + qid.n_bytes()
            + s.n_bytes.n_bytes()
            + s.fm.name.n_bytes()
            + s.owner.n_bytes()
            + s.group.n_bytes()
            + s.last_modified_by.n_bytes()) as u16;

        RawStat {
            size,
            ty: 0,
            dev: 0,
            qid,
            mode: Perm::from(s.fm.ty) | s.perms,
            atime: systime_as_u32(s.last_accesses),
            mtime: systime_as_u32(s.last_modified),
            length: s.n_bytes,
            name: s.fm.name.clone(),
            uid: s.owner,
            gid: s.group,
            muid: s.last_modified_by,
        }
    }
}

impl TryFrom<RawStat> for Stat {
    type Error = String;

    fn try_from(r: RawStat) -> Result<Self, String> {
        Ok(Stat {
            fm: FileMeta {
                name: r.name,
                ty: r.qid.ty.try_into()?,
                qid: r.qid.path,
            },
            perms: r.mode & 0x0000FFFF,
            last_accesses: systime_from_u32(r.atime),
            last_modified: systime_from_u32(r.mtime),
            n_bytes: r.length,
            owner: r.uid,
            group: r.gid,
            last_modified_by: r.muid,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Directory,
    Regular,
    AppendOnly,
    Exclusive,
}

impl From<FileType> for Mode {
    fn from(value: FileType) -> Self {
        match value {
            FileType::Directory => mode::QTDIR,
            FileType::Regular => mode::QTFILE,
            FileType::AppendOnly => mode::QTAPPEND,
            FileType::Exclusive => mode::QTEXCL,
        }
    }
}

impl TryFrom<Mode> for FileType {
    type Error = String;

    fn try_from(value: Mode) -> Result<Self, String> {
        match value {
            mode::QTDIR => Ok(Self::Directory),
            mode::QTFILE => Ok(Self::Regular),
            mode::QTAPPEND => Ok(Self::AppendOnly),
            mode::QTEXCL => Ok(Self::Exclusive),
            m => Err(format!("invalid mode value: {m:o}")),
        }
    }
}

impl From<FileType> for Perm {
    fn from(value: FileType) -> Self {
        match value {
            FileType::Directory => perm::DMDIR,
            FileType::Regular => perm::DMFILE,
            FileType::AppendOnly => perm::DMAPPEND,
            FileType::Exclusive => perm::DMEXCL,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileMeta {
    pub(super) name: String,
    pub(super) ty: FileType,
    pub(super) qid: u64,
}

impl FileMeta {
    pub(super) fn as_qid(&self) -> Qid {
        Qid {
            ty: self.ty.into(),
            version: 0,
            path: self.qid,
        }
    }

    pub fn dir(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::Directory,
            qid,
        }
    }

    pub fn file(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::Regular,
            qid,
        }
    }

    pub fn append_only_file(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::AppendOnly,
            qid,
        }
    }

    pub fn exclusive_file(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::Exclusive,
            qid,
        }
    }
}

fn systime_as_u32(t: SystemTime) -> u32 {
    match t.duration_since(UNIX_EPOCH) {
        Ok(d) => d.as_secs() as u32,
        Err(_) => 0,
    }
}

fn systime_from_u32(t: u32) -> SystemTime {
    UNIX_EPOCH + Duration::from_secs(t as u64)
}
