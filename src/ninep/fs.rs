//! Types for describing files in a 9p virtual filesystem
use super::protocol::{Format9p, Qid, RawStat};
use std::{
    mem::size_of,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

pub const QID_ROOT: u64 = 0;

bitflags::bitflags! {
    /// The file mode contains some additional attributes besides the permissions. If bit 31 (DMDIR) is
    /// set, the file is a directory; if bit 30 (DMAPPEND) is set, the file is append-only (offset is
    /// ignored in writes); if bit 29 (DMEXCL) is set, the file is exclusive-use (only one client may
    /// have it open at a time); if bit 27 (DMAUTH) is set, the file is an authentication file
    /// established by auth messages; if bit 26 (DMTMP) is set, the contents of the file (or directory)
    /// are not included in nightly archives. (Bit 28 is skipped for historical reasons.) These bits
    /// are reproduced, from the top bit down, in the type byte of the Qid: QTDIR, QTAPPEND, QTEXCL,
    /// (skipping one bit) QTAUTH, and QTTMP. The name QTFILE, defined to be zero, identifies the value
    /// of the type for a plain file.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Mode: u8 {
        const DIR = 0x80;
        const APPEND = 0x40;
        const EXCLUSIVE = 0x20;
        const MOUNT = 0x10;
        const AUTH = 0x08;
        const TMP = 0x04;
        const SYMLINK = 0x02;
        const FILE = 0x00;
    }
}

impl Mode {
    pub fn new(bits: u8) -> Self {
        Mode::from_bits_truncate(bits)
    }
}

bitflags::bitflags! {
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
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Perm: u32 {
        const DIR = 0x80000000;
        const APPEND = 0x40000000;
        const EXCLUSIVE = 0x20000000;
        const FILE = 0x00000000;
        const MOUNT = 0x10000000;
        const AUTH = 0x08000000;
        const TMP = 0x04000000;
        const SYMLINK = 0x02000000;
        const DEVICE = 0x00800000;
        const NAMED_PIPE = 0x00200000;
        const SOCKET = 0x00100000;
        const SET_UID = 0x00080000;
        const SET_GID = 0x00040000;

        const OWNER_READ = 0o400;
        const OWNER_WRITE = 0o200;
        const OWNER_EXEC = 0o100;

        const GROUP_READ = 0o040;
        const GROUP_WRITE = 0o020;
        const GROUP_EXEC = 0o010;

        const OTHER_READ = 0o004;
        const OTHER_WRITE = 0o002;
        const OTHER_EXEC = 0o001;
    }
}

impl Perm {
    pub fn new(bits: u32) -> Self {
        Perm::from_bits_truncate(bits)
    }
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
    pub perms: Perm,
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
            ty: Mode::from(s.fm.ty).bits(),
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
            mode: (Perm::from(s.fm.ty) | s.perms).bits(),
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
                ty: Mode::new(r.qid.ty).try_into()?,
                qid: r.qid.path,
            },
            perms: Perm::new(r.mode & 0x0000FFFF),
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
            FileType::Directory => Mode::DIR,
            FileType::Regular => Mode::FILE,
            FileType::AppendOnly => Mode::APPEND,
            FileType::Exclusive => Mode::EXCLUSIVE,
        }
    }
}

impl TryFrom<Mode> for FileType {
    type Error = String;

    fn try_from(value: Mode) -> Result<Self, String> {
        match value {
            Mode::DIR => Ok(Self::Directory),
            Mode::FILE => Ok(Self::Regular),
            Mode::APPEND => Ok(Self::AppendOnly),
            Mode::EXCLUSIVE => Ok(Self::Exclusive),
            m => Err(format!("invalid mode value: {m:o}")),
        }
    }
}

impl From<FileType> for Perm {
    fn from(value: FileType) -> Self {
        match value {
            FileType::Directory => Perm::DIR,
            FileType::Regular => Perm::FILE,
            FileType::AppendOnly => Perm::APPEND,
            FileType::Exclusive => Perm::EXCLUSIVE,
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
            ty: Mode::from(self.ty).bits(),
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
