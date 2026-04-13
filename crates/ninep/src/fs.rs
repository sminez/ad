//! Types for describing files in a 9p virtual filesystem
use crate::sansio::protocol::{NineP, Qid, RawStat};
use std::{
    mem::size_of,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

/// The default root qid for 9p server implementations
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
        /// Directory
        const DIR = 0x80;
        /// Append only
        const APPEND = 0x40;
        /// Exclusive access
        const EXCLUSIVE = 0x20;
        /// Mount
        const MOUNT = 0x10;
        /// Auth
        const AUTH = 0x08;
        /// Temp
        const TMP = 0x04;
        /// Symlink
        const SYMLINK = 0x02;
        /// File
        const FILE = 0x00;
    }
}

impl Mode {
    /// Create a new [Mode] from a u8 bitmask
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
        /// Directory
        const DIR = 0x80000000;
        /// Append only
        const APPEND = 0x40000000;
        /// Exclusive access
        const EXCLUSIVE = 0x20000000;
        /// File
        const FILE = 0x00000000;
        /// Mount
        const MOUNT = 0x10000000;
        /// Auth
        const AUTH = 0x08000000;
        /// Temp
        const TMP = 0x04000000;
        /// Symlink
        const SYMLINK = 0x02000000;
        /// Device
        const DEVICE = 0x00800000;
        /// Named pipe
        const NAMED_PIPE = 0x00200000;
        /// Socket
        const SOCKET = 0x00100000;
        /// Set UID
        const SET_UID = 0x00080000;
        /// Set GID
        const SET_GID = 0x00040000;

        /// Readable by owner
        const OWNER_READ = 0o400;
        /// Writable by owner
        const OWNER_WRITE = 0o200;
        /// Executable by owner
        const OWNER_EXEC = 0o100;

        /// Readable by group
        const GROUP_READ = 0o040;
        /// Writable by group
        const GROUP_WRITE = 0o020;
        /// Executable by group
        const GROUP_EXEC = 0o010;

        /// Readable by other
        const OTHER_READ = 0o004;
        /// Writable by other
        const OTHER_WRITE = 0o002;
        /// Executable by other
        const OTHER_EXEC = 0o001;
    }
}

impl Perm {
    /// Create a new [Perm] from a u32 bitmask
    pub fn new(bits: u32) -> Self {
        Perm::from_bits_truncate(bits)
    }

    /// Apply the appropriate 9P create permission mask based on the parent directory's
    /// permissions:
    ///
    /// The create request asks the file server to create a new file with the name supplied, in the
    /// directory (dir) represented by fid, and requires write permission in the directory. The
    /// owner of the file is the implied user id of the request, the group of the file is the same
    /// as dir, and the permissions are the value of
    ///   perm & (~0666 | (dir.perm & 0666))
    /// if a regular file is being created and
    ///   perm & (~0777 | (dir.perm & 0777))
    /// if a directory is being created. This means, for example, that if the create allows read
    /// permission to others, but the containing directory does not, then the created file will not
    /// allow others to read the file.
    pub fn apply_create_mask(&self, parent_perms: Perm) -> Perm {
        let mask = if self.contains(Perm::DIR) {
            0o777
        } else {
            0o666
        };
        let bits = self.bits() & (!mask | (parent_perms.bits() & mask));

        Perm::new(bits)
    }
}

/// <http://p9f.org/magic/man2html/2/iounit>
///
/// Reads and writes of files are transmitted using the 9P protocol (see intro(5)) and in general,
/// operations involving large amounts of data must be broken into smaller pieces by the operating
/// system. The `I/O unit` associated with each file descriptor records the maximum size, in bytes,
/// that may be read or written without breaking up the transfer.
pub type IoUnit = u32;

/// A machine independent directory entry
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stat {
    /// File metadata
    pub fm: FileMeta,
    /// Permissions
    pub perms: Perm,
    /// Size in bytes
    pub n_bytes: u64,
    /// Timestamp of last access
    pub last_accesses: SystemTime,
    /// Timestamp of last modification
    pub last_modified: SystemTime,
    /// Owner
    pub owner: String,
    /// Group
    pub group: String,
    /// User who last modified this entry
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
            ty: u16::MAX,
            dev: u32::MAX,
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

/// An update to a known [Stat].
///
/// Optional fields with a [None] value denote leaving the field in the existing [Stat] unchanged.
/// The [WStat::try_apply] method can be used to update the existing stat with the corresponding
/// `qid`.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct WStat {
    /// The server Qid for this entry
    pub qid: Qid,
    /// The name of the entry
    pub name: Option<String>,
    /// Permissions
    pub perms: Option<Perm>,
    /// Size in bytes
    pub n_bytes: Option<u64>,
    /// Timestamp of last access
    pub last_accesses: Option<SystemTime>,
    /// Timestamp of last modification
    pub last_modified: Option<SystemTime>,
    /// Group
    pub group: Option<String>,
    /// User who last modified this entry
    pub last_modified_by: Option<String>,
}

impl WStat {
    /// Try to apply this wstat update to an existing [Stat].
    ///
    /// Returns `Ok` after applying set fields if the [Qid] of this update and the provided stat
    /// are equal, otherwise `Err`.
    pub fn try_apply(self, stat: &Stat) -> Result<Stat, Box<WStat>> {
        let mode = Mode::new(self.qid.ty);
        if (self.qid.path != stat.fm.qid) || (mode != Mode::from(stat.fm.ty)) {
            return Err(Box::new(self));
        }

        let mut stat = stat.clone();

        stat.fm.name = self.name.unwrap_or(stat.fm.name);
        stat.perms = self.perms.unwrap_or(stat.perms);
        stat.n_bytes = self.n_bytes.unwrap_or(stat.n_bytes);
        stat.last_accesses = self.last_accesses.unwrap_or(stat.last_accesses);
        stat.last_modified = self.last_modified.unwrap_or(stat.last_modified);
        stat.group = self.group.unwrap_or(stat.group);
        stat.last_modified_by = self.last_modified_by.unwrap_or(stat.last_modified_by);

        Ok(stat)
    }
}

// From the spec: https://9fans.github.io/plan9port/man/man9/stat.html
//
// A wstat request can avoid modifying some properties of the file by providing explicit “don’t
// touch” values in the stat data that is sent: zero-length strings for text values and the maximum
// unsigned value of appropriate size for integral values. As a special case, if all the elements
// of the directory entry in a Twstat message are “don’t touch” values, the server may interpret it
// as a request to guarantee that the contents of the associated file are committed to stable
// storage before the Rwstat message is returned. (Consider the message to mean, “make the state of
// the file exactly what it claims to be.”)
impl From<RawStat> for WStat {
    fn from(r: RawStat) -> Self {
        Self {
            qid: r.qid,
            name: if r.name.is_empty() {
                None
            } else {
                Some(r.name)
            },
            perms: if r.mode == u32::MAX {
                None
            } else {
                Some(Perm::new(r.mode & 0x0000FFFF))
            },
            n_bytes: if r.length == u64::MAX {
                None
            } else {
                Some(r.length)
            },
            last_accesses: if r.atime == u32::MAX {
                None
            } else {
                Some(systime_from_u32(r.atime))
            },
            last_modified: if r.mtime == u32::MAX {
                None
            } else {
                Some(systime_from_u32(r.mtime))
            },
            group: if r.gid.is_empty() { None } else { Some(r.gid) },
            last_modified_by: if r.muid.is_empty() {
                None
            } else {
                Some(r.muid)
            },
        }
    }
}

impl From<WStat> for RawStat {
    fn from(w: WStat) -> Self {
        let name = w.name.unwrap_or_default();
        let uid = String::new();
        let gid = w.group.unwrap_or_default();
        let muid = w.last_modified_by.unwrap_or_default();
        let mode = w.perms.map_or(u32::MAX, |p| p.bits() & 0x0000FFFF);
        let atime = w.last_accesses.map_or(u32::MAX, systime_as_u32);
        let mtime = w.last_modified.map_or(u32::MAX, systime_as_u32);
        let length = w.n_bytes.unwrap_or(u64::MAX);

        let size = (size_of::<u16>()
            + size_of::<u32>() * 4
            + w.qid.n_bytes()
            + length.n_bytes()
            + name.n_bytes()
            + uid.n_bytes()
            + gid.n_bytes()
            + muid.n_bytes()) as u16;

        RawStat {
            size,
            ty: u16::MAX,
            dev: u32::MAX,
            qid: w.qid,
            mode,
            atime,
            mtime,
            length,
            name,
            uid,
            gid,
            muid,
        }
    }
}

/// Supported filetypes
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    /// Directory
    Directory,
    /// Regular
    Regular,
    /// Append only
    AppendOnly,
    /// Exclusive access
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

/// File meta-data
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileMeta {
    /// The name of the file
    pub name: String,
    /// The type of the file
    pub ty: FileType,
    /// The server Qid for this file
    pub qid: u64,
}

impl FileMeta {
    pub(super) fn as_qid(&self) -> Qid {
        Qid {
            ty: Mode::from(self.ty).bits(),
            version: 0,
            path: self.qid,
        }
    }

    /// Construct a new [FileMeta] for a directory.
    pub fn dir(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::Directory,
            qid,
        }
    }

    /// Construct a new [FileMeta] for a regular file.
    pub fn file(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::Regular,
            qid,
        }
    }

    /// Construct a new [FileMeta] for an append only file.
    pub fn append_only_file(name: impl Into<String>, qid: u64) -> Self {
        Self {
            name: name.into(),
            ty: FileType::AppendOnly,
            qid,
        }
    }

    /// Construct a new [FileMeta] for an exclusive file.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sansio::protocol::{Qid, RawStat};
    use simple_test_case::test_case;
    use std::time::{Duration, UNIX_EPOCH};

    const TEST_QID: u64 = 42;
    const TEST_MODE: Mode = Mode::FILE;

    fn stat() -> Stat {
        Stat {
            fm: FileMeta {
                name: "test".to_string(),
                ty: FileType::Regular,
                qid: TEST_QID,
            },
            perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
            n_bytes: 100,
            last_accesses: UNIX_EPOCH,
            last_modified: UNIX_EPOCH,
            owner: "owner".to_string(),
            group: "group".to_string(),
            last_modified_by: "modifier".to_string(),
        }
    }

    fn wstat() -> WStat {
        WStat {
            qid: Qid {
                ty: TEST_MODE.bits(),
                version: 0,
                path: TEST_QID,
            },
            name: None,
            perms: None,
            n_bytes: None,
            last_accesses: None,
            last_modified: None,
            group: None,
            last_modified_by: None,
        }
    }

    #[test_case(99, Mode::FILE; "path mismatch")]
    #[test_case(42, Mode::DIR; "type mismatch")]
    #[test_case(99, Mode::DIR; "path and type mismatch")]
    #[test]
    fn try_apply_qid_mismatch_returns_err(path: u64, mode: Mode) {
        let wstat = WStat {
            qid: Qid {
                ty: mode.bits(),
                version: 0,
                path,
            },
            ..Default::default()
        };

        assert!(wstat.try_apply(&stat()).is_err());
    }

    #[test_case(wstat(), stat(); "unchanged")]
    #[test_case(
        WStat { name: Some("foo".into()), ..wstat() },
        { let mut s = stat(); s.fm.name = "foo".into(); s };
        "name"
    )]
    #[test_case(
        WStat { perms: Some(Perm::OWNER_READ), ..wstat() },
        Stat { perms: Perm::OWNER_READ, ..stat() };
        "perms"
    )]
    #[test_case(
        WStat { n_bytes: Some(200), ..wstat() },
        Stat { n_bytes: 200, ..stat() };
        "n_bytes"
    )]
    #[test_case(
        WStat { last_accesses: Some(UNIX_EPOCH + Duration::from_secs(1)), ..wstat() },
        Stat { last_accesses: UNIX_EPOCH + Duration::from_secs(1), ..stat() };
        "last_accesses"
    )]
    #[test_case(
        WStat { last_modified: Some(UNIX_EPOCH + Duration::from_secs(1)), ..wstat() },
        Stat { last_modified: UNIX_EPOCH + Duration::from_secs(1), ..stat() };
        "last_modified"
    )]
    #[test_case(
        WStat { last_modified_by: Some("new_modifier".into()), ..wstat() },
        Stat { last_modified_by: "new_modifier".into(), ..stat() };
        "last_modified_by"
    )]
    #[test_case(
        WStat { group: Some("new_group".into()), ..wstat() },
        Stat { group: "new_group".into(), ..stat() };
        "group"
    )]
    #[test]
    fn try_apply_updated_expected_fields(wstat: WStat, expected: Stat) {
        assert_eq!(wstat.try_apply(&stat()), Ok(expected));
    }

    #[test]
    fn wstat_sentinel_values_are_correctly_handled() {
        let raw = RawStat {
            name: String::new(),
            mode: u32::MAX,
            length: u64::MAX,
            atime: u32::MAX,
            mtime: u32::MAX,
            gid: String::new(),
            muid: String::new(),
            ..Default::default()
        };

        let wstat = WStat::from(raw);

        assert!(wstat.name.is_none(), "name");
        assert!(wstat.perms.is_none(), "perms");
        assert!(wstat.n_bytes.is_none(), "n_bytes");
        assert!(wstat.last_accesses.is_none(), "last_accessed");
        assert!(wstat.last_modified.is_none(), "last_modified");
        assert!(wstat.group.is_none(), "group");
        assert!(wstat.last_modified_by.is_none(), "last_modified_by");
    }

    #[test]
    fn rawstat_to_stat_strips_filetype_bits_from_perms() {
        let raw = RawStat {
            qid: Qid {
                ty: Mode::FILE.bits(),
                ..Qid::default()
            },
            mode: (Perm::DIR | Perm::OWNER_READ | Perm::OWNER_WRITE).bits(),
            ..RawStat::default()
        };
        let s = Stat::try_from(raw).unwrap();
        assert_eq!(s.perms, Perm::OWNER_READ | Perm::OWNER_WRITE);
    }

    #[test_case(FileType::Regular; "regular file")]
    #[test_case(FileType::Directory; "directory")]
    #[test_case(FileType::AppendOnly; "append only")]
    #[test_case(FileType::Exclusive; "exclusive")]
    #[test]
    fn stat_to_rawstat_file_type_encoding_works(ty: FileType) {
        let user_perms = Perm::OWNER_READ | Perm::OWNER_WRITE;
        let mut s = stat();
        s.fm.ty = ty;
        s.perms = user_perms;

        let raw = RawStat::from(s);

        assert_eq!(raw.qid.ty, Mode::from(ty).bits());
        assert_eq!(raw.mode, (Perm::from(ty) | user_perms).bits());
    }

    #[test_case(wstat(); "all fields unset")]
    #[test_case(
        WStat {
            name: Some("renamed".into()),
            perms: Some(Perm::OWNER_READ | Perm::OWNER_WRITE),
            n_bytes: Some(1_337),
            last_accesses: Some(UNIX_EPOCH + Duration::from_secs(10)),
            last_modified: Some(UNIX_EPOCH + Duration::from_secs(20)),
            group: Some("wheel".into()),
            last_modified_by: Some("alice".into()),
            ..wstat()
        };
        "all writable fields set"
    )]
    #[test]
    fn wstat_rawstat_round_trip(w: WStat) {
        assert_eq!(WStat::from(RawStat::from(w.clone())), w);
    }
}
