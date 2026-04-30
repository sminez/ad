//! Types for describing files in a 9p virtual filesystem
use crate::sansio::protocol::{NineP, RawStat};
use std::{
    mem::size_of,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

pub use crate::sansio::protocol::{FileType, Qid};

/// The default root qid for 9p server implementations
pub const QID_ROOT: u64 = 0;

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
        const DIRECTORY = 0x80000000;
        /// Append only
        const APPEND_ONLY = 0x40000000;
        /// Exclusive access
        const EXCLUSIVE = 0x20000000;
        /// Mount
        const MOUNT = 0x10000000;
        /// Auth
        const AUTH = 0x08000000;
        /// Temp
        const TMP = 0x04000000;
        /// Symlink
        const SYMLINK = 0x02000000;
        /// File
        const FILE = 0x00000000;
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

    /// Allow owner, group & other to read.
    pub fn any_read() -> Perm {
        Perm::OWNER_READ | Perm::GROUP_READ | Perm::OTHER_READ
    }

    /// Allow owner, group & other to write.
    pub fn any_write() -> Perm {
        Perm::OWNER_WRITE | Perm::GROUP_WRITE | Perm::OTHER_WRITE
    }

    /// Allow owner, group & other to exec.
    pub fn any_exec() -> Perm {
        Perm::OWNER_EXEC | Perm::GROUP_EXEC | Perm::OTHER_EXEC
    }

    /// Permissions for the root directory
    pub fn root() -> Perm {
        Perm::any_read() | Perm::any_exec()
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
        let mask = if self.contains(Perm::DIRECTORY) {
            0o777
        } else {
            0o666
        };
        let bits = self.bits() & (!mask | (parent_perms.bits() & mask));

        Perm::new(bits)
    }
}

impl From<FileType> for Perm {
    fn from(value: FileType) -> Self {
        Perm::from_bits_truncate((value.bits() as u32) << 24)
    }
}

bitflags::bitflags! {
    /// Used in `Open` and `Create` requests to request I/O capabilities with the newly opened
    /// file.
    ///
    /// The open request asks the file server to check permissions and prepare a fid for I/O with
    /// subsequent read and write messages. The provided [Mode] bits determine the type of I/O
    /// supported on the open `fid` and are checked against the corresponding permissions for the
    /// file:
    ///   - `0x00` ([Mode::READ]): read only
    ///   - `0x01` ([Mode::WRITE]): write only
    ///   - `0x02` ([Mode::READ_WRITE]): both read and write
    ///   - `0x03` ([Mode::EXECUTE]): execute
    ///
    /// In addition, the following bits can be set to request additional behaviour:
    ///   - `0x10` ([Mode::TRUNCATE]): the file should be truncated before I/O begins. If the
    ///     file is marked as append only and permission is granted, the open request succeeds but
    ///     the file will not be truncated.
    ///   - `0x40` ([Mode::REMOVE_ON_CLOSE]): the file will be removed when closed (requires write
    ///     permissions on the parent directory).
    ///
    /// All other bits in [Mode] should be zero and are ignored. It is illegal to write a
    /// directory, truncate it or attempt to remove it on close. If a file is marked for exclusive
    /// use ([Perm::EXCLUSIVE]) then only one client may have it open at any time.
    ///
    /// Permissions are checked at the time of the open request; subsequent changes to server side
    /// file permissions do not affect the ability to read, write or remove an already open file.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct Mode: u8 {
        /// Open for read
        const READ = 0x00;
        /// Open for write
        const WRITE = 0x01;
        /// Open for read and write
        const READ_WRITE = 0x02;
        /// Open for read and execute
        const EXECUTE = 0x03;
        /// Truncate the file before I/O begins
        const TRUNCATE = 0x10;
        /// Remove the file when closed
        const REMOVE_ON_CLOSE = 0x40;
    }
}

impl Mode {
    /// Create a new [Mode] from a u8 bitmask
    pub fn new(bits: u8) -> Self {
        Mode::from_bits_truncate(bits)
    }

    fn is_illegal_for_dir(&self) -> bool {
        [
            Mode::WRITE,
            Mode::READ_WRITE,
            Mode::TRUNCATE,
            Mode::REMOVE_ON_CLOSE,
        ]
        .iter()
        .any(|m| self.contains(*m))
    }

    fn is_allowed(&self, read: bool, write: bool, exec: bool) -> bool {
        let m = Mode::new(self.bits() & 0x03); // Mask off the additional truncate and remove bits

        (m == Mode::READ && read)
            || (m == Mode::WRITE && write)
            || (m == Mode::READ_WRITE && read && write)
            || (m == Mode::EXECUTE && exec)
            || false
    }

    pub(crate) fn allows_read(&self) -> bool {
        *self == Mode::READ || *self == Mode::READ_WRITE
    }

    pub(crate) fn allows_write(&self) -> bool {
        *self == Mode::WRITE || *self == Mode::READ_WRITE
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
    /// The server qid for this file
    pub qid: Qid,
    /// The name of the file
    pub name: String,
    /// Owner
    pub owner: String,
    /// Group
    pub group: String,
    /// Permissions
    pub perms: Perm,
    /// Size in bytes
    pub n_bytes: u64,
    /// Timestamp of last access
    pub last_accessed: SystemTime,
    /// Timestamp of last modification
    pub last_modified: SystemTime,
    /// User who last modified this entry
    pub last_modified_by: String,
}

impl Stat {
    /// Whether or not the provided user details are permitted to use the given [Mode] on the file
    /// described by this [Stat].
    pub(crate) fn check_user_permissions(
        &self,
        user: &str,
        user_is_in_group: bool,
        mode: Mode,
    ) -> PermCheck {
        let (can_read, can_write, can_exec) = self
            .user_type(user, user_is_in_group)
            .flags_for_user(self.perms);

        PermCheck::new(
            mode,
            self.qid.ty == FileType::DIRECTORY,
            can_read,
            can_write,
            can_exec,
        )
    }

    /// Whether or not the given user can rename a child of this directory.
    ///
    /// Returns `false` if not a directory, otherwise the value of the `can_write` flag from
    /// [UserType::flags_for_user].
    pub(crate) fn can_rename_child(&self, user: &str, user_is_in_group: bool) -> bool {
        if self.qid.ty != FileType::DIRECTORY {
            return false;
        }

        let (_, can_write, _) = self
            .user_type(user, user_is_in_group)
            .flags_for_user(self.perms);

        can_write
    }

    fn user_type(&self, user: &str, user_is_in_group: bool) -> UserType {
        if user == self.owner {
            UserType::Owner
        } else if user_is_in_group {
            UserType::Group
        } else {
            UserType::Other
        }
    }

    #[cfg(test)]
    /// Create a new stub [Stat] with default permissions and metadata.
    pub(crate) fn stub(qid: Qid, name: impl Into<String>) -> Stat {
        let perms = if qid.ty == FileType::DIRECTORY {
            Perm::OWNER_READ | Perm::OWNER_EXEC
        } else {
            Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ
        };

        Stat {
            qid,
            name: name.into(),
            owner: "owner".to_string(),
            group: "group".to_string(),
            perms,
            n_bytes: 0,
            last_accessed: SystemTime::UNIX_EPOCH,
            last_modified: SystemTime::UNIX_EPOCH,
            last_modified_by: "owner".to_string(),
        }
    }
}

impl From<Stat> for RawStat {
    fn from(s: Stat) -> Self {
        let size = (size_of::<u16>()
            + size_of::<u32>() * 4
            + s.qid.n_bytes()
            + s.n_bytes.n_bytes()
            + s.name.n_bytes()
            + s.owner.n_bytes()
            + s.group.n_bytes()
            + s.last_modified_by.n_bytes()) as u16;

        RawStat {
            size,
            ty: u16::MAX,
            dev: u32::MAX,
            qid: s.qid,
            mode: (Perm::from(s.qid.ty) | s.perms).bits(),
            atime: systime_as_u32(s.last_accessed),
            mtime: systime_as_u32(s.last_modified),
            length: s.n_bytes,
            name: s.name.clone(),
            uid: s.owner,
            gid: s.group,
            muid: s.last_modified_by,
        }
    }
}

impl From<RawStat> for Stat {
    fn from(r: RawStat) -> Self {
        Stat {
            qid: r.qid,
            name: r.name,
            owner: r.uid,
            group: r.gid,
            perms: Perm::new(r.mode & 0x0000FFFF),
            last_accessed: systime_from_u32(r.atime),
            last_modified: systime_from_u32(r.mtime),
            n_bytes: r.length,
            last_modified_by: r.muid,
        }
    }
}

/// The outcome of calling [Stat::check_user_permissions].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PermCheck {
    /// The user is allowed to open / create the file using the requested [Mode].
    Allowed,
    /// The user is not allowed to open / create the file using the requested [Mode].
    Denied,
    /// The user requires write permissions on the parent directory in order to open / create the
    /// file using the requested [Mode].
    NeedWriteOnParent,
}

impl PermCheck {
    fn new(mode: Mode, is_dir: bool, can_read: bool, can_write: bool, can_exec: bool) -> Self {
        if (is_dir && mode.is_illegal_for_dir()) || !mode.is_allowed(can_read, can_write, can_exec)
        {
            return PermCheck::Denied;
        }

        if mode.contains(Mode::REMOVE_ON_CLOSE) {
            PermCheck::NeedWriteOnParent
        } else {
            PermCheck::Allowed
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UserType {
    Owner,
    Group,
    Other,
}

impl UserType {
    /// 1. If user == self.owner then use the OWNER_* bits from self.perms
    /// 2. Else, if user_group == self.group then use the GROUP_* bits from self.perms
    /// 3. Else, use the OTHER_* bits from self.perms
    fn flags_for_user(&self, p: Perm) -> (bool, bool, bool) {
        let (rbit, wbit, ebit) = match self {
            Self::Owner => (Perm::OWNER_READ, Perm::OWNER_WRITE, Perm::OWNER_EXEC),
            Self::Group => (Perm::GROUP_READ, Perm::GROUP_WRITE, Perm::GROUP_EXEC),
            Self::Other => (Perm::OTHER_READ, Perm::OTHER_WRITE, Perm::OTHER_EXEC),
        };

        (p.contains(rbit), p.contains(wbit), p.contains(ebit))
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
    /// A [WStat] with all fields other than `qid` as [None] requests that the server commits the
    /// file associated with `qid` to stable storage.
    pub fn commit(qid: Qid) -> Self {
        WStat {
            qid,
            name: None,
            perms: None,
            n_bytes: None,
            last_accesses: None,
            last_modified: None,
            group: None,
            last_modified_by: None,
        }
    }

    /// Try to apply this wstat update to an existing [Stat].
    ///
    /// Returns `Ok` after applying set fields if the [Qid] of this update and the provided stat
    /// are equal, otherwise `Err`.
    pub fn try_apply(self, stat: &Stat) -> Result<Stat, Box<WStat>> {
        if (self.qid.path != stat.qid.path) || (self.qid.ty != stat.qid.ty) {
            return Err(Box::new(self));
        }

        let mut stat = stat.clone();

        stat.name = self.name.unwrap_or(stat.name);
        stat.perms = self.perms.unwrap_or(stat.perms);
        stat.n_bytes = self.n_bytes.unwrap_or(stat.n_bytes);
        stat.last_accessed = self.last_accesses.unwrap_or(stat.last_accessed);
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
    use std::{
        collections::HashSet,
        time::{Duration, UNIX_EPOCH},
    };

    const TEST_QID: u64 = 42;

    fn stat() -> Stat {
        Stat {
            qid: Qid {
                ty: FileType::FILE,
                version: 0,
                path: TEST_QID,
            },
            name: "test".to_string(),
            owner: "owner".to_string(),
            group: "group".to_string(),
            perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
            n_bytes: 100,
            last_accessed: UNIX_EPOCH,
            last_modified: UNIX_EPOCH,
            last_modified_by: "modifier".to_string(),
        }
    }

    fn wstat() -> WStat {
        WStat {
            qid: Qid {
                ty: FileType::FILE,
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

    #[test_case(99, FileType::FILE; "path mismatch")]
    #[test_case(42, FileType::DIRECTORY; "type mismatch")]
    #[test_case(99, FileType::DIRECTORY; "path and type mismatch")]
    #[test]
    fn try_apply_qid_mismatch_returns_err(path: u64, ty: FileType) {
        let wstat = WStat {
            qid: Qid {
                ty,
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
        { let mut s = stat(); s.name = "foo".into(); s };
        "name"
    )]
    #[test_case(
        WStat { perms: Some(Perm::OWNER_READ), ..wstat() },
        { let mut s = stat(); s.perms = Perm::OWNER_READ; s };
        "perms"
    )]
    #[test_case(
        WStat { n_bytes: Some(200), ..wstat() },
        Stat { n_bytes: 200, ..stat() };
        "n_bytes"
    )]
    #[test_case(
        WStat { last_accesses: Some(UNIX_EPOCH + Duration::from_secs(1)), ..wstat() },
        Stat { last_accessed: UNIX_EPOCH + Duration::from_secs(1), ..stat() };
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
                ty: FileType::FILE,
                ..Qid::default()
            },
            mode: (Perm::DIRECTORY | Perm::OWNER_READ | Perm::OWNER_WRITE).bits(),
            ..RawStat::default()
        };
        let s = Stat::from(raw);
        assert_eq!(s.perms, Perm::OWNER_READ | Perm::OWNER_WRITE);
    }

    #[test_case(FileType::FILE; "regular file")]
    #[test_case(FileType::DIRECTORY; "directory")]
    #[test_case(FileType::APPEND_ONLY; "append only")]
    #[test_case(FileType::EXCLUSIVE; "exclusive")]
    #[test]
    fn stat_to_rawstat_file_type_encoding_works(ty: FileType) {
        let user_perms = Perm::OWNER_READ | Perm::OWNER_WRITE;
        let mut s = stat();
        s.qid.ty = ty;
        s.perms = user_perms;

        let raw = RawStat::from(s);

        assert_eq!(raw.qid.ty, ty);
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

    #[test]
    fn dir_stat_round_trips() {
        let stat = Stat::stub(Qid::dir(0), "foo");
        let raw = RawStat::from(stat.clone());
        let rt_stat = Stat::from(raw);

        assert_eq!(stat, rt_stat);
    }

    #[test_case(FileType::DIRECTORY, Perm::DIRECTORY; "directory")]
    #[test_case(FileType::APPEND_ONLY, Perm::APPEND_ONLY; "append only")]
    #[test_case(FileType::EXCLUSIVE, Perm::EXCLUSIVE; "exclusive")]
    #[test_case(FileType::AUTH, Perm::AUTH; "auth")]
    #[test_case(FileType::TMP, Perm::TMP; "temp")]
    #[test_case(FileType::FILE, Perm::FILE; "file")]
    #[test]
    fn perm_from_file_type_works(ft: FileType, expected: Perm) {
        assert_eq!(Perm::from(ft), expected);
    }

    #[test_case("owner", false, UserType::Owner; "owner without groups")]
    #[test_case("owner", true, UserType::Owner; "owner and group")]
    #[test_case("owner", false, UserType::Owner; "owner and other group")]
    #[test_case("bob", true, UserType::Group; "group")]
    #[test_case("bob", false, UserType::Other; "other group")]
    #[test_case("bob", false, UserType::Other; "no group")]
    #[test]
    fn stat_user_type_returns_expected_type(
        user: &str,
        user_is_in_group: bool,
        expected: UserType,
    ) {
        let stat = Stat::stub(Qid::file(0), "");
        assert_eq!(stat.owner, "owner", "wrong owner from stub");
        assert_eq!(stat.user_type(user, user_is_in_group), expected);
    }

    // The cases here are a little tricky to read but given that we can exhaustively test all
    // inputs to Mode::is_allowed, we should.

    #[test_case(
        Mode::READ,
        &[
            (true, false, false),
            (true, false, true),
            (true, true, false),
            (true, true, true)
        ],
        &[
            (false, false, false),
            (false, false, true),
            (false, true, false),
            (false, true, true)
        ];
        "read"
    )]
    #[test_case(
        Mode::WRITE,
        &[
            (false, true, false),
            (false, true, true),
            (true, true, false),
            (true, true, true),
        ],
        &[
            (false, false, false),
            (false, false, true),
            (true, false, false),
            (true, false, true),
        ];
        "write"
    )]
    #[test_case(
        Mode::READ_WRITE,
        &[
            (true, true, false),
            (true, true, true),
        ],
        &[
            (false, false, false),
            (false, false, true),
            (false, true, false),
            (false, true, true),
            (true, false, false),
            (true, false, true),
        ];
        "read write"
    )]
    #[test_case(
        Mode::EXECUTE,
        &[
            (false, false, true),
            (false, true, true),
            (true, false, true),
            (true, true, true),
        ],
        &[
            (false, false, false),
            (false, true, false),
            (true, false, false),
            (true, true, false),
        ];
        "execute"
    )]
    #[test]
    fn mode_is_allowed(base: Mode, allowed: &[(bool, bool, bool)], denied: &[(bool, bool, bool)]) {
        let it = allowed.iter().chain(denied.iter());
        let all: HashSet<&(bool, bool, bool)> = HashSet::from_iter(it);
        assert_eq!(
            all.len(),
            8,
            "invalid is_allowed case: must specify all flag combinations"
        );

        let tagged_modes = [
            ("base", base),
            ("truncate", base | Mode::TRUNCATE),
            ("remove on close", base | Mode::REMOVE_ON_CLOSE),
            ("both", base | Mode::TRUNCATE | Mode::REMOVE_ON_CLOSE),
        ];

        for &(r, w, e) in allowed.iter() {
            for (tag, mode) in tagged_modes.into_iter() {
                assert!(
                    mode.is_allowed(r, w, e),
                    "{tag} allowed (r={r}, w={w}, e={e})"
                );
            }
        }

        for &(r, w, e) in denied.iter() {
            for (tag, mode) in tagged_modes.into_iter() {
                assert!(
                    !mode.is_allowed(r, w, e),
                    "{tag} denied (r={r}, w={w}, e={e})"
                );
            }
        }
    }

    #[test_case(Mode::WRITE; "write")]
    #[test_case(Mode::READ_WRITE; "read write")]
    #[test_case(Mode::TRUNCATE; "truncate")]
    #[test_case(Mode::REMOVE_ON_CLOSE; "remove on close")]
    #[test]
    fn illegal_dir_modes_are_denied(mode: Mode) {
        assert!(mode.is_illegal_for_dir(), "non-illegal dir mode");
        assert_eq!(
            PermCheck::new(mode, true, true, true, true),
            PermCheck::Denied
        )
    }

    #[test_case(true, PermCheck::NeedWriteOnParent; "with remove on close")]
    #[test_case(false, PermCheck::Allowed; "without remove on close")]
    #[test]
    fn perm_check_for_allowed_mode_respects_remove_on_close(has_roc: bool, expected: PermCheck) {
        let mode = if has_roc {
            Mode::READ | Mode::REMOVE_ON_CLOSE
        } else {
            Mode::READ
        };

        // not a dir, only allow read
        assert_eq!(PermCheck::new(mode, false, true, false, false), expected);
    }
}
