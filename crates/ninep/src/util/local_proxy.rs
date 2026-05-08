//! A 9p filesystem proxy over a local directory.
use crate::{
    Result,
    fs::{FileTree, FileType, IoUnit, Mode, Perm, QID_ROOT, Qid, Stat, Timestamp, WStat},
    sansio::server::{E_PERMISSION_DENIED, E_UNKNOWN_FILE},
    sync::server::{ClientId, ReadOutcome, Serve9p},
};
use std::{
    collections::BTreeMap,
    fs::{self, Metadata, OpenOptions},
    io::{self, Read, Seek, SeekFrom, Write},
    mem::take,
    os::unix::fs::{MetadataExt, PermissionsExt},
    path::{Path, PathBuf},
};
use uzers::{get_group_by_gid, get_user_by_uid};

const DEFAULT_IOUNIT: IoUnit = 8168;
const E_ALREADY_EXISTS: &str = "file already exists";

/// A filesystem proxy that exposes a local directory over 9p.
#[derive(Debug)]
pub struct LocalProxyFs {
    root: PathBuf,
    ft: FileTree<PathMeta>,
    iounit: IoUnit,
}

impl LocalProxyFs {
    /// Construct a new [LocalProxyFs] pointed at the provided root directory.
    pub fn new(root: impl Into<PathBuf>) -> io::Result<Self> {
        Self::new_with_io_unit(root, DEFAULT_IOUNIT)
    }

    /// Construct a new [LocalProxyFs] pointed at the provided root directory using the given
    /// [IoUnit].
    pub fn new_with_io_unit(root: impl Into<PathBuf>, iounit: IoUnit) -> io::Result<Self> {
        let root = fs::canonicalize(root.into())?;
        let meta = fs::metadata(&root)?;
        if !meta.is_dir() {
            return Err(io::Error::other("proxy root must be a directory"));
        }

        let (owner, group) = owner_and_group_from_meta(&meta);
        let perms = perms_from_meta(&meta);
        let pm = PathMeta {
            path: root.clone(),
            meta,
            needs_sync: false,
        };

        let fs = Self {
            root,
            ft: FileTree::new(&owner, &group, perms, pm),
            iounit,
        };

        fs.try_sync_dir(QID_ROOT).map_err(io::Error::other)?;

        Ok(fs)
    }

    /// On-disk path and metadata for `qid`.
    ///
    /// Panics if called for an unknown qid
    fn try_meta_for_qid(&self, qid: u64) -> Result<PathMeta> {
        self.ft.with_file(qid, |f| f.aux.clone())
    }

    /// On-disk path for `qid`.
    ///
    /// Panics if called for an unknown qid
    fn path_for_qid(&self, qid: u64) -> PathBuf {
        self.ft.with_file(qid, |f| f.aux.path.clone()).unwrap()
    }

    /// Parent qid for `qid`.
    ///
    /// Panics if called for an unknown qid or the root node.
    fn parent_qid(&self, qid: u64) -> u64 {
        self.ft.with_file(qid, |f| f.parent().unwrap()).unwrap()
    }

    fn needs_sync(&self, qid: u64) -> bool {
        self.ft.with_file(qid, |f| f.aux.needs_sync).unwrap()
    }

    fn is_dir(&self, qid: u64) -> bool {
        self.ft.with_file(qid, |f| f.aux.meta.is_dir()).unwrap()
    }

    /// Check to see if the given node has been modified since we cached metadata for it.
    /// If we are unable to read metadata from disk, prune this node.
    fn modified_since_cache_or_prune(&self, qid: u64) -> Result<bool> {
        let modified_since_cache = |cached: PathMeta| -> io::Result<bool> {
            let t_cached = cached.meta.modified()?;
            let current = fs::metadata(&cached.path)?;
            let t_current = current.modified()?;

            Ok(t_current != t_cached)
        };

        let cached = self.try_meta_for_qid(qid)?;
        match modified_since_cache(cached) {
            Ok(opt) => Ok(opt),
            Err(e) => {
                self.ft.remove(qid);
                Err(e.to_string())
            }
        }
    }

    fn try_sync_dir(&self, qid: u64) -> Result<()> {
        let path = self.try_meta_for_qid(qid)?.path;
        let mut on_disk = self.on_disk_entries_for(&path).map_err(|e| e.to_string())?;

        // Update known cache entries and prune entries that are no longer present on disk
        for stat in self.ft.read_dir(qid)?.iter() {
            let qid = stat.qid.path;

            match on_disk.remove(&qid) {
                Some((name, pm)) => {
                    _ = self.ft.with_file_mut(qid, |f| {
                        let (owner, group) = owner_and_group_from_meta(&pm.meta);
                        f.stat = Stat {
                            qid: f.stat.qid,
                            name,
                            owner,
                            group,
                            perms: perms_from_meta(&pm.meta),
                            n_bytes: pm.meta.len(),
                            last_accessed: Timestamp::from_second(pm.meta.atime()).unwrap(),
                            last_modified: Timestamp::from_second(pm.meta.mtime()).unwrap(),
                            last_modified_by: take(&mut f.stat.last_modified_by),
                        };
                        f.aux.path = pm.path;
                        f.aux.meta = pm.meta;
                    })
                }
                None => self.ft.remove(qid),
            }
        }

        // Anything remaining in on_disk is something new so insert it
        for (qid_path, (name, pm)) in on_disk.into_iter() {
            let ty = if pm.meta.is_dir() {
                FileType::DIRECTORY
            } else {
                FileType::FILE
            };
            let perms = perms_from_meta(&pm.meta);
            let meta = pm.meta.clone();
            let qid = self
                .ft
                .try_add_node_with_qid(qid, qid_path, &name, perms, ty, pm)?;
            self.try_sync_file_with_meta(qid.path, meta)?;
        }

        _ = self.ft.with_file_mut(qid, |f| f.aux.needs_sync = false);

        Ok(())
    }

    fn try_sync_file(&self, qid: u64) -> Result<()> {
        let path = self.path_for_qid(qid);
        let meta = self
            .meta_if_under_root(&path)
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())?;

        self.try_sync_file_with_meta(qid, meta)
    }

    fn try_sync_file_with_meta(&self, qid: u64, meta: Metadata) -> Result<()> {
        let path = self.path_for_qid(qid);
        let name = path
            .file_name()
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())?
            .to_string_lossy()
            .to_string();

        let (owner, group) = owner_and_group_from_meta(&meta);

        self.ft.with_file_mut(qid, |f| {
            f.stat.name = name;
            f.stat.owner = owner;
            f.stat.group = group;
            f.stat.n_bytes = meta.len();
            f.stat.perms = perms_from_meta(&meta);
            f.stat.last_accessed = Timestamp::from_second(meta.atime()).unwrap();
            f.stat.last_modified = Timestamp::from_second(meta.mtime()).unwrap();
            f.aux = PathMeta::new(path, meta);
        })
    }

    fn on_disk_entries_for(
        &self,
        dir_path: &Path,
    ) -> io::Result<BTreeMap<u64, (String, PathMeta)>> {
        let mut m = BTreeMap::new();

        for entry in fs::read_dir(dir_path)? {
            let entry = entry?;
            let name = entry.file_name().to_string_lossy().to_string();
            let path = entry.path();

            // Only include entries that resolve to being under our root
            if let Some(meta) = self.meta_if_under_root(&path) {
                let ino = meta.ino();
                m.insert(ino, (name, PathMeta::new(path, meta)));
            };
        }

        Ok(m)
    }

    fn meta_if_under_root(&self, path: &Path) -> Option<Metadata> {
        let canonicalized = fs::canonicalize(path).ok()?;
        if !canonicalized.starts_with(&self.root) {
            return None;
        }

        fs::metadata(path).ok()
    }
}

impl Serve9p for LocalProxyFs {
    fn open(&self, qid: u64, _mode: Mode, _cid: ClientId) -> Result<IoUnit> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        Ok(self.iounit)
    }

    fn walk_one(&self, parent_qid: u64, child: &str, _cid: ClientId) -> Result<Qid> {
        if self.needs_sync(parent_qid) || self.modified_since_cache_or_prune(parent_qid)? {
            self.try_sync_dir(parent_qid)?;
        }

        self.ft.walk_one(parent_qid, child)
    }

    fn read(&self, qid: u64, offset: usize, count: usize, _cid: ClientId) -> Result<ReadOutcome> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        match io_read(&self.path_for_qid(qid), offset, count) {
            Ok(data) => Ok(ReadOutcome::Immediate(data)),
            Err(e) => Err(e.to_string()),
        }
    }

    fn read_dir(&self, qid: u64, _cid: ClientId) -> Result<Vec<Stat>> {
        if self.needs_sync(qid) || self.modified_since_cache_or_prune(qid)? {
            self.try_sync_dir(qid)?;
        }

        self.ft.read_dir(qid)
    }

    fn write(&self, qid: u64, offset: usize, data: Vec<u8>, _cid: ClientId) -> Result<usize> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        match io_write(&self.path_for_qid(qid), offset, &data) {
            Ok(count) => Ok(count),
            Err(e) => Err(e.to_string()),
        }
    }

    fn stat(&self, qid: u64, _cid: ClientId) -> Result<Stat> {
        if self.modified_since_cache_or_prune(qid)? {
            if self.is_dir(qid) {
                self.try_sync_dir(qid)?;
            } else {
                self.try_sync_file(qid)?;
            }
        }

        self.ft.stat(qid)
    }

    fn write_stat(&self, qid: u64, wstat: WStat, _cid: ClientId) -> Result<()> {
        if wstat.last_accessed.is_some()
            || wstat.last_modified.is_some()
            || wstat.group.is_some()
            || wstat.last_modified_by.is_some()
            || qid == QID_ROOT
        {
            return Err(E_PERMISSION_DENIED.to_string());
        }

        let pm = self.try_meta_for_qid(qid)?;
        if pm.meta.is_dir() {
            self.try_sync_dir(qid)?;
        } else {
            self.try_sync_file(qid)?;
        }

        if let Some(perms) = wstat.perms {
            io_set_perms(&pm.path, perms).map_err(|e| e.to_string())?;
        }

        if let Some(n_bytes) = wstat.n_bytes {
            io_set_len(&pm.path, n_bytes).map_err(|e| e.to_string())?;
        }

        if let Some(name) = wstat.name {
            io_rename(&pm.path, &name).map_err(|e| e.to_string())?;
            self.try_sync_dir(self.parent_qid(qid))?;
        }

        Ok(())
    }

    fn remove(&self, qid: u64, _cid: ClientId) -> Result<()> {
        if qid == 0 {
            return Err(E_PERMISSION_DENIED.to_string());
        }

        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        let path = self.path_for_qid(qid);

        let res = if self.is_dir(qid) {
            fs::remove_dir(&path)
        } else {
            fs::remove_file(&path)
        };

        res.map_err(|e| e.to_string())?;
        self.ft.remove(qid);

        Ok(())
    }

    fn create(
        &self,
        parent: u64,
        name: &str,
        perm: Perm,
        _mode: Mode,
        _cid: ClientId,
    ) -> Result<(Qid, IoUnit)> {
        if self.modified_since_cache_or_prune(parent)? {
            self.try_sync_dir(parent)?;
        }

        let path = self.path_for_qid(parent).join(name);
        io_create(&path, perm).map_err(|e| e.to_string())?;

        // Sync to allow our normal logic to pick up the new file details and create the qid
        self.try_sync_dir(parent)?;
        let qid = self.ft.walk_one(parent, name)?;

        Ok((qid, self.iounit))
    }
}

/// The metadata we store per-node in the file tree
#[derive(Debug, Clone)]
struct PathMeta {
    path: PathBuf,
    meta: Metadata,
    needs_sync: bool,
}

impl PathMeta {
    fn new(path: PathBuf, meta: Metadata) -> Self {
        let needs_sync = meta.is_dir();

        Self {
            path,
            meta,
            needs_sync,
        }
    }
}

fn perms_from_meta(meta: &Metadata) -> Perm {
    let mut perms = Perm::new(meta.permissions().mode() & 0o777);
    if meta.is_dir() {
        perms |= Perm::DIRECTORY;
    }

    perms
}

fn owner_and_group_from_meta(meta: &Metadata) -> (String, String) {
    let owner = get_user_by_uid(meta.uid())
        .map(|u| u.name().to_string_lossy().into_owned())
        .unwrap_or_else(|| "unknown".into());

    let group = get_group_by_gid(meta.gid())
        .map(|g| g.name().to_string_lossy().into_owned())
        .unwrap_or_else(|| "unknown".into());

    (owner, group)
}

// io::Result returning functions for use in the Serve9p methods above.

fn io_read(path: &Path, offset: usize, count: usize) -> io::Result<Vec<u8>> {
    let mut f = OpenOptions::new().read(true).open(path)?;
    f.seek(SeekFrom::Start(offset as u64))?;

    let mut buf = vec![0; count];
    let n = f.read(&mut buf)?;
    buf.truncate(n);

    Ok(buf)
}

fn io_write(path: &Path, offset: usize, data: &[u8]) -> io::Result<usize> {
    let mut f = OpenOptions::new().write(true).open(path)?;
    f.seek(SeekFrom::Start(offset as u64))?;

    f.write(data)
}

fn io_set_perms(path: &Path, perms: Perm) -> io::Result<()> {
    let mut permissions = fs::metadata(path)?.permissions();
    permissions.set_mode(perms.bits() & 0o777);

    fs::set_permissions(path, permissions)
}

fn io_set_len(path: &Path, n_bytes: u64) -> io::Result<()> {
    OpenOptions::new().write(true).open(path)?.set_len(n_bytes)
}

fn io_rename(path: &Path, name: &str) -> io::Result<()> {
    let parent = path
        .parent()
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "no parent"))?;

    fs::rename(path, parent.join(name))
}

fn io_create(path: &Path, perm: Perm) -> io::Result<()> {
    if path.exists() {
        return Err(io::Error::new(
            io::ErrorKind::AlreadyExists,
            E_ALREADY_EXISTS,
        ));
    }

    if perm.contains(Perm::DIRECTORY) {
        fs::create_dir(path)?;
    } else {
        fs::File::create_new(path)?;
    }

    let mut permissions = fs::metadata(path)?.permissions();
    permissions.set_mode(perm.bits() & 0o777);

    fs::set_permissions(path, permissions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::{
        TempDir,
        prelude::{FileWriteStr, PathChild},
    };
    use simple_test_case::test_case;
    use std::{
        fs,
        os::unix::fs::{PermissionsExt, symlink},
        path::{Path, PathBuf},
        time::SystemTime,
    };

    const CID: ClientId = ClientId(0);
    const ROOT_PATH: &str = "root";
    const HELLO_PATH: &str = "hello";
    const SUBDIR_PATH: &str = "subdir";
    const NESTED_PATH: &str = "subdir/nested";

    struct FsWithTempDir {
        tmp: TempDir,
        root: PathBuf,
        proxy_fs: LocalProxyFs,
    }

    impl FsWithTempDir {
        fn new() -> Self {
            let tmp = TempDir::new().unwrap();
            let root = tmp.path().join(ROOT_PATH);

            fs::create_dir(&root).unwrap();
            fs::write(root.join(HELLO_PATH), b"hello world").unwrap();
            fs::create_dir(root.join(SUBDIR_PATH)).unwrap();
            fs::write(root.join(NESTED_PATH), b"nested").unwrap();

            let proxy_fs = LocalProxyFs::new_with_io_unit(&root, 1024).unwrap();

            Self {
                tmp,
                root,
                proxy_fs,
            }
        }

        fn chmod(&self, child_path: impl AsRef<Path>, mode: u32) -> io::Result<()> {
            let path = self.root.join(child_path);
            let mut perms = fs::metadata(&path)?.permissions();
            perms.set_mode(mode);

            fs::set_permissions(path, perms)
        }

        fn symlink_in_root(
            &self,
            link_rel: impl AsRef<Path>,
            target: impl AsRef<Path>,
        ) -> io::Result<()> {
            symlink(target, self.root.join(link_rel))
        }

        fn qid_for_path(&self, path: &str) -> u64 {
            self.proxy_fs.ft.qid_for_path(&format!("/{path}")).unwrap()
        }
    }

    #[test]
    fn new_rejects_non_directory_root() {
        let tmp = TempDir::new().unwrap();
        let child = tmp.child("not-a-dir");
        child.write_str("content").unwrap();
        fs::write(&child, b"content").unwrap();

        let err = LocalProxyFs::new(child.path()).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::Other);
        assert!(err.to_string().contains("proxy root must be a directory"));
    }

    #[test]
    fn new_populates_root_and_marks_root_as_synced() {
        let FsWithTempDir {
            tmp: _tmp,
            proxy_fs,
            ..
        } = FsWithTempDir::new();

        let ft = &proxy_fs.ft;
        assert!(ft.qid_for_path(&format!("/{HELLO_PATH}")).is_some());
        let subdir_qid = ft.qid_for_path(&format!("/{SUBDIR_PATH}")).unwrap();

        assert!(!proxy_fs.needs_sync(QID_ROOT));
        assert!(proxy_fs.needs_sync(subdir_qid));
    }

    #[test]
    fn meta_if_under_root_handles_is_some_for_path_under_root() {
        let t = FsWithTempDir::new();

        assert!(
            t.proxy_fs
                .meta_if_under_root(&t.root.join(HELLO_PATH))
                .is_some()
        );
        assert!(
            t.proxy_fs
                .meta_if_under_root(&t.root.join(SUBDIR_PATH))
                .is_some()
        );
    }

    #[test]
    fn meta_if_under_root_is_none_for_path_out_of_root() {
        let t = FsWithTempDir::new();
        let outside = t.tmp.path().join("outside-file");
        fs::write(&outside, b"outside").unwrap();

        assert!(t.proxy_fs.meta_if_under_root(&outside).is_none());
    }

    #[test]
    fn meta_if_under_root_is_none_for_symlink_to_path_out_of_root() {
        let t = FsWithTempDir::new();
        let outside = t.tmp.path().join("outside-file");
        fs::write(&outside, b"outside").unwrap();

        t.symlink_in_root("escape", &outside).unwrap();
        assert!(
            t.proxy_fs
                .meta_if_under_root(&t.root.join("escape"))
                .is_none()
        );
    }

    #[test]
    fn on_disk_entries_for_filters_symlinks_out_of_root() {
        let t = FsWithTempDir::new();
        let outside = t.tmp.path().join("outside-file");
        fs::write(&outside, b"outside").unwrap();
        t.symlink_in_root("escape", &outside).unwrap();

        let entries = t.proxy_fs.on_disk_entries_for(&t.root).unwrap();
        let names: Vec<_> = entries.values().map(|(name, _)| name.as_str()).collect();

        assert!(names.contains(&HELLO_PATH));
        assert!(names.contains(&SUBDIR_PATH));
        assert!(!names.contains(&"escape"));
    }

    #[test]
    fn modified_since_cache_or_prune_reports_changes() {
        let FsWithTempDir {
            tmp: _tmp,
            root,
            proxy_fs,
        } = FsWithTempDir::new();
        let qid = proxy_fs.ft.qid_for_path(&format!("/{HELLO_PATH}")).unwrap();

        let modified = proxy_fs.modified_since_cache_or_prune(qid);
        assert_eq!(modified, Ok(false));

        fs::File::open(root.join(HELLO_PATH))
            .unwrap()
            .set_modified(SystemTime::UNIX_EPOCH)
            .unwrap();

        let modified = proxy_fs.modified_since_cache_or_prune(qid);
        assert_eq!(modified, Ok(true));
    }

    #[test]
    fn modified_since_cache_or_prune_prunes_when_entry_missing() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        fs::remove_file(t.root.join(HELLO_PATH)).unwrap();

        assert!(t.proxy_fs.modified_since_cache_or_prune(qid).is_err());
        assert_eq!(t.proxy_fs.ft.stat(qid).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn try_sync_file_updates_cached_metadata() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        fs::write(t.root.join(HELLO_PATH), b"hi").unwrap();
        t.chmod(HELLO_PATH, 0o600).unwrap();

        t.proxy_fs.try_sync_file(qid).unwrap();

        let stat = t.proxy_fs.ft.stat(qid).unwrap();
        assert_eq!(stat.n_bytes, 2);
        assert_eq!(stat.perms.bits() & 0o777, 0o600);
    }

    #[test]
    fn try_sync_file_returns_unknown_file_for_missing_path() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        fs::remove_file(t.root.join(HELLO_PATH)).unwrap();

        assert_eq!(t.proxy_fs.try_sync_file(qid).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn try_sync_dir_prunes_missing_entries() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        fs::remove_file(t.root.join(HELLO_PATH)).unwrap();

        t.proxy_fs.try_sync_dir(QID_ROOT).unwrap();

        assert_eq!(t.proxy_fs.ft.stat(qid).unwrap_err(), E_UNKNOWN_FILE);
        assert!(!t.proxy_fs.needs_sync(QID_ROOT));
    }

    #[test]
    fn try_sync_dir_adds_entries_and_updates_existing_metadata() {
        let t = FsWithTempDir::new();
        let hello_qid = t.qid_for_path(HELLO_PATH);

        t.chmod(HELLO_PATH, 0o600).unwrap();
        fs::write(t.root.join("new-file"), b"new").unwrap();
        fs::create_dir(t.root.join("new-dir")).unwrap();

        t.proxy_fs.try_sync_dir(QID_ROOT).unwrap();

        let hello_perms = t.proxy_fs.ft.stat(hello_qid).unwrap().perms;
        assert_eq!(hello_perms.bits() & 0o777, 0o600);

        let new_file_qid = t.qid_for_path("new-file");
        let new_dir_qid = t.qid_for_path("new-dir");

        assert!(!t.proxy_fs.needs_sync(QID_ROOT));
        assert!(!t.proxy_fs.needs_sync(new_file_qid));
        assert!(t.proxy_fs.needs_sync(new_dir_qid));
    }

    #[test]
    fn open_works() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        assert_eq!(
            t.proxy_fs.open(qid, Mode::READ, CID).unwrap(),
            t.proxy_fs.iounit
        );
        assert_eq!(
            t.proxy_fs.open(42, Mode::READ, CID).unwrap_err(),
            E_UNKNOWN_FILE
        );
    }

    #[test]
    fn walk_one_works() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        assert_eq!(
            t.proxy_fs.walk_one(QID_ROOT, HELLO_PATH, CID).unwrap().path,
            qid
        );
    }

    #[test_case(0, 3, b"hel"; "from start")] // typos:ignore
    #[test_case(2, 5, b"llo w"; "middle slice")]
    #[test_case(11, 5, b""; "offset at end")]
    #[test_case(100, 5, b""; "offset beyond end")]
    #[test]
    fn read_returns_expected_data(offset: usize, count: usize, expected: &[u8]) {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        match t.proxy_fs.read(qid, offset, count, CID).unwrap() {
            ReadOutcome::Immediate(data) => assert_eq!(&data, expected),
            ReadOutcome::Blocked(_) => {
                panic!("LocalProxyFs should always return immediate read data")
            }
        }
    }

    #[test]
    fn read_dir_returns_children_for_directory() {
        let t = FsWithTempDir::new();

        let mut root_children = t
            .proxy_fs
            .read_dir(QID_ROOT, CID)
            .unwrap()
            .into_iter()
            .map(|s| s.name)
            .collect::<Vec<_>>();
        root_children.sort();

        assert_eq!(
            root_children,
            vec![HELLO_PATH.to_string(), SUBDIR_PATH.to_string()]
        );

        let qid = t.qid_for_path(SUBDIR_PATH);
        let nested_children = t.proxy_fs.read_dir(qid, CID).unwrap();
        assert_eq!(nested_children.len(), 1);
        assert_eq!(nested_children[0].name, "nested");
    }

    #[test_case(1, b"ZZ", b"hZZlo world"; "overwrite existing bytes")]
    #[test_case(11, b"X", b"hello worldX"; "append at end")]
    #[test_case(0, b"", b"hello world"; "empty write")]
    #[test_case(12, b"X", b"hello world\0X"; "past current end")]
    #[test]
    fn write_updates_content(offset: usize, payload: &[u8], expected: &[u8]) {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        let n = t
            .proxy_fs
            .write(qid, offset, payload.to_vec(), CID)
            .unwrap();
        assert_eq!(n, payload.len());

        let path = t.proxy_fs.try_meta_for_qid(qid).unwrap().path;
        let data = fs::read(path).unwrap();
        assert_eq!(&data, expected);
    }

    #[test]
    fn stat_works() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);

        let stat = t.proxy_fs.stat(qid, CID).unwrap();
        assert_eq!(stat.qid.path, qid);
        assert_eq!(stat.name, HELLO_PATH);
    }

    #[test]
    fn write_stat_updates_file_metadata() {
        let t = FsWithTempDir::new();
        let qid_path = t.qid_for_path(HELLO_PATH);
        let qid = t.proxy_fs.ft.with_file(qid_path, |f| f.stat.qid).unwrap();

        let new_perms = Perm::OWNER_READ | Perm::OWNER_WRITE;
        let wstat = WStat {
            qid,
            name: Some("renamed".into()),
            perms: Some(new_perms),
            n_bytes: Some(123),
            ..Default::default()
        };

        t.proxy_fs.write_stat(qid.path, wstat, CID).unwrap();

        let stat = t.proxy_fs.stat(qid.path, CID).unwrap();
        assert_eq!(stat.name, "renamed");
        assert_eq!(stat.perms, new_perms);
        assert_eq!(stat.n_bytes, 123);
    }

    #[test]
    fn remove_works_for_file() {
        let t = FsWithTempDir::new();
        let qid = t.qid_for_path(HELLO_PATH);
        let path = t.proxy_fs.path_for_qid(qid);

        let res = t.proxy_fs.remove(qid, CID);

        assert!(res.is_ok(), "{res:?}");

        let err = fs::read(path).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::NotFound);
    }

    #[test]
    fn remove_works_for_empty_dir() {
        let t = FsWithTempDir::new();
        let dir_qid = t.qid_for_path(SUBDIR_PATH);
        t.proxy_fs.try_sync_dir(dir_qid).unwrap();

        // Remove the nested file we create to leave us with an empty dir
        let qid = t.qid_for_path(NESTED_PATH);
        let path = t.proxy_fs.path_for_qid(qid);
        fs::remove_file(path).unwrap();

        // Now remove the dir using proxy_fs
        let path = t.proxy_fs.path_for_qid(dir_qid);
        let res = t.proxy_fs.remove(dir_qid, CID);

        assert!(res.is_ok(), "{res:?}");

        let err = fs::read_dir(path).unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::NotFound);
    }

    #[test]
    fn remove_errors_for_occupied_dir() {
        let t = FsWithTempDir::new();

        // Now remove the dir using proxy_fs
        let qid = t.qid_for_path(SUBDIR_PATH);
        let path = t.proxy_fs.path_for_qid(qid);
        let res = t.proxy_fs.remove(qid, CID);

        assert!(res.is_err(), "{res:?}");

        // will panic if dir is missing
        fs::read_dir(path).unwrap();
    }

    #[test_case(Perm::OWNER_READ, FileType::FILE; "file create")]
    #[test_case(Perm::DIRECTORY | Perm::OWNER_READ, FileType::DIRECTORY; "directory create")]
    #[test]
    fn create_produces_correct_filetypes(perm: Perm, expected_ty: FileType) {
        let t = FsWithTempDir::new();

        let (qid, iounit) = t
            .proxy_fs
            .create(QID_ROOT, "new", perm, Mode::READ, CID)
            .unwrap();

        assert_eq!(iounit, 1024);
        assert_eq!(qid.ty, expected_ty);
        assert_eq!(t.proxy_fs.walk_one(0, "new", CID).unwrap().path, qid.path);
    }

    #[test]
    fn create_errors_when_target_already_exists() {
        let t = FsWithTempDir::new();
        let err = t
            .proxy_fs
            .create(QID_ROOT, HELLO_PATH, Perm::OWNER_READ, Mode::READ, CID)
            .unwrap_err();

        assert_eq!(err, E_ALREADY_EXISTS);
    }
}
