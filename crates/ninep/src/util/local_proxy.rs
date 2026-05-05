//! A 9p filesystem proxy over a local directory.
use crate::{
    Result,
    fs::{FileTree, FileType, IoUnit, Mode, Perm, Qid, Stat, Timestamp, WStat},
    sansio::server::{E_PERMISSION_DENIED, E_UNKNOWN_FILE},
    sync::server::{ClientId, ReadOutcome, Serve9p},
};
use std::{
    collections::BTreeMap,
    fs::{self, Metadata, OpenOptions},
    io::{self, Read, Seek, SeekFrom, Write},
    os::unix::fs::{MetadataExt, PermissionsExt},
    path::{Path, PathBuf},
};
use uzers::{get_group_by_gid, get_user_by_uid};

const DEFAULT_IOUNIT: IoUnit = 8168;

/// The metadata we store per-node in the file tree
#[derive(Debug, Clone)]
struct PathMeta {
    path: PathBuf,
    meta: Metadata,
}

/// A filesystem proxy that exposes a local directory over 9p.
#[derive(Debug)]
pub struct LocalProxyFs {
    root: PathBuf,
    ft: FileTree<PathMeta>,
    iounit: IoUnit,
}

impl LocalProxyFs {
    /// Construct a new [ProxyFs] pointed at the provided root directory
    pub fn new(root: impl Into<PathBuf>) -> io::Result<Self> {
        let root = fs::canonicalize(root.into())?;
        let root_meta = fs::metadata(&root)?;
        if !root_meta.is_dir() {
            return Err(io::Error::other("proxy root must be a directory"));
        }

        let (owner, group) = owner_and_group_from_meta(&root_meta);
        let perms = perms_from_meta(&root_meta);
        let pm = PathMeta {
            path: root.clone(),
            meta: root_meta,
        };

        let ft = FileTree::new(&owner, &group, perms, pm);

        let fs = Self {
            root,
            ft,
            iounit: DEFAULT_IOUNIT,
        };

        fs.try_sync_dir(0).map_err(io::Error::other)?;

        Ok(fs)
    }

    /// On-disk path and metadata for `qid`.
    ///
    /// Panics if called for an unknown qid
    fn meta_for_qid(&self, qid: u64) -> PathMeta {
        self.ft.with_file(qid, |f| f.aux.clone()).unwrap()
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

    /// Check to see if the given node has been modified since we cached metadata for it.
    /// If we are unable to read metadata from disk, prune this node.
    fn modified_since_cache_or_prune(&self, qid: u64) -> Result<bool> {
        let modified_since_cache = |qid: u64| -> io::Result<bool> {
            let cached = self.meta_for_qid(qid);
            let t_cached = cached.meta.modified()?;
            let current = fs::metadata(&cached.path)?;
            let t_current = current.modified()?;

            Ok(t_current != t_cached)
        };

        match modified_since_cache(qid) {
            Ok(opt) => Ok(opt),
            Err(e) => {
                self.ft.remove(qid);
                Err(e.to_string())
            }
        }
    }

    // TODO: re-run perm checks if perms are now different
    fn try_sync_dir(&self, qid: u64) -> Result<()> {
        let path = self.meta_for_qid(qid).path;
        let mut on_disk = self.on_disk_entries_for(&path).map_err(|e| e.to_string())?;

        // Update known cache entries and prune entries that are no longer present on disk
        for stat in self.ft.read_dir(qid)?.iter() {
            let qid = stat.qid.path;

            match on_disk.remove(&stat.name) {
                Some(pm) => {
                    _ = self.ft.with_file_mut(qid, |f| {
                        let (owner, group) = owner_and_group_from_meta(&pm.meta);
                        f.stat.owner = owner;
                        f.stat.group = group;
                        f.stat.n_bytes = pm.meta.len();
                        f.stat.perms = perms_from_meta(&pm.meta);
                        f.stat.last_accessed = Timestamp::from_second(pm.meta.atime()).unwrap();
                        f.stat.last_modified = Timestamp::from_second(pm.meta.mtime()).unwrap();
                        f.aux = pm;
                    })
                }
                None => self.ft.remove(qid),
            }
        }

        // Anything remaining in on_disk is something new so insert it
        for (name, pm) in on_disk.into_iter() {
            let ty = if pm.meta.is_dir() {
                FileType::DIRECTORY
            } else {
                FileType::FILE
            };
            let perms = perms_from_meta(&pm.meta);
            let meta = pm.meta.clone();
            let qid = self.ft.try_add_node(qid, &name, perms, ty, pm)?;
            self.try_sync_file_with_meta(qid.path, meta)?;
        }

        Ok(())
    }

    fn try_sync_file(&self, qid: u64) -> Result<()> {
        let path = self.path_for_qid(qid);
        let meta = self
            .meta_if_under_root(&path)
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())?;

        self.try_sync_file_with_meta(qid, meta)
    }

    // TODO: re-run perm checks if perms are now different (will require Mode)
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
            f.aux = PathMeta { path, meta };
        })
    }

    fn on_disk_entries_for(&self, dir_path: &Path) -> io::Result<BTreeMap<String, PathMeta>> {
        let mut m = BTreeMap::new();

        for entry in fs::read_dir(dir_path)? {
            let entry = entry?;
            let name = entry.file_name().to_string_lossy().to_string();
            let path = entry.path();

            // Only include entries that resolve to being under our root
            if let Some(meta) = self.meta_if_under_root(&path) {
                m.insert(name, PathMeta { path, meta });
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
    fn open(&self, _cid: ClientId, qid: u64, _mode: Mode, _uname: &str) -> Result<IoUnit> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        Ok(self.iounit)
    }

    fn walk_one(&self, _cid: ClientId, parent_qid: u64, child: &str, _uname: &str) -> Result<Qid> {
        if self.modified_since_cache_or_prune(parent_qid)? {
            self.try_sync_dir(parent_qid)?;
        }

        self.ft.walk_one(parent_qid, child)
    }

    fn read(
        &self,
        _cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        _uname: &str,
    ) -> Result<ReadOutcome> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        let path = self.path_for_qid(qid);

        let mut f = OpenOptions::new()
            .read(true)
            .open(&path)
            .map_err(|e| e.to_string())?;
        f.seek(SeekFrom::Start(offset as u64))
            .map_err(|e| e.to_string())?;

        let mut buf = vec![0; count];
        let n = f.read(&mut buf).map_err(|e| e.to_string())?;
        buf.truncate(n);

        Ok(ReadOutcome::Immediate(buf))
    }

    fn read_dir(&self, _cid: ClientId, qid: u64, _uname: &str) -> Result<Vec<Stat>> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_dir(qid)?;
        }

        self.ft.read_dir(qid)
    }

    fn write(
        &self,
        _cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        _uname: &str,
    ) -> Result<usize> {
        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        let path = self.path_for_qid(qid);

        let mut f = OpenOptions::new()
            .write(true)
            .open(&path)
            .map_err(|e| e.to_string())?;
        f.seek(SeekFrom::Start(offset as u64))
            .map_err(|e| e.to_string())?;

        f.write(data.as_slice()).map_err(|e| e.to_string())
    }

    fn stat(&self, _cid: ClientId, qid: u64, _uname: &str) -> Result<Stat> {
        if self.modified_since_cache_or_prune(qid)? {
            let pm = self.meta_for_qid(qid);
            if pm.meta.is_dir() {
                self.try_sync_dir(qid)?;
            } else {
                self.try_sync_file(qid)?;
            }
        }

        self.ft.stat(qid)
    }

    fn write_stat(&self, _cid: ClientId, qid: u64, wstat: WStat, _uname: &str) -> Result<()> {
        if wstat.last_accessed.is_some()
            || wstat.last_modified.is_some()
            || wstat.group.is_some()
            || wstat.last_modified_by.is_some()
            || qid == 0
        {
            return Err(E_PERMISSION_DENIED.to_string());
        }

        let pm = self.meta_for_qid(qid);
        if pm.meta.is_dir() {
            self.try_sync_dir(qid)?;
        } else {
            self.try_sync_file(qid)?;
        }

        if let Some(perms) = wstat.perms {
            let mode = perms.bits() & 0o777;
            let mut permissions = fs::metadata(&pm.path)
                .map_err(|e| e.to_string())?
                .permissions();
            permissions.set_mode(mode);
            fs::set_permissions(&pm.path, permissions).map_err(|e| e.to_string())?;
        }

        if let Some(size) = wstat.n_bytes {
            if pm.meta.is_dir() {
                return Err(E_PERMISSION_DENIED.to_string());
            }

            let f = OpenOptions::new()
                .write(true)
                .open(&pm.path)
                .map_err(|e| e.to_string())?;
            f.set_len(size).map_err(|e| e.to_string())?;
        }

        if let Some(name) = wstat.name {
            let parent = pm.path.parent().unwrap();
            let new_path = parent.join(&name);
            fs::rename(&pm.path, &new_path).map_err(|e| e.to_string())?;

            self.try_sync_dir(self.parent_qid(qid))?;
        }

        Ok(())
    }

    fn remove(&self, _cid: ClientId, qid: u64, _uname: &str) -> Result<()> {
        if qid == 0 {
            return Err(E_PERMISSION_DENIED.to_string());
        }

        if self.modified_since_cache_or_prune(qid)? {
            self.try_sync_file(qid)?;
        }

        let is_dir = self.ft.with_file(qid, |f| f.aux.meta.is_dir())?;
        let path = self.path_for_qid(qid);

        if is_dir {
            fs::remove_dir(&path).map_err(|e| e.to_string())?;
        } else {
            fs::remove_file(&path).map_err(|e| e.to_string())?;
        }

        self.ft.remove(qid);

        Ok(())
    }

    fn create(
        &self,
        _cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        _mode: Mode,
        _uname: &str,
    ) -> Result<(Qid, IoUnit)> {
        if self.modified_since_cache_or_prune(parent)? {
            self.try_sync_dir(parent)?;
        }

        let parent_path = self.path_for_qid(parent);
        let path = parent_path.join(name);
        if path.exists() {
            return Err("file already exists".to_string());
        }

        let is_dir = perm.contains(Perm::DIRECTORY);
        if is_dir {
            fs::create_dir(&path).map_err(|e| e.to_string())?;
        } else {
            OpenOptions::new()
                .create_new(true)
                .write(true)
                .open(&path)
                .map_err(|e| e.to_string())?;
        }

        let mode_bits = perm.bits() & 0o777;
        let mut permissions = fs::metadata(&path)
            .map_err(|e| e.to_string())?
            .permissions();
        permissions.set_mode(mode_bits);
        fs::set_permissions(&path, permissions).map_err(|e| e.to_string())?;

        // Sync to allow our normal logic to pick up the new file details and create the qid
        self.try_sync_dir(parent)?;

        let qid = self.ft.walk_one(parent, name)?;

        Ok((qid, self.iounit))
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

#[cfg(test)]
mod tests {
    use super::*;
    use assert_fs::TempDir;
}
