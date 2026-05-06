//! A simple in-memory filesystem implementation.
use crate::{
    Result,
    fs::{FileTree, FileType, IoUnit, Mode, Perm, Qid, Stat, WStat},
    sansio::server::{E_PERMISSION_DENIED, E_UNKNOWN_FILE},
    sync::server::{ClientId, ReadOutcome, Serve9p},
};
use std::cmp::min;

const DEFAULT_IOUNIT: IoUnit = 8168;

/// A simple in-memory file server implementation.
#[derive(Debug, Clone)]
pub struct RamFs {
    ft: FileTree<Vec<u8>>,
    iounit: IoUnit,
}

impl RamFs {
    /// Construct a [RamFs] with default [permissions][Perm] and [IoUnit].
    pub fn new(owner: &str, group: &str) -> Self {
        Self::new_with_base_perms_and_iounit(
            owner,
            group,
            Perm::any_read() | Perm::any_write() | Perm::any_exec(),
            DEFAULT_IOUNIT,
        )
    }

    /// Construct a [RamFs] with the provided [permissions][Perm] and [IoUnit].
    pub fn new_with_base_perms_and_iounit(
        owner: &str,
        group: &str,
        perms: Perm,
        iounit: IoUnit,
    ) -> Self {
        RamFs {
            ft: FileTree::new(owner, group, perms, Vec::new()),
            iounit,
        }
    }

    /// Obtain a copy of the [FileTree] within this RamFs.
    ///
    /// [FileTree] internally stores its data within an `Arc`, so this can be used to interact with
    /// the tree directly.
    pub fn file_tree(&self) -> FileTree<Vec<u8>> {
        self.ft.clone()
    }
}

impl Serve9p for RamFs {
    fn open(&self, qid: u64, _mode: Mode, _cid: ClientId) -> Result<IoUnit> {
        if self.ft.contains_qid(qid) {
            Ok(self.iounit)
        } else {
            Err(E_UNKNOWN_FILE.to_string())
        }
    }

    fn walk_one(&self, parent_qid: u64, child: &str, _cid: ClientId) -> Result<Qid> {
        self.ft.walk_one(parent_qid, child)
    }

    fn read(&self, qid: u64, offset: usize, count: usize, _cid: ClientId) -> Result<ReadOutcome> {
        let data: Vec<u8> = self.ft.with_file(qid, |f| {
            if offset > f.aux.len() {
                Vec::new()
            } else {
                let to = min(offset + count, f.aux.len());
                f.aux[offset..to].to_vec()
            }
        })?;

        Ok(ReadOutcome::Immediate(data))
    }

    fn read_dir(&self, qid: u64, _cid: ClientId) -> Result<Vec<Stat>> {
        self.ft.read_dir(qid)
    }

    fn write(&self, qid: u64, offset: usize, data: Vec<u8>, _cid: ClientId) -> Result<usize> {
        self.ft.with_file_mut(qid, |f| {
            if offset > f.aux.len() {
                return Err("offset beyond end of file".to_string());
            }

            let n = data.len();
            if offset + data.len() > f.aux.len() {
                f.aux.resize(offset + data.len(), 0);
            }
            f.aux[offset..offset + data.len()].copy_from_slice(data.as_slice());

            Ok(n)
        })?
    }

    fn stat(&self, qid: u64, _cid: ClientId) -> Result<Stat> {
        self.ft.stat(qid)
    }

    fn write_stat(&self, qid: u64, wstat: WStat, _cid: ClientId) -> Result<()> {
        self.ft
            .with_file_mut(qid, |f| f.try_apply_wstat(wstat))?
            .map_err(|_| E_PERMISSION_DENIED.to_string())
    }

    fn remove(&self, qid: u64, _cid: ClientId) -> Result<()> {
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
        let ty = FileType::from(perm);
        let qid = self.ft.try_add_node(parent, name, perm, ty, Vec::new())?;

        Ok((qid, self.iounit))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sansio::server::E_ILLEGAL_CREATE_NAME;
    use simple_test_case::test_case;

    const CID: ClientId = ClientId(0);

    fn add_file(fs: &RamFs, parent: u64, name: &str, data: &[u8]) -> Qid {
        fs.file_tree()
            .try_add_node(
                parent,
                name,
                Perm::any_read(),
                FileType::FILE,
                data.to_vec(),
            )
            .unwrap()
    }

    #[test]
    fn open_works() {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"");

        assert_eq!(fs.open(qid.path, Mode::READ, CID).unwrap(), DEFAULT_IOUNIT);
        assert_eq!(fs.open(42, Mode::READ, CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn walk_one_works() {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"");

        assert_eq!(fs.walk_one(0, "test", CID).unwrap(), qid);
    }

    #[test_case(99, "test"; "unknown parent")]
    #[test_case(0, "missing"; "unknown child")]
    #[test]
    fn walk_one_unknown_returns_error(parent_qid: u64, child: &str) {
        let fs = RamFs::new("user", "group");
        let _ = add_file(&fs, 0, "test", b"");

        assert_eq!(
            fs.walk_one(parent_qid, child, CID).unwrap_err(),
            E_UNKNOWN_FILE
        );
    }

    #[test_case(0, 3, b"abc".to_vec(); "from start")]
    #[test_case(2, 2, b"cd".to_vec(); "middle slice")]
    #[test_case(6, 5, b"".to_vec(); "offset at end")]
    #[test_case(100, 5, b"".to_vec(); "offset beyond end")]
    #[test]
    fn read_returns_expected_data(offset: usize, count: usize, expected: Vec<u8>) {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"abcdef");

        match fs.read(qid.path, offset, count, CID).unwrap() {
            ReadOutcome::Immediate(data) => assert_eq!(data, expected),
            ReadOutcome::Blocked(_) => panic!("RamFs should always return immediate read data"),
        }
    }

    #[test]
    fn read_unknown_returns_error() {
        let fs = RamFs::new("user", "group");

        assert_eq!(fs.read(1, 0, 1, CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn read_dir_returns_children_for_directory() {
        let fs = RamFs::new("user", "group");
        let _ = add_file(&fs, 0, "root-file", b"");
        let dir = fs
            .file_tree()
            .try_add_node(0, "dir", Perm::DIRECTORY, FileType::DIRECTORY, Vec::new())
            .unwrap();
        let _ = add_file(&fs, dir.path, "nested", b"");

        let mut root_children = fs
            .read_dir(0, CID)
            .unwrap()
            .into_iter()
            .map(|s| s.name)
            .collect::<Vec<_>>();
        root_children.sort();

        assert_eq!(
            root_children,
            vec!["dir".to_string(), "root-file".to_string()]
        );

        let nested_children = fs.read_dir(dir.path, CID).unwrap();
        assert_eq!(nested_children.len(), 1);
        assert_eq!(nested_children[0].name, "nested");
    }

    #[test]
    fn read_dir_unknown_returns_error() {
        let fs = RamFs::new("user", "group");

        assert_eq!(fs.read_dir(1, CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test_case(1, b"ZZ".to_vec(), b"aZZ".to_vec(); "overwrite existing bytes")]
    #[test_case(3, b"X".to_vec(), b"abcX".to_vec(); "append at end")]
    #[test_case(0, b"".to_vec(), b"abc".to_vec(); "empty write")]
    #[test]
    fn write_updates_content(offset: usize, payload: Vec<u8>, expected: Vec<u8>) {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"abc");

        let n = fs.write(qid.path, offset, payload.clone(), CID).unwrap();
        assert_eq!(n, payload.len());

        let data = fs
            .file_tree()
            .with_file(qid.path, |f| f.aux.clone())
            .unwrap();
        assert_eq!(data, expected);
    }

    #[test]
    fn write_offset_past_end_returns_error() {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"abc");

        assert_eq!(
            fs.write(qid.path, 5, b"X".to_vec(), CID).unwrap_err(),
            "offset beyond end of file"
        );

        let data = fs
            .file_tree()
            .with_file(qid.path, |f| f.aux.clone())
            .unwrap();
        assert_eq!(data, b"abc".to_vec());
    }

    #[test]
    fn write_unknown_returns_error() {
        let fs = RamFs::new("user", "group");

        assert_eq!(fs.write(1, 0, vec![1], CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn stat_works() {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"");

        let stat = fs.stat(qid.path, CID).unwrap();
        assert_eq!(stat.qid, qid);
        assert_eq!(stat.name, "test");
    }

    #[test]
    fn stat_unknown_returns_error() {
        let fs = RamFs::new("user", "group");

        assert_eq!(fs.stat(1, CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn write_stat_updates_file_metadata() {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"");

        let wstat = WStat {
            qid,
            name: Some("renamed".into()),
            perms: Some(Perm::OWNER_READ),
            n_bytes: Some(123),
            ..Default::default()
        };

        fs.write_stat(qid.path, wstat, CID).unwrap();

        let stat = fs.stat(qid.path, CID).unwrap();
        assert_eq!(stat.name, "renamed");
        assert_eq!(stat.perms, Perm::OWNER_READ);
        assert_eq!(stat.n_bytes, 123);
    }

    #[test]
    fn write_stat_mismatched_qid_returns_permission_denied() {
        let fs = RamFs::new("user", "group");
        let qid = add_file(&fs, 0, "test", b"");

        let wstat = WStat {
            qid: Qid::file(qid.path + 1),
            ..Default::default()
        };

        assert_eq!(
            fs.write_stat(qid.path, wstat, CID).unwrap_err(),
            E_PERMISSION_DENIED
        );
    }

    #[test]
    fn write_stat_unknown_returns_error() {
        let fs = RamFs::new("user", "group");
        let wstat = WStat {
            qid: Qid::file(1),
            ..Default::default()
        };

        assert_eq!(fs.write_stat(1, wstat, CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn remove_prunes_subtree() {
        let fs = RamFs::new("user", "group");
        let dir = fs
            .file_tree()
            .try_add_node(0, "dir", Perm::DIRECTORY, FileType::DIRECTORY, Vec::new())
            .unwrap();
        let child = add_file(&fs, dir.path, "nested", b"");

        fs.remove(dir.path, CID).unwrap();

        assert_eq!(fs.stat(dir.path, CID).unwrap_err(), E_UNKNOWN_FILE);
        assert_eq!(fs.stat(child.path, CID).unwrap_err(), E_UNKNOWN_FILE);
    }

    #[test]
    fn remove_unknown_is_ok() {
        let fs = RamFs::new("user", "group");

        assert!(fs.remove(1, CID).is_ok());
    }

    #[test_case(Perm::OWNER_READ, FileType::FILE, "file"; "file create")]
    #[test_case(Perm::DIRECTORY | Perm::OWNER_READ, FileType::DIRECTORY, "dir"; "directory create")]
    #[test]
    fn create_adds_new_node(perm: Perm, expected_ty: FileType, name: &str) {
        let fs = RamFs::new_with_base_perms_and_iounit(
            "user",
            "group",
            Perm::any_read() | Perm::any_write() | Perm::any_exec(),
            999,
        );

        let (qid, iounit) = fs.create(0, name, perm, Mode::READ, CID).unwrap();
        assert_eq!(iounit, 999);
        assert_eq!(qid.ty, expected_ty);
        assert_eq!(fs.walk_one(0, name, CID).unwrap().path, qid.path);
    }

    #[test]
    fn create_unknown_parent_returns_error() {
        let fs = RamFs::new("user", "group");

        assert_eq!(
            fs.create(999, "test", Perm::OWNER_READ, Mode::READ, CID)
                .unwrap_err(),
            E_UNKNOWN_FILE
        );
    }

    #[test_case("."; "single dot")]
    #[test_case(".."; "double dot")]
    #[test]
    fn create_rejects_illegal_names(name: &str) {
        let fs = RamFs::new("user", "group");

        assert_eq!(
            fs.create(0, name, Perm::OWNER_READ, Mode::READ, CID)
                .unwrap_err(),
            E_ILLEGAL_CREATE_NAME
        );
    }

    #[test]
    fn create_rejects_duplicate_names() {
        let fs = RamFs::new("user", "group");
        fs.create(0, "dup", Perm::OWNER_READ, Mode::READ, CID)
            .unwrap();

        assert_eq!(
            fs.create(0, "dup", Perm::OWNER_READ, Mode::READ, CID)
                .unwrap_err(),
            "file already exists"
        );
    }
}
