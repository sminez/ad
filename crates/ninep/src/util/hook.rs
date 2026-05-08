//! A proxy filesystem that runs a user provided hook function before each operation.
use crate::{
    Result,
    fs::{IoUnit, Mode, Perm, Qid, Stat, WStat},
    sync::server::{ClientId, ReadOutcome, Serve9p},
};
use std::{fmt, sync::Arc};

type Hook = Arc<dyn for<'a> Fn(FsOp<'a>) -> Result<()> + Send + Sync>;

/// A filesystem proxy that runs a user provided hook before each operation before deferring to an
/// inner filesystem implementation.
#[derive(Clone)]
pub struct HookFs<T>
where
    T: Serve9p,
{
    inner: T,
    hook: Hook,
}

impl<T> HookFs<T>
where
    T: Serve9p,
{
    /// Create a new [HookFs] with the provided hook function.
    pub fn new<F>(inner: T, hook: F) -> Self
    where
        F: for<'a> Fn(FsOp<'a>) -> Result<()> + Send + Sync + 'static,
    {
        Self {
            inner,
            hook: Arc::new(hook),
        }
    }
}

impl<T> fmt::Debug for HookFs<T>
where
    T: Serve9p + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HookFs")
            .field("inner", &self.inner)
            .finish_non_exhaustive()
    }
}

impl<T> Serve9p for HookFs<T>
where
    T: Serve9p,
{
    fn user_is_in_group(&self, uname: &str, group: &str) -> bool {
        self.inner.user_is_in_group(uname, group)
    }

    fn open(&self, qid: u64, mode: Mode, cid: ClientId) -> Result<IoUnit> {
        (self.hook)(FsOp::open(cid, qid, mode))?;

        self.inner.open(qid, mode, cid)
    }

    fn walk_one(&self, parent_qid: u64, child: &str, cid: ClientId) -> Result<Qid> {
        (self.hook)(FsOp::walk_one(cid, parent_qid, child))?;

        self.inner.walk_one(parent_qid, child, cid)
    }

    fn read(&self, qid: u64, offset: usize, count: usize, cid: ClientId) -> Result<ReadOutcome> {
        (self.hook)(FsOp::read(cid, qid, offset, count))?;

        self.inner.read(qid, offset, count, cid)
    }

    fn read_dir(&self, qid: u64, cid: ClientId) -> Result<Vec<Stat>> {
        (self.hook)(FsOp::read_dir(cid, qid))?;

        self.inner.read_dir(qid, cid)
    }

    fn write(&self, qid: u64, offset: usize, data: Vec<u8>, cid: ClientId) -> Result<usize> {
        (self.hook)(FsOp::write(cid, qid, offset, data.len()))?;

        self.inner.write(qid, offset, data, cid)
    }

    fn stat(&self, qid: u64, cid: ClientId) -> Result<Stat> {
        (self.hook)(FsOp::stat(cid, qid))?;

        self.inner.stat(qid, cid)
    }

    fn write_stat(&self, qid: u64, wstat: WStat, cid: ClientId) -> Result<()> {
        (self.hook)(FsOp::write_stat(cid, qid, &wstat))?;

        self.inner.write_stat(qid, wstat, cid)
    }

    fn remove(&self, qid: u64, cid: ClientId) -> Result<()> {
        (self.hook)(FsOp::remove(cid, qid))?;

        self.inner.remove(qid, cid)
    }

    fn create(
        &self,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        cid: ClientId,
    ) -> Result<(Qid, IoUnit)> {
        (self.hook)(FsOp::create(cid, parent, name, perm, mode))?;

        self.inner.create(parent, name, perm, mode, cid)
    }

    fn clunk(&self, qid: u64, cid: ClientId) {
        _ = (self.hook)(FsOp::clunk(cid, qid));

        self.inner.clunk(qid, cid)
    }

    fn flush(&self, old_tag: u16, cid: ClientId) {
        _ = (self.hook)(FsOp::flush(cid, old_tag));

        self.inner.flush(old_tag, cid)
    }
}

/// Events emitted for each filesystem operation.
#[expect(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FsOp<'a> {
    WalkOne {
        cid: ClientId,
        parent_qid: u64,
        child: &'a str,
    },
    Open {
        cid: ClientId,
        qid: u64,
        mode: Mode,
    },
    Read {
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
    },
    ReadDir {
        cid: ClientId,
        qid: u64,
    },
    Write {
        cid: ClientId,
        qid: u64,
        offset: usize,
        n_bytes: usize,
    },
    Stat {
        cid: ClientId,
        qid: u64,
    },
    WriteStat {
        cid: ClientId,
        qid: u64,
        wstat: &'a WStat,
    },
    Remove {
        cid: ClientId,
        qid: u64,
    },
    Create {
        cid: ClientId,
        parent: u64,
        name: &'a str,
        perm: Perm,
        mode: Mode,
    },
    Clunk {
        cid: ClientId,
        qid: u64,
    },
    Flush {
        cid: ClientId,
        old_tag: u16,
    },
}

impl<'a> FsOp<'a> {
    fn walk_one(cid: ClientId, parent_qid: u64, child: &'a str) -> Self {
        Self::WalkOne {
            cid,
            parent_qid,
            child,
        }
    }

    fn open(cid: ClientId, qid: u64, mode: Mode) -> Self {
        Self::Open { cid, qid, mode }
    }

    fn read(cid: ClientId, qid: u64, offset: usize, count: usize) -> Self {
        Self::Read {
            cid,
            qid,
            offset,
            count,
        }
    }

    fn read_dir(cid: ClientId, qid: u64) -> Self {
        Self::ReadDir { cid, qid }
    }

    fn write(cid: ClientId, qid: u64, offset: usize, n_bytes: usize) -> Self {
        Self::Write {
            cid,
            qid,
            offset,
            n_bytes,
        }
    }

    fn stat(cid: ClientId, qid: u64) -> Self {
        Self::Stat { cid, qid }
    }

    fn write_stat(cid: ClientId, qid: u64, wstat: &'a WStat) -> Self {
        Self::WriteStat { cid, qid, wstat }
    }

    fn remove(cid: ClientId, qid: u64) -> Self {
        Self::Remove { cid, qid }
    }

    fn create(cid: ClientId, parent: u64, name: &'a str, perm: Perm, mode: Mode) -> Self {
        Self::Create {
            cid,
            parent,
            name,
            perm,
            mode,
        }
    }

    fn clunk(cid: ClientId, qid: u64) -> Self {
        Self::Clunk { cid, qid }
    }

    fn flush(cid: ClientId, old_tag: u16) -> Self {
        Self::Flush { cid, old_tag }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fs::FileType, sansio::server::E_UNKNOWN_FILE, util::ram::RamFs};
    use simple_test_case::test_case;

    const CID: ClientId = ClientId(0);
    const HOOK_ERR: &str = "hook failed";

    fn hook_result(is_ok: bool) -> Result<()> {
        if is_ok {
            Ok(())
        } else {
            Err(HOOK_ERR.to_string())
        }
    }

    fn read_all(fs: &RamFs, qid: u64) -> Vec<u8> {
        match fs.read(qid, 0, 1024, CID).unwrap() {
            ReadOutcome::Immediate(data) => data,
            ReadOutcome::Blocked(_) => panic!("RamFs should return immediate read data"),
        }
    }

    fn add_file(fs: &RamFs, parent: u64, name: &str, data: &[u8]) -> Qid {
        fs.file_tree()
            .try_add_node(
                parent,
                name,
                Perm::any_read() | Perm::any_write(),
                FileType::FILE,
                data.to_vec(),
            )
            .unwrap()
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn open_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"");
        let target_qid = if hook_ok { qid.path } else { 99 };

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::open(CID, target_qid, Mode::READ));
            hook_result(hook_ok)
        });

        let res = fs.open(target_qid, Mode::READ, CID);
        if hook_ok {
            assert!(res.is_ok());
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn walk_one_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "child", b"");
        let parent_qid = if hook_ok { 0 } else { 99 };

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::walk_one(CID, parent_qid, "child"));
            hook_result(hook_ok)
        });

        let res = fs.walk_one(parent_qid, "child", CID);
        if hook_ok {
            assert_eq!(res.unwrap(), qid);
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn read_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"abcdef");
        let target_qid = if hook_ok { qid.path } else { 99 };

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::read(CID, target_qid, 1, 3));
            hook_result(hook_ok)
        });

        let res = fs.read(target_qid, 1, 3, CID);
        if hook_ok {
            match res.unwrap() {
                ReadOutcome::Immediate(data) => assert_eq!(data, b"bcd".to_vec()),
                ReadOutcome::Blocked(_) => panic!("RamFs should return immediate read data"),
            }
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn read_dir_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let _ = add_file(&inner, 0, "entry", b"");
        let target_qid = if hook_ok { 0 } else { 99 };

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::read_dir(CID, target_qid));
            hook_result(hook_ok)
        });

        let res = fs.read_dir(target_qid, CID);
        if hook_ok {
            let entries = res.unwrap();
            assert_eq!(entries.len(), 1);
            assert_eq!(entries[0].name, "entry");
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn write_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"abc");
        let fs = HookFs::new(inner.clone(), move |op| {
            assert_eq!(op, FsOp::write(CID, qid.path, 1, 2));
            hook_result(hook_ok)
        });

        let res = fs.write(qid.path, 1, b"ZZ".to_vec(), CID);
        if hook_ok {
            assert_eq!(res.unwrap(), 2);
            assert_eq!(read_all(&inner, qid.path), b"aZZ".to_vec());
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
            assert_eq!(read_all(&inner, qid.path), b"abc".to_vec());
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn stat_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"");
        let target_qid = if hook_ok { qid.path } else { 99 };

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::stat(CID, target_qid));
            hook_result(hook_ok)
        });

        let res = fs.stat(target_qid, CID);
        if hook_ok {
            assert_eq!(res.unwrap().name, "f");
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn write_stat_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"");
        let wstat = WStat {
            qid,
            name: Some("renamed".into()),
            ..Default::default()
        };
        let expected_wstat = wstat.clone();

        let fs = HookFs::new(inner.clone(), move |op| {
            assert_eq!(op, FsOp::write_stat(CID, qid.path, &expected_wstat));
            hook_result(hook_ok)
        });

        let res = fs.write_stat(qid.path, wstat, CID);
        if hook_ok {
            assert!(res.is_ok());
            assert_eq!(inner.stat(qid.path, CID).unwrap().name, "renamed");
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
            assert_eq!(inner.stat(qid.path, CID).unwrap().name, "f");
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn remove_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"");
        let fs = HookFs::new(inner.clone(), move |op| {
            assert_eq!(op, FsOp::remove(CID, qid.path));
            hook_result(hook_ok)
        });

        let res = fs.remove(qid.path, CID);
        if hook_ok {
            assert!(res.is_ok());
            assert_eq!(inner.stat(qid.path, CID).unwrap_err(), E_UNKNOWN_FILE);
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
            assert_eq!(inner.stat(qid.path, CID).unwrap().name, "f");
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn create_hook_controls_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let fs = HookFs::new(inner.clone(), move |op| {
            assert_eq!(
                op,
                FsOp::create(CID, 0, "new-file", Perm::OWNER_READ, Mode::READ)
            );
            hook_result(hook_ok)
        });

        let res = fs.create(0, "new-file", Perm::OWNER_READ, Mode::READ, CID);
        if hook_ok {
            let (qid, _) = res.unwrap();
            assert_eq!(inner.walk_one(0, "new-file", CID).unwrap().path, qid.path);
        } else {
            assert_eq!(res.unwrap_err(), HOOK_ERR);
            assert_eq!(
                inner.walk_one(0, "new-file", CID).unwrap_err(),
                E_UNKNOWN_FILE
            );
        }
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn clunk_hook_doesnt_affect_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");
        let qid = add_file(&inner, 0, "f", b"");

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::clunk(CID, qid.path));
            hook_result(hook_ok)
        });

        fs.clunk(qid.path, CID);
    }

    #[test_case(true; "hook ok")]
    #[test_case(false; "hook err")]
    #[test]
    fn flush_hook_doesnt_affect_execution(hook_ok: bool) {
        let inner = RamFs::new("user", "group");

        let fs = HookFs::new(inner, move |op| {
            assert_eq!(op, FsOp::flush(CID, 42));
            hook_result(hook_ok)
        });

        fs.flush(42, CID);
    }
}
