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
    fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        (self.hook)(FsOp::Open {
            cid,
            qid,
            mode,
            uname,
        })?;

        self.inner.open(cid, qid, mode, uname)
    }

    fn walk_one(&self, cid: ClientId, parent_qid: u64, child: &str, uname: &str) -> Result<Qid> {
        (self.hook)(FsOp::WalkOne {
            cid,
            parent_qid,
            child,
            uname,
        })?;

        self.inner.walk_one(cid, parent_qid, child, uname)
    }

    fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome> {
        (self.hook)(FsOp::Read {
            cid,
            qid,
            offset,
            count,
            uname,
        })?;

        self.inner.read(cid, qid, offset, count, uname)
    }

    fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        (self.hook)(FsOp::ReadDir { cid, qid, uname })?;

        self.inner.read_dir(cid, qid, uname)
    }

    fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize> {
        (self.hook)(FsOp::Write {
            cid,
            qid,
            offset,
            n_bytes: data.len(),
            uname,
        })?;

        self.inner.write(cid, qid, offset, data, uname)
    }

    fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat> {
        (self.hook)(FsOp::Stat { cid, qid, uname })?;

        self.inner.stat(cid, qid, uname)
    }

    fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Result<()> {
        (self.hook)(FsOp::WriteStat {
            cid,
            qid,
            wstat: &wstat,
            uname,
        })?;

        self.inner.write_stat(cid, qid, wstat, uname)
    }

    fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()> {
        (self.hook)(FsOp::Remove { cid, qid, uname })?;

        self.inner.remove(cid, qid, uname)
    }

    fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(Qid, IoUnit)> {
        (self.hook)(FsOp::Create {
            cid,
            parent,
            name,
            perm,
            mode,
            uname,
        })?;

        self.inner.create(cid, parent, name, perm, mode, uname)
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
        uname: &'a str,
    },
    Open {
        cid: ClientId,
        qid: u64,
        mode: Mode,
        uname: &'a str,
    },
    Read {
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &'a str,
    },
    ReadDir {
        cid: ClientId,
        qid: u64,
        uname: &'a str,
    },
    Write {
        cid: ClientId,
        qid: u64,
        offset: usize,
        n_bytes: usize,
        uname: &'a str,
    },
    Stat {
        cid: ClientId,
        qid: u64,
        uname: &'a str,
    },
    WriteStat {
        cid: ClientId,
        qid: u64,
        wstat: &'a WStat,
        uname: &'a str,
    },
    Remove {
        cid: ClientId,
        qid: u64,
        uname: &'a str,
    },
    Create {
        cid: ClientId,
        parent: u64,
        name: &'a str,
        perm: Perm,
        mode: Mode,
        uname: &'a str,
    },
}
