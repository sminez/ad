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
    fn open(&self, qid: u64, mode: Mode, cid: ClientId) -> Result<IoUnit> {
        (self.hook)(FsOp::Open { cid, qid, mode })?;

        self.inner.open(qid, mode, cid)
    }

    fn walk_one(&self, parent_qid: u64, child: &str, cid: ClientId) -> Result<Qid> {
        (self.hook)(FsOp::WalkOne {
            cid,
            parent_qid,
            child,
        })?;

        self.inner.walk_one(parent_qid, child, cid)
    }

    fn read(&self, qid: u64, offset: usize, count: usize, cid: ClientId) -> Result<ReadOutcome> {
        (self.hook)(FsOp::Read {
            cid,
            qid,
            offset,
            count,
        })?;

        self.inner.read(qid, offset, count, cid)
    }

    fn read_dir(&self, qid: u64, cid: ClientId) -> Result<Vec<Stat>> {
        (self.hook)(FsOp::ReadDir { cid, qid })?;

        self.inner.read_dir(qid, cid)
    }

    fn write(&self, qid: u64, offset: usize, data: Vec<u8>, cid: ClientId) -> Result<usize> {
        (self.hook)(FsOp::Write {
            cid,
            qid,
            offset,
            n_bytes: data.len(),
        })?;

        self.inner.write(qid, offset, data, cid)
    }

    fn stat(&self, qid: u64, cid: ClientId) -> Result<Stat> {
        (self.hook)(FsOp::Stat { cid, qid })?;

        self.inner.stat(qid, cid)
    }

    fn write_stat(&self, qid: u64, wstat: WStat, cid: ClientId) -> Result<()> {
        (self.hook)(FsOp::WriteStat {
            cid,
            qid,
            wstat: &wstat,
        })?;

        self.inner.write_stat(qid, wstat, cid)
    }

    fn remove(&self, qid: u64, cid: ClientId) -> Result<()> {
        (self.hook)(FsOp::Remove { cid, qid })?;

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
        (self.hook)(FsOp::Create {
            cid,
            parent,
            name,
            perm,
            mode,
        })?;

        self.inner.create(parent, name, perm, mode, cid)
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
}
