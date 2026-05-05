//! A proxy filesystem that supports toggling on a read-only mode that auto-denies mutating
//! operations.
use crate::{
    Result,
    fs::{IoUnit, Mode, Perm, Qid, Stat, WStat},
    sansio::server::E_PERMISSION_DENIED,
    sync::server::{ClientId, ReadOutcome, Serve9p},
};

/// A proxy filesystem that supports toggling on a read-only mode that auto-denies mutating
/// operations.
#[derive(Debug, Clone)]
pub struct ReadOnlyFs<T>
where
    T: Serve9p,
{
    inner: T,
    read_only: bool,
}

impl<T> ReadOnlyFs<T>
where
    T: Serve9p,
{
    /// Create a new [ReadOnlyFs] that starts in read-only mode.
    pub fn new<F>(inner: T) -> Self {
        Self {
            inner,
            read_only: true,
        }
    }

    /// Set read-only mode to be on or off.
    pub fn read_only(&mut self, read_only: bool) {
        self.read_only = read_only;
    }
}

impl<T> Serve9p for ReadOnlyFs<T>
where
    T: Serve9p,
{
    fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        self.inner.open(cid, qid, mode, uname)
    }

    fn walk_one(&self, cid: ClientId, parent_qid: u64, child: &str, uname: &str) -> Result<Qid> {
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
        self.inner.read(cid, qid, offset, count, uname)
    }

    fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>> {
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
        self.inner.write(cid, qid, offset, data, uname)
    }

    fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat> {
        let mut stat = self.inner.stat(cid, qid, uname)?;
        if self.read_only {
            stat.perms
                .remove(Perm::OWNER_WRITE | Perm::GROUP_WRITE | Perm::OTHER_WRITE);
        }

        Ok(stat)
    }

    fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Result<()> {
        self.inner.write_stat(cid, qid, wstat, uname)
    }

    fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()> {
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
        if self.read_only && mode.allows_write() {
            return Err(E_PERMISSION_DENIED.to_string());
        }

        self.inner.create(cid, parent, name, perm, mode, uname)
    }
}
