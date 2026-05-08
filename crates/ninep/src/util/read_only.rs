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
    fn user_is_in_group(&self, uname: &str, group: &str) -> bool {
        self.inner.user_is_in_group(uname, group)
    }

    fn open(&self, qid: u64, mode: Mode, cid: ClientId) -> Result<IoUnit> {
        self.inner.open(qid, mode, cid)
    }

    fn walk_one(&self, parent_qid: u64, child: &str, cid: ClientId) -> Result<Qid> {
        self.inner.walk_one(parent_qid, child, cid)
    }

    fn read(&self, qid: u64, offset: usize, count: usize, cid: ClientId) -> Result<ReadOutcome> {
        self.inner.read(qid, offset, count, cid)
    }

    fn read_dir(&self, qid: u64, cid: ClientId) -> Result<Vec<Stat>> {
        self.inner.read_dir(qid, cid)
    }

    fn write(&self, qid: u64, offset: usize, data: Vec<u8>, cid: ClientId) -> Result<usize> {
        self.inner.write(qid, offset, data, cid)
    }

    fn stat(&self, qid: u64, cid: ClientId) -> Result<Stat> {
        let mut stat = self.inner.stat(qid, cid)?;
        if self.read_only {
            stat.perms
                .remove(Perm::OWNER_WRITE | Perm::GROUP_WRITE | Perm::OTHER_WRITE);
        }

        Ok(stat)
    }

    fn write_stat(&self, qid: u64, wstat: WStat, cid: ClientId) -> Result<()> {
        self.inner.write_stat(qid, wstat, cid)
    }

    fn remove(&self, qid: u64, cid: ClientId) -> Result<()> {
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
        if self.read_only && mode.allows_write() {
            return Err(E_PERMISSION_DENIED.to_string());
        }

        self.inner.create(parent, name, perm, mode, cid)
    }

    fn clunk(&self, qid: u64, cid: ClientId) {
        self.inner.clunk(qid, cid)
    }

    fn flush(&self, old_tag: u16, cid: ClientId) {
        self.inner.flush(old_tag, cid)
    }
}
