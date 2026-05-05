//! A simple in-memory filesystem implementation.
use crate::{
    Result,
    fs::{FileTree, FileType, IoUnit, Mode, Perm, Qid, Stat, WStat},
    sansio::server::E_PERMISSION_DENIED,
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
}

impl Serve9p for RamFs {
    fn open(&self, _cid: ClientId, _qid: u64, _mode: Mode, _uname: &str) -> Result<IoUnit> {
        Ok(self.iounit)
    }

    fn walk_one(&self, _cid: ClientId, parent_qid: u64, child: &str, _uname: &str) -> Result<Qid> {
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

    fn read_dir(&self, _cid: ClientId, qid: u64, _uname: &str) -> Result<Vec<Stat>> {
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
        self.ft.with_file_mut(qid, |f| {
            let n = data.len();
            if offset + data.len() > f.aux.len() {
                f.aux.resize(offset + data.len(), 0);
            }
            f.aux[offset..offset + data.len()].copy_from_slice(data.as_slice());

            n
        })
    }

    fn stat(&self, _cid: ClientId, qid: u64, _uname: &str) -> Result<Stat> {
        self.ft.stat(qid)
    }

    fn write_stat(&self, _cid: ClientId, qid: u64, wstat: WStat, _uname: &str) -> Result<()> {
        self.ft
            .with_file_mut(qid, |f| f.try_apply_wstat(wstat))?
            .map_err(|_| E_PERMISSION_DENIED.to_string())
    }

    fn remove(&self, _cid: ClientId, qid: u64, _uname: &str) -> Result<()> {
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
        let ty = FileType::from(perm);
        let qid = self.ft.try_add_node(parent, name, perm, ty, Vec::new())?;

        Ok((qid, self.iounit))
    }
}
