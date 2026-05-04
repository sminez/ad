use ninep::{
    Result,
    fs::{FileTree, FileType, IoUnit, Mode, Perm, Qid, Stat, WStat},
    sync::server::{ClientId, ReadOutcome, Serve9p, Server},
};
use std::{
    env::{args, current_dir},
    sync::atomic::{AtomicBool, Ordering},
};

static CHATTY: AtomicBool = AtomicBool::new(false);
const E_DENIED: &str = "permission denied";
const IOUNIT: IoUnit = 8168;

macro_rules! log {
    ($s:expr $(, $arg:expr)*) => {
        if CHATTY.load(Ordering::Relaxed) {
            println!($s $(, $arg)*);
        }
    };
}

fn main() {
    let chatty = args().nth(1).as_deref() == Some("--chatty");
    CHATTY.store(chatty, Ordering::Relaxed);

    let s = Server::new(RamFs::new());
    let socket_path = current_dir().unwrap().join("ramfs");

    log!("starting ram-fs file server at {}", socket_path.display());

    if s.serve_socket_with_custom_path(socket_path).join().is_err() {
        eprintln!("server thread died");
    }
}

struct RamFs {
    ft: FileTree<Vec<u8>>,
}

impl RamFs {
    fn new() -> Self {
        Self {
            ft: FileTree::new(
                "owner",
                "group",
                Perm::any_read() | Perm::any_write() | Perm::any_exec(),
                vec![],
            ),
        }
    }
}

impl Serve9p for RamFs {
    fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        log!("{cid:?} {uname} opening {qid} in mode {mode:?}");

        Ok(IOUNIT)
    }

    fn walk_one(&self, cid: ClientId, parent_qid: u64, child: &str, uname: &str) -> Result<Qid> {
        log!("{cid:?} {uname} walking from {parent_qid} to {child}");

        self.ft.walk_one(parent_qid, child)
    }

    fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome> {
        log!("{cid:?} {uname} reading {qid} (offset={offset} qid={qid})");

        let data: Vec<u8> = self.ft.with_file(qid, |f| {
            f.aux.iter().skip(offset).take(count).copied().collect()
        })?;

        Ok(ReadOutcome::Immediate(data))
    }

    fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        log!("{cid:?} {uname} reading directory {qid}");
        self.ft.read_dir(qid)
    }

    fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize> {
        let n = data.len();
        log!("{cid:?} {uname} writing to {qid} (offset={offset} n_bytes={n})");

        self.ft.with_file_mut(qid, |f| {
            if offset + data.len() > f.aux.len() {
                f.aux.resize(offset + data.len(), 0);
            }
            f.aux[offset..offset + data.len()].copy_from_slice(data.as_slice());
        })?;

        Ok(n)
    }

    fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat> {
        log!("{cid:?} {uname} statting {qid}");
        self.ft.stat(qid)
    }

    fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Result<()> {
        log!("{cid:?} {uname} updating sat for {qid}");
        self.ft
            .with_file_mut(qid, |f| f.try_apply_wstat(wstat))?
            .map_err(|_| E_DENIED.to_string())
    }

    fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()> {
        log!("{cid:?} {uname} removing {qid}");
        self.ft.remove(qid);

        Ok(())
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
        let ty = FileType::from(perm);
        log!(
            "{cid:?} {uname} creating {name} in {parent} (type={ty:?} mode={mode:?} perm={perm:?})"
        );

        let qid = self.ft.try_add_node(parent, name, perm, ty, Vec::new())?;

        Ok((qid, IOUNIT))
    }
}
