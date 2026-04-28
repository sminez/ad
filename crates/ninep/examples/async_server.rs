//! A simple demo of the 9p server interface
//!
//! You can use the `9p` command from https://github.com/9fans/plan9port to interact
//! with the server and test it out.
//!
//!   https://9fans.github.io/plan9port/man/man1/9p.html
//!
//! ```sh
//! # Let 9p know where to find the socket we have opened
//! $ export NAMESPACE="/tmp/ns.$USER.$DISPLAY"
//!
//! # List the contents of the filesystem and read the contents of a file
//! $ 9p ls ninep-server
//! $ 9p read ninep-server/foo
//!
//! # List the contents of a subdirectory and a file in that subdirectory
//! $ 9p ls ninep-server/bar
//! $ 9p read ninep-server/bar/baz
//!
//! # Read and then update the contents of a file
//! $ 9p read ninep-server/rw
//! $ echo "updated" | 9p write ninep-server/rw
//! $ 9p read ninep-server/rw
//! ```
use ninep::{
    Result,
    fs::{IoUnit, Mode, Perm, Qid, Stat, WStat},
    tokio::server::{AsyncServe9p, ClientId, ReadOutcome, Server},
};
use std::{
    sync::{Arc, RwLock},
    time::{Duration, SystemTime},
};
use tokio::{spawn, sync::mpsc::channel, time::sleep};

#[tokio::main]
async fn main() {
    let s = Server::new(EchoServer {
        state: Arc::new(RwLock::new(State {
            rw: "initial".to_string(),
            blocking: "0\n".to_string(),
            n: 0,
        })),
    });
    println!("starting server");
    _ = s.serve_socket_async("ninep-server").await;
}

struct State {
    rw: String,
    blocking: String,
    n: usize,
}

struct EchoServer {
    state: Arc<RwLock<State>>,
}

impl EchoServer {
    fn rw_size(&self) -> u64 {
        self.state.read().unwrap().rw.len() as u64
    }
}

const ROOT: u64 = 0;
const BAR: u64 = 1;
const FOO: u64 = 2;
const BAZ: u64 = 3;
const RW: u64 = 4;
const BLOCKING: u64 = 5;

fn dir_stat(qid_path: u64, name: &str, n_bytes: u64) -> Stat {
    Stat {
        qid: Qid::dir(qid_path),
        name: name.into(),
        owner: "owner".into(),
        group: "group".into(),
        perms: Perm::any_read() | Perm::any_exec(),
        n_bytes,
        last_accessed: SystemTime::now(),
        last_modified: SystemTime::now(),
        last_modified_by: "owner".into(),
    }
}

fn file_stat(qid_path: u64, name: &str, n_bytes: u64) -> Stat {
    Stat {
        qid: Qid::file(qid_path),
        name: name.into(),
        owner: "owner".into(),
        group: "group".into(),
        perms: Perm::any_read() | Perm::any_write(),
        n_bytes,
        last_accessed: SystemTime::now(),
        last_modified: SystemTime::now(),
        last_modified_by: "owner".into(),
    }
}

impl AsyncServe9p for EchoServer {
    async fn write(
        &self,
        _cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        _uname: &str,
    ) -> Result<usize> {
        if qid != RW {
            return Err(format!("write not supported for {qid} @ {offset}"));
        }

        println!("writing data to rw file");
        let s = String::from_utf8(data).unwrap();
        let n = s.len();
        self.state.write().unwrap().rw = s;

        Ok(n)
    }

    #[allow(unused_variables)]
    async fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(Qid, IoUnit)> {
        Err("create not supported".to_string())
    }

    #[allow(unused_variables)]
    async fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()> {
        Err("remove not supported".to_string())
    }

    #[allow(unused_variables)]
    async fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Result<()> {
        Err("write_stat not supported".to_string())
    }

    async fn walk_one(
        &self,
        _cid: ClientId,
        parent_qid: u64,
        child: &str,
        _uname: &str,
    ) -> Result<Qid> {
        println!("handling walk request: parent={parent_qid} child={child}");
        match (parent_qid, child) {
            (ROOT, "bar") => Ok(Qid::dir(BAR)),
            (ROOT, "foo") => Ok(Qid::file(FOO)),
            (ROOT, "rw") => Ok(Qid::file(RW)),
            (ROOT, "blocking") => Ok(Qid::file(BLOCKING)),
            (BAR, "baz") => Ok(Qid::file(BAZ)),
            (qid, child) => Err(format!("unknown child: qid={qid}, child={child}")),
        }
    }

    async fn stat(&self, _cid: ClientId, qid: u64, uname: &str) -> Result<Stat> {
        println!("handling stat request: qid={qid} uname={uname}");
        match qid {
            ROOT => Ok(dir_stat(ROOT, "/", 0)),
            BAR => Ok(dir_stat(BAR, "bar", 0)),
            FOO => Ok(file_stat(FOO, "foo", 0)),
            BAZ => Ok(file_stat(BAZ, "baz", 0)),
            RW => Ok(file_stat(RW, "rw", self.rw_size())),
            BLOCKING => Ok(file_stat(BLOCKING, "blocking", 0)),

            qid => Err(format!("stat for qid={qid}")),
        }
    }

    async fn open(&self, _cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        println!("handling open request: qid={qid} mode={mode:?} uname={uname}");
        match (qid, mode) {
            (FOO | BAZ | RW | BLOCKING, Mode::READ) => Ok(8168),
            (ROOT | BAR, Mode::READ) => Ok(8168),
            (RW, _) => Ok(8168),
            (qid, mode) => Err(format!("{qid} is not a known qid (mode={mode:?})")),
        }
    }

    async fn read(
        &self,
        _cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome> {
        println!("handling read request: qid={qid} offset={offset} count={count} uname={uname}");
        let chunk = |s: &str| {
            s.as_bytes()
                .iter()
                .skip(offset)
                .take(count)
                .copied()
                .collect::<Vec<u8>>()
        };

        let mut s = self.state.write().unwrap();

        let data = match qid {
            FOO => chunk("foo contents\n"),
            BAZ => chunk("contents of baz\n"),
            RW => chunk(&format!("server state is currently: '{}'", s.rw)),
            BLOCKING => {
                let (tx, rx) = channel(1);
                let data = chunk(&s.blocking);
                s.n += 1;
                let n_str = s.n.to_string();
                s.blocking.push_str(&n_str);
                s.blocking.push('\n');

                spawn(async move {
                    sleep(Duration::from_secs(1)).await;
                    _ = tx.send(data).await;
                });

                return Ok(ReadOutcome::Blocked(rx));
            }

            _ => Vec::new(),
        };

        Ok(ReadOutcome::Immediate(data))
    }

    async fn read_dir(&self, _cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        println!("handling read_dir request: qid={qid} uname={uname}");
        match qid {
            ROOT => Ok(vec![
                dir_stat(BAR, "bar", 0),
                file_stat(FOO, "foo", 0),
                file_stat(RW, "rw", self.rw_size()),
                file_stat(BLOCKING, "blocking", 0),
            ]),

            BAR => Ok(vec![file_stat(BAZ, "baz", 0)]),

            s => Err(format!("unknown dir: '{s}'")),
        }
    }
}
