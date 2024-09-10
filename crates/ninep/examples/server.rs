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
    fs::{FileMeta, IoUnit, Mode, Perm, Stat},
    server::{ReadOutcome, Result, Serve9p, Server},
};
use std::{
    sync::mpsc::channel,
    thread::{sleep, spawn},
    time::{Duration, SystemTime},
};

fn main() {
    let s = Server::new(EchoServer {
        rw: "initial".to_string(),
        blocking: "0\n".to_string(),
        n: 0,
    });
    println!("starting server");
    _ = s.serve_socket("ninep-server").join();
}

struct EchoServer {
    rw: String,
    blocking: String,
    n: usize,
}

const ROOT: u64 = 0;
const BAR: u64 = 1;
const FOO: u64 = 2;
const BAZ: u64 = 3;
const RW: u64 = 4;
const BLOCKING: u64 = 5;

impl Serve9p for EchoServer {
    fn write(&mut self, qid: u64, offset: usize, data: Vec<u8>) -> Result<usize> {
        if qid != RW {
            return Err(format!("write not supported for {qid} @ {offset}"));
        }

        println!("writing data to rw file");
        self.rw = String::from_utf8(data).unwrap();

        Ok(self.rw.len())
    }

    #[allow(unused_variables)]
    fn create(
        &mut self,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(FileMeta, IoUnit)> {
        Err("create not supported".to_string())
    }

    #[allow(unused_variables)]
    fn remove(&mut self, qid: u64, uname: &str) -> Result<()> {
        Err("remove not supported".to_string())
    }

    #[allow(unused_variables)]
    fn write_stat(&mut self, qid: u64, stat: Stat, uname: &str) -> Result<()> {
        Err("write_stat not supported".to_string())
    }

    fn walk(&mut self, parent_qid: u64, child: &str) -> Result<FileMeta> {
        println!("handling walk request: parent={parent_qid} child={child}");
        match (parent_qid, child) {
            (ROOT, "bar") => Ok(FileMeta::dir("bar", BAR)),
            (ROOT, "foo") => Ok(FileMeta::file("foo", FOO)),
            (ROOT, "rw") => Ok(FileMeta::file("rw", RW)),
            (ROOT, "blocking") => Ok(FileMeta::file("blocking", BLOCKING)),
            (BAR, "baz") => Ok(FileMeta::file("baz", BAZ)),
            (qid, child) => Err(format!("unknown child: qid={qid}, child={child}")),
        }
    }

    fn stat(&mut self, qid: u64, uname: &str) -> Result<Stat> {
        println!("handling stat request: qid={qid} uname={uname}");
        match qid {
            ROOT => Ok(Stat {
                fm: FileMeta::dir("/", ROOT),
                perms: Perm::OWNER_READ | Perm::OWNER_EXEC,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            BAR => Ok(Stat {
                fm: FileMeta::dir("bar", BAR),
                perms: Perm::OWNER_READ | Perm::OWNER_EXEC,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            FOO => Ok(Stat {
                fm: FileMeta::file("foo", FOO),
                perms: Perm::OWNER_READ,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            BAZ => Ok(Stat {
                fm: FileMeta::file("baz", BAZ),
                perms: Perm::OWNER_READ,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            RW => Ok(Stat {
                fm: FileMeta::file("rw", BAZ),
                perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
                n_bytes: self.rw.as_bytes().len() as u64,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            BLOCKING => Ok(Stat {
                fm: FileMeta::file("blocking", BLOCKING),
                perms: Perm::OWNER_READ,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            qid => Err(format!("stat for qid={qid}")),
        }
    }

    fn open(&mut self, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        println!("handling open request: qid={qid} mode={mode:?} uname={uname}");
        match (qid, mode) {
            (FOO | BAZ | RW | BLOCKING, Mode::FILE) => Ok(8168),
            (ROOT | BAR, Mode::DIR) => Ok(8168),
            (RW, _) => Ok(8168),
            (qid, mode) => Err(format!("{qid} is not a known qid (mode={mode:?})")),
        }
    }

    fn read(&mut self, qid: u64, offset: usize, count: usize, uname: &str) -> Result<ReadOutcome> {
        println!("handling read request: qid={qid} offset={offset} count={count} uname={uname}");
        let chunk = |s: &str| {
            s.as_bytes()
                .iter()
                .skip(offset)
                .take(count)
                .copied()
                .collect::<Vec<u8>>()
        };

        let data = match qid {
            FOO => chunk("foo contents\n"),
            BAZ => chunk("contents of baz\n"),
            RW => chunk(&format!("server state is currently: '{}'", self.rw)),
            BLOCKING => {
                let (tx, rx) = channel();
                let data = chunk(&self.blocking);
                self.n += 1;
                self.blocking.push_str(&self.n.to_string());
                self.blocking.push('\n');

                spawn(move || {
                    sleep(Duration::from_secs(1));
                    _ = tx.send(data);
                });

                return Ok(ReadOutcome::Blocked(rx));
            }

            _ => Vec::new(),
        };

        Ok(ReadOutcome::Immediate(data))
    }

    fn read_dir(&mut self, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        println!("handling read_dir request: qid={qid} uname={uname}");
        match qid {
            ROOT => Ok(vec![
                Stat {
                    fm: FileMeta::dir("bar", BAR),
                    perms: Perm::OWNER_READ | Perm::OWNER_EXEC,
                    n_bytes: 0,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
                Stat {
                    fm: FileMeta::file("foo", FOO),
                    perms: Perm::OWNER_READ,
                    n_bytes: 42,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
                Stat {
                    fm: FileMeta::file("rw", RW),
                    perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
                    n_bytes: self.rw.as_bytes().len() as u64,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
                Stat {
                    fm: FileMeta::file("blocking", BLOCKING),
                    perms: Perm::OWNER_READ,
                    n_bytes: 0,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
            ]),

            BAR => Ok(vec![Stat {
                fm: FileMeta::file("baz", BAZ),
                perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }]),

            s => Err(format!("unknown dir: '{s}'")),
        }
    }
}
