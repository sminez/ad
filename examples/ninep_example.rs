//! A simple demo of the 9p server interface
use ad::ninep::{
    fs::{FileMeta, IoUnit, Mode, Perm, Stat},
    server::{Result, Serve9p, Server, DEFAULT_SOCKET_NAME},
};
use std::time::SystemTime;

fn main() {
    let s = Server::new(EchoServer {
        rw: "initial".to_string(),
    });
    _ = s.serve_socket(DEFAULT_SOCKET_NAME.to_string()).join();
}

struct EchoServer {
    rw: String,
}

const ROOT: u64 = 0;
const BAR: u64 = 1;
const FOO: u64 = 2;
const BAZ: u64 = 3;
const RW: u64 = 4;

impl Serve9p for EchoServer {
    fn write(&mut self, qid: u64, offset: usize, data: Vec<u8>) -> Result<usize> {
        if qid != RW {
            return Err(format!("write not supported for {qid} @ {offset}"));
        }

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
        match (parent_qid, child) {
            (ROOT, "bar") => Ok(FileMeta::dir("bar", BAR)),
            (ROOT, "foo") => Ok(FileMeta::file("foo", FOO)),
            (ROOT, "rw") => Ok(FileMeta::file("rw", RW)),
            (BAR, "baz") => Ok(FileMeta::file("baz", BAZ)),
            (qid, child) => Err(format!("unknown child: qid={qid}, child={child}")),
        }
    }

    fn stat(&mut self, qid: u64, uname: &str) -> Result<Stat> {
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
                perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            BAZ => Ok(Stat {
                fm: FileMeta::file("baz", BAZ),
                perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
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
                n_bytes: self.rw.len() as u64,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            qid => Err(format!("stat for qid={qid}")),
        }
    }

    fn open(&mut self, qid: u64, mode: Mode, _uname: &str) -> Result<IoUnit> {
        match (qid, mode) {
            (ROOT | FOO | BAR | BAZ | RW, Mode::FILE) => Ok(8168),
            (RW, _) => Ok(8168),
            (qid, mode) => Err(format!("{qid} is not a known qid (mode={mode:?})")),
        }
    }

    fn read(&mut self, qid: u64, offset: usize, _count: usize, _uname: &str) -> Result<Vec<u8>> {
        match (qid, offset) {
            (FOO, 0) => Ok("foo contents\n".as_bytes().to_vec()),
            (BAZ, 0) => Ok("contents of baz\n".as_bytes().to_vec()),
            (RW, 0) => Ok(format!("server state is currently: '{}'", self.rw)
                .as_bytes()
                .to_vec()),

            _ => Ok(vec![]),
        }
    }

    fn read_dir(&mut self, qid: u64, uname: &str) -> Result<Vec<Stat>> {
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
                    perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
                    n_bytes: 42,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
                Stat {
                    fm: FileMeta::file("rw", FOO),
                    perms: Perm::OWNER_READ | Perm::OWNER_WRITE,
                    n_bytes: self.rw.len() as u64,
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
