//! A simple demo of the filesystem interface
use ad::ninep::{
    fs::{FileMeta, FileType, IoUnit, Mode, Stat},
    server::{Result, Serve9p, Server, DEFAULT_SOCKET_NAME},
};
use std::time::SystemTime;

fn main() {
    let s = Server::new(EchoServer);
    s.serve_socket(DEFAULT_SOCKET_NAME)
}

struct EchoServer;
const ROOT: u64 = 0;
const BAR: u64 = 1;
const FOO: u64 = 2;
const BAZ: u64 = 3;

impl Serve9p for EchoServer {
    fn walk(&mut self, parent_qid: u64, child: &str) -> Result<FileMeta> {
        match (parent_qid, child) {
            (ROOT, "bar") => Ok(FileMeta {
                path: "bar".into(),
                ty: FileType::Directory,
                qid: BAR,
            }),

            (ROOT, "foo") => Ok(FileMeta {
                path: "foo".into(),
                ty: FileType::Regular,
                qid: FOO,
            }),

            (BAR, "baz") => Ok(FileMeta {
                path: "baz".into(),
                ty: FileType::Regular,
                qid: BAZ,
            }),

            (qid, child) => Err(format!("unknown child: qid={qid}, child={child}")),
        }
    }

    fn stat(&mut self, qid: u64, uname: &str) -> Result<Stat> {
        match qid {
            ROOT => Ok(Stat {
                qid: ROOT,
                name: "/".into(),
                ty: FileType::Directory,
                perms: 0o500,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: "ad".into(),
                group: "ad".into(),
                last_modified_by: "ad".into(),
            }),

            BAR => Ok(Stat {
                qid: BAR,
                name: "bar".into(),
                ty: FileType::Directory,
                perms: 0o500,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            FOO => Ok(Stat {
                qid: FOO,
                name: "foo".into(),
                ty: FileType::Regular,
                perms: 0o600,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: uname.into(),
                group: uname.into(),
                last_modified_by: uname.into(),
            }),

            BAZ => Ok(Stat {
                qid: BAZ,
                name: "baz".into(),
                ty: FileType::Regular,
                perms: 0o600,
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

    fn open(&mut self, qid: u64, mode: Mode, _uname: &str) -> Result<IoUnit> {
        match (qid, mode) {
            (ROOT | FOO | BAR | BAZ, 0) => Ok(8168),
            (qid, mode) => Err(format!("{qid} is not a known qid (mode={mode})")),
        }
    }

    fn read(&mut self, qid: u64, offset: usize, _count: usize, _uname: &str) -> Result<Vec<u8>> {
        match (qid, offset) {
            (FOO, 0) => Ok("foo contents\n".as_bytes().to_vec()),
            (BAZ, 0) => Ok("contents of baz\n".as_bytes().to_vec()),

            _ => Ok(vec![]),
        }
    }

    fn read_dir(&mut self, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        match qid {
            ROOT => Ok(vec![
                Stat {
                    qid: BAR,
                    name: "bar".into(),
                    ty: FileType::Directory,
                    perms: 0o500,
                    n_bytes: 0,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
                Stat {
                    qid: FOO,
                    name: "foo".into(),
                    ty: FileType::Regular,
                    perms: 0o600,
                    n_bytes: 42,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: uname.into(),
                    group: uname.into(),
                    last_modified_by: uname.into(),
                },
            ]),

            BAR => Ok(vec![Stat {
                qid: BAZ,
                name: "baz".into(),
                ty: FileType::Regular,
                perms: 0o500,
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
