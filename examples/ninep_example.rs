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
    fn walk(&mut self, qid: u64, elems: &[String]) -> Result<Vec<FileMeta>> {
        match qid {
            ROOT | BAR if elems.is_empty() => Ok(vec![]),

            ROOT if elems == ["bar".to_string()] => Ok(vec![FileMeta {
                path: "bar".into(),
                ty: FileType::Directory,
                qid: BAR,
            }]),

            ROOT if elems == ["foo".to_string()] => Ok(vec![FileMeta {
                path: "foo".into(),
                ty: FileType::Regular,
                qid: FOO,
            }]),

            ROOT if elems == ["bar".to_string(), "baz".to_string()] => Ok(vec![
                FileMeta {
                    path: "bar".into(),
                    ty: FileType::Directory,
                    qid: BAR,
                },
                FileMeta {
                    path: "baz".into(),
                    ty: FileType::Regular,
                    qid: BAZ,
                },
            ]),

            qid => Err(format!("unknown directory {qid}")),
        }
    }

    fn stat(&mut self, qid: u64) -> Result<Stat> {
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
                owner: "ad".into(),
                group: "ad".into(),
                last_modified_by: "ad".into(),
            }),

            FOO => Ok(Stat {
                qid: FOO,
                name: "foo".into(),
                ty: FileType::Regular,
                perms: 0o600,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: "ad".into(),
                group: "ad".into(),
                last_modified_by: "ad".into(),
            }),

            BAZ => Ok(Stat {
                qid: BAZ,
                name: "baz".into(),
                ty: FileType::Regular,
                perms: 0o600,
                n_bytes: 0,
                last_accesses: SystemTime::now(),
                last_modified: SystemTime::now(),
                owner: "ad".into(),
                group: "ad".into(),
                last_modified_by: "ad".into(),
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

    fn read_dir(&mut self, qid: u64, _uname: &str) -> Result<Vec<Stat>> {
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
                    owner: "ad".into(),
                    group: "ad".into(),
                    last_modified_by: "ad".into(),
                },
                Stat {
                    qid: FOO,
                    name: "foo".into(),
                    ty: FileType::Regular,
                    perms: 0o600,
                    n_bytes: 42,
                    last_accesses: SystemTime::now(),
                    last_modified: SystemTime::now(),
                    owner: "ad".into(),
                    group: "ad".into(),
                    last_modified_by: "ad".into(),
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
                owner: "ad".into(),
                group: "ad".into(),
                last_modified_by: "ad".into(),
            }]),

            s => Err(format!("unknown dir: '{s}'")),
        }
    }
}
