//! A simple demo of the filesystem interface
use ad::ninep::{
    fs::{FileType, IoUnit, Mode, Stat},
    server::{Result, Serve9p, Server, DEFAULT_SOCKET_NAME},
};
use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

fn main() {
    let s = Server::new(EchoServer);
    s.serve_socket(DEFAULT_SOCKET_NAME)
}

struct EchoServer;

impl Serve9p for EchoServer {
    fn walk(&mut self, path: &Path) -> Result<Vec<(FileType, PathBuf)>> {
        if path.as_os_str() == "/" {
            Ok(vec![
                (FileType::Regular, "foo".into()),
                (FileType::Directory, "bar".into()),
            ])
        } else {
            Err("unknown directory".to_string())
        }
    }

    fn stat(&mut self, path: &Path) -> Result<Stat> {
        match path.as_os_str().to_str().unwrap() {
            "/" => Ok(Stat {
                qid: 0,
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

            "foo" => Err("stat for /foo".into()),
            "bar" => Err("stat for /bar".into()),
            s => Err(format!("stat for {s}")),
        }
    }

    fn open(&mut self, path: &Path, mode: Mode) -> Result<IoUnit> {
        if path.as_os_str() == "/" && mode == 0 {
            Ok(8168)
        } else {
            Err(format!("{path:?} is not a known path"))
        }
    }

    fn read(&mut self, _path: &Path, _offset: usize, _count: usize) -> Result<Vec<u8>> {
        Ok(vec![])
    }

    fn read_dir(&mut self, path: &Path) -> Result<Vec<Stat>> {
        match path.as_os_str().to_str().unwrap() {
            "/" => Ok(vec![
                Stat {
                    qid: 1,
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
                    qid: 2,
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

            s => Err(format!("unknown dir: '{s}'")),
        }
    }
}
