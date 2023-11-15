//! A simple demo of the filesystem interface
use ad::ninep::{FileType, Result, Serve9p, Server, Stat};
use std::{
    path::{Path, PathBuf},
    time::SystemTime,
};

fn main() {
    let s = Server::new(EchoServer);
    s.serve()
}

struct EchoServer;

impl Serve9p for EchoServer {
    fn walk(&mut self, path: &Path) -> Result<Vec<(FileType, PathBuf)>> {
        if path.as_os_str() == "/" {
            Ok(vec![
                (FileType::Regular, "/foo".into()),
                (FileType::Directory, "/bar".into()),
            ])
        } else {
            Err("unknown directory".to_string())
        }
    }

    fn stat(&mut self, path: &Path) -> Result<Stat> {
        match path.as_os_str().to_str().unwrap() {
            "/" => Ok(Stat {
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

            "/foo" => Err("stat for /foo".into()),
            "/bar" => Err("stat for /bar".into()),
            s => Err(format!("stat for {s}")),
        }
    }
}
