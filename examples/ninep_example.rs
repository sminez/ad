//! A simple demo of the filesystem interface
use ad::ninep::{FileType, Serve9p, Server};
use std::path::{Path, PathBuf};

fn main() {
    let s = Server::new(EchoServer);
    s.serve()
}

struct EchoServer;

impl Serve9p for EchoServer {
    fn walk(&mut self, path: &Path) -> Result<Vec<(FileType, PathBuf)>, String> {
        if path.as_os_str() == "/" {
            Ok(vec![
                (FileType::Regular, "foo".into()),
                (FileType::Directory, "bar".into()),
            ])
        } else {
            Err("unknown directory".to_string())
        }
    }
}
