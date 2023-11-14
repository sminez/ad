//! Traits for implementing a 9p fileserver
use crate::ninep::protocol::{Format9p, Qid, Rmessage, Stat, Tmessage, MAX_DATA_LEN};
use std::{collections::BTreeMap, path::PathBuf};

pub type Result<T> = std::result::Result<T, String>;

// TODO: the types here need to be converted to something that is more friendly to work with
// rather than using the types from the protocol layer directly

pub trait Serve9p {
    #[allow(unused_variables)]
    fn auth(&mut self, afid: &Fid, uname: &str, aname: &str) -> Result<Qid> {
        Err("authentication not required".to_string())
    }

    fn attach(&mut self, fid: &Fid, afid: &Fid, uname: &str, aname: &str) -> Result<Qid>;

    fn flush(&mut self, old_tag: u16) -> Result<()>;

    fn walk(&mut self, fid: &Fid, new_fid: &Fid, names: Vec<String>) -> Result<Vec<Qid>>;

    fn open(&mut self, fid: &Fid, mode: u8) -> Result<(Qid, u32)>;
    fn create(&mut self, fid: &Fid, name: &str, perm: u32, mode: u8) -> Result<(Qid, u32)>;

    fn read(&mut self, fid: &Fid, offset: usize, count: usize) -> Result<Vec<u8>>;
    fn write(&mut self, fid: &Fid, offset: usize, data: Vec<u8>) -> Result<usize>;

    fn clunk(&mut self, fid: &Fid) -> Result<()>;
    fn remove(&mut self, fid: &Fid) -> Result<()>;

    fn stat(&mut self, fid: &Fid) -> Result<Stat>;
    fn write_stat(&mut self, fid: &Fid, stat: Stat) -> Result<()>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Regular,
    Directory,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fid {
    pub path: PathBuf,
    pub ty: FileType,
}

#[derive(Debug)]
pub struct Server<S: Serve9p> {
    s: S,
    root: PathBuf,
    max_data_len: usize,
    fids: BTreeMap<u32, Fid>,
}

impl<S: Serve9p> Server<S> {
    pub fn new(s: S) -> Self {
        Self {
            s,
            root: "/".into(),
            max_data_len: MAX_DATA_LEN,
            fids: BTreeMap::default(),
        }
    }
}
