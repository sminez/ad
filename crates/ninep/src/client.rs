//! A simple 9p client for building out application specific client applications.
use crate::{
    fs::{Mode, Perm, Stat},
    protocol::{Data, Format9p, RawStat, Rdata, Rmessage, Tdata, Tmessage},
    Stream,
};
use std::{
    cmp::min,
    collections::HashMap,
    env,
    io::{self, Cursor, ErrorKind},
    mem,
    net::{TcpStream, ToSocketAddrs},
    os::unix::net::UnixStream,
};

// TODO:
// - need a proper error enum rather than just using io::Error

macro_rules! expect_rmessage {
    ($resp:expr, $variant:ident { $($field:ident),+, .. }) => {
        match $resp.content {
            Rdata::$variant { $($field),+, .. } => ($($field),+),
            Rdata::Error { ename } => return err(ename),
            m => return err(format!("unexpected response: {m:?}")),
        }

    };

    ($resp:expr, $variant:ident { $($field:ident),+ }) => {
        match $resp.content {
            Rdata::$variant { $($field),+ } => ($($field),+),
            Rdata::Error { ename } => return err(ename),
            m => return err(format!("unexpected response: {m:?}")),
        }

    };
}

const MSIZE: u32 = u16::MAX as u32;
const VERSION: &str = "9P2000";

fn err<T, E>(e: E) -> io::Result<T>
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Err(io::Error::new(io::ErrorKind::Other, e))
}

pub type UnixClient = Client<UnixStream>;
pub type TcpClient = Client<TcpStream>;

#[derive(Debug)]
pub struct Client<S>
where
    S: Stream,
{
    stream: S,
    uname: String,
    msize: u32,
    fids: HashMap<String, u32>,
    next_fid: u32,
}

impl<S> Drop for Client<S>
where
    S: Stream,
{
    fn drop(&mut self) {
        let fids = std::mem::take(&mut self.fids);
        for (_, fid) in fids.into_iter() {
            _ = self.send(0, Tdata::Clunk { fid });
        }
    }
}

impl Client<UnixStream> {
    pub fn new_unix_with_explicit_path(
        uname: String,
        path: String,
        aname: impl Into<String>,
    ) -> io::Result<Self> {
        let stream = UnixStream::connect(path)?;
        let mut fids = HashMap::new();
        fids.insert(String::new(), 0);

        let mut client = Self {
            stream,
            uname,
            msize: MSIZE,
            fids,
            next_fid: 1,
        };
        client.connect(aname)?;

        Ok(client)
    }

    pub fn new_unix(ns: impl Into<String>, aname: impl Into<String>) -> io::Result<Self> {
        let ns = ns.into();
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return err("USER env var not set"),
        };
        let path = format!("/tmp/ns.{uname}.:0/{ns}");

        Self::new_unix_with_explicit_path(uname, path, aname)
    }
}

impl Client<TcpStream> {
    pub fn new_tcp<T>(uname: String, addr: T, aname: impl Into<String>) -> io::Result<Self>
    where
        T: ToSocketAddrs,
    {
        let stream = TcpStream::connect(addr)?;
        let mut fids = HashMap::new();
        fids.insert(String::new(), 0);

        let mut client = Self {
            stream,
            uname,
            msize: MSIZE,
            fids,
            next_fid: 1,
        };
        client.connect(aname)?;

        Ok(client)
    }
}

impl<S> Client<S>
where
    S: Stream,
{
    fn send(&mut self, tag: u16, content: Tdata) -> io::Result<Rmessage> {
        let t = Tmessage { tag, content };
        t.write_to(&mut self.stream)?;

        match Rmessage::read_from(&mut self.stream)? {
            Rmessage {
                content: Rdata::Error { ename },
                ..
            } => err(ename),
            msg => Ok(msg),
        }
    }

    fn next_fid(&mut self) -> u32 {
        let fid = self.next_fid;
        self.next_fid += 1;

        fid
    }

    /// Establish our connection to the target 9p server and begin the session.
    fn connect(&mut self, aname: impl Into<String>) -> io::Result<()> {
        let resp = self.send(
            u16::MAX,
            Tdata::Version {
                msize: MSIZE,
                version: VERSION.to_string(),
            },
        )?;

        let (msize, version) = expect_rmessage!(resp, Version { msize, version });
        if version != VERSION {
            return err("server version not supported");
        }
        self.msize = msize;

        self.send(
            0,
            Tdata::Attach {
                fid: 0,
                afid: u32::MAX, // no auth
                uname: self.uname.clone(),
                aname: aname.into(),
            },
        )?;

        Ok(())
    }

    /// Associate the given path with a new fid.
    pub fn walk(&mut self, path: impl Into<String>) -> io::Result<u32> {
        let path = path.into();
        if let Some(fid) = self.fids.get(&path) {
            return Ok(*fid);
        }

        let new_fid = self.next_fid();

        self.send(
            0,
            Tdata::Walk {
                fid: 0,
                new_fid,
                wnames: path.split('/').map(Into::into).collect(),
            },
        )?;

        self.fids.insert(path, new_fid);

        Ok(new_fid)
    }

    /// Free server side state for the given fid.
    ///
    /// Clunks of the root fid (0) will be ignored
    pub fn clunk(&mut self, fid: u32) -> io::Result<()> {
        if fid != 0 {
            self.send(0, Tdata::Clunk { fid })?;
            self.fids.retain(|_, v| *v != fid);
        }

        Ok(())
    }

    /// Free server side state for the given path.
    pub fn clunk_path(&mut self, path: impl Into<String>) -> io::Result<()> {
        match self.fids.get(&path.into()) {
            Some(fid) => self.clunk(*fid),
            None => Ok(()),
        }
    }

    pub fn stat(&mut self, path: impl Into<String>) -> io::Result<Stat> {
        let fid = self.walk(path)?;
        let resp = self.send(0, Tdata::Stat { fid })?;
        let raw_stat = expect_rmessage!(resp, Stat { stat, .. });

        match raw_stat.try_into() {
            Ok(s) => Ok(s),
            Err(e) => err(e),
        }
    }

    fn _read_count(&mut self, fid: u32, offset: u64, count: u32) -> io::Result<Vec<u8>> {
        let resp = self.send(0, Tdata::Read { fid, offset, count })?;
        let Data(data) = expect_rmessage!(resp, Read { data });

        Ok(data)
    }

    fn _read_all(&mut self, path: impl Into<String>, mode: Mode) -> io::Result<Vec<u8>> {
        let fid = self.walk(path)?;
        let mode = mode.bits();
        self.send(0, Tdata::Open { fid, mode })?;

        let count = self.msize;
        let mut bytes = Vec::new();
        let mut offset = 0;
        loop {
            let data = self._read_count(fid, offset, count)?;
            if data.is_empty() {
                break;
            }
            offset += data.len() as u64;
            bytes.extend(data);
        }

        Ok(bytes)
    }

    pub fn read(&mut self, path: impl Into<String>) -> io::Result<Vec<u8>> {
        self._read_all(path, Mode::FILE)
    }

    pub fn read_str(&mut self, path: impl Into<String>) -> io::Result<String> {
        let bytes = self._read_all(path, Mode::FILE)?;
        let s = match String::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return err("invalid utf8"),
        };

        Ok(s)
    }

    pub fn read_dir(&mut self, path: impl Into<String>) -> io::Result<Vec<Stat>> {
        let bytes = self._read_all(path, Mode::DIR)?;
        let mut buf = Cursor::new(bytes);
        let mut stats: Vec<Stat> = Vec::new();

        loop {
            match RawStat::read_from(&mut buf) {
                Ok(rs) => match rs.try_into() {
                    Ok(s) => stats.push(s),
                    Err(e) => return err(e),
                },
                Err(e) if e.kind() == ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(e),
            }
        }

        Ok(stats)
    }

    pub fn iter_chunks(&mut self, path: impl Into<String>) -> io::Result<ChunkIter<S>> {
        let fid = self.walk(path)?;
        let mode = Mode::FILE.bits();
        let count = self.msize;
        self.send(0, Tdata::Open { fid, mode })?;

        Ok(ChunkIter {
            client: self,
            fid,
            offset: 0,
            count,
        })
    }

    pub fn iter_lines(&mut self, path: impl Into<String>) -> io::Result<ReadLineIter<S>> {
        let fid = self.walk(path)?;
        let mode = Mode::FILE.bits();
        let count = self.msize;
        self.send(0, Tdata::Open { fid, mode })?;

        Ok(ReadLineIter {
            client: self,
            buf: Vec::new(),
            fid,
            offset: 0,
            count,
            at_eof: false,
        })
    }

    pub fn write(
        &mut self,
        path: impl Into<String>,
        mut offset: u64,
        content: &[u8],
    ) -> io::Result<usize> {
        let fid = self.walk(path)?;
        let len = content.len();
        let mut cur = 0;
        let header_size = 4 + 8 + 4; // fid + offset + data len
        let chunk_size = (self.msize - header_size) as usize;

        while cur <= len {
            let end = min(cur + chunk_size, len);
            let resp = self.send(
                0,
                Tdata::Write {
                    fid,
                    offset,
                    data: Data(content[cur..end].to_vec()),
                },
            )?;
            let n = expect_rmessage!(resp, Write { count });
            if n == 0 {
                break;
            }
            cur += n as usize;
            offset += n as u64;
        }

        if cur != len {
            return err(format!("partial write: {cur} < {len}"));
        }

        Ok(cur)
    }

    pub fn write_str(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        content: &str,
    ) -> io::Result<usize> {
        self.write(path, offset, content.as_bytes())
    }

    pub fn create(
        &mut self,
        dir: impl Into<String>,
        name: impl Into<String>,
        perms: Perm,
        mode: Mode,
    ) -> io::Result<()> {
        let fid = self.walk(dir)?;
        self.send(
            0,
            Tdata::Create {
                fid,
                name: name.into(),
                perm: perms.bits(),
                mode: mode.bits(),
            },
        )?;

        Ok(())
    }

    pub fn remove(&mut self, path: impl Into<String>) -> io::Result<()> {
        let fid = self.walk(path)?;
        self.send(0, Tdata::Remove { fid })?;

        Ok(())
    }
}

pub struct ChunkIter<'a, S>
where
    S: Stream,
{
    client: &'a mut Client<S>,
    fid: u32,
    offset: u64,
    count: u32,
}

impl<'a, S> Iterator for ChunkIter<'a, S>
where
    S: Stream,
{
    type Item = Vec<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        let data = self
            .client
            ._read_count(self.fid, self.offset, self.count)
            .ok()?;

        if data.is_empty() {
            _ = self.client.clunk(self.fid);
            return None;
        }

        self.offset += data.len() as u64;

        Some(data)
    }
}

pub struct ReadLineIter<'a, S>
where
    S: Stream,
{
    client: &'a mut Client<S>,
    buf: Vec<u8>,
    fid: u32,
    offset: u64,
    count: u32,
    at_eof: bool,
}

impl<'a, S> Iterator for ReadLineIter<'a, S>
where
    S: Stream,
{
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_eof {
            return None;
        }

        loop {
            match self.buf.iter().position(|&b| b == b'\n') {
                Some(pos) => {
                    let (line, remaining) = self.buf.split_at(pos + 1);
                    let s = String::from_utf8(line.to_vec()).ok();
                    self.buf = remaining.to_vec();
                    return s;
                }

                _ => {
                    let data = self
                        .client
                        ._read_count(self.fid, self.offset, self.count)
                        .ok()?;

                    if data.is_empty() {
                        self.at_eof = true;
                        return String::from_utf8(mem::take(&mut self.buf)).ok();
                    }

                    self.offset += data.len() as u64;
                    self.buf.extend(data);
                }
            }
        }
    }
}
