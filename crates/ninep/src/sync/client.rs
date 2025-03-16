//! A simple 9p client for building out application specific client applications.
use crate::{
    fs::{Mode, Perm, Stat},
    sansio::{
        client::{err, State, MSIZE},
        protocol::{Rdata, Rmessage, Tdata, Tmessage},
    },
    sync::{SyncNineP, SyncStream},
};
use simple_coro::CoroState;
use std::{
    collections::HashMap,
    env, io, mem,
    net::{TcpStream, ToSocketAddrs},
    os::unix::net::UnixStream,
    path::Path,
    sync::{Arc, Mutex, MutexGuard},
};

/// A synchronous 9p client.
///
/// Support for each of the operations exposed by this client is determined by the server
/// implementation that it is connected to.
#[derive(Debug)]
pub struct Client<S> {
    state: Arc<Mutex<State>>,
    stream: Arc<Mutex<S>>,
}

impl<S> Clone for Client<S> {
    fn clone(&self) -> Self {
        Self {
            state: Arc::clone(&self.state),
            stream: Arc::clone(&self.stream),
        }
    }
}

impl<S> Client<S> {
    fn new(stream: S) -> Self {
        Self {
            state: Arc::new(Mutex::new(State {
                msize: MSIZE,
                fids: HashMap::from([(String::new(), 0)]),
                next_fid: 1,
            })),
            stream: Arc::new(Mutex::new(stream)),
        }
    }

    #[inline]
    fn state(&self) -> MutexGuard<'_, State> {
        match self.state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }

    #[inline]
    fn stream(&self) -> MutexGuard<'_, S> {
        match self.stream.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }
}

/// A client that operates over an underlying [UnixStream].
pub type UnixClient = Client<UnixStream>;

/// A client that operates over an underlying [TcpStream].
pub type TcpClient = Client<TcpStream>;

impl Client<UnixStream> {
    /// Create a new [Client] connected to a unix socket at the specified path.
    pub fn new_unix_with_explicit_path(
        uname: impl Into<String>,
        path: impl AsRef<Path>,
        aname: impl Into<String>,
    ) -> io::Result<Self> {
        let stream = UnixStream::connect(path.as_ref())?;

        let mut client = Self::new(stream);
        client.connect(uname, aname)?;

        Ok(client)
    }

    /// Create a new [Client] connected to a unix socket at the given aname under the default
    /// namespace.
    ///
    /// The default namespace is located in /tmp/ns.$USER.$DISPLAY/
    pub fn new_unix(ns: impl Into<String>, aname: impl Into<String>) -> io::Result<Self> {
        let ns = ns.into();
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return err("USER env var not set"),
        };
        let display = env::var("DISPLAY").unwrap_or(":0".to_string());
        let path = format!("/tmp/ns.{uname}.{display}/{ns}");

        Self::new_unix_with_explicit_path(uname, path, aname)
    }
}

impl Client<TcpStream> {
    /// Create a new [Client] connected to a tcp socket at the specified address.
    pub fn new_tcp(
        uname: impl Into<String>,
        addr: impl ToSocketAddrs,
        aname: impl Into<String>,
    ) -> io::Result<Self> {
        let stream = TcpStream::connect(addr)?;

        let mut client = Self::new(stream);
        client.connect(uname, aname)?;

        Ok(client)
    }
}

macro_rules! run_9p_coro {
    ($self:ident, $method:ident, $($arg:expr),*) => {{
        let mut state = $self.state();
        let mut coro = state.$method($($arg),*);
        loop {
            coro = match coro.resume() {
                CoroState::Complete(res) => break res,
                CoroState::Pending(c, t) => {
                    let mut stream = $self.stream();
                    t.write_to(&mut *stream)?;

                    match Rmessage::read_from(&mut *stream)? {
                        Rmessage {
                            content: Rdata::Error { ename },
                            ..
                        } => return err(ename),
                        rmessage => c.send(rmessage),
                    }
                }
            }
        }
    }};
}

impl<S> Client<S>
where
    S: SyncStream,
{
    fn send(&mut self, tag: u16, content: Tdata) -> io::Result<Rmessage> {
        let mut stream = self.stream();
        Tmessage { tag, content }.write_to(&mut *stream)?;

        match Rmessage::read_from(&mut *stream)? {
            Rmessage {
                content: Rdata::Error { ename },
                ..
            } => err(ename),
            msg => Ok(msg),
        }
    }

    /// Establish our connection to the target 9p server and begin the session.
    fn connect(&mut self, uname: impl Into<String>, aname: impl Into<String>) -> io::Result<()> {
        run_9p_coro!(self, handle_connect, uname.into(), aname.into())
    }

    /// Associate the given path with a new fid.
    pub fn walk(&mut self, path: impl Into<String>) -> io::Result<u32> {
        run_9p_coro!(self, handle_walk, path.into())
    }

    /// Free server side state for the given fid.
    ///
    /// Clunks of the root fid (0) will be ignored
    pub fn clunk(&mut self, fid: u32) -> io::Result<()> {
        if fid != 0 {
            self.send(0, Tdata::Clunk { fid })?;
            self.state().fids.retain(|_, v| *v != fid);
        }

        Ok(())
    }

    /// Free server side state for the given path.
    pub fn clunk_path(&mut self, path: impl Into<String>) -> io::Result<()> {
        let fid = match self.state().fids.get(&path.into()) {
            Some(fid) => *fid,
            None => return Ok(()),
        };

        self.clunk(fid)
    }

    /// Request the current [Stat] of the file or directory identified by the given path.
    pub fn stat(&mut self, path: impl Into<String>) -> io::Result<Stat> {
        run_9p_coro!(self, handle_stat, path.into())
    }

    /// Read the full contents of the file at `path` as bytes.
    pub fn read(&mut self, path: impl Into<String>) -> io::Result<Vec<u8>> {
        run_9p_coro!(self, handle_read, path.into())
    }

    /// Read the full contents of the file at `path` as utf-8 encoded text.
    pub fn read_str(&mut self, path: impl Into<String>) -> io::Result<String> {
        let bytes = run_9p_coro!(self, handle_read, path.into())?;
        let s = match String::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return err("invalid utf8"),
        };

        Ok(s)
    }

    /// Read the directory listing of the directory at `path`.
    pub fn read_dir(&mut self, path: impl Into<String>) -> io::Result<Vec<Stat>> {
        run_9p_coro!(self, handle_read_dir, path.into())
    }

    /// Write the provided data to the file at `path` at the given offset.
    pub fn write(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        content: &[u8],
    ) -> io::Result<usize> {
        run_9p_coro!(self, handle_write, path.into(), offset, content)
    }

    /// Write the provided string data to the file at `path` at the given offset.
    pub fn write_str(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        content: &str,
    ) -> io::Result<usize> {
        run_9p_coro!(self, handle_write, path.into(), offset, content.as_bytes())
    }

    /// Attempt to create a new file within the connected filesystem.
    pub fn create(
        &mut self,
        dir: impl Into<String>,
        name: impl Into<String>,
        perms: Perm,
        mode: Mode,
    ) -> io::Result<()> {
        run_9p_coro!(self, handle_create, dir.into(), name.into(), perms, mode)
    }

    /// Attempt to remove a file from the connected filesystem.
    pub fn remove(&mut self, path: impl Into<String>) -> io::Result<()> {
        run_9p_coro!(self, handle_remove, path.into())
    }

    /// Iterate over Vec's of bytes from the file at `path`.
    ///
    /// The size of each chunk is determined by the supported message size of the server replying
    /// to the requests.
    pub fn iter_chunks(&mut self, path: impl Into<String>) -> io::Result<ChunkIter<S>> {
        let fid = self.walk(path)?;
        let mode = Mode::FILE.bits();
        let count = self.state().msize;
        self.send(0, Tdata::Open { fid, mode })?;

        Ok(ChunkIter {
            client: self.clone(),
            fid,
            offset: 0,
            count,
        })
    }

    /// Iterate over newline delimited lines of utf-8 encoded text from the file at `path`.
    pub fn iter_lines(&mut self, path: impl Into<String>) -> io::Result<ReadLineIter<S>> {
        let fid = self.walk(path)?;
        let mode = Mode::FILE.bits();
        let count = self.state().msize;
        self.send(0, Tdata::Open { fid, mode })?;

        Ok(ReadLineIter {
            client: self.clone(),
            buf: Vec::new(),
            fid,
            offset: 0,
            count,
            at_eof: false,
        })
    }

    fn _read_count(&mut self, fid: u32, offset: u64, count: u32) -> io::Result<Vec<u8>> {
        run_9p_coro!(self, handle_read_count, fid, offset, count)
    }
}

/// An iterator of [Vec<u8>] chunks out of a given file.
#[derive(Debug)]
pub struct ChunkIter<S>
where
    S: SyncStream,
{
    client: Client<S>,
    fid: u32,
    offset: u64,
    count: u32,
}

impl<S> Iterator for ChunkIter<S>
where
    S: SyncStream,
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

/// An iterator of [String] lines out of a given file.
#[derive(Debug)]
pub struct ReadLineIter<S>
where
    S: SyncStream,
{
    client: Client<S>,
    buf: Vec<u8>,
    fid: u32,
    offset: u64,
    count: u32,
    at_eof: bool,
}

impl<S> Iterator for ReadLineIter<S>
where
    S: SyncStream,
{
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.at_eof {
            _ = self.client.clunk(self.fid);
            return None;
        }

        loop {
            match self.buf.iter().position(|&b| b == b'\n') {
                Some(pos) => {
                    let (raw_line, remaining) = self.buf.split_at(pos + 1);
                    let mut line = raw_line.to_vec();
                    line.pop();
                    let s = String::from_utf8(line).ok();
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
                        if self.buf.is_empty() {
                            _ = self.client.clunk(self.fid);
                            return None;
                        }
                        return String::from_utf8(mem::take(&mut self.buf)).ok();
                    }

                    self.offset += data.len() as u64;
                    self.buf.extend(data);
                }
            }
        }
    }
}
