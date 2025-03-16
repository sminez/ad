//! A simple async 9p client for building out application specific client applications.
use crate::{
    fs::{Mode, Perm, Stat},
    sansio::{
        client::{err, State, MSIZE},
        protocol::{Rdata, Rmessage, Tdata, Tmessage},
    },
    tokio::{AsyncNineP, AsyncStream},
};
use simple_coro::CoroState;
use std::{collections::HashMap, env, io, mem, path::Path, sync::Arc};
use tokio::{
    net::{TcpStream, ToSocketAddrs, UnixStream},
    sync::Mutex,
};

/// A 9p client.
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
    fn new(fids: HashMap<String, u32>, stream: S) -> Self {
        Self {
            state: Arc::new(Mutex::new(State {
                msize: MSIZE,
                fids,
                next_fid: 1,
            })),
            stream: Arc::new(Mutex::new(stream)),
        }
    }
}

/// A client that operates over an underlying tokio [UnixStream].
pub type UnixClient = Client<UnixStream>;

/// A client that operates over an underlying tokio [TcpStream].
pub type TcpClient = Client<TcpStream>;

impl Client<UnixStream> {
    /// Create a new [Client] connected to a unix socket at the specified path.
    pub async fn new_unix_with_explicit_path(
        uname: impl Into<String>,
        path: impl AsRef<Path>,
        aname: impl Into<String>,
    ) -> io::Result<Self> {
        let stream = UnixStream::connect(path.as_ref()).await?;
        let mut fids = HashMap::new();
        fids.insert(String::new(), 0);

        let mut client = Self::new(fids, stream);
        client.connect(uname, aname).await?;

        Ok(client)
    }

    /// Create a new [Client] connected to a unix socket at the given aname under the default
    /// namespace.
    ///
    /// The default namespace is located in /tmp/ns.$USER.$DISPLAY/
    pub async fn new_unix(ns: impl Into<String>, aname: impl Into<String>) -> io::Result<Self> {
        let ns = ns.into();
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return err("USER env var not set"),
        };
        let display = env::var("DISPLAY").unwrap_or(":0".to_string());
        let path = format!("/tmp/ns.{uname}.{display}/{ns}");

        Self::new_unix_with_explicit_path(uname, path, aname).await
    }
}

impl Client<TcpStream> {
    /// Create a new [Client] connected to a tcp socket at the specified address.
    pub async fn new_tcp(
        uname: impl Into<String>,
        addr: impl ToSocketAddrs,
        aname: impl Into<String>,
    ) -> io::Result<Self> {
        let stream = TcpStream::connect(addr).await?;
        let mut fids = HashMap::new();
        fids.insert(String::new(), 0);

        let mut client = Self::new(fids, stream);
        client.connect(uname, aname).await?;

        Ok(client)
    }
}

macro_rules! run_9p_coro {
    ($self:ident, $method:ident, $($arg:expr),*) => {
        {
            let mut state = $self.state.lock().await;
            let mut coro = state.$method($($arg),*);
            loop {
                coro = match coro.resume() {
                    CoroState::Complete(res) => break res,
                    CoroState::Pending(c, t) => {
                        let mut stream = $self.stream.lock().await;
                        t.write_to(&mut *stream).await?;

                        match Rmessage::read_from(&mut *stream).await? {
                            Rmessage {
                                content: Rdata::Error { ename },
                                ..
                            } => return err(ename),
                            rmessage => c.send(rmessage),
                        }
                    }
                }
            }
        }
    };
}

impl<S> Client<S>
where
    S: AsyncStream,
{
    async fn send(&mut self, tag: u16, content: Tdata) -> io::Result<Rmessage> {
        let mut stream = self.stream.lock().await;
        Tmessage { tag, content }.write_to(&mut *stream).await?;

        match Rmessage::read_from(&mut *stream).await? {
            Rmessage {
                content: Rdata::Error { ename },
                ..
            } => err(ename),
            msg => Ok(msg),
        }
    }

    /// Establish our connection to the target 9p server and begin the session.
    async fn connect(
        &mut self,
        uname: impl Into<String>,
        aname: impl Into<String>,
    ) -> io::Result<()> {
        run_9p_coro!(self, handle_connect, uname.into(), aname.into())
    }

    /// Associate the given path with a new fid.
    pub async fn walk(&mut self, path: impl Into<String>) -> io::Result<u32> {
        run_9p_coro!(self, handle_walk, path.into())
    }

    /// Free server side state for the given fid.
    ///
    /// Clunks of the root fid (0) will be ignored
    pub async fn clunk(&mut self, fid: u32) -> io::Result<()> {
        if fid != 0 {
            self.send(0, Tdata::Clunk { fid }).await?;
            self.state.lock().await.fids.retain(|_, v| *v != fid);
        }

        Ok(())
    }

    /// Free server side state for the given path.
    pub async fn clunk_path(&mut self, path: impl Into<String>) -> io::Result<()> {
        let fid = match self.state.lock().await.fids.get(&path.into()) {
            Some(fid) => *fid,
            None => return Ok(()),
        };

        self.clunk(fid).await
    }

    /// Request the current [Stat] of the file or directory identified by the given path.
    pub async fn stat(&mut self, path: impl Into<String>) -> io::Result<Stat> {
        run_9p_coro!(self, handle_stat, path.into())
    }

    /// Read the full contents of the file at `path` as bytes.
    pub async fn read(&mut self, path: impl Into<String>) -> io::Result<Vec<u8>> {
        run_9p_coro!(self, handle_read, path.into())
    }

    /// Read the full contents of the file at `path` as utf-8 encoded text.
    pub async fn read_str(&mut self, path: impl Into<String>) -> io::Result<String> {
        let bytes = run_9p_coro!(self, handle_read, path.into())?;
        let s = match String::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return err("invalid utf8"),
        };

        Ok(s)
    }

    /// Read the directory listing of the directory at `path`.
    pub async fn read_dir(&mut self, path: impl Into<String>) -> io::Result<Vec<Stat>> {
        run_9p_coro!(self, handle_read_dir, path.into())
    }

    /// Write the provided data to the file at `path` at the given offset.
    pub async fn write(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        content: &[u8],
    ) -> io::Result<usize> {
        run_9p_coro!(self, handle_write, path.into(), offset, content)
    }

    /// Write the provided string data to the file at `path` at the given offset.
    pub async fn write_str(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        content: &str,
    ) -> io::Result<usize> {
        run_9p_coro!(self, handle_write, path.into(), offset, content.as_bytes())
    }

    /// Attempt to create a new file within the connected filesystem.
    pub async fn create(
        &mut self,
        dir: impl Into<String>,
        name: impl Into<String>,
        perms: Perm,
        mode: Mode,
    ) -> io::Result<()> {
        run_9p_coro!(self, handle_create, dir.into(), name.into(), perms, mode)
    }

    /// Attempt to remove a file from the connected filesystem.
    pub async fn remove(&mut self, path: impl Into<String>) -> io::Result<()> {
        run_9p_coro!(self, handle_remove, path.into())
    }

    /// Asynchronously iterate over Vec's of bytes from the file at `path`.
    ///
    /// The size of each chunk is determined by the supported message size of the server replying
    /// to the requests and provides no guarantees over the structure of the content of each chunk.
    ///
    /// The [ChunkStream] returned by this method provides an asynchronous `next` method that can
    /// be called to await the next chunk.
    pub async fn stream_chunks(&mut self, path: impl Into<String>) -> io::Result<ChunkStream<S>> {
        let fid = self.walk(path).await?;
        let mode = Mode::FILE.bits();
        let count = self.state.lock().await.msize;
        self.send(0, Tdata::Open { fid, mode }).await?;

        Ok(ChunkStream {
            client: self.clone(),
            fid,
            offset: 0,
            count,
        })
    }

    /// Asynchronously iterate over newline delimited lines of utf-8 encoded text from the file at `path`.
    ///
    /// The [ReadLineStream] returned by this method provides an asynchronous `next` method that can
    /// be called to await the next chunk.
    pub async fn stream_lines(&mut self, path: impl Into<String>) -> io::Result<ReadLineStream<S>> {
        let fid = self.walk(path).await?;
        let mode = Mode::FILE.bits();
        let count = self.state.lock().await.msize;
        self.send(0, Tdata::Open { fid, mode }).await?;

        Ok(ReadLineStream {
            client: self.clone(),
            buf: Vec::new(),
            fid,
            offset: 0,
            count,
            at_eof: false,
        })
    }

    async fn _read_count(&mut self, fid: u32, offset: u64, count: u32) -> io::Result<Vec<u8>> {
        run_9p_coro!(self, handle_read_count, fid, offset, count)
    }
}

/// An asynchronous stream of [Vec<u8>] chunks out of a given file.
#[derive(Debug)]
pub struct ChunkStream<S>
where
    S: AsyncStream,
{
    client: Client<S>,
    fid: u32,
    offset: u64,
    count: u32,
}

impl<S> ChunkStream<S>
where
    S: AsyncStream,
{
    /// Await the next chunk of data out of a file.
    pub async fn next(&mut self) -> Option<Vec<u8>> {
        let data = self
            .client
            ._read_count(self.fid, self.offset, self.count)
            .await
            .ok()?;

        if data.is_empty() {
            _ = self.client.clunk(self.fid);
            return None;
        }

        self.offset += data.len() as u64;

        Some(data)
    }
}

/// An asynchronous stream of [String] lines out of a given file.
#[derive(Debug)]
pub struct ReadLineStream<S>
where
    S: AsyncStream,
{
    client: Client<S>,
    buf: Vec<u8>,
    fid: u32,
    offset: u64,
    count: u32,
    at_eof: bool,
}

impl<S> ReadLineStream<S>
where
    S: AsyncStream,
{
    /// Await the next newline delimited line out of a file.
    pub async fn next(&mut self) -> Option<String> {
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
                        .await
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
