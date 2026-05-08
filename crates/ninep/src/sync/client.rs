//! A simple 9p client for building out application specific client applications.
use crate::{
    fs::{Mode, Perm, Stat, WStat},
    sansio::{
        client::{MSIZE, State, err},
        protocol::{Rdata, Rmessage, SharedBuf, Tdata, Tmessage},
    },
    sync::{SyncNineP, SyncStream},
};
use simple_coro::CoroState;
use std::{
    collections::HashMap,
    env, mem,
    net::{TcpStream, ToSocketAddrs},
    os::unix::net::UnixStream,
    path::Path,
    sync::{
        Arc, Mutex, MutexGuard,
        atomic::{AtomicU32, Ordering},
        mpsc::{Receiver, Sender, channel},
    },
    thread::spawn,
};

// re-export
pub use crate::sansio::client::{Error, Result};

/// A synchronous 9p client.
///
/// Support for each of the operations exposed by this client is determined by the server
/// implementation that it is connected to.
#[derive(Debug)]
pub struct Client {
    state: Arc<Mutex<State>>,
    tx: Sender<Req>,
    msize: Arc<AtomicU32>,
}

impl Clone for Client {
    fn clone(&self) -> Self {
        let _ = self.tx.send(Req::AddClient);
        Self {
            state: Arc::clone(&self.state),
            tx: self.tx.clone(),
            msize: Arc::clone(&self.msize),
        }
    }
}

impl Client {
    fn new<S>(stream: S) -> Self
    where
        S: SyncStream,
    {
        let (tx, rx) = channel();
        let msize = Arc::new(AtomicU32::new(MSIZE));
        let conn = Connection::new(stream, rx, Arc::clone(&msize));
        spawn(move || conn.run());

        Self {
            state: Default::default(),
            tx,
            msize,
        }
    }

    #[inline]
    fn state(&self) -> MutexGuard<'_, State> {
        match self.state.lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }

    fn send_raw(&self, data: Tdata) -> Result<Rmessage> {
        let (tx, rx) = channel();
        let req = Req::Send { data, tx };
        self.tx.send(req).map_err(|_| Error::ConnectionClosed)?;

        rx.recv().map_err(|_| Error::ConnectionClosed)?
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        let _ = self.tx.send(Req::RemoveClient);
    }
}

impl Client {
    /// Create a new [Client] connected to a unix socket at the specified path.
    pub fn new_unix_with_explicit_path(
        uname: impl Into<String>,
        path: impl AsRef<Path>,
        aname: impl Into<String>,
    ) -> Result<Self> {
        let stream = UnixStream::connect(path.as_ref())?;

        let mut client = Self::new(stream);
        client.connect(uname, aname)?;

        Ok(client)
    }

    /// Create a new [Client] connected to a unix socket at the given aname under the default
    /// namespace.
    ///
    /// The default namespace is located in /tmp/ns.$USER.$DISPLAY/
    pub fn new_unix(ns: impl Into<String>, aname: impl Into<String>) -> Result<Self> {
        let ns = ns.into();
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return err("USER env var not set"),
        };
        let display = env::var("DISPLAY").unwrap_or(":0".to_string());
        let path = format!("/tmp/ns.{uname}.{display}/{ns}");

        Self::new_unix_with_explicit_path(uname, path, aname)
    }

    /// Create a new [Client] using an existing [stream][UnixStream].
    pub fn new_from_unix_stream(
        uname: impl Into<String>,
        aname: impl Into<String>,
        stream: UnixStream,
    ) -> Result<Self> {
        let mut client = Self::new(stream);
        client.connect(uname, aname)?;

        Ok(client)
    }

    /// Create a new [Client] connected to a tcp socket at the specified address.
    pub fn new_tcp(
        uname: impl Into<String>,
        addr: impl ToSocketAddrs,
        aname: impl Into<String>,
    ) -> Result<Self> {
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
                    let rmsg = $self.send_raw(t.content)?;
                    c.send(rmsg)
                }
            }
        }
    }};
}

impl Client {
    fn send(&self, content: Tdata) -> Result<Rmessage> {
        match self.send_raw(content)? {
            Rmessage {
                content: Rdata::Error { ename },
                ..
            } => Err(Error::Rerror { ename }),
            msg => Ok(msg),
        }
    }

    /// Establish our connection to the target 9p server and begin the session.
    fn connect(&mut self, uname: impl Into<String>, aname: impl Into<String>) -> Result<()> {
        run_9p_coro!(self, handle_connect, uname.into(), aname.into())?;
        let msize = self.state().msize;
        self.msize.store(msize, Ordering::Release);

        Ok(())
    }

    /// Associate the given path with a new fid.
    pub fn walk(&mut self, path: impl Into<String>) -> Result<u32> {
        run_9p_coro!(self, handle_walk, path.into())
    }

    /// Free server side state for the given fid.
    ///
    /// Clunks of the root fid (0) will be ignored
    pub fn clunk(&mut self, fid: u32) -> Result<()> {
        if fid != 0 {
            self.send(Tdata::Clunk { fid })?;
            self.state().fids.remove(fid);
        }

        Ok(())
    }

    /// Free server side state for the given path.
    pub fn clunk_path(&mut self, path: impl Into<String>) -> Result<()> {
        let fid = match self.state().fids.fid_for_unnormalised_path(&path.into()) {
            Some(fid) => fid,
            None => return Ok(()),
        };

        self.clunk(fid)
    }

    /// Request the current [Stat] of the file or directory identified by the given path.
    pub fn stat(&mut self, path: impl Into<String>) -> Result<Stat> {
        run_9p_coro!(self, handle_stat, path.into())
    }

    /// Attempt to modify the current [Stat] of the file or directory identified by the given path
    /// using the given [WStat].
    pub fn write_stat(&mut self, path: impl Into<String>, wstat: WStat) -> Result<()> {
        run_9p_coro!(self, handle_wstat, path.into(), wstat)
    }

    /// Read the full contents of the file at `path` as bytes.
    pub fn read(&mut self, path: impl Into<String>) -> Result<Vec<u8>> {
        run_9p_coro!(self, handle_read, path.into())
    }

    /// Read up to `count` bytes from the file at `path` starting at byte `offset`.
    pub fn read_from(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        count: u32,
    ) -> Result<Vec<u8>> {
        run_9p_coro!(self, handle_read_from, path.into(), offset, count)
    }

    /// Read the full contents of the file at `path` as utf-8 encoded text.
    pub fn read_str(&mut self, path: impl Into<String>) -> Result<String> {
        let bytes = run_9p_coro!(self, handle_read, path.into())?;
        let s = match String::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return err("invalid utf8"),
        };

        Ok(s)
    }

    /// Read the directory listing of the directory at `path`.
    pub fn read_dir(&mut self, path: impl Into<String>) -> Result<Vec<Stat>> {
        run_9p_coro!(self, handle_read_dir, path.into())
    }

    /// Write the provided data to the file at `path` at the given offset.
    pub fn write(&mut self, path: impl Into<String>, offset: u64, content: &[u8]) -> Result<usize> {
        run_9p_coro!(self, handle_write, path.into(), offset, content)
    }

    /// Write the provided string data to the file at `path` at the given offset.
    pub fn write_str(
        &mut self,
        path: impl Into<String>,
        offset: u64,
        content: &str,
    ) -> Result<usize> {
        run_9p_coro!(self, handle_write, path.into(), offset, content.as_bytes())
    }

    /// Attempt to create a new file within the connected filesystem.
    pub fn create(
        &mut self,
        dir: impl Into<String>,
        name: impl Into<String>,
        perms: Perm,
        mode: Mode,
    ) -> Result<()> {
        run_9p_coro!(self, handle_create, dir.into(), name.into(), perms, mode)
    }

    /// Attempt to remove a file from the connected filesystem.
    pub fn remove(&mut self, path: impl Into<String>) -> Result<()> {
        run_9p_coro!(self, handle_remove, path.into())
    }

    /// Iterate over Vec's of bytes from the file at `path`.
    ///
    /// The size of each chunk is determined by the supported message size of the server replying
    /// to the requests.
    pub fn iter_chunks(&mut self, path: impl Into<String>) -> Result<ChunkIter> {
        let fid = self.walk(path)?;
        let mode = Mode::READ.bits();
        let count = self.state().msize;
        self.send(Tdata::Open { fid, mode })?;

        Ok(ChunkIter {
            client: self.clone(),
            fid,
            offset: 0,
            count,
        })
    }

    /// Iterate over newline delimited lines of utf-8 encoded text from the file at `path`.
    pub fn iter_lines(&mut self, path: impl Into<String>) -> Result<ReadLineIter> {
        let fid = self.walk(path)?;
        let mode = Mode::READ.bits();
        let count = self.state().msize;
        self.send(Tdata::Open { fid, mode })?;

        Ok(ReadLineIter {
            client: self.clone(),
            buf: Vec::new(),
            fid,
            offset: 0,
            count,
            at_eof: false,
        })
    }

    fn _read_count(&mut self, fid: u32, offset: u64, count: u32) -> Result<Vec<u8>> {
        run_9p_coro!(self, handle_read_count, fid, offset, count)
    }
}

pub(crate) enum Req {
    AddClient,
    RemoveClient,
    Send {
        data: Tdata,
        tx: Sender<Result<Rmessage>>,
    },
}

pub(crate) struct Connection<S>
where
    S: SyncStream,
{
    stream: S,
    rx: Receiver<Req>,
    msize: Arc<AtomicU32>,
    pending: HashMap<u16, Sender<Result<Rmessage>>>,
    buf: SharedBuf,
    n_clients: usize,
    next_tag: u16,
}

impl<S> Connection<S>
where
    S: SyncStream,
{
    fn new(stream: S, rx: Receiver<Req>, msize: Arc<AtomicU32>) -> Self {
        Self {
            stream,
            rx,
            msize,
            pending: HashMap::new(),
            buf: SharedBuf::default(),
            n_clients: 1,
            next_tag: 0,
        }
    }

    fn tag_for(&mut self, data: &Tdata) -> u16 {
        if matches!(data, Tdata::Version { .. }) {
            return u16::MAX;
        }

        let tag = self.next_tag;
        self.next_tag = self.next_tag.saturating_add(1);
        if self.next_tag == u16::MAX {
            self.next_tag = 0;
        }

        tag
    }

    fn run(mut self) {
        while self.n_clients > 0 {
            if self.pending.is_empty() {
                let req = match self.rx.recv() {
                    Ok(req) => req,
                    Err(_) => return,
                };

                self.handle_req(req);
                continue;
            }

            while let Ok(req) = self.rx.try_recv() {
                self.handle_req(req);
                if self.n_clients == 0 {
                    return;
                }
            }

            let size = self.msize.load(Ordering::Acquire);
            let rmsg = match Rmessage::read_from(size, &self.buf, &mut self.stream) {
                Ok(rmsg) => rmsg,
                Err(e) => {
                    let msg = e.to_string();
                    for tx in self.pending.drain().map(|(_, s)| s) {
                        let _ = tx.send(err(msg.clone()));
                    }
                    break;
                }
            };

            if let Some(sender) = self.pending.remove(&rmsg.tag) {
                let _ = sender.send(Ok(rmsg));
            }
        }
    }

    fn handle_req(&mut self, req: Req)
    where
        S: SyncStream,
    {
        match req {
            Req::AddClient => self.n_clients += 1,
            Req::RemoveClient => self.n_clients = self.n_clients.saturating_sub(1),
            Req::Send { data, tx } => {
                let tag = self.tag_for(&data);
                let msg = Tmessage::new(tag, data);
                if let Err(e) = msg.write_to(&mut self.stream) {
                    let msg = e.to_string();
                    let _ = tx.send(err(msg.clone()));

                    for tx in self.pending.drain().map(|(_, s)| s) {
                        let _ = tx.send(err(msg.clone()));
                    }

                    self.n_clients = 0;
                    return;
                }

                self.pending.insert(tag, tx);
            }
        }
    }
}

/// An iterator of [`Vec<u8>`] chunks out of a given file.
#[derive(Debug)]
pub struct ChunkIter {
    client: Client,
    fid: u32,
    offset: u64,
    count: u32,
}

impl Iterator for ChunkIter {
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
pub struct ReadLineIter {
    client: Client,
    buf: Vec<u8>,
    fid: u32,
    offset: u64,
    count: u32,
    at_eof: bool,
}

impl Iterator for ReadLineIter {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        assert_9p_client_result, generate_client_test_suite,
        sync::server::Server,
        test_utils::{
            TestFs,
            client_cases::{Step, TestCase},
        },
    };
    use std::{os::unix::net::UnixStream, thread};

    // We stamp out the test suite using this helper macro rather than using simple_test_case in
    // order to ensure that both the sync and tokio implementations run exactly the same cases
    // without needing to define that set of cases in two places.
    generate_client_test_suite!(sync, run_one);

    fn run_one(case: TestCase) {
        let fs = TestFs::default();
        let mut server = Server::new(fs);
        let (client_stream, server_stream) = UnixStream::pair().unwrap();
        let mut client = Client::new(client_stream);
        let handle = thread::spawn(move || {
            server.handle_single_client_stream(server_stream);
        });

        for (i, step) in case.into_iter().enumerate() {
            handle_step(i, step, &mut client);
        }

        drop(client);
        handle.join().expect("server thread join failed");
    }

    fn handle_step(i: usize, step: Step, client: &mut Client) {
        match step {
            Step::Connect { uname, aname, res } => {
                let actual = client.connect(uname, aname);
                assert_9p_client_result!("connect", i, actual, res);
            }

            Step::Clunk { fid, res } => {
                let actual = client.clunk(fid);
                assert_9p_client_result!("clunk", i, actual, res);
            }

            Step::Create {
                dir,
                name,
                perms,
                mode,
                res,
            } => {
                let actual = client.create(dir, name, perms, mode);
                assert_9p_client_result!("create", i, actual, res);
            }

            Step::Walk { path, res } => {
                let actual = client.walk(path);
                assert_9p_client_result!("walk", i, actual, res);
            }

            Step::Read { path, res } => {
                let actual = client.read_str(path);
                assert_9p_client_result!("read", i, actual, res);
            }

            Step::ReadFrom {
                path,
                offset,
                count,
                res,
            } => {
                let actual = client.read_from(path, offset, count);
                assert_9p_client_result!("read_at", i, actual, res);
            }

            Step::ReadDir { path, res } => {
                let actual = client.read_dir(path);
                assert_9p_client_result!("read dir", i, actual, res);
            }

            Step::Remove { path, res } => {
                let actual = client.remove(path);
                assert_9p_client_result!("remove", i, actual, res);
            }

            Step::Write {
                path,
                offset,
                content,
                res,
            } => {
                let actual = client.write(path, offset, content);
                assert_9p_client_result!("write", i, actual, res);
            }

            Step::Stat { path, res } => {
                let actual = client.stat(path);
                assert_9p_client_result!("stat", i, actual, res);
            }

            Step::WriteStat { path, wstat, res } => {
                let actual = client.write_stat(path, wstat);
                assert_9p_client_result!("write stat", i, actual, res);
            }

            Step::AssertState { next_fid, fids } => {
                let st = client.state();
                assert_eq!(st.next_fid, next_fid, "(step {i}) next_fid");
                assert_eq!(st.fids.path_to_fid(), &fids, "(step {i}) fids");
            }
        }
    }
}
