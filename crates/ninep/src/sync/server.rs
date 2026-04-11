//! Synchronous [Server] implementation using [Read][0] and [Write][1].
//!
//!  [0]: std::io::Read
//!  [1]: std::io::Write
use crate::{
    Result,
    fs::{FileMeta, FileType, IoUnit, Mode, Perm, Stat, WStat},
    sansio::{
        protocol::{DEFAULT_MSIZE, Data, RawStat, Rdata, Rmessage, Tdata, Tmessage},
        server::{
            Attached, E_ALREADY_ATTACHED, E_CREATE_NON_DIR, E_ILLEGAL_CREATE_NAME, E_UNKNOWN_FID,
            Either, Session, SessionType, Unattached,
        },
    },
    sync::{SyncNineP, SyncServerStream, SyncStream},
};
use simple_coro::CoroState;
use std::{
    collections::btree_map::Entry,
    fs,
    mem::size_of,
    net::TcpListener,
    os::unix::net::UnixListener,
    path::PathBuf,
    sync::mpsc::Receiver,
    thread::{JoinHandle, spawn},
};

// re-exports
pub use crate::sansio::server::{ClientId, Server, socket_dir, socket_path};

/// The outcome of a client attempting to [read](Serve9p::read) a given file.
#[derive(Debug)]
pub enum ReadOutcome {
    /// The data is immediately available.
    Immediate(Vec<u8>),
    /// No response should be sent until data is received on the provided channel
    Blocked(Receiver<Vec<u8>>),
}

#[derive(Debug)]
struct Socket {
    path: PathBuf,
    listener: UnixListener,
}

impl Drop for Socket {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

fn unix_socket(path: impl Into<PathBuf>) -> Socket {
    let path = path.into();
    if let Some(dir) = path.parent() {
        let _ = fs::create_dir_all(dir);
    }

    // FIXME: really we should be handling this on exit but we'll need to catch
    // ctrl-c to do that properly. For now this works but it means that if you
    // start a second file server with the same name then it'll remove the socket
    // for the first.
    let _ = fs::remove_file(&path);
    let listener = UnixListener::bind(&path).unwrap();

    Socket { path, listener }
}

fn tcp_socket(port: u16) -> TcpListener {
    let addr = format!("127.0.0.1:{port}");
    TcpListener::bind(addr).unwrap()
}

/// A type capable of handling [9p](http://9p.cat-v.org/) requests in order to implement a
/// 9p virtual filesystem. The [Server] struct is used to handle the lower level protocol and
/// underlying connection, allowing implementers of this trait to focus on the semantics of the
/// virtual filesystem itself.
///
/// # The 9p protocol
/// Please see [the documentation page on cat-v](http://9p.cat-v.org/documentation/) for an
/// overview of how the protocol works along with various papers covering the original implementation
/// from Bell Labs. For simple filesystems you should be able to get away with referring to the
/// docs on each of the methods for this trait, but you are advised to read through the semantics
/// around permissions and file creation as this is something handled by the trait implementer, not
/// [Server].
///
/// ## Client fids and server-side qids
/// [Server] handles establishing and maintaining per-client sessions along with all of their `fids`,
/// as such, [Serve9p] only needs to worry about maintaining `qids` for resources.
///
/// The source code of [Server] is a useful reference for those wanting to learn more.
pub trait Serve9p: Send + Sync + 'static {
    // #[allow(unused_variables)]
    // fn auth(&self, afid: u32, uname: &str, aname: &str) -> Result<Qid> {
    //     Err("authentication not required".to_string())
    // }

    /// Lookup a child node under a known parent directory by name.
    ///
    /// `9p` Twalk messages received by the server will specify a full path from a known parent
    /// to a target child. This method is called for each element of that path in sequence in order,
    /// stopping either when the target is reached or some element of the path returns an error.
    ///
    /// [Server] will ensure that this method is only called for known parents who have previously
    /// been identified has having [FileType::Directory].
    fn walk(&self, cid: ClientId, parent_qid: u64, child: &str, uname: &str) -> Result<FileMeta>;

    /// Open an existing file in the requested mode for subsequent I/O via [read](Serve9p::read) and
    /// [write](Serve9p::write) calls.
    ///
    /// The return of this method is an [IoUnit] used to inform the client of the maximum number of
    /// bytes that will be supported per read/write call on this resource.
    fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit>;

    /// Clunk a currently open file.
    #[allow(unused_variables)]
    fn clunk(&self, cid: ClientId, qid: u64) {}

    /// Create a new file in the given parent directory.
    fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(FileMeta, IoUnit)>;

    /// Read `count` bytes from the requested file starting from the given `offset`.
    fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome>;

    /// List the contents of the given directory.
    fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>>;

    /// Write the given `data` to the requested file starting at `offset`
    fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize>;

    /// Remove the requested file from the filesystem.
    fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()>;

    /// Request a machine independent "directory entry" for the given resource.
    fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat>;

    /// Attempt to set the machine independent "directory entry" for the given resource.
    fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Result<()>;
}

impl<S> Server<S>
where
    S: Serve9p,
{
    /// Bind this server to the specified port and serve over a tcp socket.
    pub fn serve_tcp(mut self, port: u16) -> JoinHandle<()> {
        spawn(move || {
            let listener = tcp_socket(port);

            for stream in listener.incoming() {
                let stream = stream.unwrap();
                let session = self.new_session(stream);
                spawn(move || session.handle_connection());
            }
        })
    }

    /// Bind this server to the specified socket name and serve over a unix socket created under
    /// the default [socket_dir].
    pub fn serve_socket(self, socket_name: impl Into<String>) -> JoinHandle<()> {
        let socket_name = socket_name.into();
        let path = socket_dir().join(socket_name);

        self.serve_socket_with_custom_path(path)
    }

    /// Bind this server to the specified absolute path and serve over a unix socket created under
    /// an arbitrary directory.
    ///
    /// It is recommended that you use [Server::serve_socket] for most purposes so that your server
    /// creates its socket in the known default [socket_dir].
    pub fn serve_socket_with_custom_path(mut self, socket_path: PathBuf) -> JoinHandle<()> {
        spawn(move || {
            let sock = unix_socket(socket_path);

            for stream in sock.listener.incoming() {
                let stream = stream.unwrap();
                let session = self.new_session(stream);
                spawn(move || session.handle_connection());
            }
        })
    }
}

impl<T, S, U> Session<T, S, U>
where
    T: SessionType,
    S: Serve9p,
    U: SyncStream,
{
    fn reply(&mut self, tag: u16, resp: Result<Rdata>) {
        let mut r: Rmessage = (tag, resp).into();
        r.clamp(self.msize);
        let _ = r.write_to(&mut self.stream);
    }
}

impl<S, U> Session<Unattached, S, U>
where
    S: Serve9p,
    U: SyncServerStream,
{
    fn handle_connection(mut self) {
        loop {
            let t = match Tmessage::read_from(DEFAULT_MSIZE, &self.buf, &mut self.stream) {
                Ok(t) => t,
                Err(_) => return,
            };

            match self.handle_tmessage_unattached(t) {
                Either::L((tag, resp)) => self.reply(tag, resp),
                Either::R((tag, st, aqid)) => {
                    self.reply(tag, Ok(Rdata::Attach { aqid }));
                    return self.into_attached(st).handle_connection();
                }
            }
        }
    }
}

impl<S, U> Session<Attached, S, U>
where
    S: Serve9p,
    U: SyncServerStream,
{
    /// Explicitly clunk all
    fn clunk_and_clear(&mut self) {
        for &qid in self.state.fids.values() {
            self.s.clunk(self.client_id, qid);
        }
        self.state.fids.clear();
    }

    fn handle_connection(mut self) {
        use Tdata::*;

        loop {
            let t = match Tmessage::read_from(self.msize, &self.buf, &mut self.stream) {
                Ok(t) => t,
                Err(_) => return self.clunk_and_clear(),
            };

            let Tmessage { tag, content } = t;

            let resp = match content {
                Auth { .. } | Attach { .. } => Err(E_ALREADY_ATTACHED.into()),
                Flush { .. } => Ok(Rdata::Flush {}),
                Version { msize, version } => {
                    let rdata = self.handle_version(msize, version);
                    self.clunk_and_clear();

                    Ok(rdata)
                }

                Walk {
                    fid,
                    new_fid,
                    wnames,
                } => self.handle_walk(fid, new_fid, wnames),
                Clunk { fid } => self.handle_clunk(fid),
                Stat { fid } => self.handle_stat(fid),
                Open { fid, mode } => self.handle_open(fid, Mode::new(mode)),
                Create {
                    fid,
                    name,
                    perm,
                    mode,
                } => self.handle_create(fid, name, Perm::new(perm), Mode::new(mode)),
                Read { fid, offset, count } => match self.handle_read(tag, fid, offset, count) {
                    Ok(Some(resp)) => Ok(resp),
                    Err(err) => Err(err),
                    Ok(None) => continue,
                },
                Write { fid, offset, data } => self.handle_write(fid, offset, data.0),
                Remove { fid } => self.handle_remove(fid),
                Wstat { fid, stat, .. } => self.handle_wstat(fid, stat),
            };

            self.reply(tag, resp);
        }
    }

    /// The walk request carries as arguments an existing fid and a proposed newfid (which must not
    /// be in use unless it is the same as fid) that the client wishes to associate with the result
    /// of traversing the directory hierarchy by ‘walking’ the hierarchy using the successive path
    /// name elements wname.
    ///
    /// The fid must represent a directory unless zero path name elements are specified.
    ///
    /// The fid must be valid in the current session and must not have been opened for I/O by an
    /// open or create message. If the full sequence of nwname elements is walked successfully,
    /// newfid will represent the file that results. If not, newfid (and fid) will be unaffected.
    /// However, if newfid is in use or otherwise illegal, an Rerror is returned.
    ///
    /// The name “..” (dot-dot) represents the parent directory. The name “.” (dot), meaning the
    /// current directory, is not used in the protocol.
    ///
    /// It is legal for nwname to be zero, in which case newfid will represent the same file as fid
    /// and the walk will usually succeed; this is equivalent to walking to dot. The rest of this
    /// discussion assumes nwname is greater than zero.
    ///
    /// The nwname path name elements wname are walked in order, “elementwise”. For the first
    /// elementwise walk to succeed, the file identified by fid must be a directory, and the
    /// implied user of the request must have permission to search the directory (see intro(9P)).
    /// Subsequent elementwise walks have equivalent restrictions applied to the implicit fid that
    /// results from the preceding elementwise walk.
    ///
    /// If the first element cannot be walked for any reason, Rerror is returned. Otherwise, the
    /// walk will return an Rwalk message containing nwqid qids corresponding, in order, to the
    /// files that are visited by the nwqid successful elementwise walks; nwqid is therefore either
    /// nwname or the index of the first elementwise walk that failed. The value of nwqid cannot be
    /// zero unless nwname is zero. Also, nwqid will always be less than or equal to nwname. Only
    /// if it is equal, however, will newfid be affected, in which case newfid will represent the
    /// file reached by the final elementwise walk requested in the message.
    ///
    /// A walk of the name “..” in the root directory of a server is equivalent to a walk with no
    /// name elements.
    ///
    /// If newfid is the same as fid, the above discussion applies, with the obvious difference
    /// that if the walk changes the state of newfid, it also changes the state of fid; and if
    /// newfid is unaffected, then fid is also unaffected.
    ///
    /// To simplify the implementation of the servers, a maximum of sixteen name elements or qids
    /// may be packed in a single message. This constant is called MAXWELEM in fcall(3). Despite
    /// this restriction, the system imposes no limit on the number of elements in a file name,
    /// only the number that may be transmitted in a single message.
    fn handle_walk(&mut self, fid: u32, new_fid: u32, wnames: Vec<String>) -> Result<Rdata> {
        let client_id = self.client_id;
        let mut coro = self
            .session_state
            .handle_attached_walk(fid, new_fid, &wnames);

        loop {
            coro = match coro.resume() {
                CoroState::Complete(res) => return res,
                CoroState::Pending(c, (qid, name, uname)) => {
                    let res = self.s.walk(client_id, qid, name, uname);
                    c.send(res)
                }
            };
        }
    }

    fn handle_clunk(&mut self, fid: u32) -> Result<Rdata> {
        match self.state.fids.entry(fid) {
            Entry::Occupied(ent) => {
                let qid = ent.remove();
                self.s.clunk(self.client_id, qid);

                Ok(Rdata::Clunk {})
            }
            Entry::Vacant(_) => Err(E_UNKNOWN_FID.to_string()),
        }
    }

    fn handle_stat(&mut self, fid: u32) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;
        let s = self.s.stat(self.client_id, fm.qid, &self.state.uname)?;
        let stat: RawStat = s.into();
        let size = stat.size + size_of::<u16>() as u16;

        Ok(Rdata::Stat { size, stat })
    }

    fn handle_wstat(&mut self, fid: u32, raw_stat: RawStat) -> Result<Rdata> {
        let wstat: WStat = raw_stat.into();
        let fm = self.try_file_meta(fid)?;
        self.s
            .write_stat(self.client_id, fm.qid, wstat, &self.state.uname)?;

        Ok(Rdata::Wstat {})
    }

    fn handle_open(&mut self, fid: u32, mode: Mode) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;
        let iounit = self
            .s
            .open(self.client_id, fm.qid, mode, &self.state.uname)?;

        Ok(Rdata::Open {
            qid: fm.as_qid(),
            iounit,
        })
    }

    fn handle_create(&mut self, fid: u32, name: String, perm: Perm, mode: Mode) -> Result<Rdata> {
        if name == "." || name == ".." {
            return Err(E_ILLEGAL_CREATE_NAME.to_string());
        }

        let fm = self.try_file_meta(fid)?;
        if fm.ty != FileType::Directory {
            return Err(E_CREATE_NON_DIR.to_string());
        }

        let (fm, iounit) =
            self.s
                .create(self.client_id, fm.qid, &name, perm, mode, &self.state.uname)?;

        // fid is now changed to point to the newly created file rather than the parent
        let qid = fm.as_qid();
        self.state.fids.insert(fid, fm.qid);
        self.qids.entry(fm.qid).or_insert(fm);

        Ok(Rdata::Create { qid, iounit })
    }

    // The read request asks for count bytes of data from the file identified by fid, which must be
    // opened for reading, starting offset bytes after the beginning of the file. The bytes are
    // returned with the read reply message.
    // The count field in the reply indicates the number of bytes returned. This may be less than
    // the requested amount. If the offset field is greater than or equal to the number of bytes in
    // the file, a count of zero will be returned.
    // For directories, read returns an integral number of directory entries exactly as in stat
    // (see stat(9P)), one for each member of the directory. The read request message must have
    // offset equal to zero or the value of offset in the previous read on the directory, plus the
    // number of bytes returned in the previous read. In other words, seeking other than to the
    // beginning is illegal in a directory.
    fn handle_read(
        &mut self,
        tag: u16,
        fid: u32,
        offset: u64,
        count: u32,
    ) -> Result<Option<Rdata>> {
        let cid = self.client_id;
        let coro = self.session_state.handle_attached_read(fid, offset, count);
        let (offset, count) = (offset as usize, count as usize);

        match coro.resume() {
            CoroState::Complete(res) => res,
            CoroState::Pending(c, Either::L((qid, uname))) => {
                let stats = self.s.read_dir(cid, qid, uname)?;
                c.send(stats).resume().unwrap()
            }
            CoroState::Pending(_, Either::R((qid, uname))) => {
                let outcome = self.s.read(cid, qid, offset, count, uname)?;
                match outcome {
                    ReadOutcome::Immediate(data) => Ok(Some(Rdata::Read { data: Data(data) })),
                    ReadOutcome::Blocked(chan) => {
                        let mut stream = self.stream.try_clone()?;
                        spawn(move || {
                            let data = chan.recv().unwrap_or_default();
                            let resp = Ok(Rdata::Read { data: Data(data) });
                            let r: Rmessage = (tag, resp).into();
                            let _ = r.write_to(&mut stream);
                        });

                        Ok(None)
                    }
                }
            }
        }
    }

    fn handle_write(&mut self, fid: u32, offset: u64, data: Vec<u8>) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;
        if offset > u32::MAX as u64 {
            return Err(format!("offset too large: {offset} > {}", u32::MAX));
        }

        let count = self.s.write(
            self.client_id,
            fm.qid,
            offset as usize,
            data,
            &self.state.uname,
        )? as u32;

        Ok(Rdata::Write { count })
    }

    fn handle_remove(&mut self, fid: u32) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;
        self.s.remove(self.client_id, fm.qid, &self.state.uname)?;

        Ok(Rdata::Remove {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        generate_test_suite,
        test_utils::{SyncTestClient, TestFs, cases::Step},
    };
    use std::{net::Shutdown, os::unix::net::UnixStream, thread};

    macro_rules! run_one {
        ($case:expr) => {
            // Setup the client and server
            let fs = TestFs::default();
            let recorded = fs.calls();
            let (client_stream, server_stream) = UnixStream::pair().unwrap();
            let mut client = SyncTestClient::new(client_stream);
            let mut server = Server::new(fs);

            let mut handle = Some(thread::spawn(move || {
                server.new_session(server_stream).handle_connection();
            }));

            // Run the test case
            let mut next_tag = 0;
            let mut did_shutdown = false;

            for step in $case {
                match step {
                    Step::Request { req, resp } => {
                        let tag = next_tag;
                        next_tag += 1;

                        let rmsg = client.send_sync(tag, req).unwrap();
                        assert_eq!(rmsg, Rmessage { tag, content: resp });
                    }

                    Step::AssertCalls { calls } => assert_eq!(recorded.take(), calls),

                    Step::CloseStream => {
                        let _ = client.stream.shutdown(Shutdown::Both);
                        if let Some(h) = handle.take() {
                            h.join().expect("server thread join failed");
                        }
                        did_shutdown = true;
                    }
                }
            }

            if !did_shutdown {
                let _ = client.stream.shutdown(Shutdown::Both);
            }

            // wait for the server to shutdown
            if let Some(h) = handle.take() {
                h.join().expect("server thread join failed");
            }
        };
    }

    generate_test_suite!(sync, run_one);
}
