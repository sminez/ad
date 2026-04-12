//! Synchronous [Server] implementation using [Read][0] and [Write][1].
//!
//!  [0]: std::io::Read
//!  [1]: std::io::Write
use crate::{
    Result,
    fs::{FileMeta, FileType, IoUnit, Mode, Perm, Stat, WStat},
    sansio::{
        protocol::{DEFAULT_MSIZE, Data, RawStat, Rdata, Rmessage, SharedBuf, Tdata, Tmessage},
        server::{
            Attached, E_ALREADY_ATTACHED, E_CREATE_NON_DIR, E_FID_ALREADY_OPEN,
            E_ILLEGAL_CREATE_NAME, E_ILLEGAL_DIRECTORY_WRITE, E_UNKNOWN_FID, Either, FidMeta,
            FlushHandle, Session, SessionType, Unattached,
        },
    },
    sync::{SyncNineP, SyncServerStream, SyncStream},
};
use simple_coro::CoroState;
use std::{
    fs,
    mem::size_of,
    net::TcpListener,
    os::unix::net::UnixListener,
    path::PathBuf,
    sync::{
        Arc,
        atomic::{AtomicU32, Ordering},
        mpsc::{Receiver, Sender, channel},
    },
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

/// Tri-state of the three cases we need to process in the event loop for handling an ongoing
/// connection:
/// 1. Tmessage from the client
/// 2. Blocked read resolving
/// 3. Error on the client stream (None)
type Event = Option<Either<Tmessage, (u16, Vec<u8>)>>;

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
    #[expect(unused_variables)]
    fn clunk(&self, cid: ClientId, qid: u64) {}

    /// Handle "best effort" cancellation of an in-flight message identified by `old_tag`.
    ///
    /// Invoked when the server receives a flush message for a message that is still pending. As
    /// per the 9p spec, this is a hint _only_: the server is still permitted to complete any
    /// outstanding work and send a response to the original message being flushed.
    ///
    /// Clients are permitted to send multiple flush messages for the same tag. As such,
    /// implementations of this method should be resilient to being called multiple times with the
    /// same arguments. (The default implementation is a no-op.)
    #[expect(unused_variables)]
    fn flush(&self, cid: ClientId, old_tag: u16) {}

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
        for meta in self.state.fids.values() {
            self.s.clunk(self.client_id, meta.qid);
        }
        self.state.fids.clear();
    }

    fn flush_waiters(&mut self, flush_handle: &mut FlushHandle, tag: u16) {
        for tag in flush_handle.pending_flush_tags(tag) {
            self.reply(tag, Ok(Rdata::Flush {}));
        }
    }

    fn spawn_reader(&self, tx: Sender<Event>, msize: Arc<AtomicU32>) -> Option<JoinHandle<()>>
    where
        U: SyncServerStream,
    {
        let mut stream = self.stream.try_clone().ok()?;
        let h = spawn(move || {
            let buf = SharedBuf::default();
            loop {
                let msize = msize.load(Ordering::Relaxed);
                let t = match Tmessage::read_from(msize, &buf, &mut stream) {
                    Ok(t) => t,
                    Err(_) => {
                        _ = tx.send(None);
                        return;
                    }
                };

                if tx.send(Some(Either::L(t))).is_err() {
                    return;
                }
            }
        });

        Some(h)
    }

    fn handle_connection(mut self) {
        use Tdata::*;

        let current_msize = Arc::new(AtomicU32::new(self.msize));
        let (tx, rx) = channel();
        let mut flush_handle = FlushHandle::default();

        let _handle = match self.spawn_reader(tx.clone(), current_msize.clone()) {
            None => return self.clunk_and_clear(),
            Some(h) => h,
        };

        loop {
            let (tag, content) = match rx.recv() {
                Ok(Some(Either::L(Tmessage { tag, content }))) => (tag, content),
                Ok(Some(Either::R((tag, data)))) => {
                    self.reply(tag, Ok(Rdata::Read { data: Data(data) }));
                    self.flush_waiters(&mut flush_handle, tag);
                    continue;
                }
                Ok(None) | Err(_) => return self.clunk_and_clear(),
            };

            if !matches!(content, Flush { .. }) {
                flush_handle.mark_pending(tag);
            }

            let resp = match content {
                Auth { .. } | Attach { .. } => Err(E_ALREADY_ATTACHED.into()),
                Version { msize, version } => {
                    let rdata = self.handle_version(msize, version);
                    current_msize.store(self.msize, Ordering::Relaxed);
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
                Read { fid, offset, count } => {
                    let res = self.handle_read(tag, fid, offset, count, &tx);
                    match res {
                        Ok(Some(resp)) => Ok(resp),
                        Err(err) => Err(err),
                        Ok(None) => continue,
                    }
                }
                Write { fid, offset, data } => self.handle_write(fid, offset, data.0),
                Remove { fid } => self.handle_remove(fid),
                Wstat { fid, stat, .. } => self.handle_wstat(fid, stat),
                Flush { old_tag } => {
                    let sent_flush = flush_handle.flush_or_chain(tag, old_tag).run_sync(|_| {
                        self.reply(tag, Ok(Rdata::Flush {}));
                    });

                    if !sent_flush {
                        self.s.flush(self.client_id, old_tag);
                    }

                    continue;
                }
            };

            self.reply(tag, resp);
            self.flush_waiters(&mut flush_handle, tag);
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
            .handle_attached_walk(fid, new_fid, wnames);

        loop {
            coro = match coro.resume() {
                CoroState::Complete(res) => return res,
                CoroState::Pending(c, (qid, name, uname)) => {
                    let res = self.s.walk(client_id, qid, &name, &uname);
                    c.send(res)
                }
            };
        }
    }

    fn handle_clunk(&mut self, fid: u32) -> Result<Rdata> {
        match self.state.fids.remove(&fid) {
            Some(meta) => {
                self.s.clunk(self.client_id, meta.qid);

                Ok(Rdata::Clunk {})
            }
            None => Err(E_UNKNOWN_FID.to_string()),
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
        if self.try_fid_meta(fid)?.is_open {
            return Err(E_FID_ALREADY_OPEN.to_string());
        }

        let fm = self.try_file_meta(fid)?;
        let iounit = self
            .s
            .open(self.client_id, fm.qid, mode, &self.state.uname)?;

        self.state
            .fids
            .get_mut(&fid)
            .expect("known fid after try_file_meta")
            .is_open = true;

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

        let parent = self.s.stat(self.client_id, fm.qid, &self.state.uname)?;
        let (fm, iounit) = self.s.create(
            self.client_id,
            fm.qid,
            &name,
            perm.apply_create_mask(parent.perms),
            mode,
            &self.state.uname,
        )?;

        // fid is now changed to point to the newly created file rather than the parent
        let qid = fm.as_qid();
        self.state.fids.insert(fid, FidMeta::open(fm.qid));
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
        tx: &Sender<Event>,
    ) -> Result<Option<Rdata>> {
        let cid = self.client_id;
        let coro = self.session_state.handle_attached_read(fid, offset, count);
        let (offset, count) = (offset as usize, count as usize);

        match coro.resume() {
            CoroState::Complete(res) => res,
            CoroState::Pending(c, Either::L((qid, uname))) => {
                let stats = self.s.read_dir(cid, qid, &uname)?;
                c.send(stats).resume().unwrap()
            }
            CoroState::Pending(_, Either::R((qid, uname))) => {
                let outcome = self.s.read(cid, qid, offset, count, &uname)?;
                match outcome {
                    ReadOutcome::Immediate(data) => Ok(Some(Rdata::Read { data: Data(data) })),
                    ReadOutcome::Blocked(chan) => {
                        let tx = tx.clone();
                        spawn(move || {
                            let data = chan.recv().unwrap_or_default();
                            _ = tx.send(Some(Either::R((tag, data))));
                        });

                        Ok(None)
                    }
                }
            }
        }
    }

    fn handle_write(&mut self, fid: u32, offset: u64, data: Vec<u8>) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;

        if fm.ty == FileType::Directory {
            return Err(E_ILLEGAL_DIRECTORY_WRITE.to_string());
        } else if offset > u32::MAX as u64 {
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
        let res = self.s.remove(self.client_id, fm.qid, &self.state.uname);

        // ensure that we clunk before erroring
        self.s.clunk(self.client_id, fm.qid);
        res?;

        Ok(Rdata::Remove {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        generate_test_suite,
        sansio::protocol::Tmessage,
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
            let mut did_shutdown = false;

            for step in $case {
                match step {
                    Step::Request { tag, req, resp } => {
                        let rmsg = client.send_sync(tag, req).unwrap();
                        assert_eq!(rmsg, Rmessage { tag, content: resp });
                    }

                    Step::Send { tag, req } => {
                        Tmessage::new(tag, req)
                            .write_to(&mut client.stream)
                            .unwrap();
                    }

                    Step::Receive { tag, resp } => {
                        let rmsg = <Rmessage as SyncNineP>::read_from(
                            client.msize,
                            &client.buf,
                            &mut client.stream,
                        )
                        .unwrap();
                        assert_eq!(rmsg, Rmessage::new(tag, resp));
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
