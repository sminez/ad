//! Synchronous [Server] implementation using [Read][0] and [Write][1].
//!
//!  [0]: std::io::Read
//!  [1]: std::io::Write
use crate::{
    Result,
    fs::{IoUnit, Mode, Perm, Qid, Stat, WStat},
    sansio::{
        protocol::{
            DEFAULT_MSIZE, Data, FileType, RawStat, Rdata, Rmessage, SharedBuf, Tdata, Tmessage,
        },
        server::{
            Attached, E_ALREADY_ATTACHED, E_CREATE_NON_DIR, E_ILLEGAL_CREATE_NAME,
            E_ILLEGAL_DIRECTORY_WRITE, E_PERMISSION_DENIED, E_UNKNOWN_FID, Either, FidMeta,
            FlushHandle, QidMeta, Session, SessionType, Unattached,
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

    /// Lookup the [Qid] for `child` under the directory represented by `parent_qid`.
    ///
    /// `9p` walk messages received by the [Server] will specify a full path from a known parent
    /// (of file type [Directory][FileType::DIRECTORY]) to a target `child`. This method is called
    /// for each element of that path in order, stopping either when the target is reached or some
    /// element of the path returns an error.
    fn walk_one(&self, cid: ClientId, parent_qid: u64, child: &str, uname: &str) -> Result<Qid>;

    /// Open an existing file for subsequent I/O via [read](Serve9p::read) and
    /// [write](Serve9p::write) messages.
    ///
    /// [Server] calls this only for known, currently-closed fids. On success, [Server] marks the
    /// fid as open with further `walk`, `open` and `create` messages using that fid being rejected
    /// until the fid is clunked (either [explicitly](Serve9p::clunk) or via session reset).
    ///
    /// The return of this method is an [IoUnit] used to inform the client of the maximum number of
    /// bytes that will be supported per read/write call on this resource. An `Err` should be
    /// returned if access is denied, mode is unsupported, or the target cannot be opened.
    fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit>;

    /// Release client specific server-side resources associated with with provided qid.
    ///
    /// [Server] calls this when a fid is discarded (i.e. [clunk](Serve9p::clunk), successful or
    /// failed [remove](Serve9p::remove), `version` session reset, or connection close). It is
    /// possible that the provided qid may represent a file that was never opened for I/O in this
    /// session.
    ///
    /// Implementations should be resilient to repeated calls for the same qid. (The default
    /// implementation is a no-op.)
    #[expect(unused_variables)]
    fn clunk(&self, cid: ClientId, qid: u64) {}

    /// Handle "best effort" cancellation of an in-flight message identified by `old_tag`.
    ///
    /// Invoked when the server receives a `flush` message for a message that is still pending. As
    /// per the `9p` spec, this is a hint _only_: the server is still permitted to complete any
    /// outstanding work and send a response to the original message being flushed.
    ///
    /// Clients are permitted to send multiple `flush` messages for the same tag. As such,
    /// implementations of this method should be resilient to being called multiple times with the
    /// same arguments. (The default implementation is a no-op.)
    #[expect(unused_variables)]
    fn flush(&self, cid: ClientId, old_tag: u16) {}

    /// Create a new entry in `parent`, returning its [Qid] and [IoUnit].
    ///
    /// [Server] ensures that this method is only called when the target fid is known, pointing to
    /// a directory and not currently open for I/O. [Server] also handles rejecting `.` and `..` as
    /// invalid entry names, and applying `9p` permission masking before this call, meaning `perm`
    /// is already normalized against the parent directory's permissions.
    ///
    /// On success, [Server] rebinds the creating fid to the qid found in the returned [Qid]
    /// and marks it as open for I/O. Implementations should return `Err` if `name` already exists
    /// or creation cannot be completed for any reason.
    fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(Qid, IoUnit)>;

    /// Read up to `count` bytes from `qid` starting at `offset`.
    ///
    /// [Server] calls this for non-directory files only; directory reads are routed through
    /// [read_dir](Serve9p::read_dir). The returned [ReadOutcome] controls how the response is sent
    /// to the client: [Immediate][ReadOutcome::Immediate] is send directly to the client on
    /// completion of this method, while [blocked][ReadOutcome::Blocked] spawns a background task
    /// to wait for data to become available before responding.
    ///
    /// Implementations should tolerate flush hints while blocked (see [flush](Serve9p::flush)) and
    /// must respect client specified byte `count` limit.
    fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome>;

    /// List [Stat] entries for a given client's view of a directory.
    ///
    /// [Server] calls this for `read` requests on a directory, handling read offsets and count
    /// limits automatically (unlike [read][Serve9p::read]). Implementations should return the full
    /// logical entry list in stable order for the client's view of the directory (as identified by
    /// `cid`).
    fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>>;

    /// Write the provided `data` to the file denoted by `qid` starting at the provided byte
    /// `offset`.
    ///
    /// [Server] ensures that this is only called for entries of type [file][FileType::FILE].
    ///
    /// Returns the number of bytes written. Returning `n < data.len()` is permitted and is treated
    /// as a short write which may result in further `write` messages from the client.
    ///
    /// Implementations are required to enforce mode/permission rules and return `Err` when writes
    /// are not permitted.
    fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize>;

    /// Remove the entry identified by `qid` from the filesystem.
    ///
    /// [Server] calls this for each `remove` request received from the client followed by
    /// [clunking][Serve9p::clunk] the `qid` regardless of success. Implementations should return
    /// `Err` when removal is not permitted or fails.
    fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()>;

    /// Fetch the current [Stat] metadata for the filesystem entry identified by `qid`.
    ///
    /// [Server] uses this for client `stat` messages and internally for [create][Serve9p::create]
    /// permission masking against parent directories.
    fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat>;

    /// Apply a [WStat] update to the [Stat] of the filesystem entry identified by `qid`.
    ///
    /// [Server] validates fid/qid identity before calling this and passes a [WStat] containing
    /// only caller-requested field changes. Implementations must enforce authorization and
    /// supported field semantics, returning `Err` for invalid or disallowed changes.
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

    #[cfg(test)]
    pub(crate) fn handle_single_test_stream_sync<U>(&mut self, stream: U)
    where
        U: SyncServerStream,
    {
        self.new_session(stream).handle_connection();
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
    /// Explicitly clunk all open fids
    fn clunk_and_clear(&mut self) {
        let fids: Vec<u32> = self.state.fids.keys().copied().collect();
        for fid in fids.into_iter() {
            _ = self.handle_clunk(fid);
        }
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
        let uname = self.state.uname.clone();
        let mut coro = self
            .session_state
            .handle_attached_walk(fid, new_fid, wnames);

        loop {
            coro = match coro.resume() {
                CoroState::Complete(res) => return res.map(|wqids| Rdata::Walk { wqids }),
                CoroState::Pending(c, (qid, name)) => {
                    let res = self.s.walk_one(client_id, qid, &name, &uname);
                    c.send(res)
                }
            };
        }
    }

    fn handle_stat(&mut self, fid: u32) -> Result<Rdata> {
        let qid = self.try_map_fid(fid)?;
        let s = self.s.stat(self.client_id, qid.path, &self.state.uname)?;
        let stat: RawStat = s.into();
        let size = stat.size + size_of::<u16>() as u16;

        Ok(Rdata::Stat { size, stat })
    }

    fn handle_wstat(&mut self, fid: u32, raw_stat: RawStat) -> Result<Rdata> {
        let wstat: WStat = raw_stat.into();
        let qid = self.try_map_fid(fid)?;
        self.s
            .write_stat(self.client_id, qid.path, wstat, &self.state.uname)?;

        Ok(Rdata::Wstat {})
    }

    fn qid_if_perms_hold(&self, fid: u32, mode: Mode) -> Result<Qid> {
        let qid = self.try_map_fid(fid)?;
        let stat = self.s.stat(self.client_id, qid.path, &self.state.uname)?;

        let coro = self.handle_perm_check(stat, &[], mode);
        match coro.resume() {
            CoroState::Complete(res) => res?,
            CoroState::Pending(c, _) => {
                let parent = self
                    .parent_qid(qid.path)
                    .ok_or_else(|| E_PERMISSION_DENIED.to_string())?;
                let stat = self.s.stat(self.client_id, parent, &self.state.uname)?;
                c.send(stat).resume().unwrap()?;
            }
        }

        Ok(qid)
    }

    fn handle_open(&mut self, fid: u32, mode: Mode) -> Result<Rdata> {
        let qid = self.qid_if_perms_hold(fid, mode)?;
        self.try_add_client_id_to_open_qids(fid)?;

        let iounit = self
            .s
            .open(self.client_id, qid.path, mode, &self.state.uname)?;

        self.state
            .fids
            .get_mut(&fid)
            .expect("known fid after try_file_meta")
            .mode = Some(mode);

        Ok(Rdata::Open { qid, iounit })
    }

    fn handle_create(&mut self, fid: u32, name: String, perm: Perm, mode: Mode) -> Result<Rdata> {
        if name == "." || name == ".." {
            return Err(E_ILLEGAL_CREATE_NAME.to_string());
        }

        let qid = self.try_map_fid(fid)?;
        if qid.ty != FileType::DIRECTORY {
            return Err(E_CREATE_NON_DIR.to_string());
        }

        let parent = self.s.stat(self.client_id, qid.path, &self.state.uname)?;
        let (qid, iounit) = self.s.create(
            self.client_id,
            qid.path,
            &name,
            perm.apply_create_mask(parent.perms),
            mode,
            &self.state.uname,
        )?;

        // fid is now changed to point to the newly created file rather than the parent
        self.state.fids.insert(fid, FidMeta::open(qid.path, mode));
        self.with_shared_qids_mut(|qids| {
            qids.entry(qid.path)
                .or_insert(QidMeta::new(qid, Some(parent.qid.path)));
        });

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
        self.session_state
            .try_fid_meta(fid)?
            .check_open_for_read()?;

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
        self.session_state
            .try_fid_meta(fid)?
            .check_open_for_write()?;

        let qid = self.try_map_fid(fid)?;

        if qid.ty == FileType::DIRECTORY {
            return Err(E_ILLEGAL_DIRECTORY_WRITE.to_string());
        } else if offset > u32::MAX as u64 {
            return Err(format!("offset too large: {offset} > {}", u32::MAX));
        }

        let count = self.s.write(
            self.client_id,
            qid.path,
            offset as usize,
            data,
            &self.state.uname,
        )? as u32;

        Ok(Rdata::Write { count })
    }

    fn _clunk<F>(&mut self, fid: u32, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self, u64) -> Result<()>,
    {
        match self.state.fids.remove(&fid) {
            Some(meta) => {
                let res = f(self, meta.qid);
                self.s.clunk(self.client_id, meta.qid);
                self.remove_client_id_from_open_qids(meta.qid);

                res
            }

            None => Err(E_UNKNOWN_FID.to_string()),
        }
    }

    fn handle_clunk(&mut self, fid: u32) -> Result<Rdata> {
        self._clunk(fid, |_, _| Ok(()))?;

        Ok(Rdata::Clunk {})
    }

    fn handle_remove(&mut self, fid: u32) -> Result<Rdata> {
        self._clunk(fid, |sa, qid| {
            sa.s.remove(sa.client_id, qid, &sa.state.uname)
        })?;

        Ok(Rdata::Remove {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        generate_server_test_suite,
        sansio::protocol::Tmessage,
        test_utils::{
            RecordedCalls, SyncTestClient, TestFs,
            server_cases::{Step, TestCase},
        },
    };
    use std::{net::Shutdown, os::unix::net::UnixStream, thread};

    // We stamp out the test suite using this helper macro rather than using simple_test_case in
    // order to ensure that both the sync and tokio implementations run exactly the same cases
    // without needing to define that set of cases in two places.
    generate_server_test_suite!(sync, run_one);

    fn run_one(case: TestCase) {
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

        for (i, step) in case.into_iter().enumerate() {
            did_shutdown = handle_step(i, step, &mut client, &recorded, &mut handle);
        }

        if !did_shutdown {
            let _ = client.stream.shutdown(Shutdown::Both);
        }

        // wait for the server to shutdown
        if let Some(h) = handle.take() {
            h.join().expect("server thread join failed");
        }
    }

    fn handle_step(
        i: usize,
        step: Step,
        client: &mut SyncTestClient,
        recorded: &RecordedCalls,
        handle: &mut Option<JoinHandle<()>>,
    ) -> bool {
        match step {
            Step::Request { tag, req, resp } => {
                let rmsg = client.send_sync(tag, req).unwrap();
                assert_eq!(rmsg, Rmessage { tag, content: resp }, "step {i}");
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
                assert_eq!(rmsg, Rmessage::new(tag, resp), "step {i}");
            }

            Step::AssertCalls { calls } => assert_eq!(recorded.take(), calls, "step {i}"),

            Step::CloseStream => {
                let _ = client.stream.shutdown(Shutdown::Both);
                if let Some(h) = handle.take() {
                    h.join().expect("server thread join failed");
                }
                return true;
            }
        }

        false
    }
}
