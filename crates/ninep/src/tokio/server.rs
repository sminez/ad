//! Asynchronous [Server] implementation using tokio's [AsyncRead][0] and [AsyncWrite][1].
//!
//!  [0]: tokio::io::AsyncRead
//!  [1]: tokio::io::AsyncWrite
use crate::{
    Result,
    fs::{FileMeta, FileType, IoUnit, Mode, Perm, Stat, WStat},
    sansio::{
        protocol::{Data, RawStat, Rdata, Tdata, Tmessage},
        server::{
            Attached, E_CREATE_NON_DIR, E_FID_ALREADY_OPEN, E_ILLEGAL_CREATE_NAME,
            E_ILLEGAL_DIRECTORY_WRITE, E_UNKNOWN_FID, Either, FidMeta, Session, SessionType,
            Unattached,
        },
    },
    sync::server::Serve9p,
    tokio::{AsyncNineP, AsyncStream},
};
use simple_coro::CoroState;
use std::{collections::btree_map::Entry, fs, future::Future, mem::size_of, path::PathBuf};
use tokio::{
    net::{TcpListener, UnixListener},
    sync::mpsc::{Receiver, UnboundedSender, channel, unbounded_channel},
    task::{JoinHandle, spawn},
};

// re-exports
pub use crate::sansio::server::{ClientId, Server, socket_dir, socket_path};

/// The outcome of a client attempting to [read](AsyncServe9p::read) a given file.
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

async fn tcp_socket(port: u16) -> TcpListener {
    let addr = format!("127.0.0.1:{port}");
    TcpListener::bind(addr).await.unwrap()
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
/// as such, [AsyncServe9p] only needs to worry about maintaining `qids` for resources.
///
/// The source code of [Server] is a useful reference for those wanting to learn more.
pub trait AsyncServe9p: Send + Sync + 'static {
    // #[allow(unused_variables)]
    // fn auth(&self, afid: u32, uname: &str, aname: &str) -> impl Future<Output=Result<Qid>> + Send {
    //     async { Err("authentication not required".to_string()) }
    // }

    /// Lookup a child node under a known parent directory by name.
    ///
    /// `9p` Twalk messages received by the server will specify a full path from a known parent
    /// to a target child. This method is called for each element of that path in sequence in order,
    /// stopping either when the target is reached or some element of the path returns an error.
    ///
    /// [Server] will ensure that this method is only called for known parents who have previously
    /// been identified has having [FileType::Directory].
    fn walk(
        &self,
        cid: ClientId,
        parent_qid: u64,
        child: &str,
        uname: &str,
    ) -> impl Future<Output = Result<FileMeta>> + Send;

    /// Open an existing file in the requested mode for subsequent I/O via [read](AsyncServe9p::read) and
    /// [write](AsyncServe9p::write) calls.
    ///
    /// The return of this method is an [IoUnit] used to inform the client of the maximum number of
    /// bytes that will be supported per read/write call on this resource.
    fn open(
        &self,
        cid: ClientId,
        qid: u64,
        mode: Mode,
        uname: &str,
    ) -> impl Future<Output = Result<IoUnit>> + Send;

    /// Clunk a currently open file.
    #[allow(unused_variables)]
    fn clunk(&self, cid: ClientId, qid: u64) -> impl Future<Output = ()> + Send {
        async {}
    }

    /// Create a new file in the given parent directory.
    fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> impl Future<Output = Result<(FileMeta, IoUnit)>> + Send;

    /// Read `count` bytes from the requested file starting from the given `offset`.
    fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> impl Future<Output = Result<ReadOutcome>> + Send;

    /// List the contents of the given directory.
    fn read_dir(
        &self,
        cid: ClientId,
        qid: u64,
        uname: &str,
    ) -> impl Future<Output = Result<Vec<Stat>>> + Send;

    /// Write the given `data` to the requested file starting at `offset`
    fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> impl Future<Output = Result<usize>> + Send;

    /// Remove the requested file from the filesystem.
    fn remove(
        &self,
        cid: ClientId,
        qid: u64,
        uname: &str,
    ) -> impl Future<Output = Result<()>> + Send;

    /// Request a machine independent "directory entry" for the given resource.
    fn stat(
        &self,
        cid: ClientId,
        qid: u64,
        uname: &str,
    ) -> impl Future<Output = Result<Stat>> + Send;

    /// Attempt to set the machine independent "directory entry" for the given resource.
    fn write_stat(
        &self,
        cid: ClientId,
        qid: u64,
        wstat: WStat,
        uname: &str,
    ) -> impl Future<Output = Result<()>> + Send;
}

/// Helper trait for auto-implementing [AsyncServe9p] using an existing synchronous [Serve9p]
/// implementation.
///
/// Each method will implemented by delegating to the existing synchronous implementation.
pub trait AsyncServe9pFromSync: Serve9p {}

impl<T> AsyncServe9p for T
where
    T: AsyncServe9pFromSync,
{
    async fn walk(
        &self,
        cid: ClientId,
        parent_qid: u64,
        child: &str,
        uname: &str,
    ) -> Result<FileMeta> {
        <T as Serve9p>::walk(self, cid, parent_qid, child, uname)
    }

    async fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Result<IoUnit> {
        <T as Serve9p>::open(self, cid, qid, mode, uname)
    }

    async fn clunk(&self, cid: ClientId, qid: u64) {
        <T as Serve9p>::clunk(self, cid, qid);
    }

    async fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Result<(FileMeta, IoUnit)> {
        <T as Serve9p>::create(self, cid, parent, name, perm, mode, uname)
    }

    async fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> Result<ReadOutcome> {
        use crate::sync::server::ReadOutcome as SyncReadOutcome;

        let ro = match <T as Serve9p>::read(self, cid, qid, offset, count, uname)? {
            SyncReadOutcome::Immediate(data) => ReadOutcome::Immediate(data),
            SyncReadOutcome::Blocked(srx) => {
                let (tx, rx) = channel(1);

                tokio::spawn(async move {
                    if let Ok(data) = srx.recv() {
                        _ = tx.send(data).await;
                    }
                });

                ReadOutcome::Blocked(rx)
            }
        };

        Ok(ro)
    }

    async fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Vec<Stat>> {
        <T as Serve9p>::read_dir(self, cid, qid, uname)
    }

    async fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Result<usize> {
        <T as Serve9p>::write(self, cid, qid, offset, data, uname)
    }

    async fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> Result<()> {
        <T as Serve9p>::remove(self, cid, qid, uname)
    }

    async fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> Result<Stat> {
        <T as Serve9p>::stat(self, cid, qid, uname)
    }

    async fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Result<()> {
        <T as Serve9p>::write_stat(self, cid, qid, wstat, uname)
    }
}

impl<S> Server<S>
where
    S: AsyncServe9p,
{
    /// Bind this server to the specified port and serve over a tcp socket.
    pub fn serve_tcp_async(mut self, port: u16) -> JoinHandle<()> {
        spawn(async move {
            let listener = tcp_socket(port).await;
            loop {
                if let Ok((stream, _addr)) = listener.accept().await {
                    let session = self.new_session(stream);
                    spawn(session.handle_connection_async());
                }
            }
        })
    }

    /// Bind this server to the specified socket name and serve over a unix socket created under
    /// the default [socket_dir].
    pub fn serve_socket_async(self, socket_name: impl Into<String>) -> JoinHandle<()> {
        let socket_name = socket_name.into();
        let path = socket_dir().join(socket_name);

        self.serve_socket_with_custom_path_async(path)
    }

    /// Bind this server to the specified absolute path and serve over a unix socket created under
    /// an arbitrary directory.
    ///
    /// It is recommended that you use [Server::serve_socket_async] for most purposes so that your
    /// server creates its socket in the known default [socket_dir].
    pub fn serve_socket_with_custom_path_async(mut self, socket_path: PathBuf) -> JoinHandle<()> {
        spawn(async move {
            let sock = unix_socket(socket_path);
            loop {
                if let Ok((stream, _addr)) = sock.listener.accept().await {
                    let session = self.new_session(stream);
                    spawn(session.handle_connection_async());
                }
            }
        })
    }
}

impl<T, S, U> Session<T, S, U>
where
    T: SessionType,
    S: AsyncServe9p,
    U: AsyncStream,
{
    async fn reply_async(&mut self, tag: u16, resp: Result<Rdata>) {
        self.stream.reply(self.session_state.msize, tag, resp).await
    }
}

impl<S, U> Session<Unattached, S, U>
where
    S: AsyncServe9p,
    U: AsyncStream,
{
    async fn handle_connection_async(mut self) {
        loop {
            let t = match Tmessage::read_from(self.msize, &self.buf, &mut self.stream).await {
                Ok(t) => t,
                Err(_) => return,
            };

            match self.handle_tmessage_unattached(t) {
                Either::L((tag, resp)) => self.reply_async(tag, resp).await,
                Either::R((tag, st, aqid)) => {
                    self.reply_async(tag, Ok(Rdata::Attach { aqid })).await;
                    return self.into_attached(st).handle_connection_async().await;
                }
            }
        }
    }
}

impl<S, U> Session<Attached, S, U>
where
    S: AsyncServe9p,
    U: AsyncStream,
{
    /// Explicitly clunk all
    async fn clunk_and_clear_async(&mut self) {
        for meta in self.state.fids.values() {
            self.s.clunk(self.client_id, meta.qid).await;
        }
        self.state.fids.clear();
    }

    async fn handle_connection_async(mut self) {
        use Tdata::*;
        let (tx, mut rx) = unbounded_channel();

        loop {
            let Tmessage { tag, content } = tokio::select! {
                // Blocked read came through so send it to the client
                Some((tag, data)) = rx.recv() => {
                    self.stream
                        .reply(self.msize, tag, Ok(Rdata::Read { data: Data(data) }))
                        .await;
                    continue;
                },
                res = Tmessage::read_from(self.msize, &self.buf, &mut self.stream) => match res {
                    Ok(t) => t,
                    Err(_) => return self.clunk_and_clear_async().await,
                },
                else => continue,
            };

            let resp = match content {
                Version { msize, version } => {
                    let resp = self.handle_version(msize, version);
                    self.clunk_and_clear_async().await;

                    Ok(resp)
                }
                Auth { .. } | Attach { .. } => Err("session is already attached".into()),
                Flush { .. } => Ok(Rdata::Flush {}),

                Walk {
                    fid,
                    new_fid,
                    wnames,
                } => self.handle_walk_async(fid, new_fid, wnames).await,
                Clunk { fid } => self.handle_clunk_async(fid).await,
                Stat { fid } => self.handle_stat_async(fid).await,
                Open { fid, mode } => self.handle_open_async(fid, Mode::new(mode)).await,
                Create {
                    fid,
                    name,
                    perm,
                    mode,
                } => {
                    self.handle_create_async(fid, name, Perm::new(perm), Mode::new(mode))
                        .await
                }
                Read { fid, offset, count } => {
                    match self.handle_read_async(tag, fid, offset, count, &tx).await {
                        Ok(Some(resp)) => Ok(resp),
                        Err(err) => Err(err),
                        Ok(None) => continue,
                    }
                }
                Write { fid, offset, data } => self.handle_write_async(fid, offset, data.0).await,
                Remove { fid } => self.handle_remove_async(fid).await,
                Wstat { fid, stat, .. } => self.handle_wstat_async(fid, stat).await,
            };

            self.reply_async(tag, resp).await;
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
    async fn handle_walk_async(
        &mut self,
        fid: u32,
        new_fid: u32,
        wnames: Vec<String>,
    ) -> Result<Rdata> {
        let client_id = self.client_id;
        let mut coro = self
            .session_state
            .handle_attached_walk(fid, new_fid, &wnames);

        loop {
            coro = match coro.resume() {
                CoroState::Complete(res) => return res,
                CoroState::Pending(c, (qid, name, uname)) => {
                    let res = self.s.walk(client_id, qid, name, uname).await;
                    c.send(res)
                }
            };
        }
    }

    async fn handle_clunk_async(&mut self, fid: u32) -> Result<Rdata> {
        match self.state.fids.entry(fid) {
            Entry::Occupied(ent) => {
                let meta = ent.remove();
                self.s.clunk(self.client_id, meta.qid).await;

                Ok(Rdata::Clunk {})
            }
            Entry::Vacant(_) => Err(E_UNKNOWN_FID.to_string()),
        }
    }

    async fn handle_stat_async(&mut self, fid: u32) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;
        let s = self
            .s
            .stat(self.client_id, fm.qid, &self.state.uname)
            .await?;
        let stat: RawStat = s.into();
        let size = stat.size + size_of::<u16>() as u16;

        Ok(Rdata::Stat { size, stat })
    }

    async fn handle_wstat_async(&mut self, fid: u32, raw_stat: RawStat) -> Result<Rdata> {
        let wstat: WStat = raw_stat.into();
        let fm = self.try_file_meta(fid)?;
        self.s
            .write_stat(self.client_id, fm.qid, wstat, &self.state.uname)
            .await?;

        Ok(Rdata::Wstat {})
    }

    async fn handle_open_async(&mut self, fid: u32, mode: Mode) -> Result<Rdata> {
        if self.try_fid_meta(fid)?.is_open {
            return Err(E_FID_ALREADY_OPEN.to_string());
        }

        let fm = self.try_file_meta(fid)?;
        let iounit = self
            .s
            .open(self.client_id, fm.qid, mode, &self.state.uname)
            .await?;

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

    async fn handle_create_async(
        &mut self,
        fid: u32,
        name: String,
        perm: Perm,
        mode: Mode,
    ) -> Result<Rdata> {
        if name == "." || name == ".." {
            return Err(E_ILLEGAL_CREATE_NAME.to_string());
        }

        let fm = self.try_file_meta(fid)?;
        if fm.ty != FileType::Directory {
            return Err(E_CREATE_NON_DIR.to_string());
        }

        let (fm, iounit) = self
            .s
            .create(self.client_id, fm.qid, &name, perm, mode, &self.state.uname)
            .await?;

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
    async fn handle_read_async(
        &mut self,
        tag: u16,
        fid: u32,
        offset: u64,
        count: u32,
        tx: &UnboundedSender<(u16, Vec<u8>)>,
    ) -> Result<Option<Rdata>> {
        let cid = self.client_id;
        let coro = self.session_state.handle_attached_read(fid, offset, count);
        let (offset, count) = (offset as usize, count as usize);

        match coro.resume() {
            CoroState::Complete(res) => res,
            CoroState::Pending(c, Either::L((qid, uname))) => {
                let stats = self.s.read_dir(cid, qid, uname).await?;
                c.send(stats).resume().unwrap()
            }
            CoroState::Pending(_, Either::R((qid, uname))) => {
                let outcome = self.s.read(cid, qid, offset, count, uname).await?;
                match outcome {
                    ReadOutcome::Immediate(data) => Ok(Some(Rdata::Read { data: Data(data) })),
                    ReadOutcome::Blocked(mut chan) => {
                        let tx = tx.clone();
                        spawn(async move {
                            let data = chan.recv().await.unwrap_or_default();
                            tx.send((tag, data))
                        });

                        Ok(None)
                    }
                }
            }
        }
    }

    async fn handle_write_async(&mut self, fid: u32, offset: u64, data: Vec<u8>) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;

        if fm.ty == FileType::Directory {
            return Err(E_ILLEGAL_DIRECTORY_WRITE.to_string());
        } else if offset > u32::MAX as u64 {
            return Err(format!("offset too large: {offset} > {}", u32::MAX));
        }

        let count = self
            .s
            .write(
                self.client_id,
                fm.qid,
                offset as usize,
                data,
                &self.state.uname,
            )
            .await? as u32;

        Ok(Rdata::Write { count })
    }

    async fn handle_remove_async(&mut self, fid: u32) -> Result<Rdata> {
        let fm = self.try_file_meta(fid)?;
        let res = self
            .s
            .remove(self.client_id, fm.qid, &self.state.uname)
            .await;

        // ensure that we clunk before erroring
        self.s.clunk(self.client_id, fm.qid).await;
        res?;

        Ok(Rdata::Remove {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        generate_test_suite,
        sansio::protocol::Rmessage,
        test_utils::{AsyncTestClient, TestFs, cases::Step},
    };
    use tokio::{
        io::{AsyncWriteExt, duplex},
        task,
    };

    macro_rules! run_one {
        ($case:expr) => {
            // Setup the client and server
            let fs = TestFs::default();
            let recorded = fs.calls();
            let (client_stream, server_stream) = duplex(8192);
            let mut client = AsyncTestClient::new(client_stream);
            let mut server = Server::new(fs);

            let mut handle = Some(task::spawn(async move {
                server
                    .new_session(server_stream)
                    .handle_connection_async()
                    .await;
            }));

            // Run the test case
            let mut next_tag = 0;
            let mut did_shutdown = false;

            for step in $case {
                match step {
                    Step::Request { req, resp } => {
                        let tag = next_tag;
                        next_tag += 1;

                        let rmsg = client.send_async(tag, req).await.unwrap();
                        assert_eq!(rmsg, Rmessage { tag, content: resp });
                    }

                    Step::AssertCalls { calls } => assert_eq!(recorded.take(), calls),

                    Step::CloseStream => {
                        let _ = client.stream.shutdown().await;
                        if let Some(h) = handle.take() {
                            h.await.expect("server task join failed");
                        }
                        did_shutdown = true;
                    }
                }
            }

            if !did_shutdown {
                let _ = client.stream.shutdown().await;
            }

            if let Some(h) = handle.take() {
                h.await.expect("server task join failed");
            }
        };
    }

    generate_test_suite!(tokio, run_one);
}
