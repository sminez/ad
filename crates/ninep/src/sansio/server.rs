//! Traits and structs for implementing a 9p fileserver
use crate::{
    Result,
    fs::{Mode, PermCheck, QID_ROOT, Stat, WStat},
    sansio::protocol::{
        DEFAULT_MSIZE, FileType, MAXWELEM, NineP, Qid, RawStat, Rdata, SharedBuf, Tdata, Tmessage,
    },
};
use parking_lot::RwLock;
use simple_coro::{Coro, Handle, ReadyCoro};
use std::{
    cmp::min,
    collections::{BTreeMap, HashSet, VecDeque},
    env,
    future::Future,
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    sync::Arc,
};

/// Marker afid to denode that auth is not required for establishing connections
pub const AFID_NO_AUTH: u32 = u32::MAX;

// Error messages
pub(crate) const E_ALREADY_ATTACHED: &str = "session is already attached";
pub(crate) const E_AUTH_NOT_REQUIRED: &str = "authentication not required";
pub(crate) const E_CREATE_NON_DIR: &str = "create in non-directory";
pub(crate) const E_DUPLICATE_FID: &str = "duplicate fid";
pub(crate) const E_EXCLUSIVE_ALREADY_OPEN: &str = "exclusive file already open";
pub(crate) const E_FILE_NOT_OPEN: &str = "file not open";
pub(crate) const E_ILLEGAL_CREATE_NAME: &str = "creating files named '.' or '..' is not allowed";
pub(crate) const E_ILLEGAL_DIRECTORY_WRITE: &str = "illegal write to directory";
pub(crate) const E_INVALID_OFFSET: &str = "invalid offset for read on directory";
pub(crate) const E_NO_VERSION_MESSAGE: &str = "first message must be Tversion";
pub(crate) const E_OVER_MAXWELEM: &str = "too many walk elements";
pub(crate) const E_PERMISSION_DENIED: &str = "permission denied";
pub(crate) const E_UNATTACHED: &str = "session is not attached";
pub(crate) const E_UNKNOWN_FID: &str = "unknown fid";
pub(crate) const E_UNKNOWN_FILE: &str = "unknown file";
pub(crate) const E_UNKNOWN_ROOT: &str = "unknown root directory";
pub(crate) const E_WALK_OPEN_FID: &str = "cannot clone open fid";
pub(crate) const E_WALK_NON_DIR: &str = "walk in non-directory";
pub(crate) const E_WSTAT_WRONG_QID: &str = "wstat qid does not match target";

pub(crate) const UNKNOWN_VERSION: &str = "unknown";
pub(crate) const SUPPORTED_VERSION: &str = "9P2000";

const DEFAULT_DISPLAY_VALUE: &str = ":0";

/// Determine the 9p socket directory based on the USER and DISPLAY environment variables
pub fn socket_dir() -> PathBuf {
    let uname = env::var("USER").unwrap();
    let display = env::var("DISPLAY").unwrap_or(String::from(DEFAULT_DISPLAY_VALUE));

    PathBuf::from("/tmp").join(format!("ns.{uname}.{display}"))
}

/// The unix socket path that will be used for a given server name.
pub fn socket_path(name: impl AsRef<Path>) -> PathBuf {
    socket_dir().join(name)
}

/// An opaque client ID that can be used by server implementations to determine which client a
/// request originated from by comparing equality.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClientId(pub(crate) u64);

#[derive(Debug)]
pub(crate) enum Either<L, R> {
    L(L),
    R(R),
}

/// A 9p server wrapping an `S` that must implement an IO specific handler trait to provide the
/// actual filesystem implementation.
#[derive(Debug)]
pub struct Server<S>
where
    S: Send,
{
    pub(crate) s: Arc<S>,
    pub(crate) roots: BTreeMap<String, u64>,
    pub(crate) qids: Arc<RwLock<BTreeMap<u64, QidMeta>>>,
    pub(crate) next_client_id: u64,
}

impl<S> Server<S>
where
    S: Send,
{
    /// Create a new file server with a single anonymous root (aname will be "") and
    /// qid of [QID_ROOT].
    pub fn new(s: S) -> Self {
        Self::new_with_roots(s, [("".to_string(), QID_ROOT)].into_iter().collect())
    }

    /// Create a new file server with the given roots for clients to attach to.
    pub fn new_with_roots(s: S, roots: BTreeMap<String, u64>) -> Self {
        let qids = Arc::new(RwLock::new(
            roots
                .iter()
                .map(|(_, &qid)| (qid, QidMeta::new(Qid::dir(qid), None)))
                .collect(),
        ));

        Self {
            s: Arc::new(s),
            roots,
            qids,
            next_client_id: 0,
        }
    }

    /// Construct a new unattached [Session] over the provided [Stream]
    pub(crate) fn new_session<U>(&mut self, stream: U) -> Session<Unattached, S, U> {
        let session = Session::new_unattached(
            ClientId(self.next_client_id),
            self.roots.clone(),
            self.s.clone(),
            self.qids.clone(),
            stream,
            SharedBuf::default(),
        );
        self.next_client_id += 1;

        session
    }
}

/// Marker trait for implementing a type state for Session
pub(crate) trait SessionType: Send {}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Unattached {
    /// Whether or not we have seen a successful Version Tmessage
    pub(crate) seen_version: bool,
}

impl SessionType for Unattached {}

#[derive(Debug, Clone)]
pub(crate) struct Attached {
    /// uname of the attached user
    pub(crate) uname: String,
    /// Map of client fids to server file metadata
    pub(crate) fids: BTreeMap<u32, FidMeta>,

    // Copy of SessionState data so we can clean up server level open state on drop
    client_id: ClientId,
    qids: Arc<RwLock<BTreeMap<u64, QidMeta>>>,
}

impl SessionType for Attached {}

impl Drop for Attached {
    fn drop(&mut self) {
        let mut guard = self.qids.write();
        for fm in self.fids.values() {
            if let Some(meta) = guard.get_mut(&fm.qid) {
                meta.opened_by.remove(&self.client_id);
            }
        }
    }
}

impl Attached {
    fn new(
        uname: String,
        root_fid: u32,
        root_qid: u64,
        client_id: ClientId,
        qids: Arc<RwLock<BTreeMap<u64, QidMeta>>>,
    ) -> Self {
        Self {
            uname,
            fids: [(root_fid, FidMeta::closed(root_qid))]
                .into_iter()
                .collect(),
            client_id,
            qids,
        }
    }

    /// Whether or not the given fid requires removal on close based on its [Mode].
    ///
    /// Returns `false` for unknown fids.
    pub(crate) fn fid_requires_remove_on_close(&self, fid: u32) -> bool {
        self.fids
            .get(&fid)
            .map(|meta| meta.requires_remove_on_close())
            .unwrap_or(false)
    }
}

/// Internal state for a running client session other than the user provided filesystem
/// implementation. We keep this separate in order to allow for splitting borrows between this
/// state and the filesystem impl when creating coroutine based helper methods.
#[derive(Debug)]
pub(crate) struct SessionState<T>
where
    T: SessionType,
{
    pub(crate) state: T,
    pub(crate) client_id: ClientId,
    pub(crate) msize: u32,
    pub(crate) roots: BTreeMap<String, u64>,
    pub(crate) qids: Arc<RwLock<BTreeMap<u64, QidMeta>>>,
}

impl<T> SessionState<T>
where
    T: SessionType,
{
    /// Run a closure with access to the shared server-level Qid map.
    ///
    /// # WARNING
    /// Calling this method locks the shared server-level state so closures _must_ be quick to
    /// execute.
    pub(crate) fn with_shared_qids<F, U>(&self, f: F) -> U
    where
        F: FnOnce(&BTreeMap<u64, QidMeta>) -> U,
    {
        f(&self.qids.read())
    }

    /// Run a closure with mutable access to the shared server-level Qid map.
    ///
    /// # WARNING
    /// Calling this method locks the shared server-level state so closures _must_ be quick to
    /// execute.
    pub(crate) fn with_shared_qids_mut<F, U>(&self, f: F) -> U
    where
        F: FnOnce(&mut BTreeMap<u64, QidMeta>) -> U,
    {
        f(&mut self.qids.write())
    }

    pub(crate) fn parent_qid(&self, qid: u64) -> Option<u64> {
        self.with_shared_qids(|qids| qids.get(&qid).and_then(|qm| qm.parent_qid))
    }

    pub(crate) fn qid(&self, qid: u64) -> Option<Qid> {
        self.with_shared_qids(|qids| qids.get(&qid).map(|qm| qm.qid))
    }
}

impl SessionState<Attached> {
    pub(crate) fn try_fid_meta(&self, fid: u32) -> Result<FidMeta> {
        self.state
            .fids
            .get(&fid)
            .copied()
            .ok_or_else(|| E_UNKNOWN_FID.to_string())
    }

    pub(crate) fn try_map_fid(&self, fid: u32) -> Result<Qid> {
        let fm = self.try_fid_meta(fid)?;
        self.qid(fm.qid).ok_or_else(|| E_UNKNOWN_FID.to_string())
    }

    pub(crate) fn try_add_client_id_to_open_qids(&self, fid: u32) -> Result<()> {
        let qid = self.try_fid_meta(fid)?.qid;

        self.with_shared_qids_mut(|qids| match qids.get_mut(&qid) {
            Some(meta) if meta.is_exclusive_and_open(self.client_id) => {
                Err(E_EXCLUSIVE_ALREADY_OPEN.into())
            }
            Some(meta) => {
                meta.opened_by.insert(self.client_id);
                Ok(())
            }
            None => Err(E_UNKNOWN_FILE.into()),
        })
    }

    pub(crate) fn remove_client_id_from_open_qids(&self, qid: u64) {
        self.with_shared_qids_mut(|qids| {
            if let Some(meta) = qids.get_mut(&qid) {
                meta.opened_by.remove(&self.client_id);
            }
        });
    }

    #[expect(clippy::type_complexity)]
    pub(crate) fn handle_attached_walk<'s>(
        &'s mut self,
        fid: u32,
        new_fid: u32,
        wnames: Vec<String>,
    ) -> ReadyCoro<
        (u64, String),
        Result<Qid>,
        Result<Vec<Qid>>,
        impl Future<Output = Result<Vec<Qid>>> + use<'s>,
    > {
        Coro::from(
            move |handle: Handle<(u64, String), Result<Qid>>| async move {
                if wnames.len() > MAXWELEM {
                    return Err(E_OVER_MAXWELEM.to_string());
                } else if new_fid != fid && self.state.fids.contains_key(&new_fid) {
                    return Err(E_DUPLICATE_FID.to_string());
                }

                if self.try_fid_meta(fid)?.is_open() {
                    return Err(E_WALK_OPEN_FID.to_string());
                }

                let qid = self.try_map_fid(fid)?;

                if wnames.is_empty() {
                    self.state.fids.insert(new_fid, FidMeta::closed(qid.path));
                    return Ok(vec![]);
                } else if matches!(qid.ty, FileType::FILE) {
                    return Err(E_WALK_NON_DIR.to_string());
                }

                let mut wqids = Vec::with_capacity(wnames.len());
                let mut qid_path = qid.path;

                for name in wnames.iter() {
                    match handle.yield_value((qid_path, name.clone())).await {
                        Ok(elem) => {
                            let parent = qid_path;
                            qid_path = elem.path;
                            wqids.push(elem);
                            self.with_shared_qids_mut(|qids| {
                                qids.entry(elem.path)
                                    .or_insert(QidMeta::new(elem, Some(parent)));
                            });
                        }
                        Err(_) => break,
                    }
                }

                // Spec: first element failure must be Rerror, not Rwalk with zero qids
                if wqids.is_empty() {
                    return Err(E_UNKNOWN_FILE.to_string());
                }

                // new_fid is only bound when all elements were walked successfully
                if wqids.len() == wnames.len() {
                    let qid = wqids.last().expect("empty was handled above").path;
                    self.state.fids.insert(new_fid, FidMeta::closed(qid));
                }

                Ok(wqids)
            },
        )
    }

    #[expect(clippy::type_complexity)]
    pub(crate) fn handle_attached_read<'s>(
        &'s mut self,
        fid: u32,
        offset: u64,
        count: u32,
    ) -> ReadyCoro<
        Either<u64, u64>, // L=read_dir R=read
        Vec<Stat>,        // we never send or use a value in response to a read, only read-dir
        Result<Option<Rdata>>,
        impl Future<Output = Result<Option<Rdata>>> + use<'s>,
    > {
        Coro::from(
            move |handle: Handle<Either<u64, u64>, Vec<Stat>>| async move {
                let qid = self.try_map_fid(fid)?;
                if offset > u32::MAX as u64 {
                    return Err(format!("offset too large: {offset} > {}", u32::MAX));
                }

                let stats = if qid.ty == FileType::DIRECTORY {
                    handle.yield_value(Either::L(qid.path)).await
                } else {
                    handle.yield_value(Either::R(qid.path)).await;
                    return Ok(None); // processing of the ReadOutcome is handled by the caller
                };

                let mut buf = Vec::with_capacity(count as usize);
                let mut to_skip = offset as usize;

                for stat in stats.into_iter() {
                    self.with_shared_qids_mut(|qids| {
                        qids.entry(stat.qid.path)
                            .or_insert(QidMeta::new(stat.qid, Some(qid.path)));
                    });
                    let rstat: RawStat = stat.into();
                    let tmp = rstat.write_9p_bytes().unwrap();

                    if to_skip != 0 {
                        if tmp.len() > to_skip {
                            return Err(E_INVALID_OFFSET.to_string());
                        } else {
                            to_skip -= tmp.len();
                            continue;
                        }
                    }

                    if buf.len() + tmp.len() > count as usize {
                        break;
                    }
                    buf.extend(tmp);
                }

                Ok(Some(Rdata::read(buf)))
            },
        )
    }

    pub(crate) fn handle_perm_check<'s>(
        &'s self,
        stat: &'s Stat,
        user_is_in_group: bool,
        mode: Mode,
    ) -> ReadyCoro<(), (Stat, bool), Result<()>, impl Future<Output = Result<()>> + use<'s>> {
        Coro::from(move |handle: Handle<(), (Stat, bool)>| async move {
            match stat.check_user_permissions(&self.state.uname, user_is_in_group, mode) {
                PermCheck::Denied => return Err(E_PERMISSION_DENIED.into()),
                PermCheck::Allowed => return Ok(()),
                PermCheck::NeedWriteOnParent => (),
            }

            let (parent_stat, user_is_in_group) = handle.yield_value(()).await;
            if parent_stat.can_rename_or_remove_child(&self.state.uname, user_is_in_group) {
                Ok(())
            } else {
                Err(E_PERMISSION_DENIED.to_string())
            }
        })
    }

    pub(crate) fn check_wstat_perms<'s>(
        &'s self,
        stat: &'s Stat,
        wstat: &'s WStat,
        user_is_in_group: bool,
    ) -> ReadyCoro<(), (Stat, bool), Result<()>, impl Future<Output = Result<()>> + use<'s>> {
        Coro::from(move |handle: Handle<(), (Stat, bool)>| async move {
            if stat.qid.path != wstat.qid.path {
                return Err(E_WSTAT_WRONG_QID.into());
            }

            // Always illegal to change the directory bit
            let stat_is_dir = stat.qid.ty == FileType::DIRECTORY;
            let wstat_is_dir = wstat.qid.ty == FileType::DIRECTORY;
            if stat_is_dir != wstat_is_dir {
                return Err(E_PERMISSION_DENIED.into());
            }

            // Modify perms, last modified or group requires owner
            if (wstat.perms.is_some() | wstat.last_modified.is_some() | wstat.group.is_some())
                && stat.owner != self.state.uname
            {
                return Err(E_PERMISSION_DENIED.into());
            }

            // Modify length requires write on the file itself
            if wstat.n_bytes.is_some() {
                match stat.check_user_permissions(&self.state.uname, user_is_in_group, Mode::WRITE)
                {
                    PermCheck::NeedWriteOnParent => unreachable!("only checking write"),
                    PermCheck::Denied => return Err(E_PERMISSION_DENIED.into()),
                    PermCheck::Allowed => (),
                }
            }

            // Modifying name requires write on parent directory
            if wstat.name.is_some() {
                let (parent_stat, user_is_in_parent_group) = handle.yield_value(()).await;
                if !parent_stat
                    .can_rename_or_remove_child(&self.state.uname, user_is_in_parent_group)
                {
                    return Err(E_PERMISSION_DENIED.into());
                }
            }

            Ok(())
        })
    }
}

/// A connected client session over a given stream
#[derive(Debug)]
pub(crate) struct Session<T, S, U>
where
    T: SessionType,
    S: Send,
{
    pub(crate) s: Arc<S>,
    pub(crate) stream: U,
    pub(crate) session_state: SessionState<T>,
    pub(crate) buf: SharedBuf,
}

impl<T, S, U> Deref for Session<T, S, U>
where
    T: SessionType,
    S: Send,
{
    type Target = SessionState<T>;

    fn deref(&self) -> &Self::Target {
        &self.session_state
    }
}

impl<T, S, U> DerefMut for Session<T, S, U>
where
    T: SessionType,
    S: Send,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.session_state
    }
}

impl<T, S, U> Session<T, S, U>
where
    T: SessionType,
    S: Send,
{
    /// The version request negotiates the protocol version and message size to be used on the
    /// connection and initializes the connection for I/O. Tversion must be the first message sent
    /// on the 9P connection, and the client cannot issue any further requests until it has
    /// received the Rversion reply. The tag should be NOTAG (value (ushort)~0) for a version
    /// message.
    /// The client suggests a maximum message size, msize, that is the maximum length, in bytes, it
    /// will ever generate or expect to receive in a single 9P message. This count includes all 9P
    /// protocol data, starting from the size field and extending through the message, but excludes
    /// enveloping transport protocols. The server responds with its own maximum, msize, which must
    /// be less than or equal to the client’s value. Thenceforth, both sides of the connection must
    /// honor this limit.
    /// The version string identifies the level of the protocol. The string must always begin with
    /// the two characters “9P”. If the server does not understand the client’s version string, it
    /// should respond with an Rversion message (not Rerror) with the version string the 7
    /// characters “unknown”.
    /// The server may respond with the client’s version string, or a version string identifying an
    /// earlier defined protocol version. Currently, the only defined version is the 6 characters
    /// “9P2000”. Version strings are defined such that, if the client string contains one or more
    /// period characters, the initial substring up to but not including any single period in the
    /// version string defines a version of the protocol. After stripping any such period-separated
    /// suffix, the server is allowed to respond with a string of the form 9Pnnnn, where nnnn is
    /// less than or equal to the digits sent by the client.
    /// The client and server will use the protocol version defined by the server’s response for
    /// all subsequent communication on the connection.
    /// A successful version request initializes the connection. All outstanding I/O on the
    /// connection is aborted; all active fids are freed (‘clunked’) automatically. The set of
    /// messages between version requests is called a session.
    pub(crate) fn handle_version(&mut self, msize: u32, version: String) -> Rdata {
        let server_version = if version != SUPPORTED_VERSION {
            UNKNOWN_VERSION
        } else {
            SUPPORTED_VERSION
        };

        self.msize = min(DEFAULT_MSIZE, msize);

        Rdata::version(self.msize, server_version)
    }

    /// If the client does wish to authenticate, it must acquire and validate an afid using an auth
    /// message before doing the attach.
    /// The auth message contains afid, a new fid to be established for authentication, and the
    /// uname and aname that will be those of the following attach message. If the server does not
    /// require authentication, it returns Rerror to the Tauth message.
    /// If the server does require authentication, it returns aqid defining a file of type QTAUTH
    /// (see intro(9P)) that may be read and written (using read and write messages in the usual
    /// way) to execute an authentication protocol. That protocol’s definition is not part of 9P
    /// itself.
    /// Once the protocol is complete, the same afid is presented in the attach message for the
    /// user, granting entry. The same validated afid may be used for multiple attach messages with
    /// the same uname and aname.
    #[expect(unused_variables)]
    pub(crate) fn handle_auth(&mut self, afid: u32, uname: String, aname: String) -> Result<Rdata> {
        // TODO: handle auth
        // let aqid = self.s.lock().unwrap().auth(afid, &uname, &aname)?;
        // Ok(Rdata::Auth { aqid })

        Err(E_AUTH_NOT_REQUIRED.to_string())
    }
}

impl<S, U> Session<Unattached, S, U>
where
    S: Send,
{
    fn new_unattached(
        client_id: ClientId,
        roots: BTreeMap<String, u64>,
        s: Arc<S>,
        qids: Arc<RwLock<BTreeMap<u64, QidMeta>>>,
        stream: U,
        buf: SharedBuf,
    ) -> Self {
        Self {
            s,
            stream,
            session_state: SessionState {
                client_id,
                state: Unattached::default(),
                msize: DEFAULT_MSIZE,
                roots,
                qids,
            },
            buf,
        }
    }

    pub(crate) fn handle_tmessage_unattached(
        &mut self,
        Tmessage { tag, content }: Tmessage,
    ) -> Either<(u16, Result<Rdata>), (u16, Attached, Qid)> {
        use Tdata::*;

        let resp = match content {
            Version { msize, version } => {
                self.state.seen_version = version == SUPPORTED_VERSION;
                Ok(self.handle_version(msize, version))
            }

            Auth { afid, uname, aname } => {
                if !self.state.seen_version {
                    return Either::L((tag, Err(E_NO_VERSION_MESSAGE.to_string())));
                }

                self.handle_auth(afid, uname, aname)
            }

            Attach {
                fid,
                afid,
                uname,
                aname,
            } => {
                if !self.state.seen_version {
                    return Either::L((tag, Err(E_NO_VERSION_MESSAGE.to_string())));
                }

                let (st, aqid) = match self.handle_attach(fid, afid, uname, aname) {
                    Err(e) => return Either::L((tag, Err(e))),
                    Ok((st, aqid)) => (st, aqid),
                };

                return Either::R((tag, st, aqid));
            }

            _ => Err(E_UNATTACHED.into()),
        };

        Either::L((tag, resp))
    }

    pub(crate) fn into_attached(self, state: Attached) -> Session<Attached, S, U> {
        Session {
            s: self.s,
            stream: self.stream,
            session_state: SessionState {
                client_id: self.session_state.client_id,
                state,
                msize: self.session_state.msize,
                roots: self.session_state.roots,
                qids: self.session_state.qids,
            },
            buf: self.buf,
        }
    }

    /// The attach message serves as a fresh introduction from a user on the client machine to the
    /// server. The message identifies the user (uname) and may select the file tree to access
    /// (aname). The afid argument specifies a fid previously established by an auth message.
    /// As a result of the attach transaction, the client will have a connection to the root
    /// directory of the desired file tree, represented by fid. An error is returned if fid is
    /// already in use. The server’s idea of the root of the file tree is represented by the
    /// returned qid.
    ///
    /// If the client does not wish to authenticate the connection, or knows that authentication is
    /// not required, the afid field in the attach message should be set to NOFID, defined as
    /// (u32int)~0 in <fcall.h>.
    pub(crate) fn handle_attach(
        &mut self,
        root_fid: u32,
        _afid: u32,
        uname: String,
        aname: String,
    ) -> Result<(Attached, Qid)> {
        let root_qid = match self.roots.get(&aname) {
            Some(qid) => *qid,
            None => return Err(E_UNKNOWN_ROOT.to_string()),
        };

        // TODO: handle checking afids (AFID_NO_AUTH should be accepted if there is no auth)

        let st = Attached::new(uname, root_fid, root_qid, self.client_id, self.qids.clone());
        let aqid = self.qid(root_qid).expect("to have root qid");

        Ok((st, aqid))
    }
}

/// Internal metadata for known fids
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FidMeta {
    pub(crate) qid: u64,
    pub(crate) mode: Option<Mode>,
}

impl FidMeta {
    pub(crate) fn open(qid: u64, mode: Mode) -> Self {
        Self {
            qid,
            mode: Some(mode),
        }
    }

    pub(crate) fn closed(qid: u64) -> Self {
        Self { qid, mode: None }
    }

    pub(crate) fn is_open(&self) -> bool {
        self.mode.is_some()
    }

    pub(crate) fn requires_remove_on_close(&self) -> bool {
        self.mode
            .as_ref()
            .map(|m| m.contains(Mode::REMOVE_ON_CLOSE))
            .unwrap_or(false)
    }

    pub(crate) fn check_open_for_read(&self) -> Result<()> {
        match self.mode.as_ref() {
            Some(mode) if mode.allows_read() => Ok(()),
            Some(_) => Err(E_PERMISSION_DENIED.into()),
            None => Err(E_FILE_NOT_OPEN.into()),
        }
    }

    pub(crate) fn check_open_for_write(&self) -> Result<()> {
        match self.mode.as_ref() {
            Some(mode) if mode.allows_write() => Ok(()),
            Some(_) => Err(E_PERMISSION_DENIED.into()),
            None => Err(E_FILE_NOT_OPEN.into()),
        }
    }
}

/// Internal metadata for known qids
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct QidMeta {
    pub(crate) qid: Qid,
    pub(crate) parent_qid: Option<u64>,
    pub(crate) opened_by: HashSet<ClientId>,
}

impl QidMeta {
    pub(crate) fn new(qid: Qid, parent_qid: Option<u64>) -> Self {
        Self {
            qid,
            parent_qid,
            opened_by: HashSet::new(),
        }
    }

    pub(crate) fn is_exclusive_and_open(&self, cid: ClientId) -> bool {
        self.qid.ty == FileType::EXCLUSIVE && self.opened_by.iter().any(|id| *id != cid)
    }
}

/// We track in-flight messages so we can associate flush messages against their target `old_tag`.
/// https://9fans.github.io/plan9port/man/man9/flush.html
#[derive(Debug, Default)]
pub(crate) struct FlushHandle {
    /// Map of message tag to queued flush tags that came in while that message was being processed
    pending_flushes: BTreeMap<u16, Vec<u16>>,
}

impl FlushHandle {
    /// Mark `tag` as being pending so we are able to associate future flush messages with it.
    pub(crate) fn mark_pending(&mut self, tag: u16) {
        self.pending_flushes.entry(tag).or_default();
    }

    /// Build a coroutine that will either request that the caller immediately respond to the
    /// provided flush tag or chain it behind an existing flush.
    pub(crate) fn flush_or_chain<'s>(
        &'s mut self,
        flush_tag: u16,
        old_tag: u16,
    ) -> ReadyCoro<(), (), bool, impl Future<Output = bool> + use<'s>> {
        Coro::from(move |handle: Handle<(), ()>| async move {
            let should_flush_now =
                flush_tag == old_tag || !self.pending_flushes.contains_key(&old_tag);

            if should_flush_now {
                handle.yield_value(()).await;
                true
            } else {
                self.mark_pending(flush_tag);
                if let Some(pending) = self.pending_flushes.get_mut(&old_tag) {
                    pending.push(flush_tag);
                }
                false
            }
        })
    }

    /// Collapse any chained flushes for this tag into the complete list of all outstanding flush
    /// messages that now need to have their responses sent.
    pub(crate) fn pending_flush_tags(&mut self, tag: u16) -> Vec<u16> {
        let mut flushed = Vec::new();
        let mut to_flush: VecDeque<u16> =
            self.pending_flushes.remove(&tag).unwrap_or_default().into();

        while let Some(flush_tag) = to_flush.pop_front() {
            flushed.push(flush_tag);
            to_flush.extend(self.pending_flushes.remove(&flush_tag).unwrap_or_default());
        }

        flushed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fs::Perm;
    use jiff::Timestamp;
    use simple_coro::CoroState;
    use simple_test_case::test_case;

    fn attached_session_state() -> SessionState<Attached> {
        let qids = Arc::new(RwLock::new(BTreeMap::from([(
            QID_ROOT,
            QidMeta::new(Qid::dir(QID_ROOT), None),
        )])));

        SessionState {
            client_id: ClientId(0),
            msize: DEFAULT_MSIZE,
            roots: BTreeMap::from([("".to_string(), QID_ROOT)]),
            qids: qids.clone(),
            state: Attached {
                uname: "testuser".to_string(),
                fids: BTreeMap::from([(0, FidMeta::closed(QID_ROOT))]),
                client_id: ClientId(0),
                qids,
            },
        }
    }

    fn test_stat(name: &str, qid: u64) -> Stat {
        Stat {
            qid: Qid::dir(qid),
            name: name.into(),
            owner: "owner".to_string(),
            group: "group".to_string(),
            perms: Perm::any_read() | Perm::any_exec(),
            n_bytes: 0,
            last_accessed: Timestamp::UNIX_EPOCH,
            last_modified: Timestamp::UNIX_EPOCH,
            last_modified_by: "owner".to_string(),
        }
    }

    #[test_case(SUPPORTED_VERSION, DEFAULT_MSIZE  / 2, DEFAULT_MSIZE  / 2, SUPPORTED_VERSION; "client msize smaller than server")]
    #[test_case(SUPPORTED_VERSION, DEFAULT_MSIZE  + 1, DEFAULT_MSIZE , SUPPORTED_VERSION; "client msize larger than server")]
    #[test_case("12345", DEFAULT_MSIZE  / 2, DEFAULT_MSIZE  / 2, UNKNOWN_VERSION; "unknown version still negotiates msize")]
    #[test]
    fn handle_version_returns_expected_response(
        version: &str,
        client_msize: u32,
        expected_msize: u32,
        expected_version: &str,
    ) {
        let mut session = Server::new(()).new_session(());
        let resp = session.handle_version(client_msize, version.into());

        assert_eq!(resp, Rdata::version(expected_msize, expected_version));
    }

    #[test]
    fn unsupported_version_does_not_set_seen_version() {
        let mut session = Server::new(()).new_session(());

        session.handle_tmessage_unattached(Tmessage::new(
            u16::MAX,
            Tdata::version(DEFAULT_MSIZE, "12345"),
        ));

        assert!(
            !session.state.seen_version,
            "seen_version should not be set"
        );
    }

    #[test]
    fn handle_attach_with_unknown_root_returns_err() {
        let mut session = Server::new(()).new_session(());
        let res = session.handle_attach(0, AFID_NO_AUTH, "user".into(), "unknown aname".into());

        assert_eq!(res.unwrap_err(), E_UNKNOWN_ROOT);
    }

    #[test]
    fn handle_attach_with_valid_root_initialises_state() {
        let mut session = Server::new(()).new_session(());
        assert!(
            session.roots.contains_key(""),
            "expected default root is not present"
        );

        let (attached, qid) = session
            .handle_attach(5, AFID_NO_AUTH, "testuser".into(), "".into())
            .unwrap();

        assert_eq!(
            attached.fids,
            BTreeMap::from([(5, FidMeta::closed(QID_ROOT))])
        );
        assert_eq!(attached.uname, "testuser");
        assert_eq!(qid.ty, FileType::DIRECTORY);
        assert_eq!(qid.path, QID_ROOT);
    }

    #[test]
    fn attach_before_version_returns_error() {
        let mut session = Server::new(()).new_session(());

        let resp = session.handle_tmessage_unattached(Tmessage::new(
            0,
            Tdata::attach(0, AFID_NO_AUTH, "user", ""),
        ));

        match resp {
            Either::L((_, Err(e))) => assert_eq!(e, E_NO_VERSION_MESSAGE),
            other => panic!("expected E_NO_VERSION, got {other:?}"),
        }
    }

    #[test]
    fn attach_with_unknown_root_propagates_error() {
        let mut session = Server::new(()).new_session(());
        session.state.seen_version = true;

        let resp = session.handle_tmessage_unattached(Tmessage::new(
            0,
            Tdata::attach(0, AFID_NO_AUTH, "user", "unknown aname"),
        ));

        match resp {
            Either::L((_, Err(e))) => assert_eq!(e, E_UNKNOWN_ROOT),
            other => panic!("expected E_UNKNOWN_ROOT, got {other:?}"),
        }
    }

    #[test]
    fn valid_attach_returns_expected_qid() {
        let mut session = Server::new(()).new_session(());
        session.state.seen_version = true;

        let resp = session.handle_tmessage_unattached(Tmessage::new(
            0,
            Tdata::attach(0, AFID_NO_AUTH, "user", ""),
        ));

        match resp {
            Either::R((0, _, qid)) => assert_eq!(qid.path, QID_ROOT),
            other => panic!("expected root qid, got: {other:?}"),
        }
    }

    #[test]
    fn walk_empty_wnames_binds_new_fid_to_root() {
        let mut ss = attached_session_state();
        let res = ss.handle_attached_walk(0, 1, vec![]).resume().unwrap();

        let wqids = res.unwrap();
        assert!(wqids.is_empty(), "expected empty wqids: {wqids:?}");

        assert_eq!(
            ss.state.fids.get(&1),
            Some(&FidMeta::closed(QID_ROOT)),
            "new_fid should be bound to root"
        );
    }

    #[test]
    fn walk_duplicate_new_fid_returns_error() {
        let mut ss = attached_session_state();
        ss.state.fids.insert(1, FidMeta::closed(99));

        let res = ss
            .handle_attached_walk(0, 1, vec!["child".to_string()])
            .resume()
            .unwrap();

        assert_eq!(res.unwrap_err(), E_DUPLICATE_FID);
    }

    #[test]
    fn walk_unknown_fid_returns_error() {
        let mut ss = attached_session_state();
        let res = ss
            .handle_attached_walk(99, 1, vec!["child".to_string()])
            .resume()
            .unwrap();

        assert_eq!(res.unwrap_err(), E_UNKNOWN_FID);
    }

    #[test]
    fn walk_non_dir_returns_error() {
        let mut ss = attached_session_state();
        ss.with_shared_qids_mut(|qids| qids.insert(1, QidMeta::new(Qid::file(1), Some(0))));
        ss.state.fids.insert(2, FidMeta::closed(1));

        let res = ss
            .handle_attached_walk(2, 3, vec!["child".to_string()])
            .resume()
            .unwrap();

        assert_eq!(res.unwrap_err(), E_WALK_NON_DIR);
    }

    #[test]
    fn walk_full_walk_binds_new_fid() {
        let mut ss = attached_session_state();
        let wnames = vec!["child".to_string()];
        let child_qid = 42;

        let mut coro = ss.handle_attached_walk(0, 1, wnames);
        coro = coro.resume().unwrap_pending(|(parent_qid, name)| {
            assert_eq!(parent_qid, QID_ROOT, "should walk from root");
            assert_eq!(name, "child", "should request child name");
            Ok(Qid::file(child_qid))
        });

        let wqids = coro.resume().unwrap().unwrap();

        assert_eq!(wqids.len(), 1, "expected one wqid");
        assert_eq!(wqids[0].path, child_qid, "wqid path should match child qid");
        assert_eq!(
            ss.state.fids.get(&1),
            Some(&FidMeta::closed(child_qid)),
            "new_fid should be bound to child"
        );
    }

    #[test]
    fn walk_first_element_failure_returns_error() {
        let mut ss = attached_session_state();
        let wnames = vec!["missing".to_string()];

        let mut coro = ss.handle_attached_walk(0, 1, wnames);
        coro = coro
            .resume()
            .unwrap_pending(|_| Err("not found".to_string()));

        let res = coro.resume().unwrap();
        assert!(res.is_err(), "expected Rerror");
        assert_eq!(ss.state.fids.get(&1), None, "new_fid was bound");
    }

    #[test]
    fn walk_partial_walk_returns_partial_qids_and_does_not_bind_new_fid() {
        let mut ss = attached_session_state();
        let wnames = vec!["a".to_string(), "b".to_string()];
        let a_qid = 10;

        let mut coro = ss.handle_attached_walk(0, 1, wnames);

        // First step of the walk succeeds
        coro = coro.resume().unwrap_pending(|(_, _)| Ok(Qid::dir(a_qid)));

        // Second step fails
        coro = coro
            .resume()
            .unwrap_pending(|(_, _)| Err("not found".to_string()));

        let wqids = coro.resume().unwrap().unwrap();

        assert_eq!(wqids.len(), 1, "expected partial qid list");
        assert_eq!(wqids[0].path, a_qid, "partial qid should be for 'a'");
        assert_eq!(ss.state.fids.get(&1), None, "new_fid was bound");
    }

    #[test]
    fn walk_over_maxwelem_returns_error() {
        let mut ss = attached_session_state();
        let wnames = (0..=MAXWELEM)
            .map(|i| format!("n{i}"))
            .collect::<Vec<String>>();

        let res = ss.handle_attached_walk(0, 1, wnames).resume().unwrap();

        assert_eq!(res.unwrap_err(), E_OVER_MAXWELEM);
        assert_eq!(ss.state.fids.get(&1), None, "new_fid was bound");
    }

    #[test]
    fn read_regular_file_yields_file_read_request() {
        let mut ss = attached_session_state();
        ss.with_shared_qids_mut(|qids| qids.insert(1, QidMeta::new(Qid::file(1), Some(0))));
        ss.state.fids.insert(2, FidMeta::closed(1));

        let coro = ss.handle_attached_read(2, 0, 1024);

        match coro.resume() {
            CoroState::Pending(_, Either::R(qid)) => assert_eq!(qid, 1, "wrong qid"),
            CoroState::Pending(_, s) => panic!("unexpected pending coro state: {s:?}"),
            CoroState::Complete(res) => panic!("unexpected complete coro result: {res:?}"),
        }
    }

    // Helper for readdir tests below
    fn handle_readdir(
        ss: &mut SessionState<Attached>,
        stats: &[Stat],
        offset: u64,
        count: u32,
    ) -> Vec<RawStat> {
        let mut coro = ss.handle_attached_read(0, offset, count);
        coro = coro.resume().unwrap_pending(|_| stats.to_vec());

        let data = match coro.resume().unwrap() {
            Ok(Some(Rdata::Read { data })) => data,
            other => panic!("expected Read, got: {other:?}"),
        };

        Vec::<RawStat>::try_from(data).unwrap()
    }

    #[test]
    fn read_dir_returns_serialized_stats() {
        let mut ss = attached_session_state();
        let expected = test_stat("child", 1);

        let raw_stats = handle_readdir(&mut ss, std::slice::from_ref(&expected), 0, 4096);
        let expected_raw: RawStat = expected.into();

        assert_eq!(raw_stats, vec![expected_raw]);
    }

    #[test]
    fn read_dir_valid_offset_skips_entries() {
        let mut ss = attached_session_state();
        let stat1 = test_stat("a", 1);
        let stat2 = test_stat("b", 2);
        let stat1_len = RawStat::from(stat1.clone()).n_bytes();

        let raw_stats = handle_readdir(&mut ss, &[stat1, stat2.clone()], stat1_len as u64, 4096);
        let expected_raw: RawStat = stat2.into();

        assert_eq!(raw_stats, vec![expected_raw]);
    }

    #[test]
    fn read_dir_invalid_offset_returns_error() {
        let mut ss = attached_session_state();
        let stat = test_stat("child", 1);
        let stat_len = RawStat::from(stat.clone()).n_bytes();
        let invalid_offset = (stat_len - 1) as u64; // not aligned to a stat boundary

        let coro = ss.handle_attached_read(0, invalid_offset, 4096);
        let res = match coro.resume() {
            CoroState::Pending(c, Either::L(_)) => c.send(vec![stat]).resume().unwrap(),
            _ => panic!("expected Pending with Either::L"),
        };

        assert_eq!(res.unwrap_err(), E_INVALID_OFFSET);
    }

    /// The user can specify a count for the number of bytes to be read back. This is almost always
    /// not exactly aligned with the boundaries between individual stat entries, so we should
    /// truncate to only return stats that fit within the requested number of bytes.
    #[test]
    fn read_dir_with_unaligned_count_truncates() {
        let mut ss = attached_session_state();
        let stat1 = test_stat("a", 1);
        let stat2 = test_stat("b", 2);

        // Ensure that we fit the first entry but not the second
        let stat_len = RawStat::from(stat1.clone()).n_bytes();
        let count = (stat_len + 1) as u32;

        let raw_stats = handle_readdir(&mut ss, &[stat1.clone(), stat2], 0, count);
        let expected_raw: RawStat = stat1.into();

        assert_eq!(raw_stats, vec![expected_raw]);
    }

    /// Base valid WStat for the following perm check tests
    fn ws() -> WStat {
        WStat::commit(Qid::file(1))
    }

    fn run_perm_check(wstat: WStat, uname: &str) -> Result<()> {
        let mut stat = Stat::stub(Qid::file(1), "test-file");
        stat.perms = Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::OWNER_EXEC | Perm::GROUP_WRITE;
        assert_eq!(stat.owner, "owner");
        assert_eq!(stat.group, "group");

        let mut parent_stat = Stat::stub(Qid::dir(1), "test-dir");
        parent_stat.perms = Perm::OWNER_WRITE | Perm::GROUP_WRITE;
        assert_eq!(parent_stat.owner, "owner");
        assert_eq!(parent_stat.group, "group");

        let mut ss = attached_session_state();
        ss.state.uname = uname.into();
        let in_group = uname == "group-member";

        let coro = ss.check_wstat_perms(&stat, &wstat, in_group);
        match coro.resume() {
            CoroState::Complete(res) => res,
            CoroState::Pending(c, _) => c.send((parent_stat, in_group)).resume().unwrap(),
        }
    }

    #[test_case(ws(), "owner"; "commit correct qid")]
    #[test_case(
        WStat { perms: Some(Perm::OTHER_READ), ..ws()},
        "owner"; "owner change perm"
    )]
    #[test_case(
        WStat { last_modified: Some(Timestamp::now()), ..ws()},
        "owner"; "owner change last modified"
    )]
    #[test_case(
        WStat { group: Some("new group".into()), ..ws()},
        "owner"; "owner change group"
    )]
    #[test_case(
        WStat { n_bytes: Some(1), ..ws()},
        "owner"; "owner change n_bytes"
    )]
    #[test_case(
        WStat { n_bytes: Some(1), ..ws()},
        "group-member"; "other write change n_bytes"
    )]
    #[test_case(
        WStat { name: Some("new".into()), ..ws()},
        "owner"; "owner change name"
    )]
    #[test_case(
        WStat { name: Some("new".into()), ..ws()},
        "group-member"; "other write on parent change name"
    )]
    #[test]
    fn check_wstat_perms_accepts_valid_wstats(wstat: WStat, uname: &str) {
        let res = run_perm_check(wstat, uname);
        assert!(res.is_ok());
    }

    #[test_case(WStat::commit(Qid::file(9)), "owner"; "commit wrong qid")]
    #[test_case(WStat::commit(Qid::dir(1)), "owner"; "change dir bit")]
    #[test_case(
        WStat { perms: Some(Perm::OTHER_READ), ..ws()},
        "non-owner"; "non-owner change perm"
    )]
    #[test_case(
        WStat { last_modified: Some(Timestamp::now()), ..ws()},
        "non-owner"; "non-owner change last modified"
    )]
    #[test_case(
        WStat { group: Some("new group".into()), ..ws()},
        "non-owner"; "non-owner change group"
    )]
    #[test_case(
        WStat { n_bytes: Some(1), ..ws()},
        "non-write"; "non-write change n_bytes"
    )]
    #[test_case(
        WStat { name: Some("new".into()), ..ws()},
        "non-write"; "non-write on parent change name"
    )]
    #[test]
    fn check_wstat_perms_rejects_invalid_wstats(wstat: WStat, uname: &str) {
        let res = run_perm_check(wstat, uname);
        assert!(res.is_err());
    }
}
