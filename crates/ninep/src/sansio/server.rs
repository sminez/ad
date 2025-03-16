//! Traits and structs for implementing a 9p fileserver
use crate::{
    fs::{FileMeta, FileType, Stat, QID_ROOT},
    sansio::protocol::{Data, NineP, Qid, RawStat, Rdata, Tdata, Tmessage, MAX_DATA_LEN},
    Result,
};
use simple_coro::{Coro, Handle, ReadyCoro};
use std::{
    cmp::min,
    collections::btree_map::BTreeMap,
    env,
    future::Future,
    ops::{Deref, DerefMut},
    sync::Arc,
};

/// Marker afid to denode that auth is not required for establishing connections
pub const AFID_NO_AUTH: u32 = u32::MAX;

// Error messages
pub(crate) const E_NO_VERSION_MESSAGE: &str = "first message must be Tversion";
pub(crate) const E_UNATTACHED: &str = "session is not attached";
pub(crate) const E_ALREADY_ATTACHED: &str = "session is already attached";
pub(crate) const E_AUTH_NOT_REQUIRED: &str = "authentication not required";
pub(crate) const E_DUPLICATE_FID: &str = "duplicate fid";
pub(crate) const E_UNKNOWN_FID: &str = "unknown fid";
pub(crate) const E_UNKNOWN_ROOT: &str = "unknown root directory";
pub(crate) const E_WALK_NON_DIR: &str = "walk in non-directory";
pub(crate) const E_CREATE_NON_DIR: &str = "create in non-directory";
pub(crate) const E_INVALID_OFFSET: &str = "invalid offset for read on directory";

pub(crate) const UNKNOWN_VERSION: &str = "unknown";
pub(crate) const SUPPORTED_VERSION: &str = "9P2000";

const DEFAULT_DISPLAY_VALUE: &str = ":0";

/// Determine the 9p socket directory based on the USER and DISPLAY environment variables
pub fn socket_dir() -> String {
    let uname = env::var("USER").unwrap();
    let display = env::var("DISPLAY").unwrap_or(String::from(DEFAULT_DISPLAY_VALUE));
    format!("/tmp/ns.{uname}.{display}")
}

/// The unix socket path that will be used for a given server name.
pub fn socket_path(name: &str) -> String {
    let socket_dir = socket_dir();
    format!("{socket_dir}/{name}")
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
    pub(crate) msize: u32,
    pub(crate) roots: BTreeMap<String, u64>,
    pub(crate) qids: BTreeMap<u64, FileMeta>,
    pub(crate) next_client_id: u64,
}

impl<S> Server<S>
where
    S: Send,
{
    /// Create a new file server with a single anonymous root (name will be "") and
    /// qid of [QID_ROOT].
    pub fn new(s: S) -> Self {
        Self::new_with_roots(s, [("".to_string(), QID_ROOT)].into_iter().collect())
    }

    /// Create a new file server with the given roots for clients to attach to.
    pub fn new_with_roots(s: S, roots: BTreeMap<String, u64>) -> Self {
        let qids = roots
            .iter()
            .map(|(p, &qid)| (qid, FileMeta::dir(p.clone(), qid)))
            .collect();

        Self {
            s: Arc::new(s),
            msize: MAX_DATA_LEN as u32,
            roots,
            qids,
            next_client_id: 0,
        }
    }

    /// Construct a new unattached [Session] over the provided [Stream]
    pub(crate) fn new_session<U>(&mut self, stream: U) -> Session<Unattached, S, U> {
        let session = Session::new_unattached(
            ClientId(self.next_client_id),
            self.msize,
            self.roots.clone(),
            self.s.clone(),
            self.qids.clone(),
            stream,
        );
        self.next_client_id += 1;

        session
    }
}

/// Marker trait for implementing a type state for Session
pub(crate) trait SessionType: Send {}

#[derive(Debug, Default)]
pub(crate) struct Unattached {
    pub(crate) seen_version: bool,
}

impl SessionType for Unattached {}

#[derive(Debug)]
pub(crate) struct Attached {
    pub(crate) uname: String,
    pub(crate) fids: BTreeMap<u32, u64>,
}
impl SessionType for Attached {}

impl Attached {
    fn new(uname: String, root_fid: u32, root_qid: u64) -> Self {
        Self {
            uname,
            fids: [(root_fid, root_qid)].into_iter().collect(),
        }
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
    pub(crate) qids: BTreeMap<u64, FileMeta>,
}

impl<T> SessionState<T>
where
    T: SessionType,
{
    pub(crate) fn qid(&self, qid: u64) -> Option<Qid> {
        self.qids.get(&qid).map(|fm| fm.as_qid())
    }
}

impl SessionState<Attached> {
    pub(crate) fn try_file_meta(&self, fid: u32) -> Result<FileMeta> {
        let opt = match self.state.fids.get(&fid) {
            Some(&qid) => self.qids.get(&qid).cloned(),
            None => None,
        };

        opt.ok_or_else(|| E_UNKNOWN_FID.to_string())
    }

    pub(crate) fn handle_attached_walk<'a, 's: 'a>(
        &'s mut self,
        fid: u32,
        new_fid: u32,
        wnames: &'a [String],
    ) -> ReadyCoro<
        (u64, &'a str, &'a str),
        FileMeta,
        Result<Rdata>,
        impl Future<Output = Result<Rdata>> + use<'s, 'a>,
    > {
        Coro::from(
            move |handle: Handle<(u64, &'a str, &'a str), FileMeta>| async move {
                if new_fid != fid && self.state.fids.contains_key(&new_fid) {
                    return Err(E_DUPLICATE_FID.to_string());
                }

                let fm = self.try_file_meta(fid)?;

                if wnames.is_empty() {
                    self.state.fids.insert(new_fid, fm.qid);
                    return Ok(Rdata::Walk { wqids: vec![] });
                } else if matches!(fm.ty, FileType::Regular) {
                    return Err(E_WALK_NON_DIR.to_string());
                }

                let mut wqids = Vec::with_capacity(wnames.len());
                let mut qid = fm.qid;

                for name in wnames.iter() {
                    let fm = handle.yield_value((qid, name, &self.state.uname)).await;
                    qid = fm.qid;
                    wqids.push(fm.as_qid());
                    self.qids.insert(qid, fm);
                }

                if wqids.len() == wnames.len() {
                    let qid = wqids.last().expect("empty was handled above").path;
                    self.state.fids.insert(new_fid, qid);
                }

                Ok(Rdata::Walk { wqids })
            },
        )
    }

    #[allow(clippy::type_complexity)]
    pub(crate) fn handle_attached_read<'a, 's: 'a>(
        &'s mut self,
        fid: u32,
        offset: u64,
        count: u32,
    ) -> ReadyCoro<
        Either<(u64, &'a str), (u64, &'a str)>, // L=read_dir R=read
        Vec<Stat>, // we never send or use a value in response to a read, only read-dir
        Result<Option<Rdata>>,
        impl Future<Output = Result<Option<Rdata>>> + use<'s, 'a>,
    > {
        Coro::from(
            move |handle: Handle<Either<(u64, &'a str), (u64, &'a str)>, Vec<Stat>>| async move {
                use FileType::*;

                let fm = self.try_file_meta(fid)?;
                if offset > u32::MAX as u64 {
                    return Err(format!("offset too large: {offset} > {}", u32::MAX));
                }

                let stats = match fm.ty {
                    Regular | AppendOnly | Exclusive => {
                        handle
                            .yield_value(Either::R((fm.qid, &self.state.uname)))
                            .await;
                        return Ok(None); // processing of the ReadOutcome is handled by the caller
                    }

                    Directory => {
                        handle
                            .yield_value(Either::L((fm.qid, &self.state.uname)))
                            .await
                    }
                };

                let mut buf = Vec::with_capacity(count as usize);
                let mut to_skip = offset as usize;

                for stat in stats.into_iter() {
                    self.qids.entry(stat.fm.qid).or_insert(stat.fm.clone());
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

                Ok(Some(Rdata::Read { data: Data(buf) }))
            },
        )
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

        Rdata::Version {
            msize: min(self.msize, msize),
            version: server_version.to_string(),
        }
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
    #[allow(unused_variables)]
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
        msize: u32,
        roots: BTreeMap<String, u64>,
        s: Arc<S>,
        qids: BTreeMap<u64, FileMeta>,
        stream: U,
    ) -> Self {
        Self {
            s,
            stream,
            session_state: SessionState {
                client_id,
                state: Unattached::default(),
                msize,
                roots,
                qids,
            },
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

    pub(crate) fn into_attached(self, ty: Attached) -> Session<Attached, S, U> {
        let Self {
            s,
            stream,
            session_state:
                SessionState {
                    client_id,
                    msize,
                    roots,
                    qids,
                    ..
                },
        } = self;

        Session::new_attached(client_id, ty, msize, roots, s, qids, stream)
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

        let st = Attached::new(uname, root_fid, root_qid);
        let aqid = self.qid(root_qid).expect("to have root qid");

        Ok((st, aqid))
    }
}

impl<S, U> Session<Attached, S, U>
where
    S: Send,
{
    fn new_attached(
        client_id: ClientId,
        state: Attached,
        msize: u32,
        roots: BTreeMap<String, u64>,
        s: Arc<S>,
        qids: BTreeMap<u64, FileMeta>,
        stream: U,
    ) -> Self {
        Self {
            s,
            stream,
            session_state: SessionState {
                client_id,
                state,
                msize,
                roots,
                qids,
            },
        }
    }
}
