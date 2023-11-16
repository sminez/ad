//! Traits for implementing a 9p fileserver
use crate::ninep::{
    fs::{FileMeta, FileType, IoUnit, Mode, Stat, QID_ROOT, QTDIR},
    protocol::{Data, Format9p, Qid, RawStat, Rdata, Rmessage, Tdata, Tmessage, MAX_DATA_LEN},
};
use std::{
    cmp::min,
    collections::BTreeMap,
    env, fs,
    io::{Read, Write},
    mem::size_of,
    net::TcpListener,
    os::unix::net::UnixListener,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread::spawn,
};

pub const DEFAULT_TCP_PORT: u16 = 0xADD;
pub const DEFAULT_SOCKET_NAME: &str = "ad";

fn unix_socket(name: &str) -> UnixListener {
    let uname = env::var("USER").unwrap();
    let mntp = format!("/tmp/ns.{uname}.:0/{name}");

    // FIXME: really we should be handling this on exit but we'll need to catch
    // ctrl-c to do that properly. For now this works but it means that if you
    // start a second file server with the same name then it'll remove the socket
    // for the first.
    let _ = fs::remove_file(&mntp);
    println!("Binding to {mntp}");

    UnixListener::bind(mntp).unwrap()
}

fn tcp_socket(port: u16) -> TcpListener {
    let addr = format!("127.0.0.1:{port}");
    println!("Binding to {addr}");

    TcpListener::bind(&addr).unwrap()
}

// Error messages
const E_DUPLICATE_FID: &str = "duplicate fid";
const E_UNKNOWN_FID: &str = "unknown fid";
const E_WALK_NON_DIR: &str = "walk in non-directory";

pub type Result<T> = std::result::Result<T, String>;

impl From<(u16, Result<Rdata>)> for Rmessage {
    fn from((tag, content): (u16, Result<Rdata>)) -> Self {
        Rmessage {
            tag,
            content: content.unwrap_or_else(|ename| Rdata::Error { ename }),
        }
    }
}

pub trait Serve9p {
    #[allow(unused_variables)]
    fn auth(&mut self, afid: u32, uname: &str, aname: &str) -> Result<Qid> {
        Err("authentication not required".to_string())
    }

    fn stat(&mut self, path: &Path) -> Result<Stat>;
    fn walk(&mut self, path: &Path) -> Result<Vec<(FileType, PathBuf)>>;
    fn open(&mut self, path: &Path, mode: Mode) -> Result<IoUnit>;
    fn read(&mut self, path: &Path, offset: usize, count: usize) -> Result<Vec<u8>>;
    fn read_dir(&mut self, path: &Path) -> Result<Vec<Stat>>;

    // fn create(&mut self, fid: &Fid, name: &str, perm: u32, mode: Mode) -> Result<u32>;
    // fn write(&mut self, fid: &Fid, offset: usize, data: Vec<u8>) -> Result<usize>;
    // fn remove(&mut self, fid: &Fid) -> Result<()>;
    // fn write_stat(&mut self, fid: &Fid, stat: Stat) -> Result<()>;
}

// TODO: need to track which users have each fid?

#[derive(Debug)]
pub struct Server<S>
where
    S: Serve9p + Sync + Send + 'static,
{
    s: S,
    msize: u32,
    next_qid: u64,
    fids: BTreeMap<u32, FileMeta>,
    qids: BTreeMap<PathBuf, Qid>,
}

fn handle_connection<S, RW>(srv: Arc<Mutex<Server<S>>>, mut stream: RW)
where
    S: Serve9p + Sync + Send + 'static,
    RW: Read + Write,
{
    use Tdata::*;

    loop {
        let t = match Tmessage::read_from(&mut stream) {
            Ok(t) => t,
            Err(e) => {
                eprintln!("error reading message: {e}\ndropping connection");
                srv.lock().unwrap().fids.clear();
                return;
            }
        };

        eprintln!("t-message: {t:?}");
        let Tmessage { tag, content } = t;
        let mut s = srv.lock().unwrap();

        let resp = match content {
            Version { msize, version } => s.handle_version(msize, version),
            Auth { afid, uname, aname } => s.handle_auth(afid, uname, aname),
            Attach { fid, .. } => s.handle_attach(fid),
            Walk {
                fid,
                new_fid,
                wnames,
            } => s.handle_walk(fid, new_fid, wnames),
            Clunk { fid } => s.handle_clunk(fid),
            Stat { fid } => s.handle_stat(fid),
            Open { fid, mode } => s.handle_open(fid, mode),
            Read { fid, offset, count } => s.handle_read(fid, offset, count),

            _ => {
                eprintln!("tag={tag} content={content:?}");
                continue;
            }
        };

        let r: Rmessage = (tag, resp).into();
        eprintln!("r-message: {r:?}\n");

        if let Err(e) = r.write_to(&mut stream) {
            eprintln!("ERROR sending response: {e}");
        }

        drop(s);
    }
}

impl<S> Server<S>
where
    S: Serve9p + Sync + Send + 'static,
{
    pub fn new(s: S) -> Self {
        let mut qids = BTreeMap::default();
        qids.insert(
            "/".into(),
            Qid {
                ty: QTDIR,
                version: 0,
                path: QID_ROOT,
            },
        );

        Self {
            s,
            msize: MAX_DATA_LEN as u32,
            next_qid: 1,
            fids: BTreeMap::default(),
            qids,
        }
    }

    fn next_qid(&mut self) -> u64 {
        let qid = self.next_qid;
        self.next_qid += 1;
        qid
    }

    pub fn serve_tcp(self, port: u16) {
        let listener = tcp_socket(port);
        let srv = Arc::new(Mutex::new(self));

        for stream in listener.incoming() {
            let stream = stream.unwrap();
            println!("new connection");
            let thread_srv = Arc::clone(&srv);
            spawn(move || handle_connection(thread_srv, stream));
        }
    }

    pub fn serve_socket(self, socket_name: &str) {
        let listener = unix_socket(socket_name);
        let srv = Arc::new(Mutex::new(self));

        for stream in listener.incoming() {
            let stream = stream.unwrap();
            println!("new connection");
            let thread_srv = Arc::clone(&srv);
            spawn(move || handle_connection(thread_srv, stream));
        }
    }

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
    fn handle_version(&mut self, msize: u32, version: String) -> Result<Rdata> {
        let server_version = if version != "9P2000" {
            "unknown"
        } else {
            "9P2000"
        };

        Ok(Rdata::Version {
            msize: min(self.msize, msize),
            version: server_version.to_string(),
        })
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
    fn handle_auth(&mut self, afid: u32, uname: String, aname: String) -> Result<Rdata> {
        let aqid = self.s.auth(afid, &uname, &aname)?;
        Ok(Rdata::Auth { aqid })
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
    fn handle_attach(&mut self, fid: u32) -> Result<Rdata> {
        if self.fids.contains_key(&fid) {
            return Err(E_DUPLICATE_FID.to_string());
        }

        self.fids.insert(
            fid,
            FileMeta {
                path: "/".into(),
                ty: FileType::Directory,
                qid: 0,
            },
        );

        Ok(Rdata::Attach {
            aqid: *self.qids.get(&PathBuf::from("/")).unwrap(),
        })
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
        if new_fid != fid && self.fids.contains_key(&new_fid) {
            return Err(E_DUPLICATE_FID.to_string());
        }

        let fm = match self.fids.get(&fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        if wnames.is_empty() {
            self.fids.insert(new_fid, fm.clone());
            return Ok(Rdata::Walk { wqids: vec![] });
        } else if matches!(fm.ty, FileType::Regular) {
            return Err(E_WALK_NON_DIR.to_string());
        }

        let suffix = PathBuf::from_iter(wnames);
        let paths = self.s.walk(&fm.path.join(suffix))?;
        let mut wqids = Vec::with_capacity(paths.len());
        let mut last = None;

        for (ty, p) in paths.into_iter() {
            if let Some(qid) = self.qids.get(&p) {
                wqids.push(*qid);
                continue;
            }

            let path = self.next_qid();
            let qid = Qid {
                ty: ty.into(),
                version: 0,
                path,
            };
            last = Some(FileMeta {
                path: p.clone(),
                ty,
                qid: path,
            });
            self.qids.insert(p, qid);
            wqids.push(qid);
        }

        if let Some(fm) = last {
            self.fids.insert(new_fid, fm);
        }

        Ok(Rdata::Walk { wqids })
    }

    fn handle_clunk(&mut self, fid: u32) -> Result<Rdata> {
        self.fids.remove(&fid);
        Ok(Rdata::Clunk {})
    }

    fn handle_stat(&mut self, fid: u32) -> Result<Rdata> {
        let FileMeta { path, .. } = match self.fids.get(&fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };
        let s = self.s.stat(path)?;
        let q = self.qids.get(path).unwrap();
        let stat: RawStat = (*q, s).into();
        let size = stat.size + size_of::<u16>() as u16;

        Ok(Rdata::Stat { size, stat })
    }

    fn handle_open(&mut self, fid: u32, mode: u8) -> Result<Rdata> {
        let FileMeta { path, .. } = match self.fids.get(&fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };
        let iounit = self.s.open(path, mode)?;
        let qid = *self.qids.get(path).unwrap();

        Ok(Rdata::Open { qid, iounit })
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
    fn handle_read(&mut self, fid: u32, offset: u64, count: u32) -> Result<Rdata> {
        let FileMeta { path, ty, .. } = match self.fids.get(&fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        if offset > u32::MAX as u64 {
            return Err(format!("offset too large: {offset} > {}", u32::MAX));
        }

        match ty {
            FileType::Directory => {
                let mut buf = Vec::with_capacity(count as usize);
                for stat in self.s.read_dir(path)? {
                    let q = self.qids.entry(PathBuf::from(&stat.name)).or_insert(Qid {
                        ty: stat.ty.into(),
                        version: 0,
                        path: stat.qid,
                    });
                    let rstat: RawStat = (*q, stat).into();
                    rstat.write_to(&mut buf).unwrap();
                }

                if offset as usize >= buf.len() {
                    buf.clear();
                }

                Ok(Rdata::Read { data: Data(buf) })
            }

            FileType::Regular => {
                let buf = self.s.read(path, offset as usize, count as usize)?;

                Ok(Rdata::Read { data: Data(buf) })
            }
        }
    }
}
