//! Traits for implementing a 9p fileserver
use crate::ninep::protocol::{
    Format9p, Qid, Rauth, Rclunk, Rdata, Rerror, Rmessage, Rversion, Rwalk, Tattach, Tauth, Tdata,
    Tmessage, Tversion, MAX_DATA_LEN,
};
use std::{
    cmp::min,
    collections::BTreeMap,
    net::TcpListener,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

use super::protocol::{Rattach, Tclunk, Twalk};

const NO_FID: u32 = u32::MAX;
const QID_ROOT: u64 = 0;

// File "mode" values for use in Qids
const QTDIR: u8 = 0x80;
// const QTAPPEND: u8 = 0x40;
// const QTEXCL: u8 = 0x20;
// const QTMOUNT: u8 = 0x10;
// const QTAUTH: u8 = 0x08;
// const QTTMP: u8 = 0x04;
// const QTSYMLINK: u8 = 0x02;
const QTFILE: u8 = 0x00;

// Error messages
const E_DUPLICATE_FID: &str = "duplicate fid";
const E_UNKNOWN_FID: &str = "unknown fid";
const E_WALK_NON_DIR: &str = "walk in non-directory";

pub type Result<T> = std::result::Result<T, String>;

// TODO: the types here need to be converted to something that is more friendly to work with
// rather than using the types from the protocol layer directly

pub trait Serve9p {
    #[allow(unused_variables)]
    // fn auth(&mut self, afid: &Fid, uname: &str, aname: &str) -> Result<Qid> {
    fn auth(&mut self, afid: u32, uname: &str, aname: &str) -> Result<Qid> {
        Err("authentication not required".to_string())
    }

    // fn flush(&mut self, old_tag: u16) -> Result<()>;

    fn walk(&mut self, path: &Path) -> Result<Vec<(FileType, PathBuf)>>;

    // fn open(&mut self, fid: &Fid, mode: u8) -> Result<(Qid, u32)>;
    // fn create(&mut self, fid: &Fid, name: &str, perm: u32, mode: u8) -> Result<(Qid, u32)>;

    // fn read(&mut self, fid: &Fid, offset: usize, count: usize) -> Result<Vec<u8>>;
    // fn write(&mut self, fid: &Fid, offset: usize, data: Vec<u8>) -> Result<usize>;

    // fn clunk(&mut self, fid: &Fid) -> Result<()>;
    // fn remove(&mut self, fid: &Fid) -> Result<()>;

    // fn stat(&mut self, fid: &Fid) -> Result<Stat>;
    // fn write_stat(&mut self, fid: &Fid, stat: Stat) -> Result<()>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileType {
    Regular,
    Directory,
}

impl From<FileType> for u8 {
    fn from(value: FileType) -> Self {
        match value {
            FileType::Directory => QTDIR,
            FileType::Regular => QTFILE,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileMeta {
    path: PathBuf,
    ty: FileType,
    qid: u64,
}

// TODO: need to track which users have each fid?

#[derive(Debug)]
pub struct Server<S: Serve9p> {
    s: S,
    msize: u32,
    next_qid: u64,
    fids: BTreeMap<u32, FileMeta>,
    qids: BTreeMap<PathBuf, Qid>,
}

impl<S: Serve9p> Server<S> {
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

    pub fn serve(mut self) {
        use Tdata::*;

        let listener = TcpListener::bind("127.0.0.1:9000").unwrap();

        // FIXME: this will need to handle multiple connections eventually
        for stream in listener.incoming() {
            let mut stream = stream.unwrap();

            loop {
                let t = Tmessage::read_from(&mut stream).unwrap();
                eprintln!("t-message: {t:?}");
                let Tmessage { tag, content } = t;

                let resp = match content {
                    Version(tversion) => self.handle_version(tversion),
                    Auth(tauth) => self.handle_auth(tauth),
                    Attach(tattach) => self.handle_attach(tattach),
                    Walk(twalk) => self.handle_walk(twalk),
                    Clunk(tclunk) => self.handle_clunk(tclunk),

                    _ => {
                        eprintln!("tag={tag} content={content:?}");
                        continue;
                    }
                };

                let r = Rmessage {
                    tag,
                    content: resp.unwrap_or_else(|ename| Rdata::Error(Rerror { ename })),
                };

                eprintln!("r-message: {r:?}\n");
                if let Err(e) = r.write_to(&mut stream) {
                    eprintln!("ERROR sending response: {e}");
                }
            }
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
    fn handle_version(&mut self, Tversion { msize, version }: Tversion) -> Result<Rdata> {
        let server_version = if version != "9P2000" {
            "unknown"
        } else {
            "9P2000"
        };

        Ok(Rdata::Version(Rversion {
            msize: min(self.msize, msize),
            version: server_version.to_string(),
        }))
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
    fn handle_auth(&mut self, Tauth { afid, uname, aname }: Tauth) -> Result<Rdata> {
        let aqid = self.s.auth(afid, &uname, &aname)?;
        Ok(Rdata::Auth(Rauth { aqid }))
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
    fn handle_attach(&mut self, Tattach { fid, .. }: Tattach) -> Result<Rdata> {
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

        Ok(Rdata::Attach(Rattach {
            aqid: *self.qids.get(&PathBuf::from("/")).unwrap(),
        }))
    }

    fn handle_walk(
        &mut self,
        Twalk {
            fid,
            new_fid,
            wnames,
        }: Twalk,
    ) -> Result<Rdata> {
        if new_fid != fid && self.fids.contains_key(&new_fid) {
            return Err(E_DUPLICATE_FID.to_string());
        }

        let fm = match self.fids.get(&fid) {
            Some(fm) => fm,
            None => return Err(E_UNKNOWN_FID.to_string()),
        };

        if matches!(fm.ty, FileType::Regular) {
            return Err(E_WALK_NON_DIR.to_string());
        } else if wnames.is_empty() {
            self.fids.insert(new_fid, fm.clone());
            return Ok(Rdata::Walk(Rwalk { wqids: vec![] }));
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
                version: SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_secs() as u32,
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

        Ok(Rdata::Walk(Rwalk { wqids }))
    }

    fn handle_clunk(&mut self, Tclunk { fid }: Tclunk) -> Result<Rdata> {
        self.fids.remove(&fid);
        Ok(Rdata::Clunk(Rclunk {}))
    }
}
