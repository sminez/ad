//! A simple 9p client for building out application specific client applications.
use crate::{
    fs::Stat,
    protocol::{Data, Format9p, Rdata, Rmessage, Tdata, Tmessage},
};
use std::{
    collections::HashMap,
    env, io,
    net::{TcpStream, ToSocketAddrs},
    os::unix::net::UnixStream,
};

// TODO:
// - need to handle errors coming back from the server
// - read vs read dir
// - need a proper error enum rather than just using io::Error

macro_rules! expect_rmessage {
    ($resp:expr, $variant:ident { $($field:ident),+, .. }) => {
        match $resp.content {
            Rdata::$variant { $($field),+, .. } => ($($field),+),
            Rdata::Error { ename } => return err(ename),
            m => return err(format!("unexpected response: {m:?}")),
        }

    };

    ($resp:expr, $variant:ident { $($field:ident),+ }) => {
        match $resp.content {
            Rdata::$variant { $($field),+ } => ($($field),+),
            Rdata::Error { ename } => return err(ename),
            m => return err(format!("unexpected response: {m:?}")),
        }

    };
}

const MSIZE: u32 = u16::MAX as u32;
const VERSION: &str = "9P2000";

#[derive(Debug)]
enum Socket {
    Unix(UnixStream),
    Tcp(TcpStream),
}

impl io::Write for Socket {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            Self::Unix(s) => s.write(buf),
            Self::Tcp(s) => s.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            Self::Unix(s) => s.flush(),
            Self::Tcp(s) => s.flush(),
        }
    }
}

impl io::Read for Socket {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        match self {
            Self::Unix(s) => s.read(buf),
            Self::Tcp(s) => s.read(buf),
        }
    }
}

fn err<T, E>(e: E) -> io::Result<T>
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Err(io::Error::new(io::ErrorKind::Other, e))
}

#[derive(Debug)]
pub struct Client {
    socket: Socket,
    uname: String,
    msize: u32,
    fids: HashMap<String, u32>,
    next_fid: u32,
}

impl Drop for Client {
    fn drop(&mut self) {
        let fids = std::mem::take(&mut self.fids);
        for (_, fid) in fids.into_iter() {
            _ = self.send(0, Tdata::Clunk { fid });
        }
    }
}

impl Client {
    pub fn new_unix_with_explicit_path(
        uname: String,
        path: String,
        aname: impl Into<String>,
    ) -> io::Result<Self> {
        let socket = UnixStream::connect(path)?;
        let mut client = Self {
            socket: Socket::Unix(socket),
            uname,
            msize: MSIZE,
            fids: HashMap::default(),
            next_fid: 1,
        };
        client.connect(aname)?;

        Ok(client)
    }

    pub fn new_unix(ns: impl Into<String>, aname: impl Into<String>) -> io::Result<Self> {
        let ns = ns.into();
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return err("USER env var not set"),
        };
        let path = format!("/tmp/ns.{uname}.:0/{ns}");

        Self::new_unix_with_explicit_path(uname, path, aname)
    }

    pub fn new_tcp<T>(uname: String, addr: T, aname: impl Into<String>) -> io::Result<Self>
    where
        T: ToSocketAddrs,
    {
        let socket = TcpStream::connect(addr)?;
        let mut client = Self {
            socket: Socket::Tcp(socket),
            uname,
            msize: MSIZE,
            fids: HashMap::default(),
            next_fid: 1,
        };
        client.connect(aname)?;

        Ok(client)
    }

    fn send(&mut self, tag: u16, content: Tdata) -> io::Result<Rmessage> {
        let t = Tmessage { tag, content };
        t.write_to(&mut self.socket)?;

        Rmessage::read_from(&mut self.socket)
    }

    fn next_fid(&mut self) -> u32 {
        let fid = self.next_fid;
        self.next_fid += 1;

        fid
    }

    /// Establish our connection to the target 9p server and begin the session.
    fn connect(&mut self, aname: impl Into<String>) -> io::Result<()> {
        let resp = self.send(
            u16::MAX,
            Tdata::Version {
                msize: MSIZE,
                version: VERSION.to_string(),
            },
        )?;

        let (msize, version) = expect_rmessage!(resp, Version { msize, version });
        if version != VERSION {
            return err("server version not supported");
        }
        self.msize = msize;

        self.send(
            0,
            Tdata::Attach {
                fid: 0,
                afid: u32::MAX, // no auth
                uname: self.uname.clone(),
                aname: aname.into(),
            },
        )?;

        Ok(())
    }

    fn walk(&mut self, path: impl Into<String>) -> io::Result<u32> {
        let path = path.into();
        if let Some(fid) = self.fids.get(&path) {
            return Ok(*fid);
        }

        let new_fid = self.next_fid();

        self.send(
            0,
            Tdata::Walk {
                fid: 0,
                new_fid,
                wnames: path.split('/').map(Into::into).collect(),
            },
        )?;

        Ok(new_fid)
    }

    pub fn clunk(&mut self, fid: u32) -> io::Result<()> {
        self.send(0, Tdata::Clunk { fid: 1 })?;
        self.fids.retain(|_, v| *v != fid);

        Ok(())
    }

    pub fn stat(&mut self, path: impl Into<String>) -> io::Result<Stat> {
        let fid = self.walk(path)?;
        let resp = self.send(0, Tdata::Stat { fid })?;
        let raw_stat = expect_rmessage!(resp, Stat { stat, .. });

        Ok(raw_stat.try_into().unwrap())
    }

    pub fn read(&mut self, path: impl Into<String>) -> io::Result<Vec<u8>> {
        let fid = self.walk(path)?;

        self.send(0, Tdata::Open { fid, mode: 0 })?;
        let resp = self.send(
            0,
            Tdata::Read {
                fid,
                offset: 0,
                count: 8168,
            },
        )?;

        let Data(bytes) = expect_rmessage!(resp, Read { data });

        Ok(bytes)
    }

    pub fn read_str(&mut self, path: impl Into<String>) -> io::Result<String> {
        let bytes = self.read(path)?;
        let s = match String::from_utf8(bytes) {
            Ok(s) => s,
            Err(_) => return err("invalid utf8"),
        };

        Ok(s)
    }

    // pub fn write(&mut self, path: impl AsRef<str>, content: impl AsRef<str>) -> io::Result<usize> {
    //     todo!()
    // }
}
