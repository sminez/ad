//! A synchronous implementation of 9p Servers and Clients
use crate::{
    sansio::{
        protocol::{NineP, NinepReader, Rdata, Read9p, Rmessage},
        Stream,
    },
    Result,
};
use std::{
    io::{self, Read, Write},
    net::TcpStream,
    os::unix::net::UnixStream,
};

pub mod client;
pub mod server;

/// Synchronous IO support for reading and writing 9p messages
pub trait SyncNineP: NineP {
    /// Encode self as bytes for the 9p protocol and write to the given [SyncStream].
    fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
        let mut buf = vec![0; self.n_bytes()];
        self.write_bytes(&mut buf)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        w.write_all(&buf)
    }

    /// Decode self from 9p protocol bytes coming from the given [SyncStream].
    #[allow(clippy::uninit_vec)]
    fn read_from<R: Read>(r: &mut R) -> io::Result<Self> {
        let mut nr = NinepReader::Pending(Self::reader());
        let mut buf = Vec::new();

        loop {
            match nr {
                NinepReader::Pending(r9) => {
                    let n = Self::Reader::needs_bytes(&r9);
                    buf.reserve(n.saturating_sub(buf.len()));
                    // SAFETY: we've just reserved sufficient capacity
                    unsafe { buf.set_len(n) };
                    r.read_exact(&mut buf)?;
                    nr = r9.accept_bytes(&buf[0..n])?;
                }

                NinepReader::Complete(t) => return Ok(t),
            }
        }
    }
}

impl<T> SyncNineP for T where T: NineP {}

/// A [Stream] that makes use of the standard library [Read] and [Write] traits to perform IO
pub trait SyncStream: Stream + Read + Write {
    /// Reply to the specified tag with a given Result. Err's will be converted to 9p error
    /// messages automatically.
    fn reply(&mut self, tag: u16, resp: Result<Rdata>) {
        let r: Rmessage = (tag, resp).into();
        let _ = r.write_to(self);
    }
}

impl Stream for UnixStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}

impl SyncStream for UnixStream {}

impl Stream for TcpStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}

impl SyncStream for TcpStream {}
