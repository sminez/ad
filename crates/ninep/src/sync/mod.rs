//! A synchronous implementation of 9p Servers and Clients
use crate::{
    sansio::protocol::{NineP, SharedBuf},
    Result,
};
use simple_coro::CoroState;
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
            .map_err(|e| io::Error::other(e.to_string()))?;

        w.write_all(&buf)
    }

    /// Decode self from 9p protocol bytes coming from the given [SyncStream].
    fn read_from<R: Read>(buf: &SharedBuf, r: &mut R) -> io::Result<Self> {
        let mut coro = Self::read_9p_coro(buf);
        loop {
            coro = match coro.resume() {
                CoroState::Pending(c, n) => {
                    // SAFETY: coro is currently suspended and unable to take a reference to buf
                    let mut_buf = unsafe { buf.as_inner_mut() };
                    mut_buf.resize(n, 0);
                    r.read_exact(mut_buf)?;
                    c.send(())
                }

                CoroState::Complete(res) => return res,
            };
        }
    }
}

impl<T> SyncNineP for T where T: NineP {}

/// A Stream that makes use of the standard library [Read] and [Write] traits to perform IO
pub trait SyncStream: Read + Write + Send + Sized + 'static {}

impl SyncStream for UnixStream {}
impl SyncStream for TcpStream {}

/// A Stream that makes use of the standard library [Read] and [Write] traits to perform IO
/// and additionally supports cloning the stream.
pub trait SyncServerStream: SyncStream {
    /// Clone this stream, accounting for operating system errors
    fn try_clone(&self) -> Result<Self>;
}

impl SyncServerStream for UnixStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}

impl SyncServerStream for TcpStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}
