//! A synchronous implementation of 9p Servers and Clients
use crate::{
    sansio::{
        protocol::{NineP, Rdata, Rmessage},
        State,
    },
    Result,
};
use std::{
    future::Future,
    io::{self, Read, Write},
    net::TcpStream,
    os::unix::net::UnixStream,
    pin::pin,
    sync::Arc,
    task::{Context, Poll, Waker},
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
    fn read_from<R: Read>(r: &mut R) -> io::Result<Self> {
        let s = Arc::new(State::default());
        let waker = Waker::from(s.clone());
        let mut ctx = Context::from_waker(&waker);

        // SAFETY: assumes the impl of Read9p is a valid future for us to poll
        let mut fut = unsafe { pin!(Self::read()) };
        loop {
            match fut.as_mut().poll(&mut ctx) {
                Poll::Ready(val) => return val,
                // SAFETY: the only other reference to the shared state is in the future we are
                // polling so mutating its inner state is safe
                Poll::Pending => unsafe {
                    let n = s.requested_bytes();
                    let mut buf = vec![0; n];
                    r.read_exact(&mut buf)?;
                    s.set_bytes(buf);
                },
            }
        }
    }
}

impl<T> SyncNineP for T where T: NineP {}

/// A [Stream] that makes use of the standard library [Read] and [Write] traits to perform IO
pub trait SyncStream: Read + Write + Send + Sized + 'static {
    /// Clone this stream, accounting for operating system errors
    fn try_clone(&self) -> Result<Self>;

    /// Reply to the specified tag with a given Result. Err's will be converted to 9p error
    /// messages automatically.
    fn reply(&mut self, tag: u16, resp: Result<Rdata>) {
        let r: Rmessage = (tag, resp).into();
        let _ = r.write_to(self);
    }
}

impl SyncStream for UnixStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}

impl SyncStream for TcpStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}
