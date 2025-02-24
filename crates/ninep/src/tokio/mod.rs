//! Tokio based asynchronous implementation of 9p Servers and Clients
use crate::{
    sansio::protocol::{NineP, NinepReader, Rdata, Read9p, Rmessage},
    Result,
};
use std::{io, marker::Unpin};
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt},
    net::{TcpStream, UnixStream},
};

pub mod client;
pub mod server;

/// Synchronous IO support for reading and writing 9p messages
#[async_trait::async_trait]
pub trait AsyncNineP: NineP {
    /// Encode self as bytes for the 9p protocol and write to the given [SyncStream].
    async fn write_to<W: AsyncWrite + Unpin + Send>(&self, w: &mut W) -> io::Result<()> {
        let mut buf = vec![0; self.n_bytes()];
        self.write_bytes(&mut buf)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

        w.write_all(&buf).await
    }

    /// Decode self from 9p protocol bytes coming from the given [SyncStream].
    #[allow(clippy::uninit_vec)]
    async fn read_from<R: AsyncRead + Unpin + Send>(r: &mut R) -> io::Result<Self> {
        let mut nr = NinepReader::Pending(Self::reader());
        let mut buf = Vec::new();

        loop {
            match nr {
                NinepReader::Pending(r9) => {
                    let n = Self::Reader::needs_bytes(&r9);
                    buf.reserve(n.saturating_sub(buf.len()));
                    // SAFETY: we've just reserved sufficient capacity
                    unsafe { buf.set_len(n) };
                    r.read_exact(&mut buf).await?;
                    nr = r9.accept_bytes(&buf[0..n])?;
                }

                NinepReader::Complete(t) => return Ok(t),
            }
        }
    }
}

impl<T> AsyncNineP for T where T: NineP {}

/// A [Stream] that makes use of the standard library [Read] and [Write] traits to perform IO
#[allow(async_fn_in_trait)]
pub trait AsyncStream: AsyncRead + AsyncWrite + Unpin + Send + Sized + 'static {
    /// Reply to the specified tag with a given Result. Err's will be converted to 9p error
    /// messages automatically.
    async fn reply(&mut self, tag: u16, resp: Result<Rdata>) {
        let r: Rmessage = (tag, resp).into();
        let _ = r.write_to(self).await;
    }
}

impl AsyncStream for UnixStream {}
impl AsyncStream for TcpStream {}
