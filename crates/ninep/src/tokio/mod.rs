//! Tokio based asynchronous implementation of 9p Servers and Clients
use crate::{
    sansio::protocol::{NineP, Rdata, Rmessage},
    Result,
};
use simple_coro::{Coro, CoroState};
use std::{future::Future, io, marker::Unpin};
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt},
    net::{TcpStream, UnixStream},
};

pub mod client;
pub mod server;

/// Asynchronous IO support for reading and writing 9p messages
pub trait AsyncNineP: NineP + Send + Sync {
    /// Encode self as bytes for the 9p protocol and write to the given [SyncStream].
    fn write_to<W>(&self, w: &mut W) -> impl Future<Output = io::Result<()>> + Send
    where
        W: AsyncWrite + Unpin + Send,
    {
        write_to(self, w)
    }

    /// Decode self from 9p protocol bytes coming from the given [SyncStream].
    fn read_from<R>(r: &mut R) -> impl Future<Output = io::Result<Self>> + Send
    where
        R: AsyncRead + Unpin + Send,
    {
        read_from(r)
    }
}

impl<T> AsyncNineP for T where T: NineP + Send + Sync {}

// write_to and read_from are written as free functions so we can use async/await here while also
// explicitly requiring a Send bound on the methods of the AsyncNineP trait above.

#[inline(always)]
async fn write_to<T, W>(t: &T, w: &mut W) -> io::Result<()>
where
    T: NineP + Sync,
    W: AsyncWrite + Unpin + Send,
{
    let mut buf = vec![0; t.n_bytes()];
    t.write_bytes(&mut buf)
        .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?;

    w.write_all(&buf).await
}

#[inline(always)]
async fn read_from<T, R>(r: &mut R) -> io::Result<T>
where
    T: NineP + Send,
    R: AsyncRead + Unpin + Send,
{
    let mut coro = Coro::from(T::read_9p);
    loop {
        coro = match coro.resume() {
            CoroState::Pending(c, n) => {
                let mut buf = vec![0; n];
                r.read_exact(&mut buf).await?;
                c.send(buf)
            }

            CoroState::Complete(res) => return res,
        };
    }
}

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
