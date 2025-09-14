//! Tokio based asynchronous implementation of 9p Servers and Clients
use crate::{
    Result,
    sansio::protocol::{NineP, Rdata, Rmessage, SharedBuf},
};
use simple_coro::CoroState;
use std::{future::Future, io, marker::Unpin};
use tokio::{
    io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt},
    net::{TcpStream, UnixStream},
};

pub mod client;
pub mod server;

/// Asynchronous IO support for reading and writing 9p messages
pub trait AsyncNineP: NineP + Send + Sync {
    /// Encode self as bytes for the 9p protocol and write to the given [AsyncWrite].
    fn write_to<W>(&self, w: &mut W) -> impl Future<Output = io::Result<()>> + Send
    where
        W: AsyncWrite + Unpin + Send,
    {
        write_to(self, w)
    }

    /// Decode self from 9p protocol bytes coming from the given [AsyncRead].
    fn read_from<R>(buf: &SharedBuf, r: &mut R) -> impl Future<Output = io::Result<Self>> + Send
    where
        R: AsyncRead + Unpin + Send,
    {
        read_from(buf, r)
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
        .map_err(|e| io::Error::other(e.to_string()))?;

    w.write_all(&buf).await
}

#[inline(always)]
async fn read_from<T, R>(buf: &SharedBuf, r: &mut R) -> io::Result<T>
where
    T: NineP + Send,
    R: AsyncRead + Unpin + Send,
{
    let mut coro = T::read_9p_coro(buf);
    loop {
        coro = match coro.resume() {
            CoroState::Pending(c, n) => {
                // SAFETY: coro is currently suspended and unable to take a reference to buf
                let mut_buf = unsafe { buf.as_inner_mut() };
                mut_buf.resize(n, 0);
                r.read_exact(mut_buf).await?;
                c.send(())
            }

            CoroState::Complete(res) => return res,
        };
    }
}

/// A Stream that makes use of the tokio [AsyncRead] and [AsyncWrite] traits to perform IO
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
