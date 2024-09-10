//! A simple 9p Protocol implementation for serving filesystem interfaces
#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::style,
    future_incompatible,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    rustdoc::all,
    clippy::undocumented_unsafe_blocks
)]

use std::{
    io::{Read, Write},
    net::TcpStream,
    os::unix::net::UnixStream,
};

pub mod client;
pub mod fs;
pub mod protocol;
pub mod server;

use protocol::{Format9p, Rdata, Rmessage};

impl From<(u16, Result<Rdata>)> for Rmessage {
    fn from((tag, content): (u16, Result<Rdata>)) -> Self {
        Rmessage {
            tag,
            content: content.unwrap_or_else(|ename| Rdata::Error { ename }),
        }
    }
}

/// A simple result type for errors returned from this crate
pub type Result<T> = std::result::Result<T, String>;

/// An underlying stream over which we can handle 9p connections
pub trait Stream: Read + Write + Send + Sized + 'static {
    /// The underlying try_clone implementations for file descriptors can fail at the libc level so
    /// we need to account for that here.
    fn try_clone(&self) -> Result<Self>;

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

impl Stream for TcpStream {
    fn try_clone(&self) -> Result<Self> {
        self.try_clone().map_err(|e| e.to_string())
    }
}
