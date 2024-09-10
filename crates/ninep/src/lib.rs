//! A simple 9p Protocol implementation for serving filesystem interfaces
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

pub type Result<T> = std::result::Result<T, String>;

pub trait Stream: Read + Write + Send + Sized + 'static {
    /// The underlying try_clone implementations for file descriptors can fail at the libc level so
    /// we need to account for that here.
    fn try_clone(&self) -> Result<Self>;

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
