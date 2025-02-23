//! Sans-IO layer for working with the 9p protocol to write clients and servers
//!
//! See https://sans-io.readthedocs.io/how-to-sans-io.html for information on sans-io
use crate::{
    sansio::protocol::{Rdata, Rmessage},
    Result,
};

pub mod protocol;
pub mod server;

impl From<(u16, Result<Rdata>)> for Rmessage {
    fn from((tag, content): (u16, Result<Rdata>)) -> Self {
        Rmessage {
            tag,
            content: content.unwrap_or_else(|ename| Rdata::Error { ename }),
        }
    }
}

// TODO: pull up as much as possible into this trait

/// An underlying stream over which we can handle 9p connections
pub trait Stream: Send + Sized + 'static {
    /// The underlying try_clone implementations for file descriptors can fail at the libc level so
    /// we need to account for that here.
    fn try_clone(&self) -> Result<Self>;
}
