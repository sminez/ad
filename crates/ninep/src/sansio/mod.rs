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
