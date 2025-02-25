//! Sans-IO layer for working with the 9p protocol to write clients and servers
//!
//! See https://sans-io.readthedocs.io/how-to-sans-io.html for information on sans-io
use crate::{
    sansio::protocol::{Rdata, Rmessage},
    Result,
};
use std::{
    sync::{Arc, Mutex},
    task::Wake,
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

/// Shared state between a NineP impl and a parent read loop that is performing IO.
#[derive(Default, Debug)]
pub(crate) struct State {
    pub(crate) inner: Mutex<StateInner>,
}

#[derive(Default, Debug)]
pub(crate) struct StateInner {
    pub(crate) n: usize,
    pub(crate) buf: Option<Vec<u8>>,
}

impl Wake for State {
    fn wake(self: Arc<Self>) {}
    fn wake_by_ref(self: &Arc<Self>) {}
}
