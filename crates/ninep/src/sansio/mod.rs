//! Sans-IO layer for working with the 9p protocol to write clients and servers
//!
//! See https://sans-io.readthedocs.io/how-to-sans-io.html for information on sans-io
use crate::{
    sansio::protocol::{Rdata, Rmessage},
    Result,
};
use std::{cell::UnsafeCell, sync::Arc, task::Wake};

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
    inner: UnsafeCell<StateInner>,
}

/// SAFETY: we can only access the inner state in this crate
unsafe impl Send for State {}
/// SAFETY: we can only access the inner state in this crate
unsafe impl Sync for State {}

impl State {
    pub(crate) unsafe fn requested_bytes(&self) -> usize {
        // SAFETY: can only be called inside of an I/O read loop that owns the state
        unsafe { (*self.inner.get()).n }
    }

    pub(crate) unsafe fn take_bytes(&self) -> Vec<u8> {
        // SAFETY: can only be called inside of an I/O read loop that owns the state
        unsafe { (*self.inner.get()).buf.take().unwrap_unchecked() }
    }

    pub(crate) unsafe fn set_requested(&self, n: usize) {
        // SAFETY: can only be called inside of an I/O read loop that owns the state
        unsafe { (*self.inner.get()).n = n };
    }

    pub(crate) unsafe fn set_bytes(&self, buf: Vec<u8>) {
        // SAFETY: can only be called inside of an I/O read loop that owns the state
        unsafe { (*self.inner.get()).buf = Some(buf) };
    }
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
