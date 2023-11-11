//! RPC messaging between the fuse filesystem thread and the main editor thread
use std::sync::mpsc::{channel, Sender};

pub struct Message {
    pub req: Req,
    pub tx: Sender<String>,
}

impl Message {
    /// Make a request to the main thread and return the response we got back
    pub(super) fn send(req: Req, mtx: &Sender<Message>) -> String {
        let (tx, rx) = channel();
        let msg = Self { req, tx };

        mtx.send(msg).expect("main thread to be running");
        rx.recv().expect("response from main thread")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Req {
    CreateNewBuffer { name: String },

    ReadBufferName { id: usize },
    ReadBufferDot { id: usize },
    ReadBufferAddr { id: usize },
    ReadBufferBody { id: usize },

    SetBufferAddr { id: usize, addr: String },
    SetBufferContent { id: usize, s: String },
}
