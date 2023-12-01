//! RPC messaging between the fuse filesystem thread and the main editor thread
use crate::editor::InputEvent;
use std::sync::mpsc::{channel, Sender};

#[derive(Debug)]
pub struct Message {
    pub req: Req,
    pub tx: Sender<Result<String, String>>,
}

impl Message {
    /// Make a request to the main thread and return the response we got back
    pub(super) fn send(req: Req, etx: &Sender<InputEvent>) -> Result<String, String> {
        let (tx, rx) = channel();
        let evt = InputEvent::Message(Self { req, tx });

        etx.send(evt).expect("main thread to be running");
        rx.recv().expect("response from main thread")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Req {
    ControlMessage { msg: String },

    ReadBufferName { id: usize },
    ReadBufferDot { id: usize },
    ReadBufferXDot { id: usize },
    ReadBufferAddr { id: usize },
    ReadBufferXAddr { id: usize },
    ReadBufferBody { id: usize },

    SetBufferDot { id: usize, s: String },
    SetBufferXDot { id: usize, s: String },
    SetBufferAddr { id: usize, s: String },
    SetBufferXAddr { id: usize, s: String },
    ClearBufferBody { id: usize },
    InsertBufferBody { id: usize, s: String, offset: usize },
}
