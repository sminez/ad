//! RPC messaging between the fuse filesystem thread and the main editor thread
use crate::input::InputEvent;
use std::sync::mpsc::{channel, Sender};
use tracing::error;

/// A wrapper around a [Req] that can be sent to the main editor event loop
#[derive(Debug)]
pub(crate) struct Message {
    pub req: Req,
    pub tx: Sender<Result<String, String>>,
}

impl Message {
    /// Make a request to the main thread and return the response we got back
    pub(super) fn send(req: Req, etx: &Sender<InputEvent>) -> Result<String, String> {
        let (tx, rx) = channel();
        let evt = InputEvent::Message(Self { req, tx });

        if let Err(e) = etx.send(evt) {
            error!("error sending message from fsys to main thread: {e}");
        }

        match rx.recv() {
            Ok(res) => res,
            Err(e) => {
                let err = format!("error receiving message from main thread: {e}");
                error!("{err}");
                Err(err)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Req {
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
    AppendOutput { id: usize, s: String },
}
