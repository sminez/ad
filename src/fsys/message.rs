//! RPC messaging between the fuse filesystem thread and the main editor thread
use crate::{buffer::BufferId, fsys::event::InputFilter, input::Event};
use std::sync::mpsc::{Sender, channel};
use tracing::error;

/// A wrapper around a [Req] that can be sent to the main editor event loop
#[derive(Debug)]
pub struct Message {
    pub req: Req,
    pub tx: Sender<Result<String, String>>,
}

impl Message {
    /// Make a request to the main thread and return the response we got back
    pub(super) fn send(req: Req, etx: &Sender<Event>) -> Result<String, String> {
        let (tx, rx) = channel();
        let evt = Event::Message(Self { req, tx });

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

#[derive(Debug, Clone)]
pub enum Req {
    ControlMessage {
        id: Option<BufferId>,
        msg: String,
    },
    MinibufferSelect {
        prompt: Option<String>,
        lines: String,
        tx: Sender<String>,
    },
    ReadBufferName {
        id: BufferId,
    },
    ReadBufferDot {
        id: BufferId,
    },
    ReadBufferXDot {
        id: BufferId,
    },
    ReadBufferAddr {
        id: BufferId,
    },
    ReadBufferXAddr {
        id: BufferId,
    },
    ReadBufferBody {
        id: BufferId,
    },
    ReadBufferFtype {
        id: BufferId,
    },
    SetBufferName {
        id: BufferId,
        s: String,
    },
    SetBufferDot {
        id: BufferId,
        s: String,
    },
    SetBufferXDot {
        id: BufferId,
        s: String,
    },
    SetBufferAddr {
        id: BufferId,
        s: String,
    },
    SetBufferXAddr {
        id: BufferId,
        s: String,
    },
    ClearBufferBody {
        id: BufferId,
    },
    AppendBufferBody {
        id: BufferId,
        s: String,
    },
    AppendOutput {
        id: BufferId,
        s: String,
    },
    AddInputEventFilter {
        id: BufferId,
        filter: InputFilter,
    },
    RemoveInputEventFilter {
        id: BufferId,
    },
    LoadInBuffer {
        id: BufferId,
        txt: String,
    },
    ExecuteInBuffer {
        id: BufferId,
        txt: String,
    },
}
