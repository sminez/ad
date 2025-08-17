//! Message formats for the events file
use crate::{
    dot::Range,
    fsys::{
        Result,
        message::{Message, Req},
    },
    input::Event,
};
use ad_event::{FsysEvent, Kind, Source};
use ninep::sync::server::ReadOutcome;
use std::{
    sync::mpsc::{Receiver, Sender, channel},
    thread::spawn,
};

/// A filter handle that is stored on a particular Buffer for filtering input events by passing
/// them back to fsys for a connected client.
#[derive(Debug, Clone)]
pub struct InputFilter {
    tx: Sender<FsysEvent>,
    is_scratch: bool,
}

impl InputFilter {
    pub(crate) fn new(tx: Sender<FsysEvent>) -> Self {
        Self {
            tx,
            is_scratch: false,
        }
    }

    /// Create a copy of this filter for events coming from the scratch buffer
    pub(crate) fn paired_tag_filter(&self) -> Self {
        Self {
            tx: self.tx.clone(),
            is_scratch: true,
        }
    }

    pub fn notify_insert(&self, source: Source, ch_from: usize, ch_to: usize, txt: &str) {
        let k = if self.is_scratch {
            Kind::InsertScratch
        } else {
            Kind::InsertBody
        };
        let evt = FsysEvent::new(source, k, ch_from, ch_to, txt);
        _ = self.tx.send(evt);
    }

    pub fn notify_delete(&self, source: Source, ch_from: usize, ch_to: usize) {
        let k = if self.is_scratch {
            Kind::DeleteScratch
        } else {
            Kind::DeleteBody
        };
        let evt = FsysEvent::new(source, k, ch_from, ch_to, "");
        _ = self.tx.send(evt);
    }

    pub fn notify_load(&self, source: Source, ch_from: usize, ch_to: usize, txt: &str) {
        let k = if self.is_scratch {
            Kind::LoadScratch
        } else {
            Kind::LoadBody
        };
        let evt = FsysEvent::new(source, k, ch_from, ch_to, txt);
        _ = self.tx.send(evt);
    }

    pub fn notify_execute(
        &self,
        source: Source,
        ch_from: usize,
        ch_to: usize,
        txt: &str,
        arg: Option<(Range, String)>,
    ) {
        if let Some((rng, arg)) = arg {
            let (from, to) = (rng.start.idx, rng.end.idx);
            let evt = FsysEvent::new(source, Kind::ChordedArgument, from, to, &arg);
            _ = self.tx.send(evt);
        }
        let k = if self.is_scratch {
            Kind::ExecuteScratch
        } else {
            Kind::ExecuteBody
        };

        let evt = FsysEvent::new(source, k, ch_from, ch_to, txt);
        _ = self.tx.send(evt);
    }
}

#[derive(Debug)]
pub enum InputRequest {
    Shutdown,
    Read { tx: Sender<ReadOutcome> },
}

/// Spawn an input listener that is connected to a paired [InputFilter] stored in the main editor
/// state.
///
/// When read requests come through from the fsys they are either immediately handled if data is
/// pending or a blocked read is returned and then this thread will wait for the next event to come
/// through before returning it.
pub fn run_threaded_input_listener(event_rx: Receiver<FsysEvent>) -> Sender<InputRequest> {
    let (fsys_tx, fsys_rx) = channel();

    spawn(move || {
        loop {
            let tx = match fsys_rx.recv() {
                Ok(InputRequest::Shutdown) | Err(_) => return,
                Ok(InputRequest::Read { tx }) => tx,
            };

            // If events are available now then return them immediately
            let content: String = event_rx
                .try_iter()
                .map(|e| e.as_event_file_line())
                .collect();

            if !content.is_empty() {
                _ = tx.send(ReadOutcome::Immediate(content.into_bytes()));
                continue;
            }

            // Otherwise return a blocked read and wait for the next event to come through
            let (read_tx, read_rx) = channel();
            _ = tx.send(ReadOutcome::Blocked(read_rx));
            let data = match event_rx.recv() {
                Ok(evt) => evt.as_event_file_line().into_bytes(),
                Err(_) => return,
            };

            _ = read_tx.send(data);
        }
    });

    fsys_tx
}

pub fn send_event_to_editor(id: usize, s: &str, tx: &Sender<Event>) -> Result<usize> {
    let n_bytes_written = s.len();

    let evt = FsysEvent::try_from_str(s)?;
    let req = match evt.kind {
        Kind::LoadBody | Kind::LoadScratch => Req::LoadInBuffer { id, txt: evt.txt },
        Kind::ExecuteBody | Kind::ExecuteScratch => Req::ExecuteInBuffer { id, txt: evt.txt },
        _ => return Ok(n_bytes_written),
    };

    Message::send(req, tx)?;

    Ok(n_bytes_written)
}
