use ninep::server::{ClientId, ReadOutcome};
use std::{
    collections::HashMap,
    mem::swap,
    sync::mpsc::{channel, Receiver, Sender},
    thread::spawn,
};
use tracing::{debug, error};

/// Spawn a log listener that handles immediately returning data to blocked readers of the log file
/// as well as passing events to fsys itself so it can update its internal state.
pub(super) fn spawn_log_listener(
    log_event_rx: Receiver<LogEvent>,
    listener_tx: Sender<LogEvent>,
    pending_rx: Receiver<Sender<Vec<u8>>>,
) {
    spawn(move || {
        for event in log_event_rx.iter() {
            // Handle ReadOutcome::Blocked responses first
            for tx in pending_rx.try_iter() {
                _ = tx.send(event.as_log_line_bytes());
            }

            // Lazy update of fsys state
            _ = listener_tx.send(event);
        }
    });
}

/// A message sent by the main editor thread to notify the fs thread that
/// the current buffer list has changed.
#[derive(Debug, Clone, Copy)]
pub(crate) enum LogEvent {
    /// A newly created buffer
    Open(usize),
    /// A buffer that has now been closed and needs removing from state
    Close(usize),
    /// A change to the currently active buffer
    Focus(usize),
    /// A buffer was saved
    Save(usize),
}

impl LogEvent {
    pub(crate) fn as_log_line_bytes(self) -> Vec<u8> {
        let s = match self {
            LogEvent::Open(id) => format!("{id} open\n"),
            LogEvent::Close(id) => format!("{id} close\n"),
            LogEvent::Focus(id) => format!("{id} focus\n"),
            LogEvent::Save(id) => format!("{id} save\n"),
        };

        s.as_bytes().to_vec()
    }
}

#[derive(Debug)]
pub(super) enum ClientLog {
    Events(Vec<LogEvent>),
    Pending,
}

impl ClientLog {
    fn is_empty_events(&self) -> bool {
        matches!(self, ClientLog::Events(v) if v.is_empty())
    }
}

#[derive(Debug)]
pub(super) struct Log {
    events: HashMap<ClientId, ClientLog>,
    tx: Sender<Sender<Vec<u8>>>,
}

impl Log {
    pub(super) fn new(tx: Sender<Sender<Vec<u8>>>) -> Self {
        Self {
            events: HashMap::default(),
            tx,
        }
    }

    #[inline]
    pub(super) fn push(&mut self, evt: LogEvent) {
        for cl in self.events.values_mut() {
            match cl {
                ClientLog::Events(v) => v.push(evt),
                ClientLog::Pending => {
                    // Event will have been sent from the listener so drop this one and start
                    // storing new ones as they arrive
                    *cl = ClientLog::Events(Vec::new());
                }
            }
        }
    }

    #[inline]
    pub(super) fn add_client(&mut self, cid: ClientId) {
        self.events.insert(cid, ClientLog::Events(Vec::new()));
    }

    #[inline]
    pub(super) fn remove_client(&mut self, cid: ClientId) {
        self.events.remove(&cid);
    }

    #[inline]
    pub(super) fn events_since_last_read(&mut self, cid: ClientId) -> ReadOutcome {
        match self.events.get_mut(&cid) {
            Some(cl) if cl.is_empty_events() => {
                let (tx, rx) = channel();
                if self.tx.send(tx).is_err() {
                    error!("log listener died");
                    return ReadOutcome::Immediate(Vec::new());
                }

                *cl = ClientLog::Pending;

                ReadOutcome::Blocked(rx)
            }

            Some(ClientLog::Events(events)) => {
                let mut v = Vec::new();
                swap(events, &mut v);

                ReadOutcome::Immediate(v.into_iter().flat_map(|e| e.as_log_line_bytes()).collect())
            }

            Some(ClientLog::Pending) => {
                debug!("got log read from {cid:?} while blocked read was outstanding");
                ReadOutcome::Immediate(Vec::new())
            }

            None => {
                error!("got log read from {cid:?} without initialising");
                ReadOutcome::Immediate(Vec::new())
            }
        }
    }
}
