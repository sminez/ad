//! Buffer state for the fuse filesystem
use crate::{
    die,
    fsys::{
        empty_dir_stat, empty_file_stat,
        event::{run_threaded_input_listener, send_event_to_editor, InputFilter, InputRequest},
        InternalRead, Message, Req, Result, BUFFERS_DIR, BUFFERS_QID, CURRENT_BUFFER,
        CURRENT_BUFFER_QID, E_UNKNOWN_FILE, INDEX_BUFFER, INDEX_BUFFER_QID, QID_OFFSET,
    },
    input::Event,
};
use ninep::{
    fs::Stat,
    server::{ClientId, ReadOutcome},
};
use std::{
    collections::{BTreeMap, HashMap},
    mem::swap,
    sync::mpsc::{channel, Receiver, Sender},
    time::SystemTime,
};
use tracing::{debug, error, trace};

const FILENAME: &str = "filename";
const DOT: &str = "dot";
const ADDR: &str = "addr";
const XDOT: &str = "xdot";
const XADDR: &str = "xaddr";
const BODY: &str = "body";
const EVENT: &str = "event";
const OUTPUT: &str = "output";

pub(super) const BUFFER_FILES: [(u64, &str); QID_OFFSET as usize - 1] = [
    (1, FILENAME),
    (2, DOT),
    (3, ADDR),
    (4, XDOT),
    (5, XADDR),
    (6, BODY),
    (7, EVENT),
    (8, OUTPUT),
];

fn parent_and_fname(qid: u64) -> (u64, &'static str) {
    assert!(qid > CURRENT_BUFFER_QID, "invalid buffer file qid");

    let (cur, off) = (CURRENT_BUFFER_QID, QID_OFFSET);
    let parent = cur + 1 + ((qid - cur - 1) / off) * off;
    let fname = BUFFER_FILES[((qid - cur - 2) % off) as usize].1;

    (parent, fname)
}

/// A message sent by the main editor thread to notify the fs thread that
/// the current buffer list has changed.
#[derive(Debug, Clone, Copy)]
pub(crate) enum LogEvent {
    /// A newly created buffer
    Add(usize),
    /// A buffer that has now been closed and needs removing from state
    Remove(usize),
    /// A change to the currently active buffer
    Current(usize),
    /// A buffer was saved
    Saved(usize),
}

impl LogEvent {
    pub(crate) fn as_log_line_bytes(self) -> Vec<u8> {
        let s = match self {
            LogEvent::Add(id) => format!("{id} buffer opened\n"),
            LogEvent::Remove(id) => format!("{id} buffer closed\n"),
            LogEvent::Current(id) => format!("{id} buffer focused\n"),
            LogEvent::Saved(id) => format!("{id} buffer saved\n"),
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
    fn new(tx: Sender<Sender<Vec<u8>>>) -> Self {
        Self {
            events: HashMap::default(),
            tx,
        }
    }

    #[inline]
    fn push(&mut self, evt: LogEvent) {
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

#[derive(Debug)]
pub(super) enum QidCheck {
    Unknown,
    EventFile { buf_qid: u64 },
    OtherFile,
}

#[derive(Debug)]
pub(super) struct BufferNodes {
    pub(super) known: BTreeMap<u64, BufferNode>,
    pub(super) log: Log,
    next_qid: u64,
    current_buffid: usize,
    stat: Stat,
    current_buff_stat: Stat,
    index_stat: Stat,
    tx: Sender<Event>,
    brx: Receiver<LogEvent>,
}

impl BufferNodes {
    pub(super) fn new(
        tx: Sender<Event>,
        brx: Receiver<LogEvent>,
        log_tx: Sender<Sender<Vec<u8>>>,
    ) -> Self {
        Self {
            known: BTreeMap::default(),
            log: Log::new(log_tx),
            next_qid: CURRENT_BUFFER_QID + 1,
            current_buffid: 1,
            stat: empty_dir_stat(BUFFERS_QID, BUFFERS_DIR),
            current_buff_stat: empty_file_stat(CURRENT_BUFFER_QID, CURRENT_BUFFER),
            index_stat: empty_file_stat(INDEX_BUFFER_QID, INDEX_BUFFER),
            tx,
            brx,
        }
    }

    pub(super) fn stat(&self) -> Stat {
        self.stat.clone()
    }

    pub(super) fn top_level_stats(&self) -> Vec<Stat> {
        let mut stats: Vec<Stat> = self.known.values().map(|b| b.stat.clone()).collect();
        stats.push(self.current_buff_stat.clone());
        stats.push(self.index_stat.clone());

        stats
    }

    pub(super) fn buffer_level_stats(&self, qid: u64) -> Option<Vec<Stat>> {
        self.known.get(&qid).map(|b| b.file_stats())
    }

    pub(super) fn is_known_buffer_qid(&self, qid: u64) -> bool {
        self.known.contains_key(&qid) || self.known.values().any(|bn| bn.contains_qid(qid))
    }

    pub(super) fn check_if_known_qid(&self, qid: u64) -> QidCheck {
        if self.known.contains_key(&qid) {
            QidCheck::OtherFile
        } else {
            for (&buf_qid, bn) in self.known.iter() {
                match bn.check_if_known_qid(qid) {
                    QidCheck::Unknown => (),
                    QidCheck::EventFile { .. } => return QidCheck::EventFile { buf_qid },
                    QidCheck::OtherFile => return QidCheck::OtherFile,
                }
            }

            QidCheck::Unknown
        }
    }

    pub(super) fn attach_input_filter(&mut self, buf_qid: u64) -> Result<()> {
        match self.known.get_mut(&buf_qid) {
            Some(bn) => bn.attach_input_filter(&self.tx),
            None => Err(E_UNKNOWN_FILE.to_string()),
        }
    }

    pub(super) fn clear_input_filter(&mut self, buf_qid: u64) {
        if let Some(bn) = self.known.get_mut(&buf_qid) {
            bn.clear_input_filter(&self.tx);
        }
    }

    pub(super) fn lookup_file_stat(&mut self, parent: u64, name: &str) -> Option<Stat> {
        match (parent, name) {
            (BUFFERS_QID, CURRENT_BUFFER) => Some(self.current_buff_stat.clone()),
            (BUFFERS_QID, INDEX_BUFFER) => Some(self.index_stat.clone()),
            (BUFFERS_QID, _) => self
                .known
                .values()
                .find(|b| b.str_id == name)
                .map(|b| b.stat()),
            _ => self
                .known
                .get_mut(&parent)?
                .refreshed_file_stat(name, &self.tx)
                .clone(),
        }
    }

    pub(super) fn get_stat_for_qid(&mut self, qid: u64) -> Option<Stat> {
        // If this is a known directory then we return the stat for it
        if qid == BUFFERS_QID {
            return Some(self.stat());
        } else if qid == CURRENT_BUFFER_QID {
            return Some(self.current_buff_stat.clone());
        } else if qid == INDEX_BUFFER_QID {
            return Some(self.index_stat.clone());
        } else if let Some(b) = self.known.get(&qid) {
            return Some(b.stat());
        }

        // Otherwise we see if this is known buffer file
        let (parent, fname) = parent_and_fname(qid);
        self.known
            .get_mut(&parent)?
            .refreshed_file_stat(fname, &self.tx)
    }

    pub(super) fn get_file_content(&mut self, qid: u64) -> InternalRead {
        if qid == CURRENT_BUFFER_QID {
            return InternalRead::Immediate(self.current_buffid.to_string().as_bytes().to_vec());
        } else if qid == INDEX_BUFFER_QID {
            return InternalRead::Immediate(self.index().as_bytes().to_vec());
        }

        let (parent, fname) = parent_and_fname(qid);
        match self.known.get_mut(&parent) {
            Some(bn) => bn.current_file_content(fname, &self.tx),
            None => InternalRead::Unknown,
        }
    }

    fn index(&mut self) -> String {
        let mut entries = Vec::with_capacity(self.known.len());

        for b in self.known.values_mut() {
            let filename = b.current_file_content_as_string(FILENAME, &self.tx);
            let id = &b.str_id;

            entries.push(format!("{id}\t{filename}\n"));
        }

        entries.join("")
    }

    pub(super) fn truncate(&mut self, qid: u64) {
        if qid == CURRENT_BUFFER_QID || qid == INDEX_BUFFER_QID {
            return;
        }

        let (parent, fname) = parent_and_fname(qid);
        let b = match self.known.get_mut(&parent) {
            Some(b) => b,
            None => return,
        };
        b.stat.last_modified = SystemTime::now();
        b.stat.n_bytes = 0;
        let id = b.id;

        if fname == BODY {
            _ = Message::send(Req::ClearBufferBody { id }, &self.tx);
        }
    }

    pub(super) fn write(&mut self, qid: u64, s: String, offset: usize) -> Result<usize> {
        let (parent, fname) = parent_and_fname(qid);
        let b = match self.known.get_mut(&parent) {
            Some(b) => b,
            None => return Err(E_UNKNOWN_FILE.to_string()),
        };
        b.stat.last_modified = SystemTime::now();
        let id = b.id;

        let n_bytes = s.len();
        let req = match fname {
            DOT => Req::SetBufferDot { id, s },
            ADDR => Req::SetBufferAddr { id, s },
            BODY => Req::InsertBufferBody { id, s, offset },
            XDOT => Req::SetBufferXDot { id, s },
            XADDR => Req::SetBufferXAddr { id, s },
            OUTPUT => Req::AppendOutput { id, s },
            EVENT => return send_event_to_editor(id, &s, &self.tx),
            FILENAME => return Err(E_UNKNOWN_FILE.to_string()),
            _ => return Err(E_UNKNOWN_FILE.to_string()),
        };

        match Message::send(req, &self.tx) {
            Ok(_) => Ok(n_bytes),
            Err(e) => Err(format!("unable to execute control message: {e}")),
        }
    }

    /// Process any pending updates from the main thread for changes to the buffer set
    pub(super) fn update(&mut self) {
        for bid in self.brx.try_iter() {
            self.log.push(bid);
            match bid {
                LogEvent::Add(id) => {
                    debug!(%id, "adding buffer to fsys state");
                    let qid = self.next_qid;
                    self.next_qid += QID_OFFSET;
                    self.known.insert(qid, BufferNode::new(id, qid));
                }

                // TODO: handle closing defered reads of files associated with this buffer
                LogEvent::Remove(id) => {
                    debug!(%id, "removing buffer from fsys state");
                    self.known.retain(|_, v| v.id != id);
                }

                LogEvent::Current(id) => {
                    debug!(%id, "setting current buffer in fsys state");
                    self.current_buffid = id;
                    self.current_buff_stat.n_bytes = id.to_string().len() as u64;
                }

                LogEvent::Saved(_) => (), // only used in the log
            };
        }
    }
}

/// A BufferNode in the filesystem is a directory containing a fixed
/// set of control files
///
/// The qids generated for each of the control files are based on offsets
/// from the qid of the buffer directory itself (see QID_OFFSET above).
#[derive(Debug)]
pub(super) struct BufferNode {
    id: usize,
    str_id: String,
    stat: Stat,
    file_stats: BTreeMap<&'static str, Stat>,
    input_handle: Option<Sender<InputRequest>>,
}

impl BufferNode {
    fn new(id: usize, qid: u64) -> Self {
        Self {
            id,
            str_id: id.to_string(),
            stat: empty_dir_stat(qid, &id.to_string()),
            file_stats: stub_file_stats(qid),
            input_handle: None,
        }
    }

    fn stat(&self) -> Stat {
        self.stat.clone()
    }

    fn file_stats(&self) -> Vec<Stat> {
        self.file_stats.values().cloned().collect()
    }

    fn contains_qid(&self, qid: u64) -> bool {
        self.file_stats.values().any(|s| s.fm.qid == qid)
    }

    fn check_if_known_qid(&self, qid: u64) -> QidCheck {
        for (&fname, s) in self.file_stats.iter() {
            if s.fm.qid == qid {
                return if fname == EVENT {
                    // replaced with the correct qid by BufferNodes
                    QidCheck::EventFile { buf_qid: 0 }
                } else {
                    QidCheck::OtherFile
                };
            }
        }

        QidCheck::Unknown
    }

    fn attach_input_filter(&mut self, etx: &Sender<Event>) -> Result<()> {
        let (tx, rx) = channel();
        let req = Req::AddInputEventFilter {
            id: self.id,
            filter: InputFilter::new(tx),
        };

        Message::send(req, etx)?;
        self.input_handle = Some(run_threaded_input_listener(rx));

        Ok(())
    }

    fn clear_input_filter(&mut self, etx: &Sender<Event>) {
        if let Some(tx) = self.input_handle.take() {
            _ = tx.send(InputRequest::Shutdown);
            _ = Message::send(Req::RemoveInputEventFilter { id: self.id }, etx);
        }
    }

    fn refreshed_file_stat(&mut self, fname: &str, tx: &Sender<Event>) -> Option<Stat> {
        if fname == OUTPUT || fname == EVENT {
            return self.file_stats.get(fname).cloned();
        }

        trace!(id=%self.id, %fname, "refreshing file stat");
        let content = self.current_file_content_as_string(fname, tx);
        let stat = self.file_stats.get_mut(fname)?;
        stat.n_bytes = content.as_bytes().len() as u64;

        Some(stat.clone())
    }

    // Must not be called for the event file or unknown files
    fn current_file_content_as_string(&mut self, fname: &str, tx: &Sender<Event>) -> String {
        let req = match fname {
            FILENAME => Req::ReadBufferName { id: self.id },
            DOT => Req::ReadBufferDot { id: self.id },
            ADDR => Req::ReadBufferAddr { id: self.id },
            BODY => Req::ReadBufferBody { id: self.id },
            XDOT => Req::ReadBufferXDot { id: self.id },
            XADDR => Req::ReadBufferXAddr { id: self.id },
            OUTPUT => return String::new(),
            fname => die!("current_file_as_string called for {fname}"),
        };

        match Message::send(req, tx) {
            Ok(s) => s,
            Err(e) => {
                error!("fsys failed to read file content: {e}");
                String::new()
            }
        }
    }

    fn current_file_content(&mut self, fname: &str, tx: &Sender<Event>) -> InternalRead {
        let req = match fname {
            FILENAME => Req::ReadBufferName { id: self.id },
            DOT => Req::ReadBufferDot { id: self.id },
            ADDR => Req::ReadBufferAddr { id: self.id },
            BODY => Req::ReadBufferBody { id: self.id },
            XDOT => Req::ReadBufferXDot { id: self.id },
            XADDR => Req::ReadBufferXAddr { id: self.id },
            OUTPUT => return InternalRead::Immediate(Vec::new()),
            EVENT => {
                return match self.pending_events() {
                    Ok(ir) => ir,
                    Err(_) => {
                        self.clear_input_filter(tx);
                        InternalRead::Unknown
                    }
                }
            }

            _ => return InternalRead::Unknown,
        };

        match Message::send(req, tx) {
            Ok(s) => InternalRead::Immediate(s.as_bytes().to_vec()),
            Err(e) => {
                error!("fsys failed to read file content: {e}");
                InternalRead::Unknown
            }
        }
    }

    fn pending_events(&self) -> Result<InternalRead> {
        let tx = match self.input_handle.as_ref() {
            Some(tx) => tx,
            None => {
                error!("pending_events without input filter (id={})", self.id);
                return Ok(InternalRead::Unknown);
            }
        };

        let (read_tx, read_rx) = channel();
        if tx.send(InputRequest::Read { tx: read_tx }).is_err() {
            return Err("failed to send".to_string());
        }

        match read_rx.recv() {
            Ok(ReadOutcome::Immediate(data)) => Ok(InternalRead::Immediate(data)),
            Ok(ReadOutcome::Blocked(rx)) => Ok(InternalRead::Blocked(rx)),
            Err(_) => Err("failed to read".to_string()),
        }
    }
}

fn stub_file_stats(qid: u64) -> BTreeMap<&'static str, Stat> {
    let mut m = BTreeMap::new();

    for (offset, name) in BUFFER_FILES.into_iter() {
        m.insert(name, empty_file_stat(qid + offset, name));
    }

    m
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case(CURRENT_BUFFER_QID + 1 + 1, CURRENT_BUFFER_QID + 1, FILENAME; "filename first buffer")]
    #[test_case(8, 6, DOT; "dot second buffer")]
    #[test_case(21, 15, BODY; "body second buffer")]
    #[test]
    fn parent_and_fname_works(qid: u64, parent: u64, fname: &str) {
        let (p, f) = parent_and_fname(qid);

        assert_eq!(p, parent);
        assert_eq!(f, fname);
    }
}
