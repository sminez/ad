//! Buffer state for the fuse filesystem
use crate::{
    fsys::{
        BUFFERS_DIR, BUFFERS_QID, CURRENT_BUFFER, CURRENT_BUFFER_QID, E_UNKNOWN_FILE, INDEX_BUFFER,
        INDEX_BUFFER_QID, InternalRead, Message, QID_OFFSET, Req, Result, apply_offset,
        empty_dir_stat, empty_file_stat, empty_file_stat_with_perms,
        event::{InputFilter, InputRequest, run_threaded_input_listener, send_event_to_editor},
        log::{Log, LogEvent},
    },
    input::Event,
};
use ninep::{
    fs::{FileType, Perm, Stat, Timestamp},
    sync::server::ReadOutcome,
};
use std::{
    collections::BTreeMap,
    sync::mpsc::{Receiver, Sender, channel},
};
use tracing::{debug, error, trace};

pub(super) const ADDR: &str = "addr";
pub(super) const BODY: &str = "body";
pub(super) const CTL: &str = "ctl"; // write only
pub(super) const DOT: &str = "dot";
pub(super) const EVENT: &str = "event"; // exclusive
pub(super) const FILENAME: &str = "filename";
pub(super) const FILETYPE: &str = "filetype";
pub(super) const OUTPUT: &str = "output"; // write only
pub(super) const XADDR: &str = "xaddr";
pub(super) const XDOT: &str = "xdot";

pub(super) const BUFFER_FILES: [(u64, &str); QID_OFFSET as usize - 1] = [
    (1, ADDR),
    (2, BODY),
    (3, CTL),
    (4, DOT),
    (5, EVENT),
    (6, FILENAME),
    (7, FILETYPE),
    (8, OUTPUT),
    (9, XADDR),
    (10, XDOT),
];

fn parent_and_fname(qid: u64) -> (u64, &'static str) {
    assert!(qid > CURRENT_BUFFER_QID, "invalid buffer file qid");

    let (cur, off) = (CURRENT_BUFFER_QID, QID_OFFSET);
    let parent = cur + 1 + ((qid - cur - 1) / off) * off;
    let fname = BUFFER_FILES[((qid - cur - 2) % off) as usize].1;

    (parent, fname)
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
            index_stat: empty_file_stat_with_perms(
                INDEX_BUFFER_QID,
                INDEX_BUFFER,
                Perm::OWNER_READ,
            ),
            tx,
            brx,
        }
    }

    pub(super) fn stat(&self) -> Stat {
        self.stat.clone()
    }

    pub(super) fn top_level_stats(&self) -> Vec<Stat> {
        let mut stats = vec![self.index_stat.clone(), self.current_buff_stat.clone()];
        stats.extend(self.known.values().map(|b| b.stat.clone()));

        stats
    }

    pub(super) fn buffer_level_stats(&self, qid: u64) -> Option<Vec<Stat>> {
        self.known.get(&qid).map(|b| b.file_stats())
    }

    pub(super) fn is_known_qid(&self, qid: u64) -> bool {
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

    pub(super) fn has_input_filter(&self, buf_qid: u64) -> bool {
        match self.known.get(&buf_qid) {
            Some(bn) => bn.input_handle.is_some(),
            None => false,
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

    pub(super) fn get_file_content(
        &mut self,
        qid: u64,
        offset: usize,
        count: usize,
    ) -> InternalRead {
        if qid == CURRENT_BUFFER_QID {
            return InternalRead::Immediate(apply_offset(
                self.current_buffid.to_string().as_bytes(),
                offset,
                count,
            ));
        } else if qid == INDEX_BUFFER_QID {
            return InternalRead::Immediate(apply_offset(self.index().as_bytes(), offset, count));
        }

        let (parent, fname) = parent_and_fname(qid);
        match self.known.get_mut(&parent) {
            Some(bn) => bn.current_file_content(fname, offset, count, &self.tx),
            None => InternalRead::Unknown,
        }
    }

    fn index(&mut self) -> String {
        let mut entries = Vec::with_capacity(self.known.len());

        for b in self.known.values_mut() {
            let filename = b
                .current_file_content_as_string(FILENAME, &self.tx)
                .expect("FILENAME to be valid");
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
        b.stat.last_modified = Timestamp::now();
        b.stat.n_bytes = 0;
        let id = b.id;

        if fname == BODY {
            _ = Message::send(Req::ClearBufferBody { id }, &self.tx);
        }
    }

    pub(super) fn write(&mut self, qid: u64, s: String, _offset: usize) -> Result<usize> {
        let (parent, fname) = parent_and_fname(qid);
        let b = match self.known.get_mut(&parent) {
            Some(b) => b,
            None => return Err(E_UNKNOWN_FILE.to_string()),
        };
        b.stat.last_modified = Timestamp::now();
        let id = b.id;

        let n_bytes = s.len();
        let req = match fname {
            ADDR => Req::SetBufferAddr { id, s },
            BODY => Req::AppendBufferBody { id, s },
            CTL => Req::ControlMessage {
                id: Some(id),
                msg: s,
            },
            DOT => Req::SetBufferDot { id, s },
            EVENT => return send_event_to_editor(id, &s, &self.tx),
            FILENAME => Req::SetBufferName { id, s },
            OUTPUT => Req::AppendOutput { id, s },
            XADDR => Req::SetBufferXAddr { id, s },
            XDOT => Req::SetBufferXDot { id, s },
            _ => return Err(E_UNKNOWN_FILE.to_string()),
        };

        match Message::send(req, &self.tx) {
            Ok(_) => Ok(n_bytes),
            Err(e) => Err(format!(
                "unable to write to {fname} (n_bytes={n_bytes}): {e}",
            )),
        }
    }

    /// Process any pending updates from the main thread for changes to the buffer set
    pub(super) fn update(&mut self) {
        for bid in self.brx.try_iter() {
            self.log.push(bid);
            match bid {
                LogEvent::Open(id) => {
                    debug!(%id, "adding buffer to fsys state");
                    let qid = self.next_qid;
                    self.next_qid += QID_OFFSET;
                    self.known.insert(qid, BufferNode::new(id, qid));
                }

                // TODO: handle closing deferred reads of files associated with this buffer
                LogEvent::Close(id) => {
                    debug!(%id, "removing buffer from fsys state");
                    self.known.retain(|_, v| v.id != id);
                }

                LogEvent::Focus(id) => {
                    debug!(%id, "setting current buffer in fsys state");
                    self.current_buffid = id;
                    self.current_buff_stat.n_bytes = id.to_string().len() as u64;
                }

                LogEvent::Save(_) => (), // only used in the log
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
    pub id: usize,
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
        self.file_stats.values().any(|s| s.qid.path == qid)
    }

    fn check_if_known_qid(&self, qid: u64) -> QidCheck {
        for (&fname, s) in self.file_stats.iter() {
            if s.qid.path == qid {
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
        if fname == OUTPUT || fname == EVENT || fname == CTL {
            return self.file_stats.get(fname).cloned();
        }

        trace!(id=%self.id, %fname, "refreshing file stat");
        let content = self.current_file_content_as_string(fname, tx)?;
        let stat = self.file_stats.get_mut(fname)?;
        stat.n_bytes = content.len() as u64;

        Some(stat.clone())
    }

    // Must not be called for the event file or unknown files
    fn current_file_content_as_string(
        &mut self,
        fname: &str,
        tx: &Sender<Event>,
    ) -> Option<String> {
        let req = match fname {
            ADDR => Req::ReadBufferAddr { id: self.id },
            BODY => Req::ReadBufferBody { id: self.id },
            CTL => return None,
            DOT => Req::ReadBufferDot { id: self.id },
            EVENT => return None,
            FILENAME => Req::ReadBufferName { id: self.id },
            FILETYPE => Req::ReadBufferFtype { id: self.id },
            OUTPUT => return Some(String::new()),
            XADDR => Req::ReadBufferXAddr { id: self.id },
            XDOT => Req::ReadBufferXDot { id: self.id },
            _ => return None, // can hit this as part of walk for unknown files
        };

        match Message::send(req, tx) {
            Ok(s) => Some(s),
            Err(e) => {
                error!("fsys failed to read file content: {e}");
                Some(String::new())
            }
        }
    }

    fn current_file_content(
        &mut self,
        fname: &str,
        offset: usize,
        count: usize,
        tx: &Sender<Event>,
    ) -> InternalRead {
        let req = match fname {
            ADDR => Req::ReadBufferAddr { id: self.id },
            BODY => Req::ReadBufferBody { id: self.id },
            CTL => return InternalRead::Immediate(Vec::new()),
            DOT => Req::ReadBufferDot { id: self.id },
            FILENAME => Req::ReadBufferName { id: self.id },
            FILETYPE => Req::ReadBufferFtype { id: self.id },
            OUTPUT => return InternalRead::Immediate(Vec::new()),
            XADDR => Req::ReadBufferXAddr { id: self.id },
            XDOT => Req::ReadBufferXDot { id: self.id },

            EVENT => {
                // ignoring offset
                return match self.pending_events() {
                    Ok(ir) => ir,
                    Err(e) => {
                        error!("error reading events: {e}");
                        self.clear_input_filter(tx);
                        InternalRead::Unknown
                    }
                };
            }

            _ => return InternalRead::Unknown,
        };

        match Message::send(req, tx) {
            Ok(s) => InternalRead::Immediate(apply_offset(s.as_bytes(), offset, count)),
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
        let mut stat = empty_file_stat(qid + offset, name);
        match name {
            EVENT => stat.qid.ty = FileType::EXCLUSIVE,
            CTL | OUTPUT => stat.perms.remove(Perm::OWNER_READ),
            _ => (),
        }

        m.insert(name, stat);
    }

    m
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fsys::{UNAME, tests::test_server_with_file};
    use simple_test_case::test_case;

    #[test_case(CURRENT_BUFFER_QID + 1 + 1, CURRENT_BUFFER_QID + 1, ADDR; "addr first buffer")]
    #[test_case(10, 8, BODY; "body first buffer")]
    #[test_case(25, 19, FILENAME; "filename second buffer")]
    #[test]
    fn parent_and_fname_works(qid: u64, parent: u64, fname: &str) {
        let (p, f) = parent_and_fname(qid);

        assert_eq!(p, parent);
        assert_eq!(f, fname);
    }

    // can read
    #[test_case(ADDR, true; "addr")]
    #[test_case(BODY, true; "body")]
    #[test_case(DOT, true; "dot")]
    #[test_case(EVENT, true; "event")]
    #[test_case(FILENAME, true; "filename")]
    #[test_case(FILETYPE, true; "filetype")]
    #[test_case(XADDR, true; "xaddr")]
    #[test_case(XDOT, true; "xdot")]
    // can not read
    #[test_case(CTL, false; "ctl")]
    #[test_case(OUTPUT, false; "output")]
    #[test]
    fn buffer_files_have_expected_read_access(fname: &str, can_read: bool) {
        let mut server = test_server_with_file();
        let (client, _handle) = server.session_with_attached_client(&*UNAME, "").unwrap();

        let stats = client.read_dir("buffers/1").unwrap();
        let stat = stats
            .iter()
            .find(|s| s.name == fname)
            .unwrap_or_else(|| panic!("no file named {fname} found"));

        assert_eq!(stat.perms.contains(Perm::OWNER_READ), can_read, "{stat:#?}");
    }
}
