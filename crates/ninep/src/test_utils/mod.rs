//! Shared test infrastructure for sync and tokio tests.
use crate::{
    Result,
    fs::{IoUnit, Mode, Perm, Qid, Stat, WStat},
    sansio::{
        client::MSIZE,
        protocol::{FileType, Rmessage, SharedBuf, Tdata, Tmessage},
        server::ClientId,
    },
    sync::{
        SyncNineP,
        server::{ReadOutcome, Serve9p},
    },
    tokio::{AsyncNineP, server::AsyncServe9pFromSync},
};
use jiff::Timestamp;
use std::{
    io, mem,
    os::unix::net::UnixStream,
    sync::{Arc, Mutex, mpsc},
    thread::{sleep, spawn},
    time::Duration,
};
use tokio::io::DuplexStream;

pub(crate) mod client_cases;
pub(crate) mod server_cases;

pub(crate) const ROOT_QID: u64 = 0;
pub(crate) const HELLO_QID: u64 = 1;
pub(crate) const SUBDIR_QID: u64 = 2;
pub(crate) const BLOCKED_QID: u64 = 3;
pub(crate) const SUBFILE_QID: u64 = 4;
pub(crate) const PERMFILE_QID: u64 = 5;
pub(crate) const CREATED_QID: u64 = 99;

pub(crate) const HELLO_CONTENT: &[u8] = b"hello world";
pub(crate) const SUBFILE_CONTENT: &[u8] = b"subfile";
pub(crate) const BLOCKED_CONTENT: &[u8] = b"delayed";
pub(crate) const PERMFILE_CONTENT: &[u8] = b"permissions checks";
pub(crate) const TEST_IOUNIT: IoUnit = 8192;

fn dir_qid(path: u64) -> Qid {
    Qid {
        ty: FileType::DIRECTORY,
        version: 0,
        path,
    }
}

fn file_qid(path: u64) -> Qid {
    Qid {
        ty: FileType::FILE,
        version: 0,
        path,
    }
}

pub(crate) fn perm_file_stat() -> Stat {
    Stat {
        qid: file_qid(PERMFILE_QID),
        name: "perm-checks".into(),
        owner: "owner".to_string(),
        group: "group".to_string(),
        perms: Perm::OWNER_READ | Perm::GROUP_WRITE,
        n_bytes: 0,
        last_accessed: Timestamp::UNIX_EPOCH,
        last_modified: Timestamp::UNIX_EPOCH,
        last_modified_by: "owner".to_string(),
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct TestFs {
    calls: RecordedCalls,
}

impl TestFs {
    /// Obtain a handle to the [RecordedCalls] being made to this [TestFs].
    ///
    /// Use [RecordedCalls::take] to extract the calls up until that point in order to make
    /// assertions.
    pub(crate) fn calls(&self) -> RecordedCalls {
        self.calls.clone()
    }
}

impl Serve9p for TestFs {
    fn user_is_in_group(&self, uname: &str, _group: &str) -> bool {
        // Not tracked
        uname == "group-member"
    }

    fn walk_one(&self, parent_qid: u64, child: &str, cid: ClientId) -> Result<Qid> {
        self.calls.push(Call::walk(cid, parent_qid, child));

        match (parent_qid, child) {
            (ROOT_QID, "hello") => Ok(Qid::file(HELLO_QID)),
            (ROOT_QID, "subdir") => Ok(Qid::dir(SUBDIR_QID)),
            (ROOT_QID, "blocked") => Ok(Qid::file(BLOCKED_QID)),
            (SUBDIR_QID, "subfile") => Ok(Qid::file(SUBFILE_QID)),
            (SUBDIR_QID, "perm-checks") => Ok(Qid::file(PERMFILE_QID)),
            _ => Err(format!("not found: {child}")),
        }
    }

    fn open(&self, qid: u64, mode: Mode, cid: ClientId) -> Result<IoUnit> {
        self.calls.push(Call::open(cid, qid, mode));

        Ok(TEST_IOUNIT)
    }

    fn clunk(&self, qid: u64, cid: ClientId) {
        self.calls.push(Call::clunk(cid, qid));
    }

    fn flush(&self, old_tag: u16, cid: ClientId) {
        self.calls.push(Call::flush(cid, old_tag));
    }

    fn create(
        &self,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        cid: ClientId,
    ) -> Result<(Qid, IoUnit)> {
        self.calls.push(Call::create(cid, parent, name, perm, mode));

        Ok((Qid::file(CREATED_QID), TEST_IOUNIT))
    }

    fn read(&self, qid: u64, offset: usize, count: usize, cid: ClientId) -> Result<ReadOutcome> {
        self.calls.push(Call::read(cid, qid, offset, count));

        match qid {
            HELLO_QID => {
                let src = HELLO_CONTENT.get(offset..).unwrap_or(&[]);
                Ok(ReadOutcome::Immediate(src[..count.min(src.len())].to_vec()))
            }
            SUBFILE_QID => {
                let src = SUBFILE_CONTENT.get(offset..).unwrap_or(&[]);
                Ok(ReadOutcome::Immediate(src[..count.min(src.len())].to_vec()))
            }
            PERMFILE_QID => {
                let src = PERMFILE_CONTENT.get(offset..).unwrap_or(&[]);
                Ok(ReadOutcome::Immediate(src[..count.min(src.len())].to_vec()))
            }
            BLOCKED_QID => {
                let (tx, rx) = mpsc::channel();
                let data = BLOCKED_CONTENT.to_vec();
                spawn(move || {
                    sleep(Duration::from_millis(25));
                    let _ = tx.send(data);
                });

                Ok(ReadOutcome::Blocked(rx))
            }
            _ => Err(format!("unreadable qid: {qid}")),
        }
    }

    fn read_dir(&self, qid: u64, cid: ClientId) -> Result<Vec<Stat>> {
        self.calls.push(Call::read_dir(cid, qid));

        match qid {
            ROOT_QID => Ok(vec![
                Stat::stub(Qid::file(HELLO_QID), "hello"),
                Stat::stub(Qid::dir(SUBDIR_QID), "subdir"),
            ]),
            SUBDIR_QID => Ok(vec![
                Stat::stub(Qid::file(SUBFILE_QID), "subfile"),
                perm_file_stat(),
            ]),
            _ => Err(format!("not a directory: {qid}")),
        }
    }

    fn write(&self, qid: u64, offset: usize, data: Vec<u8>, cid: ClientId) -> Result<usize> {
        let n = data.len();
        self.calls.push(Call::write(cid, qid, offset, data));

        Ok(n)
    }

    fn remove(&self, qid: u64, cid: ClientId) -> Result<()> {
        self.calls.push(Call::remove(cid, qid));

        Ok(())
    }

    fn stat(&self, qid: u64, cid: ClientId) -> Result<Stat> {
        self.calls.push(Call::stat(cid, qid));

        match qid {
            ROOT_QID => Ok(Stat::stub(Qid::dir(ROOT_QID), "/")),
            HELLO_QID => Ok(Stat::stub(Qid::file(HELLO_QID), "hello")),
            BLOCKED_QID => Ok(Stat::stub(Qid::file(BLOCKED_QID), "blocked")),
            SUBDIR_QID => Ok(Stat::stub(Qid::dir(SUBDIR_QID), "subdir")),
            SUBFILE_QID => Ok(Stat::stub(Qid::file(SUBFILE_QID), "subfile")),
            PERMFILE_QID => Ok(perm_file_stat()),
            _ => Err(format!("unknown qid: {qid}")),
        }
    }

    fn write_stat(&self, qid: u64, wstat: WStat, cid: ClientId) -> Result<()> {
        self.calls.push(Call::write_stat(cid, qid, wstat));

        Ok(())
    }
}

impl AsyncServe9pFromSync for TestFs {}

/// Unlike the sync/tokio clients exported by the main crate, this client is a simple wrapper
/// around the lower level 9p protocol to facilitate testing.
pub(crate) struct TestClient<S> {
    pub(crate) stream: S,
    pub(crate) buf: SharedBuf,
    pub(crate) msize: u32,
}

impl<S> TestClient<S> {
    pub fn new(stream: S) -> Self {
        Self {
            stream,
            buf: SharedBuf::default(),
            msize: MSIZE,
        }
    }
}

/// A [TestClient] backed by a [UnixStream].
pub(crate) type SyncTestClient = TestClient<UnixStream>;

impl SyncTestClient {
    /// Synchronously send a Tmessage and receive back the Rmessage reply from the server
    pub fn send_sync(&mut self, tag: u16, content: Tdata) -> io::Result<Rmessage> {
        SyncNineP::write_to(&Tmessage { tag, content }, &mut self.stream)?;
        <Rmessage as SyncNineP>::read_from(self.msize, &self.buf, &mut self.stream)
    }
}

/// A [TestClient] backed by a [DuplexStream].
pub(crate) type AsyncTestClient = TestClient<DuplexStream>;

impl AsyncTestClient {
    /// Asynchronously send a Tmessage and receive back the Rmessage reply from the server
    pub async fn send_async(&mut self, tag: u16, content: Tdata) -> io::Result<Rmessage> {
        AsyncNineP::write_to(&Tmessage { tag, content }, &mut self.stream).await?;
        <Rmessage as AsyncNineP>::read_from(self.msize, &self.buf, &mut self.stream).await
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct RecordedCalls(Arc<Mutex<Vec<Call>>>);

impl RecordedCalls {
    fn push(&self, call: Call) {
        self.0.lock().unwrap().push(call);
    }

    /// Extract the [Call]s that have been recorded up until this point.
    ///
    /// This method clears the internal log state of the associated [TestFs].
    pub(crate) fn take(&self) -> Vec<Call> {
        mem::take(&mut self.0.lock().unwrap())
    }
}

/// A recorded method call to a [TestFs].
#[rustfmt::skip]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Call {
    Clunk { cid: ClientId, qid: u64 },
    Create { cid: ClientId, parent: u64, name: String, perm: Perm, mode: Mode  },
    Flush { cid: ClientId, old_tag: u16 },
    Open { cid: ClientId, qid: u64, mode: Mode },
    Read { cid: ClientId, qid: u64, offset: usize, count: usize },
    ReadDir { cid: ClientId, qid: u64 },
    Remove { cid: ClientId, qid: u64 },
    Stat { cid: ClientId, qid: u64 },
    Walk { cid: ClientId, parent_qid: u64, child: String, },
    Write { cid: ClientId, qid: u64, offset: usize, data: Vec<u8> },
    WriteStat { cid: ClientId, qid: u64, wstat: WStat },
}

impl Call {
    pub(crate) fn clunk(cid: ClientId, qid: u64) -> Self {
        Self::Clunk { cid, qid }
    }

    pub(crate) fn create(cid: ClientId, parent: u64, name: &str, perm: Perm, mode: Mode) -> Self {
        Self::Create {
            cid,
            parent,
            name: name.into(),
            perm,
            mode,
        }
    }

    pub(crate) fn flush(cid: ClientId, old_tag: u16) -> Self {
        Self::Flush { cid, old_tag }
    }

    pub(crate) fn open(cid: ClientId, qid: u64, mode: Mode) -> Self {
        Self::Open { cid, qid, mode }
    }

    pub(crate) fn read(cid: ClientId, qid: u64, offset: usize, count: usize) -> Self {
        Self::Read {
            cid,
            qid,
            offset,
            count,
        }
    }

    pub(crate) fn read_dir(cid: ClientId, qid: u64) -> Self {
        Self::ReadDir { cid, qid }
    }

    pub(crate) fn remove(cid: ClientId, qid: u64) -> Self {
        Self::Remove { cid, qid }
    }

    pub(crate) fn stat(cid: ClientId, qid: u64) -> Self {
        Self::Stat { cid, qid }
    }

    pub(crate) fn walk(cid: ClientId, parent_qid: u64, child: &str) -> Self {
        Self::Walk {
            cid,
            parent_qid,
            child: child.into(),
        }
    }

    pub(crate) fn write(cid: ClientId, qid: u64, offset: usize, data: Vec<u8>) -> Self {
        Self::Write {
            cid,
            qid,
            offset,
            data,
        }
    }

    pub(crate) fn write_stat(cid: ClientId, qid: u64, wstat: WStat) -> Self {
        Self::WriteStat { cid, qid, wstat }
    }
}
