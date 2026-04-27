//! Shared test infrastructure for sync and tokio tests.
use crate::{
    fs::{FileMeta, FileType, IoUnit, Mode, Perm, Stat, WStat},
    sansio::{
        client::MSIZE,
        protocol::{Qid, Rmessage, SharedBuf, Tdata, Tmessage},
        server::ClientId,
    },
    sync::{
        SyncNineP,
        server::{ReadOutcome, Serve9p},
    },
    tokio::{AsyncNineP, server::AsyncServe9pFromSync},
};
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
pub(crate) const CREATED_QID: u64 = 99;

pub(crate) const HELLO_CONTENT: &[u8] = b"hello world";
pub(crate) const SUBFILE_CONTENT: &[u8] = b"subfile";
pub(crate) const BLOCKED_CONTENT: &[u8] = b"delayed";
pub(crate) const TEST_IOUNIT: IoUnit = 8192;

fn dir_qid(path: u64) -> Qid {
    Qid {
        ty: FileType::DIRECTORY.bits(),
        version: 0,
        path,
    }
}

fn file_qid(path: u64) -> Qid {
    Qid {
        ty: FileType::FILE.bits(),
        version: 0,
        path,
    }
}

fn dir_perms() -> Perm {
    Perm::OWNER_READ | Perm::OWNER_EXEC
}

fn file_perms() -> Perm {
    Perm::OWNER_READ | Perm::OWNER_WRITE | Perm::GROUP_READ | Perm::OTHER_READ
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
    fn walk_one(
        &self,
        cid: ClientId,
        parent_qid: u64,
        child: &str,
        uname: &str,
    ) -> crate::Result<FileMeta> {
        self.calls.push(Call::walk(cid, parent_qid, child, uname));

        match (parent_qid, child) {
            (ROOT_QID, "hello") => Ok(FileMeta::file("hello", HELLO_QID, file_perms())),
            (ROOT_QID, "subdir") => Ok(FileMeta::dir("subdir", SUBDIR_QID, dir_perms())),
            (ROOT_QID, "blocked") => Ok(FileMeta::file("blocked", BLOCKED_QID, file_perms())),
            (SUBDIR_QID, "subfile") => Ok(FileMeta::file("subfile", SUBFILE_QID, dir_perms())),
            _ => Err(format!("not found: {child}")),
        }
    }

    fn open(&self, cid: ClientId, qid: u64, mode: Mode, uname: &str) -> crate::Result<IoUnit> {
        self.calls.push(Call::open(cid, qid, mode, uname));

        Ok(TEST_IOUNIT)
    }

    fn clunk(&self, cid: ClientId, qid: u64) {
        self.calls.push(Call::clunk(cid, qid));
    }

    fn flush(&self, cid: ClientId, old_tag: u16) {
        self.calls.push(Call::flush(cid, old_tag));
    }

    fn create(
        &self,
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> crate::Result<(FileMeta, IoUnit)> {
        self.calls
            .push(Call::create(cid, parent, name, perm, mode, uname));

        Ok((FileMeta::file(name, CREATED_QID, file_perms()), TEST_IOUNIT))
    }

    fn read(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        count: usize,
        uname: &str,
    ) -> crate::Result<ReadOutcome> {
        self.calls.push(Call::read(cid, qid, offset, count, uname));

        match qid {
            HELLO_QID => {
                let src = HELLO_CONTENT.get(offset..).unwrap_or(&[]);
                Ok(ReadOutcome::Immediate(src[..count.min(src.len())].to_vec()))
            }
            SUBFILE_QID => {
                let src = SUBFILE_CONTENT.get(offset..).unwrap_or(&[]);
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

    fn read_dir(&self, cid: ClientId, qid: u64, uname: &str) -> crate::Result<Vec<Stat>> {
        self.calls.push(Call::read_dir(cid, qid, uname));

        match qid {
            ROOT_QID => Ok(vec![
                Stat::stub(FileMeta::file("hello", HELLO_QID, file_perms())),
                Stat::stub(FileMeta::dir("subdir", SUBDIR_QID, dir_perms())),
            ]),
            SUBDIR_QID => Ok(vec![Stat::stub(FileMeta::file(
                "subfile",
                SUBFILE_QID,
                file_perms(),
            ))]),
            _ => Err(format!("not a directory: {qid}")),
        }
    }

    fn write(
        &self,
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> crate::Result<usize> {
        let n = data.len();

        self.calls.push(Call::write(cid, qid, offset, data, uname));

        Ok(n)
    }

    fn remove(&self, cid: ClientId, qid: u64, uname: &str) -> crate::Result<()> {
        self.calls.push(Call::remove(cid, qid, uname));

        Ok(())
    }

    fn stat(&self, cid: ClientId, qid: u64, uname: &str) -> crate::Result<Stat> {
        self.calls.push(Call::stat(cid, qid, uname));

        match qid {
            ROOT_QID => Ok(Stat::stub(FileMeta::dir("/", ROOT_QID, Perm::empty()))),
            HELLO_QID => Ok(Stat::stub(FileMeta::file(
                "hello",
                HELLO_QID,
                Perm::empty(),
            ))),
            BLOCKED_QID => Ok(Stat::stub(FileMeta::file(
                "blocked",
                BLOCKED_QID,
                Perm::empty(),
            ))),
            SUBDIR_QID => Ok(Stat::stub(FileMeta::dir(
                "subdir",
                SUBDIR_QID,
                Perm::empty(),
            ))),
            SUBFILE_QID => Ok(Stat::stub(FileMeta::file(
                "subfile",
                SUBFILE_QID,
                Perm::empty(),
            ))),
            _ => Err(format!("unknown qid: {qid}")),
        }
    }

    fn write_stat(&self, cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> crate::Result<()> {
        self.calls.push(Call::write_stat(cid, qid, wstat, uname));

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
    Create { cid: ClientId, parent: u64, name: String, perm: Perm, mode: Mode, uname: String },
    Flush { cid: ClientId, old_tag: u16 },
    Open { cid: ClientId, qid: u64, mode: Mode, uname: String },
    Read { cid: ClientId, qid: u64, offset: usize, count: usize, uname: String },
    ReadDir { cid: ClientId, qid: u64, uname: String },
    Remove { cid: ClientId, qid: u64, uname: String },
    Stat { cid: ClientId, qid: u64, uname: String },
    Walk { cid: ClientId, parent_qid: u64, child: String, uname: String },
    Write { cid: ClientId, qid: u64, offset: usize, data: Vec<u8>, uname: String },
    WriteStat { cid: ClientId, qid: u64, wstat: WStat, uname: String },
}

impl Call {
    pub(crate) fn clunk(cid: ClientId, qid: u64) -> Self {
        Self::Clunk { cid, qid }
    }

    pub(crate) fn create(
        cid: ClientId,
        parent: u64,
        name: &str,
        perm: Perm,
        mode: Mode,
        uname: &str,
    ) -> Self {
        Self::Create {
            cid,
            parent,
            name: name.into(),
            perm,
            mode,
            uname: uname.into(),
        }
    }

    pub(crate) fn flush(cid: ClientId, old_tag: u16) -> Self {
        Self::Flush { cid, old_tag }
    }

    pub(crate) fn open(cid: ClientId, qid: u64, mode: Mode, uname: &str) -> Self {
        Self::Open {
            cid,
            qid,
            mode,
            uname: uname.into(),
        }
    }

    pub(crate) fn read(cid: ClientId, qid: u64, offset: usize, count: usize, uname: &str) -> Self {
        Self::Read {
            cid,
            qid,
            offset,
            count,
            uname: uname.into(),
        }
    }

    pub(crate) fn read_dir(cid: ClientId, qid: u64, uname: &str) -> Self {
        Self::ReadDir {
            cid,
            qid,
            uname: uname.into(),
        }
    }

    pub(crate) fn remove(cid: ClientId, qid: u64, uname: &str) -> Self {
        Self::Remove {
            cid,
            qid,
            uname: uname.into(),
        }
    }

    pub(crate) fn stat(cid: ClientId, qid: u64, uname: &str) -> Self {
        Self::Stat {
            cid,
            qid,
            uname: uname.into(),
        }
    }

    pub(crate) fn walk(cid: ClientId, parent_qid: u64, child: &str, uname: &str) -> Self {
        Self::Walk {
            cid,
            parent_qid,
            child: child.into(),
            uname: uname.into(),
        }
    }

    pub(crate) fn write(
        cid: ClientId,
        qid: u64,
        offset: usize,
        data: Vec<u8>,
        uname: &str,
    ) -> Self {
        Self::Write {
            cid,
            qid,
            offset,
            data,
            uname: uname.into(),
        }
    }

    pub(crate) fn write_stat(cid: ClientId, qid: u64, wstat: WStat, uname: &str) -> Self {
        Self::WriteStat {
            cid,
            qid,
            wstat,
            uname: uname.into(),
        }
    }
}
