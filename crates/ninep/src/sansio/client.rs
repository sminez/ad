//! Traits and structs for implementing a 9p client
use crate::{
    fs::{IoUnit, Mode, Perm, Stat, WStat},
    sansio::{
        protocol::{
            Data, IOHDRSZ, MAXWELEM, Qid, RawStat, Rdata, Rmessage, SharedBuf, Tdata, Tmessage,
        },
        server::AFID_NO_AUTH,
    },
    sync::SyncNineP,
};
use simple_coro::{Coro, Handle, ReadyCoro};
use std::{cmp::min, collections::HashMap, fmt, future::Future, io};

/// Alias for a [Result][std::result::Result] containing a 9p client [Error].
pub type Result<T> = std::result::Result<T, Error>;

/// An error that can be encountered by a 9p client.
#[derive(Debug)]
pub enum Error {
    /// The connection to the server has been closed
    ConnectionClosed,

    /// An unexpected response was received for a message sent by the client
    ProtocolViolation {
        /// The expected response type
        expected: String,
        /// The response data received
        received: Box<Rdata>,
    },

    /// The server returned an error
    Rerror {
        /// The error string returned by the server
        ename: String,
    },

    /// An IO error was encountered
    Io(io::Error),

    /// The version offered by the server is not supported
    UnsupportedServerVersion {
        /// The version requested by the server
        version: String,
    },
}

impl Error {
    #[cfg(test)]
    pub(crate) fn r(ename: impl Into<String>) -> Self {
        Self::Rerror {
            ename: ename.into(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ConnectionClosed => write!(f, "connection closed"),
            Self::ProtocolViolation { expected, received } => write!(
                f,
                "9p protocol violation: expected {expected}, but received {received:?}"
            ),
            Self::Rerror { ename } => write!(f, "9p error: {ename}"),
            Self::Io(inner) => write!(f, "IO error: {inner}"),
            Self::UnsupportedServerVersion { version } => write!(
                f,
                "requested server version of {VERSION} is not supported: got {version}"
            ),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(inner) => Some(inner),
            _ => None,
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Self::Io(error)
    }
}

impl From<Error> for io::Error {
    fn from(error: Error) -> Self {
        match error {
            Error::Io(inner) => inner,
            e => io::Error::other(e.to_string()),
        }
    }
}

macro_rules! expect_rmessage {
    ($resp:expr, $variant:ident { $($field:ident,)* .. }) => {
        match $resp.content {
            Rdata::$variant { $($field,)* .. } => Ok(($($field),*)),
            Rdata::Error { ename } => Err(Error::Rerror { ename }),
            m => Err(Error::ProtocolViolation {
                expected: stringify!($variant).to_string(),
                received: Box::new(m)
            }),
        }

    };

    ($resp:expr, $variant:ident { $($field:ident),* }) => {
        match $resp.content {
            Rdata::$variant { $($field),* } => Ok(($($field),*)),
            Rdata::Error { ename } => Err(Error::Rerror { ename }),
            m => Err(Error::ProtocolViolation {
                expected: stringify!($variant).to_string(),
                received: Box::new(m)
            }),
        }

    };
}

pub(crate) const MSIZE: u32 = u16::MAX as u32;
pub(crate) const VERSION: &str = "9P2000";

type Coro9p<T, F> = ReadyCoro<Tmessage, Rmessage, Result<T>, F>;

pub(crate) fn err<T, E>(e: E) -> Result<T>
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Err(Error::Io(io::Error::other(e)))
}

/// Internal sans-IO state for a 9p client implementation that can be used along with an I/O stream
/// to implement a concrete Client.
#[derive(Debug)]
pub(crate) struct State {
    pub(crate) msize: u32,
    pub(crate) fids: FidCache,
    pub(crate) next_fid: u32,
}

impl Default for State {
    fn default() -> Self {
        State {
            msize: MSIZE,
            fids: FidCache::default(),
            next_fid: 1,
        }
    }
}

impl State {
    fn next_fid(&mut self) -> u32 {
        let fid = self.next_fid;
        self.next_fid += 1;

        fid
    }

    /// Establish our connection to the target 9p server and begin the session.
    pub(crate) fn handle_connect(
        &mut self,
        uname: String,
        aname: String,
    ) -> Coro9p<(), impl Future<Output = Result<()>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let rmessage = handle
                .yield_value(Tmessage::new(u16::MAX, Tdata::version(MSIZE, VERSION)))
                .await;

            let (msize, version) = expect_rmessage!(rmessage, Version { msize, version })?;
            if version != VERSION {
                return Err(Error::UnsupportedServerVersion { version });
            }

            let rmessage = handle
                .yield_value(Tmessage::new(
                    0,
                    Tdata::attach(0, AFID_NO_AUTH, uname, aname),
                ))
                .await;

            expect_rmessage!(rmessage, Attach { aqid })?;
            self.msize = msize;

            Ok(())
        })
    }

    /// Run a walk from root
    pub(crate) fn handle_walk(
        &mut self,
        path: String,
    ) -> Coro9p<u32, impl Future<Output = Result<u32>> + use<'_>> {
        self.handle_walk_from(0, path)
    }

    /// Associate the given path with a new fid by walking from the starting `fid`.
    ///
    /// Panics if `fid` is not currently within the fid cache.
    pub(crate) fn handle_walk_from(
        &mut self,
        mut fid: u32,
        path: String,
    ) -> Coro9p<u32, impl Future<Output = Result<u32>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let base = self
                .fids
                .path_for(fid)
                .unwrap_or_else(|| panic!("unknown base fid for walk: {fid}"));

            let target = normalise_path(&format!("{base}/{path}"));

            // Already normalised so just key directly into the map
            if let Some(fid) = self.fids.path_to_fid.get(&target) {
                return Ok(*fid);
            }

            let wnames: Vec<String> = path
                .split('/')
                .filter(|elem| !elem.is_empty() && *elem != ".")
                .map(Into::into)
                .collect();

            if wnames.is_empty() {
                // Walk to "." -> just return the fid we already have
                return Ok(fid);
            }

            let new_fid = self.next_fid();
            for chunk in wnames.chunks(MAXWELEM) {
                let _wqids = handle
                    .yield_from(self.walk_one(fid, new_fid, chunk.to_vec()))
                    .await?;

                // no-op once we've handled the first chunk. Allows us to create a single new fid
                // and walk it all the way to the target without needing to create and clunk
                // intermediate fids per-chunk.
                fid = new_fid;
            }

            self.fids.insert(new_fid, target);

            Ok(new_fid)
        })
    }

    fn walk_one(
        &mut self,
        fid: u32,
        new_fid: u32,
        wnames: Vec<String>,
    ) -> Coro9p<Vec<Qid>, impl Future<Output = Result<Vec<Qid>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let n_wnames = wnames.len();
            let rmessage = handle
                .yield_value(Tmessage::new(0, Tdata::walk(fid, new_fid, wnames)))
                .await;
            let wqids = expect_rmessage!(rmessage, Walk { wqids })?;

            if wqids.len() != n_wnames && fid == new_fid {
                _ = handle
                    .yield_value(Tmessage::new(0, Tdata::clunk(new_fid)))
                    .await;

                return err("walk failed before reaching full path");
            }

            Ok(wqids)
        })
    }

    /// Request the current [Stat] of the file or directory identified by the given path.
    pub(crate) fn handle_stat(
        &mut self,
        path: String,
    ) -> Coro9p<Stat, impl Future<Output = Result<Stat>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let rmessage = handle.yield_value(Tmessage::new(0, Tdata::stat(fid))).await;

            let raw_stat = expect_rmessage!(rmessage, Stat { stat, .. })?;

            Ok(raw_stat.into())
        })
    }

    /// Attempt to modify the current [Stat] of the file or directory identified by the given path
    /// using the given [WStat].
    pub(crate) fn handle_wstat(
        &mut self,
        path: String,
        wstat: WStat,
    ) -> Coro9p<(), impl Future<Output = Result<()>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let rmessage = handle
                .yield_value(Tmessage::new(0, wstat.into_tdata(fid)))
                .await;

            expect_rmessage!(rmessage, Wstat {})?;

            // If our update was successful then our cached state is potentially invalid. Rather
            // than trying to be "smart" about how we handle the cache, we simply evict and re-walk
            // this path the next time it is needed.
            self.fids.remove(fid);

            Ok(())
        })
    }

    pub(crate) fn handle_open(
        &mut self,
        fid: u32,
        mode: Mode,
    ) -> Coro9p<(Qid, IoUnit), impl Future<Output = Result<(Qid, IoUnit)>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let mode = mode.bits();
            let rmsg = handle
                .yield_value(Tmessage::new(0, Tdata::open(fid, mode)))
                .await;

            expect_rmessage!(rmsg, Open { qid, iounit })
        })
    }

    pub(crate) fn handle_read_count(
        &mut self,
        fid: u32,
        offset: u64,
        count: u32,
    ) -> Coro9p<Vec<u8>, impl Future<Output = Result<Vec<u8>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let rmessage = handle
                .yield_value(Tmessage::new(0, Tdata::read(fid, offset, count)))
                .await;
            let Data(data) = expect_rmessage!(rmessage, Read { data })?;

            Ok(data)
        })
    }

    /// Read up to `count` bytes from the file at `path` starting at byte `offset`.
    pub(crate) fn handle_read_from(
        &mut self,
        path: String,
        mut offset: u64,
        mut count: u32,
    ) -> Coro9p<Vec<u8>, impl Future<Output = Result<Vec<u8>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let (_qid, iounit) = handle.yield_from(self.handle_open(fid, Mode::READ)).await?;

            let max_payload = self.msize - IOHDRSZ;
            let iounit = if iounit == 0 {
                max_payload
            } else {
                min(iounit, max_payload)
            };

            let mut bytes = Vec::new();
            while count > 0 {
                let n = min(count, iounit);
                let data = handle
                    .yield_from(self.handle_read_count(fid, offset, n))
                    .await?;
                if data.is_empty() {
                    break;
                }
                offset += data.len() as u64;
                count -= data.len() as u32;
                bytes.extend(data);
            }

            Ok(bytes)
        })
    }

    /// Read the full contents of the file at `path` as bytes.
    pub(crate) fn handle_read(
        &mut self,
        path: String,
    ) -> Coro9p<Vec<u8>, impl Future<Output = Result<Vec<u8>>> + use<'_>> {
        self.handle_read_from(path, 0, u32::MAX)
    }

    /// Read the directory listing of the directory at `path`.
    pub(crate) fn handle_read_dir(
        &mut self,
        path: String,
    ) -> Coro9p<Vec<Stat>, impl Future<Output = Result<Vec<Stat>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let bytes = handle
                .yield_from(self.handle_read_from(path, 0, u32::MAX))
                .await?;
            let mut buf = io::Cursor::new(bytes);
            let mut stats: Vec<Stat> = Vec::new();
            let sb = SharedBuf::default();

            loop {
                match RawStat::read_from(self.msize, &sb, &mut buf) {
                    Ok(rs) => stats.push(rs.into()),
                    Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => break,
                    Err(e) => return Err(Error::Io(e)),
                }
            }

            Ok(stats)
        })
    }

    /// Write the provided data to the file at `path` at the given offset.
    pub(crate) fn handle_write<'a, 's: 'a>(
        &'s mut self,
        path: String,
        mut offset: u64,
        content: &'a [u8],
    ) -> Coro9p<usize, impl Future<Output = Result<usize>> + use<'a, 's>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            handle
                .yield_from(self.handle_open(fid, Mode::WRITE))
                .await?;

            if content.is_empty() {
                let rmessage = handle
                    .yield_value(Tmessage::new(0, Tdata::write(fid, offset, b"".to_vec())))
                    .await;
                let n = expect_rmessage!(rmessage, Write { count })?;

                return Ok(n as usize);
            }

            let len = content.len();
            let mut cur = 0;
            let chunk_size = (self.msize - IOHDRSZ) as usize;

            while cur < len {
                let end = min(cur + chunk_size, len);
                let rmessage = handle
                    .yield_value(Tmessage::new(
                        0,
                        Tdata::write(fid, offset, content[cur..end].to_vec()),
                    ))
                    .await;
                let n = expect_rmessage!(rmessage, Write { count })?;
                if n == 0 {
                    break;
                }
                cur += n as usize;
                offset += n as u64;
            }

            if cur != len {
                return err(format!("partial write: {cur} < {len}"));
            }

            Ok(cur)
        })
    }

    /// Attempt to create a new file within the connected filesystem.
    pub(crate) fn handle_create(
        &mut self,
        dir: String,
        name: String,
        perms: Perm,
        mode: Mode,
    ) -> Coro9p<(), impl Future<Output = Result<()>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let normalised_abspath = normalise_path(&format!("{dir}/{name}"));
            let parent_fid = handle.yield_from(self.handle_walk(dir)).await?;
            let fid = self.next_fid();

            handle
                .yield_from(self.walk_one(parent_fid, fid, vec![]))
                .await?;

            let rmessage = handle
                .yield_value(Tmessage::new(
                    0,
                    Tdata::create(fid, name, perms.bits(), mode.bits()),
                ))
                .await;

            let _qid = expect_rmessage!(rmessage, Create { qid, .. })?;

            self.fids.insert(fid, normalised_abspath);

            Ok(())
        })
    }

    /// Attempt to remove a file from the connected filesystem.
    pub(crate) fn handle_remove(
        &mut self,
        path: String,
    ) -> Coro9p<(), impl Future<Output = Result<()>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let rmessage = handle
                .yield_value(Tmessage::new(0, Tdata::remove(fid)))
                .await;

            expect_rmessage!(rmessage, Remove {})?;
            self.fids.remove(fid);

            Ok(())
        })
    }
}

/// Bi-directional mapping of cached fid <-> absolute path relationships
#[derive(Debug)]
pub(crate) struct FidCache {
    path_to_fid: HashMap<String, u32>,
    fid_to_path: HashMap<u32, String>,
}

impl Default for FidCache {
    fn default() -> Self {
        Self {
            path_to_fid: HashMap::from([("/".into(), 0)]),
            fid_to_path: HashMap::from([(0, "/".into())]),
        }
    }
}

impl FidCache {
    /// Lookup a `fid` using a user supplied path that will be normalised to create the cache key.
    pub(crate) fn fid_for_unnormalised_path(&self, path: &str) -> Option<u32> {
        self.path_to_fid.get(&normalise_path(path)).copied()
    }

    fn path_for(&self, fid: u32) -> Option<&str> {
        self.fid_to_path.get(&fid).map(|s| s.as_str())
    }

    /// Path MUST be normalised before insert.
    ///
    /// Panics if `fid` or `path` are already in the cache.
    fn insert(&mut self, fid: u32, path: String) {
        if self.fid_to_path.contains_key(&fid) || self.path_to_fid.contains_key(&path) {
            panic!("fid cache insert collision: ({fid}, {path})\ncache state: {self:?}")
        }

        self.path_to_fid.insert(path.clone(), fid);
        self.fid_to_path.insert(fid, path);
    }

    pub(crate) fn remove(&mut self, fid: u32) -> Option<String> {
        let path = self.fid_to_path.remove(&fid)?;
        self.path_to_fid.remove(&path);

        Some(path)
    }

    #[cfg(test)]
    /// Used to assert on the cache state in client integration tests
    pub(crate) fn path_to_fid(&self) -> &HashMap<String, u32> {
        &self.path_to_fid
    }
}

fn normalise_path(path: &str) -> String {
    let elems: Vec<&str> = path
        .split('/')
        .filter(|elem| !elem.is_empty() && *elem != ".")
        .collect();

    format!("/{}", elems.join("/"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sansio::protocol::Qid;

    #[test]
    fn handle_walk_chunks_requests_to_maxwelem() {
        let parts: Vec<String> = (0..=MAXWELEM).map(|i| format!("n{i}")).collect();
        let full_path = parts.join("/");

        let mut state = State::default();
        let mut coro = state.handle_walk(full_path.clone());

        // first walk should be MAXWELEM elements
        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::walk(0, 1, parts[0..MAXWELEM].to_vec()));
            Rmessage::new(tag, Rdata::walk(vec![Qid::default(); MAXWELEM]))
        });

        // second walk should contain the 17th element only
        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::walk(1, 1, parts[MAXWELEM..].to_vec()));
            Rmessage::new(tag, Rdata::walk(vec![Qid::default()]))
        });

        // the provided fid for the walk should now be bound to the full path
        let fid = coro.resume().unwrap().unwrap();
        assert_eq!(fid, 1);
        assert_eq!(state.fids.fid_for_unnormalised_path(&full_path), Some(1));
    }

    #[test]
    fn handle_read_from_respects_requested_count_and_iounit() {
        let mut state = State::default();
        let mut coro = state.handle_read_from("/hello".to_string(), 0, 5);

        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::walk(0, 1, ["hello".to_string()]));
            Rmessage::new(tag, Rdata::walk(vec![Qid::default()]))
        });

        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::open(1, Mode::READ.bits()));
            Rmessage::new(tag, Rdata::open(Qid::default(), 3))
        });

        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::read(1, 0, 3));
            Rmessage::new(tag, Rdata::read(b"abc".to_vec()))
        });

        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::read(1, 3, 2));
            Rmessage::new(tag, Rdata::read(b"de".to_vec()))
        });

        let bytes = coro.resume().unwrap().unwrap();

        assert_eq!(bytes, b"abcde".to_vec());
    }

    #[test]
    fn fidcache_insert_updates_both_maps() {
        let mut state = State::default();
        state.fids.insert(1, "/hello".into());

        assert_eq!(state.fids.fid_for_unnormalised_path("/hello"), Some(1));
        assert_eq!(state.fids.path_for(1), Some("/hello"));
    }

    #[test]
    fn fidcache_remove_updates_both_maps() {
        let mut state = State::default();
        state.fids.insert(1, "/hello".into());

        assert_eq!(state.fids.remove(1), Some("/hello".into()));
        assert_eq!(state.fids.fid_for_unnormalised_path("/hello"), None);
        assert_eq!(state.fids.path_for(1), None);
    }

    #[test]
    fn handle_walk_from_empty_path_returns_base_fid() {
        let mut state = State::default();
        state.fids.insert(1, "/subdir".into());

        let fid = state
            .handle_walk_from(1, "".to_string())
            .resume()
            .unwrap()
            .unwrap();

        assert_eq!(fid, 1);
        assert_eq!(state.next_fid, 1);
        assert_eq!(state.fids.fid_for_unnormalised_path("/subdir"), Some(1));
    }

    #[test]
    fn handle_walk_from_uses_base_path_for_cache_key() {
        let mut state = State::default();
        state.fids.insert(1, "/subdir".into());
        state.fids.insert(2, "/subdir/child".into());

        let fid = state
            .handle_walk_from(1, "child".to_string())
            .resume()
            .unwrap()
            .unwrap();

        assert_eq!(fid, 2);
        assert_eq!(state.next_fid, 1);
    }

    #[test]
    #[should_panic(expected = "unknown base fid for walk: 42")]
    fn handle_walk_from_panics_when_base_fid_not_cached() {
        let mut state = State::default();
        _ = state.handle_walk_from(42, "child".to_string()).resume();
    }
}
