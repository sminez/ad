//! Traits and structs for implementing a 9p client
use crate::{
    fs::{IoUnit, Mode, Perm, Stat},
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
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ProtocolViolation { expected, received } => write!(
                f,
                "9p protocol violation: expected {expected}, but received {received:?}"
            ),
            Self::Rerror { ename } => write!(f, "9p error: {ename}"),
            Self::Io(inner) => write!(f, "IO error: {inner}"),
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
    ($resp:expr, $variant:ident { $($field:ident),+, .. }) => {
        match $resp.content {
            Rdata::$variant { $($field),+, .. } => Ok(($($field),+)),
            Rdata::Error { ename } => Err(Error::Rerror { ename }),
            m => Err(Error::ProtocolViolation {
                expected: stringify!($variant).to_string(),
                received: Box::new(m)
            }),
        }

    };

    ($resp:expr, $variant:ident { $($field:ident),+ }) => {
        match $resp.content {
            Rdata::$variant { $($field),+ } => Ok(($($field),+)),
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
    pub(crate) fids: HashMap<String, u32>,
    pub(crate) next_fid: u32,
}

impl Default for State {
    fn default() -> Self {
        State {
            msize: MSIZE,
            fids: HashMap::from([("/".into(), 0)]),
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
                .yield_value(Tmessage::new(
                    u16::MAX,
                    Tdata::Version {
                        msize: MSIZE,
                        version: VERSION.to_string(),
                    },
                ))
                .await;

            let (msize, version) = expect_rmessage!(rmessage, Version { msize, version })?;
            if version != VERSION {
                return err("server version not supported");
            }

            let rmessage = handle
                .yield_value(Tmessage::new(
                    0,
                    Tdata::Attach {
                        fid: 0,
                        afid: AFID_NO_AUTH,
                        uname,
                        aname,
                    },
                ))
                .await;

            expect_rmessage!(rmessage, Attach { aqid })?;
            self.msize = msize;

            Ok(())
        })
    }

    /// Associate the given path with a new fid.
    pub(crate) fn handle_walk(
        &mut self,
        path: String,
    ) -> Coro9p<u32, impl Future<Output = Result<u32>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            if let Some(fid) = self.fids.get(&path) {
                return Ok(*fid);
            }

            let wnames: Vec<String> = path
                .split('/')
                .filter(|elem| !["", "."].contains(elem))
                .map(Into::into)
                .collect();

            if wnames.is_empty() {
                // walk to root but don't cache the path
                return Ok(0);
            }

            let mut intermediate_fids = Vec::new();
            let mut fid = 0;

            for chunk in wnames.chunks(MAXWELEM) {
                let new_fid = self.next_fid();
                let rmessage = handle
                    .yield_value(Tmessage::new(
                        0,
                        Tdata::Walk {
                            fid,
                            new_fid,
                            wnames: chunk.to_vec(),
                        },
                    ))
                    .await;
                let wqids = expect_rmessage!(rmessage, Walk { wqids })?;

                if wqids.len() != chunk.len() {
                    for fid in intermediate_fids.into_iter().rev() {
                        _ = handle
                            .yield_value(Tmessage::new(0, Tdata::Clunk { fid }))
                            .await;
                    }

                    return err("walk failed before reaching full path");
                }

                intermediate_fids.push(new_fid);
                fid = new_fid;
            }

            let new_fid = intermediate_fids.pop().unwrap();
            self.fids.insert(path, new_fid);

            for fid in intermediate_fids {
                _ = handle
                    .yield_value(Tmessage::new(0, Tdata::Clunk { fid }))
                    .await;
            }

            Ok(new_fid)
        })
    }

    /// Request the current [Stat] of the file or directory identified by the given path.
    pub(crate) fn handle_stat(
        &mut self,
        path: String,
    ) -> Coro9p<Stat, impl Future<Output = Result<Stat>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let rmessage = handle
                .yield_value(Tmessage::new(0, Tdata::Stat { fid }))
                .await;

            let raw_stat = expect_rmessage!(rmessage, Stat { stat, .. })?;
            match raw_stat.try_into() {
                Ok(s) => Ok(s),
                Err(e) => err(e),
            }
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
                .yield_value(Tmessage::new(0, Tdata::Open { fid, mode }))
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
                .yield_value(Tmessage::new(0, Tdata::Read { fid, offset, count }))
                .await;
            let Data(data) = expect_rmessage!(rmessage, Read { data })?;

            Ok(data)
        })
    }

    fn _read_all(
        &mut self,
        path: String,
    ) -> Coro9p<Vec<u8>, impl Future<Output = Result<Vec<u8>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            handle.yield_from(self.handle_open(fid, Mode::READ)).await?;

            let count = self.msize - IOHDRSZ;
            let mut bytes = Vec::new();
            let mut offset = 0;
            loop {
                let data = handle
                    .yield_from(self.handle_read_count(fid, offset, count))
                    .await?;
                if data.is_empty() {
                    break;
                }
                offset += data.len() as u64;
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
        self._read_all(path)
    }

    /// Read the directory listing of the directory at `path`.
    pub(crate) fn handle_read_dir(
        &mut self,
        path: String,
    ) -> Coro9p<Vec<Stat>, impl Future<Output = Result<Vec<Stat>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let bytes = handle.yield_from(self._read_all(path)).await?;
            let mut buf = io::Cursor::new(bytes);
            let mut stats: Vec<Stat> = Vec::new();
            let sb = SharedBuf::default();

            loop {
                match RawStat::read_from(self.msize, &sb, &mut buf) {
                    Ok(rs) => match rs.try_into() {
                        Ok(s) => stats.push(s),
                        Err(e) => return err(e),
                    },
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

            let len = content.len();
            let mut cur = 0;
            let chunk_size = (self.msize - IOHDRSZ) as usize;

            while cur < len {
                let end = min(cur + chunk_size, len);
                let rmessage = handle
                    .yield_value(Tmessage::new(
                        0,
                        Tdata::Write {
                            fid,
                            offset,
                            data: Data(content[cur..end].to_vec()),
                        },
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
            let path = format!("{dir}/{name}");
            let fid = handle.yield_from(self.handle_walk(dir)).await?;
            handle
                .yield_value(Tmessage::new(
                    0,
                    Tdata::Create {
                        fid,
                        name,
                        perm: perms.bits(),
                        mode: mode.bits(),
                    },
                ))
                .await;

            self.fids.insert(path, fid);

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
            handle
                .yield_value(Tmessage::new(0, Tdata::Remove { fid }))
                .await;

            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sansio::protocol::Qid;
    use std::collections::HashMap;

    #[test]
    fn handle_walk_chunks_requests_to_maxwelem() {
        let parts: Vec<String> = (0..=MAXWELEM).map(|i| format!("n{i}")).collect();
        let full_path = parts.join("/");

        let mut state = State {
            msize: MSIZE,
            fids: HashMap::new(),
            next_fid: 1,
        };

        let mut coro = state.handle_walk(full_path.clone());

        // first walk should be MAXWELEM elements
        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(
                content,
                Tdata::Walk {
                    fid: 0,
                    new_fid: 1,
                    wnames: parts[0..MAXWELEM].to_vec()
                }
            );

            Rmessage::new(
                tag,
                Rdata::Walk {
                    wqids: vec![Qid::default(); MAXWELEM],
                },
            )
        });

        // second walk should contain the 17th element only
        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(
                content,
                Tdata::Walk {
                    fid: 1,
                    new_fid: 2,
                    wnames: parts[MAXWELEM..].to_vec()
                }
            );

            Rmessage::new(
                tag,
                Rdata::Walk {
                    wqids: vec![Qid::default()],
                },
            )
        });

        // after the walks we should clunk the intermediate fid
        coro = coro.resume().unwrap_pending(|Tmessage { tag, content }| {
            assert_eq!(content, Tdata::Clunk { fid: 1 });
            Rmessage::new(tag, Rdata::Clunk {})
        });

        // the provided fid for the walk should now be bound to the full path
        let fid = coro.resume().unwrap().unwrap();
        assert_eq!(fid, 2);
        assert_eq!(state.fids.get(&full_path), Some(&2));
    }

    #[test]
    fn handle_walk_empty_path_returns_root_without_messages() {
        let mut state = State {
            msize: MSIZE,
            fids: HashMap::from([("/".to_string(), 0)]),
            next_fid: 1,
        };

        let coro = state.handle_walk(String::new());
        let fid = coro.resume().unwrap().unwrap();

        assert_eq!(fid, 0);
    }
}
