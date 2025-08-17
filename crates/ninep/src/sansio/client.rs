//! Traits and structs for implementing a 9p client
use crate::{
    fs::{Mode, Perm, Stat},
    sansio::protocol::{Data, RawStat, Rdata, Rmessage, SharedBuf, Tdata, Tmessage},
    sync::SyncNineP,
};
use simple_coro::{Coro, Handle, ReadyCoro};
use std::{cmp::min, collections::HashMap, future::Future, io};

macro_rules! expect_rmessage {
    ($resp:expr, $variant:ident { $($field:ident),+, .. }) => {
        match $resp.content {
            Rdata::$variant { $($field),+, .. } => ($($field),+),
            Rdata::Error { ename } => return err(ename),
            m => return err(format!("unexpected response: {m:?}")),
        }

    };

    ($resp:expr, $variant:ident { $($field:ident),+ }) => {
        match $resp.content {
            Rdata::$variant { $($field),+ } => ($($field),+),
            Rdata::Error { ename } => return err(ename),
            m => return err(format!("unexpected response: {m:?}")),
        }

    };
}

pub(crate) const MSIZE: u32 = u16::MAX as u32;
pub(crate) const VERSION: &str = "9P2000";

type Coro9p<T, F> = ReadyCoro<Tmessage, Rmessage, io::Result<T>, F>;

pub(crate) fn err<T, E>(e: E) -> io::Result<T>
where
    E: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    Err(io::Error::other(e))
}

/// Internal sans-IO state for a 9p client implementation that can be used along with an I/O stream
/// to implement a concrete Client.
#[derive(Debug)]
pub(crate) struct State {
    pub(crate) msize: u32,
    pub(crate) fids: HashMap<String, u32>,
    pub(crate) next_fid: u32,
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
    ) -> Coro9p<(), impl Future<Output = io::Result<()>> + use<'_>> {
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

            let (msize, version) = expect_rmessage!(rmessage, Version { msize, version });
            if version != VERSION {
                return err("server version not supported");
            }

            handle
                .yield_value(Tmessage::new(
                    0,
                    Tdata::Attach {
                        fid: 0,
                        afid: u32::MAX, // no auth
                        uname,
                        aname,
                    },
                ))
                .await;

            self.msize = msize;

            Ok(())
        })
    }

    /// Associate the given path with a new fid.
    pub(crate) fn handle_walk(
        &mut self,
        path: String,
    ) -> Coro9p<u32, impl Future<Output = io::Result<u32>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let new_fid = {
                if let Some(fid) = self.fids.get(&path) {
                    return Ok(*fid);
                }

                self.next_fid()
            };

            // If the walk succeeds then we've successfully associated our new fid with this path
            // and we don't currently do anything with the qids for each of the path elements
            handle
                .yield_value(Tmessage::new(
                    0,
                    Tdata::Walk {
                        fid: 0,
                        new_fid,
                        wnames: path.split('/').map(Into::into).collect(),
                    },
                ))
                .await;

            self.fids.insert(path, new_fid);

            Ok(new_fid)
        })
    }

    /// Request the current [Stat] of the file or directory identified by the given path.
    pub(crate) fn handle_stat(
        &mut self,
        path: String,
    ) -> Coro9p<Stat, impl Future<Output = io::Result<Stat>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let rmessage = handle
                .yield_value(Tmessage::new(0, Tdata::Stat { fid }))
                .await;

            let raw_stat = expect_rmessage!(rmessage, Stat { stat, .. });
            match raw_stat.try_into() {
                Ok(s) => Ok(s),
                Err(e) => err(e),
            }
        })
    }

    pub(crate) fn handle_read_count(
        &mut self,
        fid: u32,
        offset: u64,
        count: u32,
    ) -> Coro9p<Vec<u8>, impl Future<Output = io::Result<Vec<u8>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let rmessage = handle
                .yield_value(Tmessage::new(0, Tdata::Read { fid, offset, count }))
                .await;
            let Data(data) = expect_rmessage!(rmessage, Read { data });

            Ok(data)
        })
    }

    fn _read_all(
        &mut self,
        path: String,
        mode: Mode,
    ) -> Coro9p<Vec<u8>, impl Future<Output = io::Result<Vec<u8>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let mode = mode.bits();
            handle
                .yield_value(Tmessage::new(0, Tdata::Open { fid, mode }))
                .await;

            let count = self.msize;
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
    ) -> Coro9p<Vec<u8>, impl Future<Output = io::Result<Vec<u8>>> + use<'_>> {
        self._read_all(path, Mode::FILE)
    }

    /// Read the directory listing of the directory at `path`.
    pub(crate) fn handle_read_dir(
        &mut self,
        path: String,
    ) -> Coro9p<Vec<Stat>, impl Future<Output = io::Result<Vec<Stat>>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let bytes = handle.yield_from(self._read_all(path, Mode::DIR)).await?;
            let mut buf = io::Cursor::new(bytes);
            let mut stats: Vec<Stat> = Vec::new();
            let sb = SharedBuf::default();

            loop {
                match RawStat::read_from(&sb, &mut buf) {
                    Ok(rs) => match rs.try_into() {
                        Ok(s) => stats.push(s),
                        Err(e) => return err(e),
                    },
                    Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => break,
                    Err(e) => return Err(e),
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
    ) -> Coro9p<usize, impl Future<Output = io::Result<usize>> + use<'a, 's>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            let len = content.len();
            let mut cur = 0;
            let header_size = 4 + 8 + 4; // fid + offset + data len
            let chunk_size = (self.msize - header_size) as usize;

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
                let n = expect_rmessage!(rmessage, Write { count });
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
    ) -> Coro9p<(), impl Future<Output = io::Result<()>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
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

            Ok(())
        })
    }

    /// Attempt to remove a file from the connected filesystem.
    pub(crate) fn handle_remove(
        &mut self,
        path: String,
    ) -> Coro9p<(), impl Future<Output = io::Result<()>> + use<'_>> {
        Coro::from(move |handle: Handle<Tmessage, Rmessage>| async move {
            let fid = handle.yield_from(self.handle_walk(path)).await?;
            handle
                .yield_value(Tmessage::new(0, Tdata::Remove { fid }))
                .await;

            Ok(())
        })
    }
}
