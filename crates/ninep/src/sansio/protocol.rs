//! Sans-io 9p protocol implementation
//!
//!   <http://man.cat-v.org/plan_9/5/>
use crate::sync::SyncNineP;
use simple_coro::{Coro, CoroState, Handle, ReadyCoro};
use std::{
    cell::UnsafeCell,
    fmt,
    future::Future,
    io::{self, ErrorKind},
    mem::size_of,
};

/// The size of variable length data is denoted using a u16 so anything longer
/// than u16::MAX is not something we can handle.
pub const MAX_SIZE_FIELD: usize = u16::MAX as usize;
/// For data fields in read/write messages the size field is 32bits not 16
pub const MAX_DATA_SIZE_FIELD: usize = u32::MAX as usize;
/// The maximum number of bytes we allow in a Data buffer: a client attempting
/// to use more than this is an error.
pub const MAX_DATA_LEN: usize = 32 * 1024 * 1024;

/// Non IO related errors that can occur when attempting to serialize a [NineP] type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteError {
    /// The maximum number of bytes we allow in a Data buffer is [MAX_DATA_LEN]: a client
    /// attempting to use more than this is an error.
    DataLength(usize),
    /// The size of variable length data is denoted using a u16 so anything longer
    /// than u16::MAX is not something we can handle.
    FieldLength(usize),
}

impl fmt::Display for WriteError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DataLength(n_bytes) => write!(
                f,
                "data field too long: max={MAX_DATA_SIZE_FIELD} len={n_bytes}"
            ),
            Self::FieldLength(len) => write!(f, "string too long: max={MAX_SIZE_FIELD} len={len}"),
        }
    }
}

/// A shared byte buffer that can be accessed inside of 9p parsing functions.
///
/// The internals of this API can only be accessed from within this crate as in practice this is
/// only safe to do as part of the IO loop that is parsing a 9p message using a [Coro] that this
/// [SharedBuf] is shared with.
#[derive(Debug)]
pub struct SharedBuf(UnsafeCell<SharedBufInner>);
struct SharedBufInner {
    pos: usize,
    buf: Vec<u8>,
}

/// SAFETY: requires that the safety guarantees for as_inner_mut and as_slice_to are upheld
unsafe impl Send for SharedBuf {}
/// SAFETY: requires that the safety guarantees for as_inner_mut and as_slice_to are upheld
unsafe impl Sync for SharedBuf {}

impl Default for SharedBuf {
    fn default() -> Self {
        Self(UnsafeCell::new(SharedBufInner {
            pos: 0,
            buf: Vec::new(),
        }))
    }
}

impl SharedBuf {
    /// Get an exclusive reference to the inner buffer from a shared reference to the outer
    /// [SharedBuf], resetting `pos` to the start of the buffer.
    ///
    /// # Safety
    /// The caller must guarantee that no concurrent access to the inner buffer takes place while
    /// this exclusive reference is held.
    #[allow(clippy::mut_from_ref)]
    pub(crate) unsafe fn as_inner_mut(&self) -> &mut Vec<u8> {
        // SAFETY: the caller must guarantee that no concurrent access takes place
        unsafe {
            let inner = &mut *self.0.get();
            inner.pos = 0;

            &mut inner.buf
        }
    }

    /// Get a shared reference to the inner buffer, updating `pos` to account for the bytes read
    /// out of the buffer.
    ///
    /// # Panics
    /// This method will panic if `end` is out of bounds for the remaining data in the buffer.
    ///
    /// # Safety
    /// The caller must guarantee that no concurrent access to the inner buffer takes place while
    /// this reference is held.
    pub(crate) unsafe fn as_slice_to(&self, end: usize) -> &[u8] {
        // SAFETY: the caller must guarantee that no concurrent access takes place
        unsafe {
            let inner = &mut *self.0.get();
            let prev = inner.pos;
            inner.pos += end;

            &inner.buf[prev..inner.pos]
        }
    }

    /// Helper for reading a single [NineP] value from a [SharedBuf] that has already been filled with
    /// the correct number of bytes for parsing the entire message without any further IO.
    ///
    /// # Safety
    /// The caller must guarantee that no concurrent access to the inner buffer takes place while
    /// this reference is held.
    unsafe fn parse_from_buffer<T: NineP>(&self) -> io::Result<T> {
        let mut coro = T::read_9p_coro(self);

        loop {
            coro = match coro.resume() {
                CoroState::Pending(c, _) => c.send(()),
                CoroState::Complete(res) => return res,
            };
        }
    }
}

/// Something that can be encoded to and decoded 9p protocol messages.
///
/// From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
///   Each message consists of a sequence of bytes. Two-, four-, and eight-byte fields hold
///   unsigned integers represented in little-endian order (least significant byte first).
pub trait NineP: Sized {
    /// Number of bytes required to encode
    fn n_bytes(&self) -> usize;

    /// Encode self as bytes for the 9p protocol into a given buffer which the caller must
    /// ensure is sized to be at least [NineP::n_bytes].
    fn write_bytes(&self, buf: &mut [u8]) -> Result<(), WriteError>;

    /// Serialize into a byte buffer ready for transmission.
    fn write_9p_bytes(&self) -> Result<Vec<u8>, WriteError> {
        let mut buf = vec![0; self.n_bytes()];
        self.write_bytes(&mut buf)?;

        Ok(buf)
    }

    /// This is not a normal async function. It is used to set up a sans-io state machine that
    /// can be driven by a concrete implementation.
    fn read_9p(
        buf: &SharedBuf,
        handle: Handle<usize>,
    ) -> impl Future<Output = io::Result<Self>> + Send;

    /// Create a new [Coro] that reads from a [SharedBuf] to parse 9p protocol messages with
    /// minimal allocation.
    fn read_9p_coro(
        buf: &SharedBuf,
    ) -> ReadyCoro<usize, (), io::Result<Self>, impl Future<Output = io::Result<Self>> + Send> {
        Coro::from(async move |handle: Handle<usize>| Self::read_9p(buf, handle).await)
    }
}

/// wrapper around uX::from_le_bytes that accepts a slice rather than a fixed size array
macro_rules! from_le_bytes {
    ($ty:ty, $bytes:expr) => {
        // SAFETY: we know we are setting the correct array length
        unsafe { <$ty>::from_le_bytes($bytes[0..size_of::<$ty>()].try_into().unwrap_unchecked()) }
    };
}

// Unsigned integer types can all be treated the same way so we stamp them out using a macro.
// They are written and read in their little-endian byte form.
macro_rules! impl_u {
    ($($ty:ty),+) => {
        $(
            impl NineP for $ty {
                fn n_bytes(&self) -> usize {
                    size_of::<$ty>()
                }

                fn write_bytes(&self, buf: &mut [u8]) -> Result<(), WriteError> {
                    buf[0..size_of::<$ty>()].copy_from_slice(&self.to_le_bytes());
                    Ok(())
                }

                async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<$ty> {
                    let n = size_of::<$ty>();
                    handle.yield_value(n).await;

                    Ok(from_le_bytes!($ty, buf.as_slice_to(n)))
                }
            }
        )+
    };
}

impl_u!(u8, u16, u32, u64);

// `[size: u16] [content as bytes...]`
//
// From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
//   Data items of larger or variable lengths are represented by a two-byte field specifying
//   a count, n, followed by n bytes of data. Text strings are represented this way, with
//   the text itself stored as a UTF-8 encoded sequence of Unicode charac- ters (see utf(6)).
//
//   Text strings in 9P messages are not NUL- terminated: n counts the bytes of UTF-8 data,
//   which include no final zero byte.  The NUL character is illegal in all text strings
//   in 9P, and is therefore excluded from file names, user names, and so on.
impl NineP for String {
    fn n_bytes(&self) -> usize {
        size_of::<u16>() + self.len()
    }

    fn write_bytes(&self, buf: &mut [u8]) -> Result<(), WriteError> {
        let len = self.len();
        if len > MAX_SIZE_FIELD {
            return Err(WriteError::FieldLength(len));
        }

        (len as u16).write_bytes(&mut buf[0..2])?;
        buf[2..len + 2].copy_from_slice(self.as_bytes());

        Ok(())
    }

    async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<Self> {
        let len = u16::read_9p(buf, handle).await? as usize;
        handle.yield_value(len).await;

        // SAFETY: this is the only read we are doing and it matches the data we requested
        let data = unsafe { buf.as_slice_to(len).to_vec() };

        String::from_utf8(data).map_err(|e| io::Error::new(ErrorKind::InvalidData, e.to_string()))
    }
}

// `[size: u16] [content as bytes...]`
//
// From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
//   Data items of larger or variable lengths are represented by a two-byte field specifying
//   a count, n, followed by n bytes of data.
impl<T: NineP + fmt::Debug + Send> NineP for Vec<T> {
    fn n_bytes(&self) -> usize {
        size_of::<u16>() + self.iter().map(|t| t.n_bytes()).sum::<usize>()
    }

    fn write_bytes(&self, mut buf: &mut [u8]) -> Result<(), WriteError> {
        let n_bytes = self.iter().map(|t| t.n_bytes()).sum::<usize>();
        if n_bytes > MAX_SIZE_FIELD {
            return Err(WriteError::FieldLength(n_bytes));
        }

        (self.len() as u16).write_bytes(&mut buf[0..2])?;
        buf = &mut buf[2..];
        for t in self {
            let n = t.n_bytes();
            t.write_bytes(buf)?;
            buf = &mut buf[n..];
        }

        Ok(())
    }

    async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<Self> {
        let len = u16::read_9p(buf, handle).await? as usize;
        let mut elems = Vec::with_capacity(len);
        for _ in 0..len {
            elems.push(T::read_9p(buf, handle).await?);
        }

        Ok(elems)
    }
}

/// A wrapper around a `Vec<u8>` for handling data fields in read/write messages
/// ```text
/// READ(5)
///  NAME
///       read, write - transfer data from and to a file
///
///  SYNOPSIS
///       size[4] Tread tag[2] fid[4] offset[8] count[4]
///       size[4] Rread tag[2] count[4] data[count]
///
///       size[4] Twrite tag[2] fid[4] offset[8] count[4] data[count]
///       size[4] Rwrite tag[2] count[4]
/// ```
#[derive(Clone, PartialEq, Eq)]
pub struct Data(pub(crate) Vec<u8>);

impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Data(n_bytes={})", self.0.len())
    }
}

impl From<Vec<u8>> for Data {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl TryFrom<Data> for Vec<RawStat> {
    type Error = io::Error;

    fn try_from(Data(bytes): Data) -> Result<Self, io::Error> {
        let mut buf = Vec::new();
        let mut bytes = bytes.as_slice();
        let n = size_of::<RawStat>();
        let sb = SharedBuf::default();

        loop {
            match RawStat::read_from(&sb, &mut bytes) {
                Ok(rs) => {
                    buf.push(rs);
                    bytes = &bytes[n..];
                }
                Err(e) if e.kind() == ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(e),
            }
        }

        Ok(buf)
    }
}

impl NineP for Data {
    fn n_bytes(&self) -> usize {
        size_of::<u32>() + self.0.len()
    }

    fn write_bytes(&self, buf: &mut [u8]) -> Result<(), WriteError> {
        let n_bytes = self.0.len();
        if n_bytes > MAX_DATA_SIZE_FIELD {
            return Err(WriteError::DataLength(n_bytes));
        }

        (n_bytes as u32).write_bytes(&mut buf[0..4])?;
        buf[4..].copy_from_slice(&self.0);

        Ok(())
    }

    async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<Self> {
        let len = u32::read_9p(buf, handle).await? as usize;
        if len > MAX_DATA_LEN {
            return Err(io::Error::new(
                ErrorKind::InvalidData,
                format!("data field too long: max={MAX_DATA_LEN} len={len}"),
            ));
        }
        handle.yield_value(len).await;

        // SAFETY: this is the only read we are doing and it matches the data we requested
        let data = unsafe { buf.as_slice_to(len).to_vec() };

        Ok(Data(data))
    }
}

/// A machine-independent directory entry
/// <http://man.cat-v.org/plan_9/5/stat>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawStat {
    /// `size[2]`      total byte count of the following data
    pub size: u16,
    /// `type[2]`      for kernel use
    pub ty: u16,
    /// `dev[4]`       for kernel use
    pub dev: u32,
    /// Qid type, version and path
    pub qid: Qid,
    /// `mode[4]`      permissions and flags
    pub mode: u32,
    /// `atime[4]`     last access time
    pub atime: u32,
    /// `mtime[4]`     last modification time
    pub mtime: u32,
    /// `length[8]`    length of file in bytes
    pub length: u64,
    /// `name[ s ]`    file name; must be / if the file is the root directory of the server
    pub name: String,
    /// `uid[ s ]`     owner name
    pub uid: String,
    /// `gid[ s ]`     group name
    pub gid: String,
    /// `muid[ s ]`    name of the user who last modified the file
    pub muid: String,
}

macro_rules! write_fields {
    ($buf:expr, $self:expr, $($field:ident),+) => {
        #[allow(unused_assignments)]
        {
            $(
                let len = $self.$field.n_bytes();
                $self.$field.write_bytes(&mut $buf[0..len])?;
                $buf = &mut $buf[len..];
            )+
            Ok(())
        }

    };
}

impl NineP for RawStat {
    fn n_bytes(&self) -> usize {
        // 2 2 4 13 4 4 4 8 -> 41
        41 + self.name.n_bytes() + self.uid.n_bytes() + self.gid.n_bytes() + self.muid.n_bytes()
    }

    fn write_bytes(&self, mut buf: &mut [u8]) -> Result<(), WriteError> {
        write_fields!(
            buf, self, size, ty, dev, qid, mode, atime, mtime, length, name, uid, gid, muid
        )
    }

    async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<Self> {
        // Request the fixed sized data before the strings in one block up front
        handle.yield_value(41).await;

        // SAFETY: this is the only read we are doing and it matches the data we requested
        let bytes = unsafe { buf.as_slice_to(41) };

        let size = from_le_bytes!(u16, bytes);
        let ty = from_le_bytes!(u16, &bytes[2..]);
        let dev = from_le_bytes!(u32, &bytes[4..]);
        let qid = Qid::from_bytes(&bytes[8..]);
        let mode = from_le_bytes!(u32, &bytes[21..]);
        let atime = from_le_bytes!(u32, &bytes[25..]);
        let mtime = from_le_bytes!(u32, &bytes[29..]);
        let length = from_le_bytes!(u64, &bytes[33..]);
        let name = String::read_9p(buf, handle).await?;
        let uid = String::read_9p(buf, handle).await?;
        let gid = String::read_9p(buf, handle).await?;
        let muid = String::read_9p(buf, handle).await?;

        Ok(RawStat {
            size,
            ty,
            dev,
            qid,
            mode,
            atime,
            mtime,
            length,
            name,
            uid,
            gid,
            muid,
        })
    }
}

/// A qid represents the server's unique identification for the file being accessed: two files
/// on the same server hierarchy are the same if and only if their qids are the same.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Qid {
    /// `qid.type[1]` the type of the file (directory, etc.), represented as a bit vector
    /// corresponding to the high 8 bits of the file's mode word.
    pub ty: u8,
    /// `qid.vers[4]`  version number for given path
    pub version: u32,
    /// `qid.path[8]`  the file server's unique identification for the file
    pub path: u64,
}

impl Qid {
    #[inline]
    fn from_bytes(bytes: &[u8]) -> Self {
        let ty = from_le_bytes!(u8, &bytes);
        let version = from_le_bytes!(u32, &bytes[1..]);
        let path = from_le_bytes!(u64, &bytes[5..]);

        Self { ty, version, path }
    }
}

impl NineP for Qid {
    fn n_bytes(&self) -> usize {
        1 + 4 + 8
    }

    fn write_bytes(&self, mut buf: &mut [u8]) -> Result<(), WriteError> {
        write_fields!(buf, self, ty, version, path)
    }

    async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<Self> {
        let ty = u8::read_9p(buf, handle).await?;
        let version = u32::read_9p(buf, handle).await?;
        let path = u64::read_9p(buf, handle).await?;

        Ok(Qid { ty, version, path })
    }
}

/// Taken from the enum in fcall.h in the plan9 source.
///   <https://github.com/9fans/plan9port/blob/master/include/fcall.h#L80>
///
/// This is just used internally to help with defining the encode / decode behaviour
/// of the various message types.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct MessageType(u8);

#[allow(non_upper_case_globals)]
impl MessageType {
    const Tversion: Self = Self(100);
    const Rversion: Self = Self(101);

    const Tauth: Self = Self(102);
    const Rauth: Self = Self(103);

    const Tattach: Self = Self(104);
    const Rattach: Self = Self(105);

    // Terror = 106,
    const Rerror: Self = Self(107);

    const Tflush: Self = Self(108);
    const Rflush: Self = Self(109);

    const Twalk: Self = Self(110);
    const Rwalk: Self = Self(111);

    const Topen: Self = Self(112);
    const Ropen: Self = Self(113);

    const Tcreate: Self = Self(114);
    const Rcreate: Self = Self(115);

    const Tread: Self = Self(116);
    const Rread: Self = Self(117);

    const Twrite: Self = Self(118);
    const Rwrite: Self = Self(119);

    const Tclunk: Self = Self(120);
    const Rclunk: Self = Self(121);

    const Tremove: Self = Self(122);
    const Rremove: Self = Self(123);

    const Tstat: Self = Self(124);
    const Rstat: Self = Self(125);

    const Twstat: Self = Self(126);
    const Rwstat: Self = Self(127);
    // Tmax = 128,
    // Topenfd = 98,
    // Ropenfd = 99,
}

/// Helper for implementing Tmessage and Rmessage
macro_rules! impl_message_format {
    (
        $message_ty:ident, $enum_ty:ident, $err:expr;
        $($enum_variant:ident => $message_variant:ident {
            $($field:ident: $ty:ty,)*
        })+
    ) => {
        impl NineP for $message_ty {
            fn n_bytes(&self) -> usize {
                let content_size = match &self.content {
                    $(
                        $enum_ty::$enum_variant { $($field,)* } => {
                            #[allow(unused_mut)]
                            let mut n = 0;
                            $(n += $field.n_bytes();)*
                            n
                        }
                    )+
                };

                // size[4] type[1] tag[2] | content[...]
                4 + 1 + 2 + content_size
            }

            #[allow(unused_assignments)]
            fn write_bytes(&self, buf: &mut [u8]) -> Result<(), WriteError> {
                let ty = match self.content {
                    $($enum_ty::$enum_variant { .. } => MessageType::$message_variant.0,)+
                };

                (self.n_bytes() as u32).write_bytes(buf)?; // 4
                ty.write_bytes(&mut buf[4..])?; // 1
                self.tag.write_bytes(&mut buf[5..])?; // 2
                let mut offset = 7;

                match &self.content {
                    $(
                        $enum_ty::$enum_variant { $($field,)* } => {
                            $(
                                $field.write_bytes(&mut buf[offset..])?;
                                offset += $field.n_bytes();
                            )*
                        },
                    )+
                }

                Ok(())
            }

            #[allow(unused_assignments, unused_unsafe)]
            async fn read_9p(buf: &SharedBuf, handle: Handle<usize>) -> io::Result<Self> {
                let len = u32::read_9p(buf, handle).await? as usize;
                handle.yield_value(len-4).await;

                // SAFETY: this is inside a running coro so the outer code is unable to take a
                //         reference to buf while we update `pos` and read from the inner buffer
                unsafe {
                    let bytes = buf.as_slice_to(3);
                    let ty = from_le_bytes!(u8, &bytes);
                    let tag = from_le_bytes!(u16, &bytes[1..]);

                    let content = match MessageType(ty) {
                        $(
                            MessageType::$message_variant => $enum_ty::$enum_variant {
                                $($field: buf.parse_from_buffer::<$ty>()?),*
                            },
                        )+

                        MessageType(ty) => return Err(io::Error::new(
                            ErrorKind::InvalidData,
                            format!($err, ty),
                        )),
                    };

                    Ok($message_ty { tag, content })
                }
            }
        }
    };
}

/// The Plan 9 File Protocol, 9P, is used for messages between clients and servers. A client
/// transmits requests (T- messages) to a server, which subsequently returns replies (R-messages)
/// to the client. The combined acts of transmitting (receiving) a request of a particular type,
/// and receiving (transmitting) its reply is called a transaction of that type.
///
/// The data we decode into this struct is of the following form:
/// ```txt
///   size[4] type[1] tag[2] | content[...]
/// ```
/// where the content is a [Tdata].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tmessage {
    /// Each T-message has a tag field, chosen and used by the client to identify the message. The
    /// reply to the message will have the same tag. Clients must arrange that no two outstanding
    /// messages on the same connection have the same tag. An exception is the tag NOTAG, defined
    /// as (ushort)~0 in <fcall.h>: the client can use it, when establishing a connection, to
    /// override tag matching in version messages.
    pub tag: u16,
    /// The t-message variant specific data sent by the client
    pub content: Tdata,
}

impl Tmessage {
    /// Construct a new [Tmessage]
    pub const fn new(tag: u16, content: Tdata) -> Self {
        Self { tag, content }
    }
}

/// Generate the Tdata enum along with the wrapped T-message types and their implementations of NineP
macro_rules! impl_tdata {
    ($(
        $(#[$docs:meta])+
        $enum_variant:ident => $message_variant:ident {
            $(
                $(#[$field_docs:meta])+
                $field:ident: $ty:ty,
            )*
        }
    )+) => {
        /// T-message data variants
        ///
        /// The [Tmessage] struct is used to decode T-messages from clients.
        /// See the individual message structs for docs on the format and semantics of each variant.
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Tdata {
            $( $(#[$docs])+ $enum_variant { $($(#[$field_docs])+ $field: $ty,)* }, )+
        }

        impl_message_format!(
            Tmessage, Tdata, "invalid message type for t-message: {}";
            $($enum_variant => $message_variant {
                $($field: $ty,)*
            })+
        );
    };
}

impl_tdata! {
    /// <http://man.cat-v.org/plan_9/5/version>
    /// `size[4] Tversion tag[2] | msize[4] version[s]`
    Version => Tversion {
        /// The requested message size
        msize: u32,
        /// The requested protocol version
        version: String,
    }

    /// <http://man.cat-v.org/plan_9/5/attach>
    /// `size[4] Tauth tag[2] | afid[4] uname[s] aname[s]`
    Auth => Tauth {
        /// The fid to authenticate against
        afid: u32,
        /// The user authenticating
        uname: String,
        /// The filetree to access
        aname: String,
    }

    /// <http://man.cat-v.org/plan_9/5/attach>
    /// `size[4] Tattach tag[2] | fid[4] afid[4] uname[s] aname[s]`
    Attach => Tattach {
        /// The fid to attach to
        fid: u32,
        /// The fid to authenticate against
        afid: u32,
        /// The user attaching
        uname: String,
        /// The filetree to access
        aname: String,
    }

    /// <http://man.cat-v.org/plan_9/5/flush>
    /// `size[4] Tflush tag[2] | oldtag[2]`
    Flush => Tflush {
        /// The tag to flush
        old_tag: u16,
    }

    /// <http://man.cat-v.org/plan_9/5/walk>
    /// `size[4] Twalk tag[2] | fid[4] newfid[4] nwname[2] nwname*(wname[s])`
    Walk => Twalk {
        /// The fid to walk from
        fid: u32,
        /// The fid to associate with the end of the walk
        new_fid: u32,
        /// Path segments to walk from fid to new_fid
        wnames: Vec<String>,
    }

    /// <http://man.cat-v.org/plan_9/5/open>
    /// `size[4] Topen tag[2] | fid[4] mode[1]`
    Open => Topen {
        /// The fid to open
        fid: u32,
        /// The mode to open the resource in
        mode: u8,
    }

    /// <http://man.cat-v.org/plan_9/5/open>
    /// `size[4] Tcreate tag[2] | fid[4] name[s] perm[4] mode[1]`
    Create => Tcreate {
        /// The fid to associate with the directory where the file should be created
        fid: u32,
        /// The name of the new file
        name: String,
        /// The permissions to use
        perm: u32,
        /// The mode to use
        mode: u8,
    }

    /// <http://man.cat-v.org/plan_9/5/read>
    /// `size[4] Tread tag[2] | fid[4] offset[8] count[4]`
    Read => Tread {
        /// The fid to read
        fid: u32,
        /// The offset in bytes to start reading at
        offset: u64,
        /// The number of bytes to read
        count: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/read>
    /// `size[4] Twrite tag[2] | fid[4] offset[8] count[4] data[count]`
    Write => Twrite {
        /// The fid to write to
        fid: u32,
        /// The offset in bytes to start writing at
        offset: u64,
        /// The data to write
        data: Data,
    }

    /// <http://man.cat-v.org/plan_9/5/clunk>
    /// `size[4] Tclunk tag[2] | fid[4]`
    Clunk => Tclunk {
        /// The fid to be closed
        fid: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/remove>
    /// `size[4] Tremove tag[2] | fid[4]`
    Remove => Tremove {
        /// The fid to be removed
        fid: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/stat>
    /// `size[4] Tstat tag[2] | fid[4]`
    Stat => Tstat {
        /// The fid to request a stat for
        fid: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/stat>
    /// `size[4] Twstat tag[2] | fid[4] stat[n]`
    Wstat => Twstat {
        /// The fid to update the stat for
        fid: u32,
        /// The size of the following stat
        size: u16,
        /// The stat data to be written
        stat: RawStat,
    }
}

/// The Plan 9 File Protocol, 9P, is used for messages between clients and servers. A client
/// transmits requests (T- messages) to a server, which subsequently returns replies (R-messages)
/// to the client. The combined acts of transmitting (receiving) a request of a particular type,
/// and receiving (transmitting) its reply is called a transaction of that type.
///
/// The data we decode into this struct is of the following form:
/// ```txt
///   size[4] type[1] tag[2] | content[...]
/// ```
/// where the content is a [Rdata].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rmessage {
    /// Each T-message has a tag field, chosen and used by the client to identify the message. The
    /// reply to the message will have the same tag. Clients must arrange that no two outstanding
    /// messages on the same connection have the same tag. An exception is the tag NOTAG, defined
    /// as (ushort)~0 in <fcall.h>: the client can use it, when establishing a connection, to
    /// override tag matching in version messages.
    pub tag: u16,
    /// The r-message variant specific data sent by the client
    pub content: Rdata,
}

/// Generate the Rdata enum along with the wrapped R-message types
/// and their implementations of Format9p
macro_rules! impl_rdata {
    ($(
        $(#[$docs:meta])+
        $enum_variant:ident => $message_variant:ident {
            $(
                $(#[$field_docs:meta])+
                $field:ident: $ty:ty,
            )*
        }
    )+) => {
        /// R-message data variants
        ///
        /// The [Rmessage] struct is used to encode and send R-messages to clients.
        /// See the individual message structs for docs on the format and semantics of each variant.
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Rdata {
            $( $(#[$docs])+ $enum_variant { $($(#[$field_docs])+ $field: $ty,)* }, )+
        }

        impl_message_format!(
            Rmessage, Rdata, "invalid message type for r-message: {}";
            $($enum_variant => $message_variant {
                $($field: $ty,)*
            })+
        );
    };
}

impl_rdata! {
    /// <http://man.cat-v.org/plan_9/5/version>
    /// `size[4] Rversion tag[2] | msize[4] version[s]`
    Version => Rversion {
        /// Supported message size
        msize: u32,
        /// Supported protocol version
        version: String,
    }

    /// <http://man.cat-v.org/plan_9/5/attach>
    /// `size[4] Rauth tag[2] | aqid[13]`
    Auth => Rauth {
        /// The authenticated Qid of the connected root
        aqid: Qid,
    }

    /// <http://man.cat-v.org/plan_9/5/error>
    /// `size[4] Rerror tag[2] | ename[s]`
    Error => Rerror {
        /// The contents of the error being returned
        ename: String,
    }

    /// <http://man.cat-v.org/plan_9/5/attach>
    /// `size[4] Rattach tag[2] | aquid[13]`
    Attach => Rattach {
        /// Qid corresponding to the Fid used to attach
        aqid: Qid,
    }

    /// <http://man.cat-v.org/plan_9/5/flush>
    /// `size[4] Rflush tag[2]`
    Flush => Rflush {}

    /// <http://man.cat-v.org/plan_9/5/walk>
    /// `size[4] Rwalk tag[2] | nwqid[2] nwqid*(wqid[13])`
    Walk => Rwalk {
        /// Qids for the path elements walked
        wqids: Vec<Qid>,
    }

    /// <http://man.cat-v.org/plan_9/5/open>
    /// `size[4] Ropen tag[2] | qid[13] iounit[4]`
    Open => Ropen {
        /// Qid of the opened resource
        qid: Qid,
        /// IO unit for subsequent read / write operations
        iounit: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/open>
    /// `size[4] Rcreate tag[2] | qid[13] iounit[4]`
    Create => Rcreate {
        /// Qid of the created resource
        qid: Qid,
        /// IO unit for subsequent read / write operations
        iounit: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/read>
    /// `size[4] Rread tag[2] | count[4] data[count]`
    Read => Rread {
        /// The bytes read
        data: Data,
    }

    /// <http://man.cat-v.org/plan_9/5/read>
    /// `size[4] Rwrite tag[2] | count[4]`
    Write => Rwrite {
        /// The number of bytes written
        count: u32,
    }

    /// <http://man.cat-v.org/plan_9/5/clunk>
    /// `size[4] Rclunk tag[2]`
    Clunk => Rclunk {}

    /// <http://man.cat-v.org/plan_9/5/remove>
    /// `size[4] Rremove tag[2]`
    Remove => Rremove {}

    /// <http://man.cat-v.org/plan_9/5/stat>
    /// `size[4] Rstat tag[2] | stat[n]`
    Stat => Rstat {
        /// The size of the following stat
        size: u16,
        /// The stat data for the requested fid
        stat: RawStat,
    }

    /// <http://man.cat-v.org/plan_9/5/stat>
    /// `size[4] Rwstat tag[2]`
    Wstat => Rwstat {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;
    use std::cmp::PartialEq;

    #[test]
    fn uint_decode() {
        let buf: Vec<u8> = vec![0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef];
        let sb = SharedBuf::default();

        assert_eq!(0x01, u8::read_from(&sb, &mut buf.as_slice()).unwrap());
        assert_eq!(0x2301, u16::read_from(&sb, &mut buf.as_slice()).unwrap());
        assert_eq!(
            0x67452301,
            u32::read_from(&sb, &mut buf.as_slice()).unwrap()
        );
        assert_eq!(
            0xefcdab8967452301,
            u64::read_from(&sb, &mut buf.as_slice()).unwrap()
        );
    }

    #[test_case("test", &[0x04, 0x00, 0x74, 0x65, 0x73, 0x74]; "single byte chars only")]
    #[test_case("", &[0x00, 0x00]; "empty string")]
    #[test_case(
        "Hello, 世界",
        &[0x0d, 0x00, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0xe4, 0xb8, 0x96, 0xe7, 0x95, 0x8c];
        "including multi-byte chars"
    )]
    #[test]
    fn string_encode(s: &str, bytes: &[u8]) {
        let s = s.to_string();
        let buf = s.write_9p_bytes().unwrap();
        assert_eq!(&buf, bytes);
    }

    enum F9 {
        U8(u8),
        U16(u16),
        U32(u32),
        U64(u64),
        S(&'static str),
        V(Vec<String>),
        D(Vec<u8>),
        RawStat,
        Clunk,
        Walk,
        Rwalk,
    }

    // simple_test_case doesn't handle generic args for parameterised
    // tests so I'm wrapping things up in the above enum and destructuring
    // into the inner types before calling this instead.
    fn round_trip_inner<T>(t1: T)
    where
        T: NineP + PartialEq + fmt::Debug,
    {
        let buf = t1.write_9p_bytes().unwrap();
        let sb = SharedBuf::default();
        let t2 = T::read_from(&sb, &mut buf.as_slice()).unwrap();

        assert_eq!(t1, t2);
    }

    #[test_case(F9::U8(42); "u8_")]
    #[test_case(F9::U16(17); "u16_")]
    #[test_case(F9::U32(773); "u32_")]
    #[test_case(F9::U64(123456); "u64_")]
    #[test_case(F9::S("testing"); "single-byte char string")]
    #[test_case(F9::S("Hello, 世界"); "multi-byte char string")]
    #[test_case(F9::V(vec!["foo".to_string(), "bar".to_string()]); "vec String")]
    #[test_case(F9::D(vec![5, 6, 7, 8, u8::MAX]); "data")]
    #[test_case(F9::RawStat; "raw stat")]
    #[test_case(F9::Clunk; "clunk")]
    #[test_case(F9::Walk; "walk")]
    #[test_case(F9::Rwalk; "rwalk")]
    #[test]
    fn round_trip_is_fine(data: F9) {
        match data {
            F9::U8(t) => round_trip_inner(t),
            F9::U16(t) => round_trip_inner(t),
            F9::U32(t) => round_trip_inner(t),
            F9::U64(t) => round_trip_inner(t),
            F9::S(t) => round_trip_inner(t.to_string()),
            F9::V(t) => round_trip_inner(t),
            F9::D(t) => round_trip_inner(Data(t)),
            F9::RawStat => round_trip_inner(RawStat {
                size: 1,
                ty: 2,
                dev: 3,
                qid: Qid {
                    ty: 1,
                    version: 2,
                    path: 3,
                },
                mode: 4,
                atime: 5,
                mtime: 6,
                length: 7,
                name: "test name".to_string(),
                uid: "test uid".to_string(),
                gid: "test gid".to_string(),
                muid: "test muid".to_string(),
            }),
            F9::Clunk => round_trip_inner(Rmessage {
                tag: 0,
                content: Rdata::Clunk {},
            }),
            F9::Walk => round_trip_inner(Tmessage {
                tag: 0,
                content: Tdata::Walk {
                    fid: 0,
                    new_fid: 2,
                    wnames: vec!["bar".to_string()],
                },
            }),
            F9::Rwalk => round_trip_inner(Rmessage {
                tag: 0,
                content: Rdata::Walk {
                    wqids: vec![
                        Qid {
                            ty: 0,
                            version: 1,
                            path: 2,
                        },
                        Qid {
                            ty: 3,
                            version: 4,
                            path: 5,
                        },
                    ],
                },
            }),
        }
    }
}
