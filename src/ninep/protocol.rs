//! 9p protocol implementation
//!
//!   http://man.cat-v.org/plan_9/5/
use std::{
    fmt,
    io::{self, ErrorKind, Read, Write},
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

/// Something that can be encoded to and decoded 9p protocol messages.
///
/// From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
///   Each message consists of a sequence of bytes. Two-, four-, and eight-byte fields hold
///   unsigned integers represented in little-endian order (least significant byte first).
pub trait Format9p: Sized {
    /// Number of bytes required to encode
    fn n_bytes(&self) -> usize;

    /// Encode self as bytes for the 9p protocol and write to the given Writer
    fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()>;

    /// Decode self from 9p protocol bytes coming from the given Reader
    fn read_from<R: Read>(r: &mut R) -> io::Result<Self>;
}

// Unsigned integer types can all be treated the same way so we stamp them out using a macro.
// They are written and read in their little-endian byte form.
macro_rules! impl_u {
    ($($ty:ty),+) => {
        $(impl Format9p for $ty {
            fn n_bytes(&self) -> usize {
                size_of::<$ty>()
            }

            fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
                w.write_all(&self.to_le_bytes())
            }

            fn read_from<R: Read>(r: &mut R) -> io::Result<Self> {
                let mut buf = [0u8; size_of::<$ty>()];
                r.read_exact(&mut buf)?;

                Ok(<$ty>::from_le_bytes(buf))
            }
        })+
    };
}

impl_u!(u8, u16, u32, u64);

// [size: u16] [content as bytes...]
//
// From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
//   Data items of larger or variable lengths are represented by a two-byte field specifying
//   a count, n, followed by n bytes of data. Text strings are represented this way, with
//   the text itself stored as a UTF-8 encoded sequence of Unicode charac- ters (see utf(6)).
//
//   Text strings in 9P messages are not NUL- terminated: n counts the bytes of UTF-8 data,
//   which include no final zero byte.  The NUL character is illegal in all text strings
//   in 9P, and is therefore excluded from file names, user names, and so on.
impl Format9p for String {
    fn n_bytes(&self) -> usize {
        size_of::<u16>() + self.len()
    }

    fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
        let len = self.len();
        if len > MAX_SIZE_FIELD {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("string too long: max={MAX_SIZE_FIELD} len={len}"),
            ));
        }

        (len as u16).write_to(w)?;
        w.write_all(self.as_bytes())
    }

    fn read_from<R: Read>(r: &mut R) -> io::Result<Self> {
        let len = u16::read_from(r)? as usize;
        let mut s = String::with_capacity(len);
        r.take(len as u64).read_to_string(&mut s)?;
        let actual = s.len();

        if actual < len {
            return Err(io::Error::new(
                ErrorKind::UnexpectedEof,
                format!("unexpected end of string: wanted {len}, got {actual}"),
            ));
        }

        Ok(s)
    }
}

// [size: u16] [content as bytes...]
//
// From [INTRO(5)](http://man.cat-v.org/plan_9/5/intro):
//   Data items of larger or variable lengths are represented by a two-byte field specifying
//   a count, n, followed by n bytes of data.
impl<T: Format9p> Format9p for Vec<T> {
    fn n_bytes(&self) -> usize {
        size_of::<u16>() + self.iter().map(|t| t.n_bytes()).sum::<usize>()
    }

    fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
        let n_bytes = self.iter().map(|t| t.n_bytes()).sum::<usize>();
        if n_bytes > MAX_SIZE_FIELD {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("vec too long: max={MAX_SIZE_FIELD} len={n_bytes}"),
            ));
        }

        (n_bytes as u16).write_to(w)?;
        for t in self {
            t.write_to(w)?;
        }

        Ok(())
    }

    fn read_from<R: Read>(r: &mut R) -> io::Result<Self> {
        let n_bytes = u16::read_from(r)? as usize;
        let len = n_bytes / size_of::<T>();
        let mut buf = Vec::with_capacity(len);

        for _ in 0..len {
            buf.push(T::read_from(r)?);
        }

        // Unexpected EOF should be handled by T::read_from

        Ok(buf)
    }
}

/// A wrapper around a Vec<u8> for handling data fields in read/write messages
///
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
#[derive(Clone, PartialEq, Eq)]
pub struct Data(Vec<u8>);

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

impl Format9p for Data {
    fn n_bytes(&self) -> usize {
        size_of::<u32>() + self.0.len()
    }

    fn write_to<W: Write>(&self, w: &mut W) -> io::Result<()> {
        let n_bytes = self.0.len();
        if n_bytes > MAX_DATA_SIZE_FIELD {
            return Err(io::Error::new(
                ErrorKind::InvalidInput,
                format!("data field too long: max={MAX_DATA_SIZE_FIELD} len={n_bytes}"),
            ));
        }

        (n_bytes as u32).write_to(w)?;
        w.write_all(&self.0)
    }

    fn read_from<R: Read>(r: &mut R) -> io::Result<Self> {
        let len = u32::read_from(r)? as usize;
        if len > MAX_DATA_LEN {
            return Err(io::Error::new(
                ErrorKind::InvalidData,
                format!("data field too long: max={MAX_DATA_LEN} len={len}"),
            ));
        }

        let mut buf = Vec::with_capacity(len);
        r.take(len as u64).read_to_end(&mut buf)?;
        let actual = buf.len();

        if actual < len {
            return Err(io::Error::new(
                ErrorKind::UnexpectedEof,
                format!("unexpected end of data: wanted {len}, got {actual}"),
            ));
        }

        Ok(Data(buf))
    }
}

/// Taken from the enum in fcall.h in the plan9 source.
///   https://github.com/9fans/plan9port/blob/master/include/fcall.h#L80
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MessageType {
    Tversion = 100,
    Rversion = 101,

    Tauth = 102,
    Rauth = 103,

    Tattach = 104,
    Rattach = 105,

    // Terror = 106,
    Rerror = 107,

    Tflush = 108,
    Rflush = 109,

    Twalk = 110,
    Rwalk = 111,

    Topen = 112,
    Ropen = 113,

    Tcreate = 114,
    Rcreate = 115,

    Tread = 116,
    Rread = 117,

    Twrite = 118,
    Rwrite = 119,

    Tclunk = 120,
    Rclunk = 121,

    Tremove = 122,
    Rremove = 123,

    Tstat = 124,
    Rstat = 125,

    Twstat = 126,
    Rwstat = 127,
    // Tmax = 128,
    // Topenfd = 98,
    // Ropenfd = 99,
}

/// Helper for defining a struct that implements Format9p
macro_rules! impl_message_type {
    (
        $(#[$docs:meta])+
        struct $struct:ident {
            $(
                $(#[$field_docs:meta])*
                $field:ident: $ty:ty,
            )*
        }
    ) => {
        $(#[$docs])+
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $struct {
            $(
                $(#[$field_docs])*
                pub $field: $ty,
            )*
        }

        impl Format9p for $struct {
            fn n_bytes(&self) -> usize {
                #[allow(unused_mut)]
                let mut n = 0;
                $(n += self.$field.n_bytes();)*
                n
            }

            fn write_to<W: Write>(&self, _w: &mut W) -> io::Result<()> {
                $(self.$field.write_to(_w)?;)*
                Ok(())
            }

            fn read_from<R: Read>(_r: &mut R) -> io::Result<Self> {
                $(let $field = <$ty>::read_from(_r)?;)*
                Ok(Self { $($field,)* })
            }
        }
    };
}

/// Generate the Tmessage enum along with the wrapped T-message types
/// and their implementations of Format9p
macro_rules! impl_tmessages {
    ($(
        $(#[$docs:meta])+
        $enum_variant:ident => $struct:ident {
            $($field:ident: $ty:ty,)*
        }
    )+) => {
        /// T-message variants
        ///
        /// See the individual message structs for docs on the format and semantics of each variant.
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Tmessage {
            $( $(#[$docs])+ $enum_variant($struct), )+
        }

        $(impl_message_type!(
            $(#[$docs])+
            struct $struct {
                $($field: $ty,)*
            }
        );)+
    };
}

/// Generate the Rmessage enum along with the wrapped R-message types
/// and their implementations of Format9p
macro_rules! impl_rmessages {
    ($(
        $(#[$docs:meta])+
        $enum_variant:ident => $struct:ident {
            $($field:ident: $ty:ty,)*
        }
    )+) => {
        /// R-message variants
        ///
        /// See the individual message structs for docs on the format and semantics of each variant.
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum Rmessage {
            $( $(#[$docs])+ $enum_variant($struct), )+
        }

        $(impl_message_type!(
            $(#[$docs])+
            struct $struct {
                $($field: $ty,)*
            }
        );)+
    };
}

impl_message_type!(
    /// A machine-independent directory entry
    /// http://man.cat-v.org/plan_9/5/stat
    struct Stat {
        /// size[2]      total byte count of the following data
        size: u16,
        /// type[2]      for kernel use
        ty: u16,
        /// dev[4]       for kernel use
        dev: u32,
        /// qid.type[1]
        /// the type of the file (directory, etc.), repre- sented as a bit vector corresponding to the
        /// high 8 bits of the file's mode word.
        quid_ty: u8,
        /// qid.vers[4]  version number for given path
        quid_vers: u32,
        /// qid.path[8]  the file server's unique identification for the file
        quid_path: u64,
        /// mode[4]      permissions and flags
        mode: u32,
        /// atime[4]     last access time
        atime: u32,
        /// mtime[4]     last modification time
        mtime: u32,
        /// length[8]    length of file in bytes
        length: u64,
        /// name[ s ]    file name; must be / if the file is the root directory of the server
        name: String,
        /// uid[ s ]     owner name
        uid: String,
        /// gid[ s ]     group name
        gid: String,
        /// muid[ s ]    name of the user who last modified the file
        muid: String,
    }
);

impl_message_type!(
    /// A qid represents the server's unique identification for the file being accessed: two files
    /// on the same server hierarchy are the same if and only if their qids are the same.
    struct Qid {
        ty: u8,
        version: u32,
        path: u64,
    }
);

impl_tmessages! {
    /// http://man.cat-v.org/plan_9/5/version
    /// size[4] Tversion tag[2] | msize[4] version[s]
    Version => Tversion {
        msize: u32,
        version: String,
    }

    /// http://man.cat-v.org/plan_9/5/attach
    /// size[4] Tauth tag[2] | afid[4] uname[s] aname[s]
    Auth => Tauth {
        afid: u32,
        uname: String,
        aname: String,
    }

    /// http://man.cat-v.org/plan_9/5/attach
    /// size[4] Tattach tag[2] | fid[4] afid[4] uname[s] aname[s]
    Attach => Tattach {
        fid: u32,
        afid: u32,
        uname: String,
        aname: String,
    }

    /// http://man.cat-v.org/plan_9/5/flush
    /// size[4] Tflush tag[2] | oldtag[2]
    Flush => Tflush {
        old_tag: u16,
    }

    /// http://man.cat-v.org/plan_9/5/walk
    /// size[4] Twalk tag[2] | fid[4] newfid[4] nwname[2] nwname*(wname[s])
    Walk => Twalk {
        fid: u32,
        new_fid: u32,
        wnames: Vec<String>,
    }

    /// http://man.cat-v.org/plan_9/5/open
    /// size[4] Topen tag[2] | fid[4] mode[1]
    Open => Topen {
        fid: u32,
        mode: u8,
    }

    /// http://man.cat-v.org/plan_9/5/open
    /// size[4] Tcreate tag[2] | fid[4] name[s] perm[4] mode[1]
    Create => Tcreate {
        fid: u32,
        name: String,
        perm: u32,
        mode: u8,
    }

    /// http://man.cat-v.org/plan_9/5/read
    /// size[4] Tread tag[2] | fid[4] offset[8] count[4]
    Read => Tread {
        fid: u32,
        offset: u64,
        count: u32,
    }

    /// http://man.cat-v.org/plan_9/5/read
    /// size[4] Twrite tag[2] | fid[4] offset[8] count[4] data[count]
    Write => Twrite {
        fid: u32,
        offset: u64,
        data: Data,
    }

    /// http://man.cat-v.org/plan_9/5/clunk
    /// size[4] Tclunk tag[2] | fid[4]
    Clunk => Tclunk {
        fid: u32,
    }

    /// http://man.cat-v.org/plan_9/5/remove
    /// size[4] Tremove tag[2] | fid[4]
    Remove => Tremove {
        fid: u32,
    }

    /// http://man.cat-v.org/plan_9/5/stat
    /// size[4] Tstat tag[2] | fid[4]
    Stat => Tstat {
        fid: u32,
    }

    /// http://man.cat-v.org/plan_9/5/stat
    /// size[4] Twstat tag[2] | fid[4] stat[n]
    Wstat => Twstat {
        fid: u32,
        stat: Stat,
    }
}

impl_rmessages! {
    /// http://man.cat-v.org/plan_9/5/version
    /// size[4] Rversion tag[2] | msize[4] version[s]
    Version => Rversion {
        msize: u32,
        version: String,
    }

    /// http://man.cat-v.org/plan_9/5/attach
    /// size[4] Rauth tag[2] | aqid[13]
    Auth => Rauth {
        aqid: Qid,
    }

    /// http://man.cat-v.org/plan_9/5/attach
    /// size[4] Rattach tag[2] | aquid[13]
    Attach => Rattach {
        aqid: Qid,
    }

    /// http://man.cat-v.org/plan_9/5/flush
    /// size[4] Rflush tag[2]
    Flush => Rflush {}

    /// http://man.cat-v.org/plan_9/5/walk
    /// size[4] Rwalk tag[2] | nwqid[2] nwqid*(wqid[13])
    Walk => Rwalk {
        wquid: Vec<Qid>,
    }

    /// http://man.cat-v.org/plan_9/5/open
    /// size[4] Ropen tag[2] | qid[13] iounit[4]
    Open => Ropen {
        qid: Qid,
        iounit: u32,
    }

    /// http://man.cat-v.org/plan_9/5/open
    /// size[4] Rcreate tag[2] | qid[13] iounit[4]
    Create => Rcreate {
        qid: Qid,
        iounit: u32,
    }

    /// http://man.cat-v.org/plan_9/5/read
    /// size[4] Rread tag[2] | count[4] data[count]
    Read => Rread {
        data: Data,
    }

    /// http://man.cat-v.org/plan_9/5/read
    /// size[4] Rwrite tag[2] | count[4]
    Write => Rwrite {
        count: u32,
    }

    /// http://man.cat-v.org/plan_9/5/clunk
    /// size[4] Rclunk tag[2]
    Clunk => Rclunk {}

    /// http://man.cat-v.org/plan_9/5/remove
    /// size[4] Rremove tag[2]
    Remove => Rremove {}

    /// http://man.cat-v.org/plan_9/5/stat
    /// size[4] Rstat tag[2] | stat[n]
    Stat => Rstat {
        stat: Stat,
    }

    /// http://man.cat-v.org/plan_9/5/stat
    /// size[4] Rwstat tag[2]
    Wstat => Rwstat {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;
    use std::{cmp::PartialEq, io::Cursor};

    #[test]
    fn uint_n_bytes_is_correct() {
        assert_eq!(0u8.n_bytes(), 1, "u8");
        assert_eq!(0u16.n_bytes(), 2, "u16");
        assert_eq!(0u32.n_bytes(), 4, "u32");
        assert_eq!(0u64.n_bytes(), 8, "u64");
    }

    #[test_case("test", 2 + 4; "single byte chars only")]
    #[test_case("", 2; "empty string")]
    #[test_case("Hello, 世界", 2 + 7 + 3 + 3; "including multi-byte chars")]
    #[test]
    fn string_n_bytes_is_correct(s: &str, expected: usize) {
        assert_eq!(s.to_string().n_bytes(), expected);
    }

    #[test]
    fn uint_decode() {
        let buf: [u8; 8] = [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef];
        let mut cur = Cursor::new(&buf);

        assert_eq!(0x01, u8::read_from(&mut cur).unwrap());
        cur.set_position(0);
        assert_eq!(0x2301, u16::read_from(&mut cur).unwrap());
        cur.set_position(0);
        assert_eq!(0x67452301, u32::read_from(&mut cur).unwrap());
        cur.set_position(0);
        assert_eq!(0xefcdab8967452301, u64::read_from(&mut cur).unwrap());
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
        let mut buf: Vec<u8> = vec![];
        s.to_string().write_to(&mut buf).unwrap();
        assert_eq!(&buf, bytes);
    }

    enum F9 {
        U8(u8),
        U16(u16),
        U32(u32),
        U64(u64),
        S(&'static str),
        V(Vec<u32>),
        D(Vec<u8>),
    }

    // simple_test_case doesn't handle generic args for parameterised
    // tests so I'm wrapping things up in the above enum and destructuring
    // into the inner types before calling this instead.
    fn round_trip_inner<T>(t1: T)
    where
        T: Format9p + PartialEq + fmt::Debug,
    {
        let mut buf = Cursor::new(Vec::new());
        t1.write_to(&mut buf).unwrap();
        buf.set_position(0);

        let t2 = T::read_from(&mut buf).unwrap();

        assert_eq!(t1, t2);
    }

    #[test_case(F9::U8(42); "u8_")]
    #[test_case(F9::U16(17); "u16_")]
    #[test_case(F9::U32(773); "u32_")]
    #[test_case(F9::U64(123456); "u64_")]
    #[test_case(F9::S("testing"); "single-byte char string")]
    #[test_case(F9::S("Hello, 世界"); "multi-byte char string")]
    #[test_case(F9::V(vec![0, 1, 2, 3, u32::MAX]); "vec u32")]
    #[test_case(F9::D(vec![5, 6, 7, 8, u8::MAX]); "data")]
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
        }
    }
}
