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
#[derive(PartialEq, Eq)]
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
