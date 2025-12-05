use crate::{Edit, buffer::GapBuffer, dot::Dot, exec::Address, regex::Haystack};
use std::{
    borrow::Cow,
    cell::RefCell,
    io::{BufRead, BufReader, Read},
};

/// Initial length of the line buffer for when we read from the reader
const LINE_BUF_LEN: usize = 100;

/// A wrapper around a [Read] that buffers and caches the data read in order to support searching
/// with a regex.
#[derive(Debug)]
pub struct CachingStream<R>
where
    R: Read,
{
    inner: RefCell<Inner<R>>,
}

impl<R> CachingStream<R>
where
    R: Read,
{
    pub fn new(r: R) -> Self {
        Self {
            inner: RefCell::new(Inner {
                reader: BufReader::new(r),
                buf: String::with_capacity(LINE_BUF_LEN),
                gb: GapBuffer::from(""),
                closed: false,
                cleared_lines: 0,
                cleared_bytes: 0,
            }),
        }
    }

    /// Clear the inner gap buffer state up until the end of the line before the given offset,
    /// tracking the historic line and byte counts so we can correctly adjust future read offsets.
    ///
    /// Data before offset will no longer be accessible.
    pub fn clear_until(&self, offset: usize) {
        self.inner.borrow_mut().clear_until(offset);
    }

    fn is_closed(&self) -> bool {
        self.inner.borrow().closed
    }

    fn get_char_at(&self, byte_idx: usize) -> Option<char> {
        let inner = self.inner.borrow();
        inner.gb.get_char_at(byte_idx - inner.cleared_bytes)
    }

    fn try_read_next_line(&self) {
        self.inner.borrow_mut().try_read_next_line();
    }
}

impl<R> Haystack for CachingStream<R>
where
    R: Read,
{
    fn try_make_contiguous(&mut self) {}

    fn is_contiguous(&self) -> bool {
        false
    }

    fn len(&self) -> usize {
        usize::MAX
    }

    fn substr_from<'a>(&'a self, byte_offset: usize) -> Option<Cow<'a, str>> {
        let inner = self.inner.borrow();
        let s = Haystack::substr_from(&inner.gb, byte_offset)?.into_owned();

        Some(Cow::Owned(s))
    }

    fn substr<'a>(&'a self, byte_from: usize, byte_to: usize) -> Cow<'a, str> {
        let inner = self.inner.borrow();
        let s = inner.gb.substr(byte_from, byte_to).into_owned();

        Cow::Owned(s)
    }

    fn byte_to_char(&self, byte_idx: usize) -> Option<usize> {
        Haystack::byte_to_char(&self.inner.borrow().gb, byte_idx)
    }

    fn char_to_byte(&self, char_idx: usize) -> Option<usize> {
        Haystack::char_to_byte(&self.inner.borrow().gb, char_idx)
    }

    fn iter_from(&self, from: usize) -> Option<impl Iterator<Item = (usize, char)>> {
        if self.inner.borrow().closed {
            None
        } else {
            Some(CachingStreamIter {
                inner: self,
                from,
                to: usize::MAX,
            })
        }
    }

    fn iter_between(&self, from: usize, to: usize) -> impl Iterator<Item = (usize, char)> {
        CachingStreamIter {
            inner: self,
            from,
            to,
        }
    }

    fn rev_iter_between(&self, _from: usize, _to: usize) -> impl Iterator<Item = (usize, char)> {
        std::iter::empty()
    }
}

impl<R> Edit for CachingStream<R>
where
    R: Read,
{
    fn insert(&mut self, ix: usize, s: &str) {
        self.inner.borrow_mut().gb.insert(ix, s);
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.inner.borrow_mut().gb.remove_range(from, to);
    }
}

impl<R> Address for CachingStream<R>
where
    R: Read,
{
    fn current_dot(&self) -> Dot {
        Dot::from_char_indices(0, usize::MAX)
    }

    fn len_bytes(&self) -> usize {
        if self.is_closed() {
            self.inner.borrow().gb.len()
        } else {
            usize::MAX
        }
    }

    fn len_chars(&self) -> usize {
        if self.is_closed() {
            self.inner.borrow().gb.len_chars()
        } else {
            usize::MAX
        }
    }

    fn max_iter(&self) -> usize {
        if self.is_closed() {
            self.inner.borrow().gb.len_chars()
        } else {
            usize::MAX
        }
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        let cur_len = self.inner.borrow().gb.len_lines();

        if line_idx > cur_len {
            for _ in cur_len..=line_idx {
                self.try_read_next_line();
                if self.is_closed() {
                    break;
                }
            }
        }

        self.inner.borrow().gb.try_line_to_char(line_idx)
    }

    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        self.inner.borrow().gb.try_char_to_line(char_idx)
    }

    fn char_to_line_end(&self, char_idx: usize) -> Option<usize> {
        let gb = &self.inner.borrow().gb;
        let line_idx = gb.try_char_to_line(char_idx)?;
        match gb.try_line_to_char(line_idx + 1) {
            None => Some(gb.len_chars() - 1),
            Some(idx) => Some(idx),
        }
    }

    fn char_to_line_start(&self, char_idx: usize) -> Option<usize> {
        let gb = &self.inner.borrow().gb;
        let line_idx = gb.try_char_to_line(char_idx)?;
        Some(gb.line_to_char(line_idx))
    }
}

#[derive(Debug)]
struct Inner<R>
where
    R: Read,
{
    reader: BufReader<R>,
    buf: String,
    gb: GapBuffer,
    closed: bool,
    cleared_lines: usize,
    cleared_bytes: usize,
}

impl<R> Inner<R>
where
    R: Read,
{
    fn try_read_next_line(&mut self) {
        self.buf.clear();

        match self.reader.read_line(&mut self.buf) {
            Ok(n) => {
                let len = self.gb.len_chars();
                self.gb.insert_str(len, &self.buf);
                self.closed = n == 0;
            }
            Err(_) => self.closed = true,
        };
    }

    fn clear_until(&mut self, logical_offset: usize) {
        let offset = logical_offset - self.cleared_bytes;
        let cleared_lines = self.gb.lines_before_byte_offset(offset);
        let char_to = self.gb.byte_to_char(offset);
        self.gb.remove_range(0, char_to);

        self.cleared_bytes = logical_offset;
        self.cleared_lines += cleared_lines;
    }
}

#[derive(Debug)]
pub struct StreamSlice<'a, R>
where
    R: Read,
{
    inner: &'a RefCell<Inner<R>>,
    from: usize,
    to: usize,
}

impl<'a, R> StreamSlice<'a, R>
where
    R: Read,
{
    pub fn len(&self) -> usize {
        self.to - self.from
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<'a, R> From<StreamSlice<'a, R>> for Cow<'a, str>
where
    R: Read,
{
    fn from(s: StreamSlice<'a, R>) -> Self {
        let inner = s.inner.borrow();
        let to = if s.to == usize::MAX {
            inner.gb.len()
        } else {
            s.to - inner.cleared_bytes
        };

        let slice = inner
            .gb
            .slice_from_byte_offsets(s.from - inner.cleared_bytes, to);

        Cow::Owned(slice.to_string())
    }
}

#[derive(Debug)]
pub struct CachingStreamIter<'a, R>
where
    R: Read,
{
    pub(super) inner: &'a CachingStream<R>,
    pub(super) from: usize,
    pub(super) to: usize,
}

impl<R> Iterator for CachingStreamIter<'_, R>
where
    R: Read,
{
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // self.from == self.to - 1 is the last character so
        // we catch end of iteration on the subsequent call
        if self.from >= self.to {
            return None;
        }

        loop {
            match self.inner.get_char_at(self.from) {
                Some(ch) => {
                    let res = (self.from, ch);
                    self.from += ch.len_utf8();
                    return Some(res);
                }
                None if self.inner.is_closed() => return None,
                None => self.inner.try_read_next_line(),
            }
        }
    }
}

mod impl_structex {
    use super::*;
    use crate::regex::Regex;
    use std::{
        io::{self, Read},
        ops::Range,
    };
    use structex::re::{Haystack, RawCaptures, Sliceable, Writable};

    impl<R> Haystack<Regex> for CachingStream<R>
    where
        R: Read,
    {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.matches_between(self, from, to)
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let m = re.find_between(self, from, to)?;

            Some(RawCaptures::new(m.iter_locs()))
        }
    }

    impl<R> Sliceable for CachingStream<R>
    where
        R: Read,
    {
        type Slice<'h>
            = StreamSlice<'h, R>
        where
            Self: 'h;

        fn char_at(&self, byte_offset: usize) -> Option<char> {
            self.get_char_at(byte_offset)
        }

        fn slice(&self, range: Range<usize>) -> Self::Slice<'_> {
            StreamSlice {
                inner: &self.inner,
                from: range.start,
                to: range.end,
            }
        }

        fn max_len(&self) -> usize {
            usize::MAX
        }
    }

    impl<R> Writable for CachingStream<R>
    where
        R: Read,
    {
        fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
        where
            W: std::io::Write,
        {
            let inner = self.inner.borrow();
            let (l, r) = inner.gb.as_byte_slices();
            w.write_all(l)?;
            w.write_all(r)?;

            Ok(l.len() + r.len())
        }
    }

    impl<'h, R> Writable for StreamSlice<'h, R>
    where
        R: Read,
    {
        fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
        where
            W: std::io::Write,
        {
            let inner = self.inner.borrow();
            let to = if self.to == usize::MAX {
                inner.gb.len()
            } else {
                self.to - inner.cleared_bytes
            };

            let s = inner
                .gb
                .slice_from_byte_offsets(self.from - inner.cleared_bytes, to);
            let (l, r) = s.as_slices();

            w.write_all(l)?;
            w.write_all(r)?;

            Ok(l.len() + r.len())
        }
    }
}
