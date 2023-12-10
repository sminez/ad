//! Internal data structures and helpers for maintaining buffer state
//!
//! ### References
//! - https://www.cs.unm.edu/~crowley/papers/sds.pdf
//! - http://doc.cat-v.org/plan_9/4th_edition/papers/sam/
//! - https://www.averylaird.com/programming/piece-table/2018/05/10/insertions-piece-table
//! - https://www.staff.city.ac.uk/~ross/papers/FingerTree.pdf
//! - https://nullprogram.com/blog/2017/09/07/
//! - https://coredumped.dev/2023/08/09/text-showdown-gap-buffers-vs-ropes/
//! - https://code.visualstudio.com/blogs/2018/03/23/text-buffer-reimplementation
use std::cmp::{max, min, Ordering};

// The internal data is [u8] so the values here are in terms of bytes
const MIN_GAP: usize = 64;
const MAX_GAP: usize = 1024 * 8;

/// For a given buffer length, calculate the new size of the gap we need when reallocating.
/// This is set to 5% of the length of our data buffer but bounded by MIN_GAP and MAX_GAP.
#[inline]
fn clamp_gap_size(len: usize, min_gap: usize) -> usize {
    min(max(len / 20, min_gap), MAX_GAP)
}

// This is bit magic equivalent to: b < 128 || b >= 192
// -> taken from the impl of is_utf8_char_boundary in
//    https://doc.rust-lang.org/src/core/num/mod.rs.html
#[inline]
fn is_char_boundary(b: u8) -> bool {
    (b as i8) >= -0x40
}

#[inline]
fn count_chars(bytes: &[u8]) -> usize {
    if bytes.is_empty() {
        return 0;
    }

    debug_assert!(is_char_boundary(bytes[bytes.len() - 1]), "invalid utf8");
    let mut n_chars = 0;
    let mut cur = 0;

    while cur < bytes.len() {
        let ch = unsafe { decode_char_at(cur, bytes) };
        cur += ch.len_utf8();
        n_chars += 1;
    }

    n_chars
}

/// An implementation of a gap buffer that tracks internal meta-data to help with accessing
/// sub-regions of the text such as character ranges and lines.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GapBuffer {
    data: Box<[u8]>,
    cap: usize,
    gap_start: usize,
    gap_end: usize,
    next_gap: usize,
    meta: Meta,
}

impl From<String> for GapBuffer {
    fn from(s: String) -> Self {
        let gap_start = s.len();
        let cap = s.capacity();

        if cap == gap_start {
            return Self::from(s.as_str());
        }

        let meta = Meta::from(s.as_str());
        let mut v = s.into_bytes();
        v.resize(cap, 0);

        let mut gb = Self {
            data: v.into_boxed_slice(),
            meta,
            cap,
            gap_start,
            gap_end: cap,
            next_gap: clamp_gap_size(gap_start, MIN_GAP),
        };

        gb.move_gap_to(0);
        gb
    }
}

impl From<&str> for GapBuffer {
    fn from(s: &str) -> Self {
        let gap_start = s.len();
        let next_gap = clamp_gap_size(gap_start, MIN_GAP);
        let cap = gap_start + next_gap;
        let mut v = Vec::with_capacity(cap);
        let meta = Meta::from(s);
        v.extend_from_slice(s.as_bytes());
        v.resize(cap, 0);

        let mut gb = Self {
            data: v.into_boxed_slice(),
            meta,
            cap,
            gap_start,
            gap_end: cap,
            next_gap,
        };

        gb.move_gap_to(0);
        gb
    }
}

impl ToString for GapBuffer {
    fn to_string(&self) -> String {
        if self.is_empty() {
            return String::new();
        }

        let mut v = Vec::with_capacity(self.len());
        v.extend(&self.data[..self.gap_start]);
        v.extend(&self.data[self.gap_end..]);

        String::from_utf8(v).expect("valid utf8")
    }
}

impl GapBuffer {
    /// Number of bytes in the gap
    #[inline]
    fn gap(&self) -> usize {
        self.gap_end - self.gap_start
    }

    /// The current length of "active" data in the buffer (i.e. not including the gap)
    #[inline]
    pub fn len(&self) -> usize {
        self.cap - self.gap()
    }

    /// Whether or not the visible buffer contents are empty or not.
    /// This can return true while there is "deleted" data in the gap.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.cap == self.gap()
    }

    /// The raw content of the buffer
    pub fn bytes(&self) -> Vec<u8> {
        let mut v = Vec::with_capacity(self.len());
        v.extend(&self.data[..self.gap_start]);
        v.extend(&self.data[self.gap_end..]);
        v
    }

    pub fn iter_lines(&self) -> impl Iterator<Item = Slice> {
        let mut line_idx = 0;

        std::iter::from_fn(move || {
            if line_idx == self.len_lines() {
                return None;
            }
            let slice = self.line(line_idx);
            line_idx += 1;
            Some(slice)
        })
    }

    #[inline]
    pub fn len_lines(&self) -> usize {
        self.meta.lines.len()
    }

    #[inline]
    pub fn len_chars(&self) -> usize {
        self.meta.n_chars
    }

    pub fn clear(&mut self) {
        self.move_gap_to(0);
        self.gap_end = self.cap;
        self.meta.lines = Default::default();
    }

    #[inline]
    pub fn char(&self, char_idx: usize) -> char {
        let byte_idx = self.char_to_byte(char_idx);

        // SAFTEY: we know that we have valid utf8 data internally
        unsafe { decode_char_at(byte_idx, &self.data) }
    }

    #[inline]
    fn char_len(&self, byte_idx: usize) -> usize {
        // SAFTEY: we know that we have valid utf8 data internally
        unsafe { decode_char_at(byte_idx, &self.data) }.len_utf8()
    }

    pub fn line(&self, line_idx: usize) -> Slice {
        self.meta.lines[line_idx].as_slice(self)
    }

    pub fn slice(&self, char_from: usize, char_to: usize) -> Slice {
        let from = self.char_to_raw_byte(char_from);
        let to = self.char_to_raw_byte(char_to);

        // NOTE: this is subtly different from LineMeta::as_slice as we have absolute
        // indicies into the data buffer rather than ones that are unaware of the gap
        if to <= self.gap_start || from >= self.gap_end {
            return Slice {
                left: &self.data[from..to],
                right: &[],
            };
        }

        Slice {
            left: &self.data[from..self.gap_start],
            right: &self.data[self.gap_end..to],
        }
    }

    pub fn byte_to_char(&self, byte_idx: usize) -> usize {
        let line = self.byte_to_line(byte_idx);
        let mut char_idx = self.meta.lines.iter().take(line).map(|lm| lm.n_chars).sum();
        char_idx += count_chars(&self.data[self.meta.lines[line].offset..=byte_idx]);

        char_idx
    }

    pub fn char_to_line(&self, char_idx: usize) -> usize {
        match self.try_char_to_line(char_idx) {
            Some(line_idx) => line_idx,
            None => panic!("out of bounds: {char_idx} > {}", self.len()),
        }
    }

    pub fn try_char_to_line(&self, char_idx: usize) -> Option<usize> {
        let mut n = 0;

        for (i, ls) in self.meta.lines.iter().enumerate() {
            let next_boundary = n + ls.n_chars;
            if char_idx <= next_boundary {
                return Some(i);
            }
            n = next_boundary;
        }

        None
    }

    pub fn line_to_char(&self, line_idx: usize) -> usize {
        match self.try_line_to_char(line_idx) {
            Some(char_idx) => char_idx,
            None => panic!("out of bounds: {line_idx} > {}", self.len_lines()),
        }
    }

    pub fn try_line_to_char(&self, line_idx: usize) -> Option<usize> {
        if line_idx > self.len_lines() - 1 {
            return None;
        }

        Some(
            self.meta
                .lines
                .iter()
                .take(line_idx)
                .map(|lm| lm.n_chars)
                .sum(),
        )
    }

    /// Insert a single character at the specifified byte index.
    ///
    /// This is O(1) if idx is at the current gap start and the gap is large enough to accomodate
    /// the new text, otherwise data will need to be copied in order to relocate the gap.
    pub fn insert_char(&mut self, char_idx: usize, ch: char) {
        let len = ch.len_utf8();
        if len >= self.gap() {
            self.grow_gap(len);
        }

        let idx = self.char_to_byte(char_idx);
        self.move_gap_to(idx);

        ch.encode_utf8(&mut self.data[self.gap_start..]);
        self.gap_start += len;
        self.meta.n_chars += 1;

        let ls = self.meta.gap_line_mut();
        ls.n_chars += 1;
        ls.n_bytes += len;
        if idx < ls.offset {
            ls.offset = idx;
        }

        if ch == '\n' {
            self.meta.mark_newline(idx, &self.data);
        }
    }

    /// Insert a string at the specifified byte index.
    ///
    /// This is O(1) if idx is at the current gap start and the gap is large enough to accomodate
    /// the new text, otherwise data will need to be copied in order to relocate the gap.
    pub fn insert_str(&mut self, char_idx: usize, s: &str) {
        let len = s.len();
        if len >= self.gap() {
            self.grow_gap(len);
        }

        let idx = self.char_to_byte(char_idx);
        self.move_gap_to(idx);

        self.data[self.gap_start..self.gap_start + len].copy_from_slice(s.as_bytes());
        self.gap_start += len;
        self.meta.n_chars += s.chars().count();

        let ls = self.meta.gap_line_mut();
        ls.n_chars += s.chars().count();
        ls.n_bytes += len;
        if idx < ls.offset {
            ls.offset = idx;
        }

        for (i, b) in s.bytes().enumerate() {
            if b == b'\n' {
                self.meta.mark_newline(idx + i, &self.data);
            }
        }
    }

    /// Remove the requested character index from the visible region of the buffer
    pub fn remove_char(&mut self, char_idx: usize) {
        let idx = self.char_to_byte(char_idx);
        let len = self.char_len(idx);

        if idx == self.gap_start {
            self.gap_end += len;
        } else {
            self.move_gap_to(idx);
            self.gap_end += len;
        }

        self.meta.n_chars -= 1;

        let ls = self.meta.gap_line_mut();
        ls.n_chars -= 1;
        ls.n_bytes -= len;

        if self.data[idx] == b'\n' && !self.meta.gap_line_is_last_line() {
            let LineMeta {
                n_chars, n_bytes, ..
            } = self.meta.lines.remove(self.meta.gap_line + 1);

            let ls = self.meta.gap_line_mut();
            ls.n_bytes += n_bytes;
            ls.n_chars += n_chars;
        }

        debug_assert_eq!(
            self.meta.lines.iter().map(|l| l.n_bytes).sum::<usize>(),
            self.len(),
            "n bytes is wrong"
        );

        if self.meta.lines.is_empty() {
            self.meta.lines.push(LineMeta::default());
        }
    }

    /// Remove the requested range (from..to) from the visible region of the buffer
    pub fn remove_range(&mut self, char_from: usize, char_to: usize) {
        if char_from == char_to {
            return;
        }

        assert!(
            char_from < char_to,
            "invalid range: from={char_from} > to={char_to}"
        );

        let from = self.char_to_byte(char_from);
        let to = self.char_to_byte(char_to);
        debug_assert!(from < to, "invalid byte range: from={from} > to={to}");
        self.move_gap_to(from);

        let mut n_bytes = to - from;
        let mut n_chars = count_chars(&self.data[from..to]);

        self.gap_end += n_bytes;
        self.meta.n_chars -= n_chars;

        match (
            to.cmp(&self.meta.gap_line_end()),
            self.meta.gap_line_is_last_line(),
        ) {
            (Ordering::Less, _) | (Ordering::Equal, true) => {
                let ls = self.meta.gap_line_mut();
                ls.n_chars -= n_chars;
                ls.n_bytes -= n_bytes;
            }

            (Ordering::Equal, false) => {
                let ls = self.meta.gap_line_mut();
                ls.n_chars -= n_chars;
                ls.n_bytes -= n_bytes;

                let idx = self.meta.gap_line + 1;
                let next_ls = self.meta.lines.remove(idx);
                let ls = self.meta.gap_line_mut();
                ls.n_bytes += next_ls.n_bytes;
                ls.n_chars += next_ls.n_chars;
            }

            (Ordering::Greater, false) => {
                while n_bytes > 0 {
                    let offset = self.meta.gap_line_offset();
                    let end = self.meta.gap_line_end();

                    if to > end && from > offset {
                        let ls = self.meta.gap_line_mut();
                        n_bytes -= end - from;
                        ls.n_bytes = from - ls.offset;
                        ls.n_chars = count_chars(&self.data[from..end]);
                        n_chars -= ls.n_chars;
                        self.meta.gap_line += 1;
                    } else if from < offset && to > end {
                        let ls = self.meta.lines.remove(self.meta.gap_line);
                        n_bytes -= ls.n_bytes;
                        n_chars -= ls.n_chars;
                    } else {
                        let ls_last = self.meta.lines.remove(self.meta.gap_line);
                        self.meta.gap_line -= 1;
                        let ls = self.meta.gap_line_mut();
                        ls.n_bytes += ls_last.n_bytes - n_bytes;
                        ls.n_chars += ls_last.n_chars - n_chars;
                        break;
                    }
                }
            }

            (Ordering::Greater, true) => panic!("remove_range out of bounds"),
        }

        debug_assert_eq!(
            self.meta.lines.iter().map(|l| l.n_bytes).sum::<usize>(),
            self.len(),
            "n bytes is wrong"
        );

        if self.meta.lines.is_empty() {
            self.meta.lines.push(LineMeta::default());
        }
    }

    fn grow_gap(&mut self, n: usize) {
        if n >= self.next_gap {
            self.next_gap = clamp_gap_size(self.len() + n, n.next_power_of_two());
        }

        let gap_increase = self.next_gap + n - self.gap();
        let cap = self.cap + self.next_gap + n;
        let mut buf = Vec::with_capacity(cap);

        buf.extend_from_slice(&self.data[..self.gap_start]); // data to gap
        buf.resize(buf.len() + self.next_gap + n, 0); // the new gap (zeroed)
        buf.extend_from_slice(&self.data[self.gap_end..]); // data after gap

        for ls in self.meta.lines.iter_mut() {
            if ls.offset > self.gap_start {
                ls.offset += gap_increase;
            }
        }

        self.next_gap = clamp_gap_size(self.len(), self.next_gap * 2);
        self.data = buf.into_boxed_slice();
        self.gap_end += gap_increase;
        self.cap = cap;
    }

    /// The byte_idx passed here must be an absolute position within the data buffer.
    fn move_gap_to(&mut self, byte_idx: usize) {
        // we need space to fit the current gap size
        assert!(
            byte_idx <= self.len(),
            "index out of bounds: {byte_idx} > {}",
            self.len()
        );

        let gap = self.gap();

        let (src, dest) = match byte_idx.cmp(&self.gap_start) {
            Ordering::Equal => return,

            // Gap moving left
            Ordering::Less => {
                for ls in self.meta.lines.iter_mut() {
                    if ls.offset >= byte_idx && ls.offset <= self.gap_start {
                        ls.offset += gap;
                    }
                }

                (byte_idx..self.gap_start, byte_idx + gap)
            }

            // Gap moving right
            Ordering::Greater => {
                for ls in self.meta.lines.iter_mut() {
                    if ls.offset >= self.gap_end && ls.offset < byte_idx + gap {
                        ls.offset -= gap;
                    }
                }

                (self.gap_end..byte_idx + gap, self.gap_start)
            }
        };

        self.meta.gap_line = self.byte_to_line(byte_idx);
        self.data.copy_within(src, dest);
        self.gap_end = byte_idx + gap;
        self.gap_start = byte_idx;
    }

    fn byte_to_line(&self, byte_idx: usize) -> usize {
        let mut n = 0;
        for (i, ls) in self.meta.lines.iter().enumerate() {
            n += ls.n_bytes;
            if byte_idx <= n {
                return i;
            }
        }

        panic!("out of bounds: {byte_idx} > {}", self.len());
    }

    fn char_to_byte(&self, char_idx: usize) -> usize {
        let find = |bytes: &[u8], offset: usize, count: &mut usize| {
            let mut idx = 0;
            while idx < bytes.len() {
                if *count == 0 {
                    return if offset > self.gap_start {
                        Some(offset - self.gap_end + idx)
                    } else {
                        Some(offset + idx)
                    };
                }
                idx += unsafe { decode_char_at(idx, bytes) }.len_utf8();
                *count -= 1;
            }

            None
        };

        self.char_to_byte_impl(char_idx, find, |lm| {
            let end = lm.end();
            if end > self.gap_end {
                end - self.gap_end
            } else {
                end
            }
        })
    }

    fn char_to_raw_byte(&self, char_idx: usize) -> usize {
        let find = |bytes: &[u8], offset: usize, count: &mut usize| {
            let mut idx = 0;
            while idx < bytes.len() {
                if *count == 0 {
                    return Some(offset + idx);
                }
                idx += unsafe { decode_char_at(idx, bytes) }.len_utf8();
                *count -= 1;
            }

            None
        };

        self.char_to_byte_impl(char_idx, find, |lm| lm.end())
    }

    fn char_to_byte_impl<F, G>(&self, char_idx: usize, mut find: F, count_0: G) -> usize
    where
        F: FnMut(&[u8], usize, &mut usize) -> Option<usize>,
        G: Fn(&LineMeta) -> usize,
    {
        let mut n = 0;

        for lm in self.meta.lines.iter() {
            let next_boundary = n + lm.n_chars;
            if char_idx > next_boundary {
                n = next_boundary;
                continue;
            }

            let Slice { left, right, .. } = lm.as_slice(self);
            let mut count = char_idx - n;
            match find(left, lm.offset, &mut count) {
                Some(byte_idx) => return byte_idx,
                None => match find(right, lm.offset, &mut count) {
                    Some(byte_idx) => return byte_idx + left.len(),
                    None if count == 0 => return count_0(lm),
                    _ => break,
                },
            }
        }

        panic!("out of bounds: {char_idx} > {}", self.len());
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct Meta {
    lines: Vec<LineMeta>,
    n_chars: usize,
    gap_line: usize,
}

impl From<&str> for Meta {
    fn from(s: &str) -> Self {
        let mut lines = Vec::new();
        let mut lm = LineMeta::default();
        let mut offset = 0;

        for ch in s.chars() {
            lm.n_bytes += ch.len_utf8();
            lm.n_chars += 1;
            offset += 1;

            if ch == '\n' {
                lines.push(lm);
                lm = LineMeta {
                    offset,
                    ..Default::default()
                };
            }
        }

        if lm.n_chars > 0 {
            lines.push(lm);
        }
        if lines.is_empty() {
            lines.push(LineMeta::default());
        }

        Self {
            lines,
            n_chars: offset,
            gap_line: 0,
        }
    }
}

impl Meta {
    fn gap_line_mut(&mut self) -> &mut LineMeta {
        &mut self.lines[self.gap_line]
    }

    #[inline]
    fn gap_line_offset(&self) -> usize {
        self.lines[self.gap_line].offset
    }

    #[inline]
    fn gap_line_end(&self) -> usize {
        self.lines[self.gap_line].end()
    }

    #[inline]
    fn gap_line_is_last_line(&self) -> bool {
        self.gap_line == self.lines.len() - 1
    }

    fn mark_newline(&mut self, idx: usize, data: &[u8]) {
        let LineMeta {
            offset,
            n_chars,
            n_bytes,
        } = self.lines[self.gap_line];

        let new_n_bytes = idx - offset + 1;
        let new_n_chars = count_chars(&data[offset..idx + 1]);
        let ls = self.gap_line_mut();
        ls.n_bytes = new_n_bytes;
        ls.n_chars = new_n_chars;
        self.gap_line += 1;
        self.lines.insert(
            self.gap_line,
            LineMeta::new(idx + 1, n_chars - new_n_chars, n_bytes - new_n_bytes),
        );
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
struct LineMeta {
    offset: usize,
    n_chars: usize,
    n_bytes: usize,
}

impl LineMeta {
    fn new(offset: usize, n_chars: usize, n_bytes: usize) -> Self {
        Self {
            offset,
            n_chars,
            n_bytes,
        }
    }

    #[inline]
    fn end(&self) -> usize {
        self.offset + self.n_bytes
    }

    #[inline]
    fn as_slice<'a>(&self, gb: &'a GapBuffer) -> Slice<'a> {
        let end = self.offset + self.n_bytes;
        if end <= gb.gap_start || self.offset >= gb.gap_end {
            return Slice {
                left: &gb.data[self.offset..end],
                right: &[],
            };
        }

        debug_assert!(self.offset <= gb.gap_start, "line offset sits in gap");

        Slice {
            left: &gb.data[self.offset..gb.gap_start],
            right: &gb.data[gb.gap_end..end + gb.gap()],
        }
    }
}

/// A view on a region of the GapBuffer.
///
/// Slices will become invalidated if the gap is moved from the position they were created with
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Slice<'a> {
    left: &'a [u8],
    right: &'a [u8],
}

impl<'a> Slice<'a> {
    pub fn as_strs(&self) -> (&str, &str) {
        // SAFTEY: we know that we have valid utf8 data internally
        unsafe {
            (
                std::str::from_utf8_unchecked(self.left),
                std::str::from_utf8_unchecked(self.right),
            )
        }
    }

    pub fn chars(self) -> Chars<'a> {
        Chars { s: self, cur: 0 }
    }

    pub fn indexed_chars(self, rev: bool) -> IdxChars<'a> {
        let (cur, idx) = if rev {
            (
                self.left.len() + self.right.len(),
                count_chars(self.left) + count_chars(self.right),
            )
        } else {
            (0, 0)
        };

        IdxChars {
            s: self,
            cur,
            idx,
            rev,
        }
    }

    fn cur_and_data(&self, cur: usize) -> (usize, &[u8]) {
        if cur <= self.left.len() {
            (cur, self.left)
        } else {
            (cur - self.left.len(), self.right)
        }
    }
}

impl<'a> ToString for Slice<'a> {
    fn to_string(&self) -> String {
        let mut v = Vec::with_capacity(self.left.len() + self.right.len());
        v.extend_from_slice(self.left);
        v.extend_from_slice(self.right);

        String::from_utf8(v).expect("valid utf8")
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Chars<'a> {
    s: Slice<'a>,
    cur: usize,
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur >= self.s.left.len() + self.s.right.len() {
            return None;
        }

        let (cur, data) = self.s.cur_and_data(self.cur);
        let ch = unsafe { decode_char_at(cur, data) };
        let len = ch.len_utf8();
        self.cur += len;
        Some(ch)
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct IdxChars<'a> {
    s: Slice<'a>,
    cur: usize,
    idx: usize,
    rev: bool,
}

impl<'a> Iterator for IdxChars<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        if (!self.rev && self.cur >= self.s.left.len() + self.s.right.len())
            || (self.rev && self.cur == 0)
        {
            return None;
        }

        if self.rev {
            let (cur, data) = self.s.cur_and_data(self.cur - 1);
            let ch = unsafe { decode_char_ending_at(cur, data) };
            let len = ch.len_utf8();
            self.idx -= 1;
            self.cur -= len;
            Some((self.idx, ch))
        } else {
            let (cur, data) = self.s.cur_and_data(self.cur);
            let ch = unsafe { decode_char_at(cur, data) };
            let len = ch.len_utf8();
            let res = Some((self.idx, ch));
            self.cur += len;
            self.idx += 1;
            res
        }
    }
}

// The following helper functions are adapted from nightly APIs in std::core::str
// -> https://doc.rust-lang.org/stable/src/core/str/validations.rs.html

/// Mask of the value bits of a continuation byte.
const CONT_MASK: u8 = 0b0011_1111;

/// Returns the initial codepoint accumulator for the first byte.
/// The first byte is special, only want bottom 5 bits for width 2, 4 bits
/// for width 3, and 3 bits for width 4.
#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Checks whether the byte is a UTF-8 continuation byte (i.e., starts with the
/// bits `10`).
#[inline]
const fn utf8_is_cont_byte(byte: u8) -> bool {
    (byte as i8) < -64
}

// FIXME:
//  - The fact that tests are failing with line offsets being left inside
//    of the gap is an indication that _something_ is messing up with the
//    tracking of line endings but I'm not sure where it is :(

/// Decode a utf-8 code point from `bytes` starting at `start`.
/// `bytes` must contain valid utf-8 data beginning at `start`
#[inline]
unsafe fn decode_char_at(start: usize, bytes: &[u8]) -> char {
    // Decode UTF-8
    // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
    let x = bytes[start];
    if x < 128 {
        return char::from_u32_unchecked(x as u32);
    }

    // Multibyte case follows
    // Decode from a byte combination out of: [[[x y] z] w]
    // NOTE: Performance is sensitive to the exact formulation here
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
    let y = bytes[start + 1];
    let mut ch = utf8_acc_cont_byte(init, y);

    if x >= 0xE0 {
        // [[x y z] w] case
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid
        // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
        let z = bytes[start + 2];
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case
            // use only the lower 3 bits of `init`
            // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
            let w = bytes[start + 3];
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    char::from_u32_unchecked(ch)
}

/// Decode a utf-8 code point from `bytes` ending at `end`.
/// `bytes` must contain valid utf-8 data ending at `end`
#[inline]
unsafe fn decode_char_ending_at(end: usize, bytes: &[u8]) -> char {
    // Decode UTF-8
    let w = match bytes[end] {
        b if b < 128 => return char::from_u32_unchecked(b as u32),
        b => b,
    };

    // Multibyte case follows
    // Decode from a byte combination out of: [x [y [z w]]]
    let mut ch;
    // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
    let z = bytes[end - 1];
    ch = utf8_first_byte(z, 2);
    if utf8_is_cont_byte(z) {
        // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
        let y = bytes[end - 2];
        ch = utf8_first_byte(y, 3);
        if utf8_is_cont_byte(y) {
            // SAFETY: `bytes` contains UTF-8-like string data so we have the next byte,
            let x = bytes[end - 3];
            ch = utf8_first_byte(x, 4);
            ch = utf8_acc_cont_byte(ch, y);
        }
        ch = utf8_acc_cont_byte(ch, z);
    }
    ch = utf8_acc_cont_byte(ch, w);

    char::from_u32_unchecked(ch)
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    fn debug_buffer_content(gb: &GapBuffer) -> String {
        let mut v = gb.data.to_vec();
        v[gb.gap_start..gb.gap_end].copy_from_slice("_".repeat(gb.gap()).as_bytes());
        String::from_utf8(v).expect("valid utf8")
    }

    fn raw_debug_buffer_content(gb: &GapBuffer) -> String {
        let mut v = gb.data.to_vec();
        for b in v[gb.gap_start..gb.gap_end].iter_mut() {
            if *b == b'\0' {
                *b = b'_';
            }
        }
        v.insert(gb.gap_end, b']');
        v.insert(gb.gap_start, b'[');

        String::from_utf8(v).expect("valid utf8")
    }

    #[test]
    fn to_string() {
        let s = "this is a test";
        let gb = GapBuffer::from(s.to_string());
        assert_eq!(gb.to_string(), s);
    }

    fn lm(offset: usize, n_chars: usize, n_bytes: usize) -> LineMeta {
        LineMeta {
            offset,
            n_chars,
            n_bytes,
        }
    }

    #[test_case(0, &[lm(64, 14, 14), lm(64+14, 12, 12)]; "BOF")]
    #[test_case(26, &[lm(0, 14, 14), lm(14, 12, 12)]; "EOF")]
    #[test]
    fn move_gap_to_cur_maintains_line_offsets(cur: usize, expected: &[LineMeta]) {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(s.len(), 26, "EOF case is not 0..s.len()");
        gb.move_gap_to(cur);
        assert_eq!(&gb.meta.lines, expected);
    }

    #[test]
    fn move_gap_to_maintains_line_content() {
        let s = "hello, world!\nhow are you?\nthis is a test";
        assert_eq!(s.len(), 41, "EOF case is not 0..s.len()");
        let mut gb = GapBuffer::from(s);

        for idx in 0..=s.len() {
            gb.move_gap_to(idx);
            assert_eq!(gb.len_lines(), 3);

            assert_eq!(gb.line(0).to_string(), "hello, world!\n", "idx={idx}");
            assert_eq!(gb.line(1).to_string(), "how are you?\n", "idx={idx}");
            assert_eq!(gb.line(2).to_string(), "this is a test", "idx={idx}");
        }
    }

    #[test_case(0, 0, 0; "BOF cur at BOF")]
    #[test_case(27, 0, 0; "BOF cur at EOF")]
    #[test_case(27, 5, 5; "in the buffer cur at EOF")]
    #[test_case(5, 5, 5; "in the buffer cur at gap")]
    #[test_case(5, 3, 3; "in the buffer cur before gap")]
    #[test_case(5, 11, 15; "in the buffer cur after gap")]
    #[test_case(5, 7, 7; "multi byte 1")]
    #[test_case(5, 8, 10; "multi byte 2")]
    #[test]
    fn char_to_byte_works(cur: usize, char_idx: usize, expected: usize) {
        let s = "hello, 世界!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(s.len(), 27, "EOF case is not 0..s.len()");
        assert_eq!("世".len(), 3);
        gb.move_gap_to(cur);

        let char_idx = gb.char_to_byte(char_idx);
        assert_eq!(char_idx, expected, "{:?}", debug_buffer_content(&gb));
    }

    #[test_case(0, 0, "hello, world!\n"; "first line cur at BOF")]
    #[test_case(0, 1, "how are you?"; "second line cur at BOF")]
    #[test_case(26, 0, "hello, world!\n"; "first line cur at EOF")]
    #[test_case(26, 1, "how are you?"; "second line cur at EOF")]
    #[test_case(10, 0, "hello, world!\n"; "first line cur in line")]
    #[test_case(10, 1, "how are you?"; "second line cur in line")]
    #[test]
    fn line_stat_bytes_works(cur: usize, line: usize, expected: &str) {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");
        gb.move_gap_to(cur);

        assert_eq!(gb.line(line).to_string(), expected);
    }

    #[test_case(&[(0, 'h')], "hello world"; "insert front")]
    #[test_case(&[(4, ',')], "ello, world"; "insert inner")]
    #[test_case(&[(10, '!')], "ello world!"; "insert back")]
    #[test_case(&[(4, ','), (11, '!')], "ello, world!"; "insert inner then back")]
    #[test_case(&[(4, ','), (0, 'h')], "hello, world"; "insert inner then front")]
    #[test_case(&[(0, 'h'), (5, ','),], "hello, world"; "insert front then inner")]
    #[test_case(&[(10, '!'), (0, 'h'), (5, ',')], "hello, world!"; "insert all")]
    #[test]
    fn insert_char(inserts: &[(usize, char)], expected: &str) {
        let mut gb = GapBuffer::from("ello world");

        for &(idx, ch) in inserts {
            gb.insert_char(idx, ch);
        }

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
    }

    #[test]
    fn insert_newline_char_is_tracked_correctly() {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(gb.len_lines(), 2);

        println!("initial: {:?}", raw_debug_buffer_content(&gb));
        gb.insert_char(6, '\n');
        println!("insert:  {:?}", raw_debug_buffer_content(&gb));

        assert_eq!(gb.len_lines(), 3);
        assert_eq!(gb.line(0).to_string(), "hello,\n");
        assert_eq!(gb.line(1).to_string(), " world!\n");
        assert_eq!(gb.line(2).to_string(), "how are you?");

        for idx in 0..=gb.len_chars() {
            gb.move_gap_to(idx);
            assert_eq!(gb.len_lines(), 3);

            assert_eq!(gb.line(0).to_string(), "hello,\n", "idx={idx}");
            assert_eq!(gb.line(1).to_string(), " world!\n", "idx={idx}");
            assert_eq!(gb.line(2).to_string(), "how are you?", "idx={idx}");
        }
    }

    #[test_case(&[(0, "hell")], "helloworl"; "insert front")]
    #[test_case(&[(1, ", ")], "o, worl"; "insert inner")]
    #[test_case(&[(5, "d!")], "oworld!"; "insert back")]
    #[test_case(&[(5, "d!"), (0, "hell"), (5, ", ")], "hello, world!"; "insert all")]
    #[test_case(&[(5, "d!"), (0, "hell"), (5, ",\n")], "hello,\nworld!"; "insert all w newline")]
    #[test]
    fn insert_str(inserts: &[(usize, &str)], expected: &str) {
        let mut gb = GapBuffer::from("oworl");
        for &(idx, s) in inserts {
            gb.insert_str(idx, s);
        }

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
    }

    #[test]
    fn insert_newline_in_str_is_tracked_correctly() {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(gb.len_lines(), 2);

        let s2 = " sailor\nisn't this fun?\nwhat a wonderful\n";
        gb.insert_str(6, s2);

        for idx in 0..=gb.len_chars() {
            gb.move_gap_to(idx);
            assert_eq!(gb.len_lines(), 5);

            assert_eq!(gb.line(0).to_string(), "hello, sailor\n", "idx={idx}");
            assert_eq!(gb.line(1).to_string(), "isn't this fun?\n", "idx={idx}");
            assert_eq!(gb.line(2).to_string(), "what a wonderful\n", "idx={idx}");
            assert_eq!(gb.line(3).to_string(), " world!\n", "idx={idx}");
            assert_eq!(gb.line(4).to_string(), "how are you?", "idx={idx}");
        }
    }

    #[test_case(6, "hello,world!"; "at gap start")]
    #[test_case(7, "hello, orld!"; "at gap end")]
    #[test_case(12, "hello, world"; "after gap")]
    #[test_case(0, "ello, world!"; "before gap")]
    #[test]
    fn remove_char(idx: usize, expected: &str) {
        let mut gb = GapBuffer::from("hello, world!");
        gb.move_gap_to(6); // space before world
        gb.remove_char(idx);

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
    }

    #[test]
    fn remove_newline_char_is_tracked_correctly() {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(gb.len_lines(), 2);

        gb.remove_char(13);

        assert_eq!(gb.len_lines(), 1);
        assert_eq!(gb.line(0).to_string(), "hello, world!how are you?");
    }

    #[test_case(6, 9, "hello,rld!"; "at gap start")]
    #[test_case(7, 10, "hello, ld!"; "at gap end")]
    #[test_case(10, 13, "hello, wor"; "after gap")]
    #[test_case(0, 5, ", world!"; "before gap")]
    #[test_case(0, 13, ""; "remove all")]
    #[test]
    fn remove_str(from: usize, to: usize, expected: &str) {
        let s = "hello, world!";
        assert_eq!(s.len(), 13, "remove all case is not 0..s.len()");

        let mut gb = GapBuffer::from(s);
        gb.move_gap_to(6); // space before world
        gb.remove_range(from, to);

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
    }

    #[test]
    fn remove_newline_in_str_is_tracked_correctly() {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(gb.len_lines(), 2);

        gb.remove_range(10, 15);

        assert_eq!(gb.len_lines(), 1);
        assert_eq!(gb.line(0).to_string(), "hello, worow are you?");
    }

    #[test]
    fn insert_remove_char_is_idempotent() {
        let s = "hello, world!";
        let mut gb = GapBuffer::from(s);
        gb.insert_char(6, 'X');
        gb.remove_char(6);

        assert_eq!(gb.to_string(), s, "{:?}", debug_buffer_content(&gb))
    }

    #[test_case("TEST", 1; "without trailing newline")]
    #[test_case("TEST\n", 2; "with trailing newline")]
    #[test_case("TEST\nTEST", 2; "with internal newline")]
    #[test]
    fn insert_remove_str_is_idempotent(edit: &str, expected_lines: usize) {
        let s = "hello, world!";
        let mut gb = GapBuffer::from(s);

        println!("initial: {:?}", raw_debug_buffer_content(&gb));
        for n in 0..gb.len_lines() {
            println!("{:?}", gb.line(n).to_string());
        }

        gb.insert_str(6, edit);
        assert_eq!(gb.len_lines(), expected_lines);
        println!("insert:  {:?}", raw_debug_buffer_content(&gb));
        for n in 0..gb.len_lines() {
            println!("{:?}", gb.line(n).to_string());
        }

        gb.remove_range(6, 6 + edit.len());
        println!("remove:  {:?}", raw_debug_buffer_content(&gb));
        for n in 0..gb.len_lines() {
            println!("{:?}", gb.line(n).to_string());
        }

        assert_eq!(gb.to_string(), s);
    }

    #[test]
    fn chars_work() {
        let s1 = "hello, world!\n";
        let s2 = "how are you?";
        let gb = GapBuffer::from(format!("{s1}{s2}"));

        let l1_chars: String = gb.line(0).chars().collect();
        assert_eq!(l1_chars, s1);

        let l2_chars: String = gb.line(1).chars().collect();
        assert_eq!(l2_chars, s2);
    }

    #[test_case(
        false,
        &[
            (0, 'h'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o'),
            (5, ','), (6, ' '), (7, '世'), (8, '界'), (9, '!'),
        ];
        "forward"
    )]
    #[test_case(
        true,
        &[
            (9, '!'), (8, '界'), (7, '世'), (6, ' '), (5, ','),
            (4, 'o'), (3, 'l'), (2, 'l'), (1, 'e'), (0, 'h')
        ];
        "reversed"
    )]
    #[test]
    fn indexed_chars_works(rev: bool, expected: &[(usize, char)]) {
        let s = "hello, 世界!";
        let gb = GapBuffer::from(s);

        let v: Vec<(usize, char)> = gb.line(0).indexed_chars(rev).collect();
        assert_eq!(&v, expected);
    }

    #[test]
    fn slice_work() {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");

        let slice = gb.slice(6, 17);
        let (s1, s2) = slice.as_strs();
        assert_eq!(s1, " world!\nhow");
        assert_eq!(s2, "");

        gb.move_gap_to(12);
        println!("after move:  {:?}", raw_debug_buffer_content(&gb));

        let slice = gb.slice(6, 17);
        let (s1, s2) = slice.as_strs();
        assert_eq!(s1, " world");
        assert_eq!(s2, "!\nhow");
    }
}
