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
    assert!(is_char_boundary(bytes[bytes.len() - 1]), "invalid utf8");
    let mut n_chars = 0;
    for &b in bytes {
        if is_char_boundary(b) {
            n_chars += 1;
        }
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

    pub fn len_lines(&self) -> usize {
        self.meta.lines.len()
    }

    pub fn len_chars(&self) -> usize {
        self.meta.n_chars
    }

    pub fn char(&self, char_idx: usize) -> char {
        let byte_idx = self.char_to_byte(char_idx);
        let len = self.char_len(byte_idx);

        // SAFTEY: we know that we have valid utf8 data internally
        unsafe {
            std::str::from_utf8_unchecked(&self.data[byte_idx..byte_idx + len])
                .chars()
                .next()
                .unwrap()
        }
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
                cur: 0,
            };
        }

        Slice {
            left: &self.data[from..self.gap_start],
            right: &self.data[self.gap_end..to],
            cur: 0,
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
    }

    /// Number of bytes in the gap
    #[inline]
    fn gap(&self) -> usize {
        self.gap_end - self.gap_start
    }

    #[inline]
    fn char_len(&self, idx: usize) -> usize {
        let mut len = 1;
        for n in 0.. {
            if is_char_boundary(self.data[idx + n]) {
                return len;
            }
            len += 1;
        }
        panic!("invalid utf8 character data");
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
        let mut n = 0;

        for ls in self.meta.lines.iter() {
            let next_boundary = n + ls.n_chars;
            if char_idx > next_boundary {
                n = next_boundary;
                continue;
            }

            let mut count = char_idx - n;
            let Slice { left, right, .. } = ls.as_slice(self);

            for (n_bytes, b) in left.iter().chain(right.iter()).enumerate() {
                if count == 0 {
                    let offset = if ls.offset > self.gap_start {
                        ls.offset - self.gap_end
                    } else {
                        ls.offset
                    };
                    return offset + n_bytes;
                } else if is_char_boundary(*b) {
                    count -= 1;
                }
            }

            if count == 0 {
                let end = ls.end();
                return if end > self.gap_end {
                    end - self.gap_end
                } else {
                    end
                };
            }
        }

        panic!("out of bounds: {char_idx} > {}", self.len());
    }

    fn char_to_raw_byte(&self, char_idx: usize) -> usize {
        let mut n = 0;

        for ls in self.meta.lines.iter() {
            let next_boundary = n + ls.n_chars;
            if char_idx > next_boundary {
                n = next_boundary;
                continue;
            }

            let mut count = char_idx - n;
            let Slice { left, right, .. } = ls.as_slice(self);

            for (n_bytes, b) in left.iter().chain(right.iter()).enumerate() {
                if count == 0 {
                    return ls.offset + n_bytes;
                } else if is_char_boundary(*b) {
                    count -= 1;
                }
            }

            if count == 0 {
                return ls.end();
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
                cur: 0,
            };
        }

        Slice {
            left: &gb.data[self.offset..gb.gap_start],
            right: &gb.data[gb.gap_end..end + gb.gap()],
            cur: 0,
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
    cur: usize,
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
}

impl<'a> ToString for Slice<'a> {
    fn to_string(&self) -> String {
        let mut v = Vec::with_capacity(self.left.len() + self.right.len());
        v.extend_from_slice(self.left);
        v.extend_from_slice(self.right);

        String::from_utf8(v).expect("valid utf8")
    }
}

impl<'a> Iterator for Slice<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cur >= self.left.len() + self.right.len() {
            return None;
        }

        let (cur, data) = if self.cur < self.left.len() {
            (self.cur, self.left)
        } else {
            (self.cur - self.left.len(), self.right)
        };

        let mut len = 1;

        for n in 0.. {
            if is_char_boundary(data[cur + n]) {
                // SAFTEY: we know that we have valid utf8 data internally
                unsafe {
                    let ch = std::str::from_utf8_unchecked(&data[cur..cur + len])
                        .chars()
                        .next();
                    self.cur += 1;
                    return ch;
                };
            }
            len += 1;
        }

        panic!("invalid utf8 character data");
    }
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
    #[test_case(26, 0, 0; "BOF cur at EOF")]
    #[test_case(26, 5, 5; "in the buffer cur at EOF")]
    #[test_case(5, 5, 5; "in the buffer cur at gap")]
    #[test_case(5, 3, 3; "in the buffer cur before gap")]
    #[test_case(5, 10, 10; "in the buffer cur after gap")]
    #[test]
    fn char_to_byte_works(cur: usize, char_idx: usize, expected: usize) {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(s.len(), 26, "EOF case is not 0..s.len()");
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
    fn line_chars_work() {
        let s1 = "hello, world!\n";
        let s2 = "how are you?";
        let gb = GapBuffer::from(format!("{s1}{s2}"));

        let l1_chars: String = gb.line(0).collect();
        assert_eq!(l1_chars, s1);

        let l2_chars: String = gb.line(1).collect();
        assert_eq!(l2_chars, s2);
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
