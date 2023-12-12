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
use std::{
    cmp::{max, min, Ordering},
    collections::BTreeMap,
};

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
// #[inline]
// fn is_char_boundary(b: u8) -> bool {
//     (b as i8) >= -0x40
// }

#[inline]
fn count_chars(bytes: &[u8]) -> usize {
    if bytes.is_empty() {
        return 0;
    }

    let mut n_chars = 0;
    let mut cur = 0;

    while cur < bytes.len() {
        let ch = unsafe { decode_char_at(cur, bytes) };
        cur += ch.len_utf8();
        n_chars += 1;
    }

    n_chars
}

type ByteOffset = usize;
type CharOffset = usize;

/// An implementation of a gap buffer that tracks internal meta-data to help with accessing
/// sub-regions of the text such as character ranges and lines.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GapBuffer {
    /// the raw data being stored (both buffer content and the gap)
    data: Box<[u8]>,
    /// current size of the allocation for data
    cap: usize,
    /// byte offset to the first character in the gap
    gap_start: usize,
    /// byte offset to the last character in the gap
    gap_end: usize,
    /// size in bytes for the next gap when re-allocating
    next_gap: usize,
    /// line ending byte offset -> char offset
    line_endings: BTreeMap<ByteOffset, CharOffset>,
    /// total number of characters in the buffer
    /// this is != line_endings.last() if there is no trailing newline
    n_chars: usize,
}

fn compute_line_endings(s: &str) -> (usize, BTreeMap<ByteOffset, CharOffset>) {
    let mut n_chars = 0;
    let mut line_endings = BTreeMap::new();

    for (line_chars, (idx, ch)) in s.char_indices().enumerate() {
        n_chars += 1;
        if ch == '\n' {
            line_endings.insert(idx, line_chars);
        }
    }

    (n_chars, line_endings)
}

impl From<String> for GapBuffer {
    fn from(s: String) -> Self {
        let gap_start = s.len();
        let cap = s.capacity();
        let (n_chars, line_endings) = compute_line_endings(&s);
        let mut v = s.into_bytes();
        v.resize(cap, 0);

        let mut gb = Self {
            data: v.into_boxed_slice(),
            cap,
            gap_start,
            gap_end: cap,
            next_gap: clamp_gap_size(gap_start, MIN_GAP),
            n_chars,
            line_endings,
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
        let (n_chars, line_endings) = compute_line_endings(s);
        let mut v = Vec::with_capacity(cap);
        v.extend_from_slice(s.as_bytes());
        v.resize(cap, 0);

        let mut gb = Self {
            data: v.into_boxed_slice(),
            cap,
            gap_start,
            gap_end: cap,
            next_gap,
            n_chars,
            line_endings,
        };

        gb.move_gap_to(0);
        gb
    }
}

impl ToString for GapBuffer {
    fn to_string(&self) -> String {
        String::from_utf8(self.bytes()).expect("valid utf8")
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
        match self.line_endings.last_key_value() {
            Some((&raw_idx, _)) => {
                let n = self.line_endings.len();
                let byte_idx = if raw_idx > self.gap_start {
                    raw_idx - self.gap_end
                } else {
                    raw_idx
                };

                if byte_idx < self.len() {
                    n + 1
                } else {
                    n
                }
            }
            None => 1,
        }
    }

    #[inline]
    pub fn len_chars(&self) -> usize {
        self.n_chars
    }

    pub fn clear(&mut self) {
        self.move_gap_to(0);
        self.gap_end = self.cap;
        self.line_endings.clear();
        self.n_chars = 0;
    }

    #[inline]
    pub fn char(&self, char_idx: usize) -> char {
        let byte_idx = self.char_to_raw_byte(char_idx);

        // SAFTEY: we know that we have valid utf8 data internally
        unsafe { decode_char_at(byte_idx, &self.data) }
    }

    #[inline]
    pub fn get_char(&self, char_idx: usize) -> Option<char> {
        if char_idx < self.n_chars {
            Some(self.char(char_idx))
        } else {
            None
        }
    }

    #[inline]
    fn char_len(&self, byte_idx: usize) -> usize {
        // SAFTEY: we know that we have valid utf8 data internally
        unsafe { decode_char_at(byte_idx, &self.data) }.len_utf8()
    }

    #[inline]
    pub fn line(&self, line_idx: usize) -> Slice {
        if line_idx >= self.len_lines() {
            panic!(
                "line index was {line_idx} but buffer has {} lines",
                self.len_lines()
            )
        }

        let to = match self.line_endings.iter().nth(line_idx) {
            Some((&idx, _)) => idx + self.char_len(idx),
            None => self.cap,
        };
        let from = if line_idx == 0 {
            0
        } else {
            let idx = *self.line_endings.iter().nth(line_idx - 1).unwrap().0;
            idx + 1
        };

        Slice::from_raw_offsets(from, to, self)
    }

    /// An exclusive range of characters from the buffer
    pub fn slice(&self, char_from: usize, char_to: usize) -> Slice {
        let from = self.char_to_raw_byte(char_from);
        let to = self.char_to_raw_byte(char_to);

        Slice::from_raw_offsets(from, to, self)
    }

    fn chars_in_raw_range(&self, raw_from: usize, raw_to: usize) -> usize {
        if raw_to <= self.gap_start || raw_from >= self.gap_end {
            count_chars(&self.data[raw_from..raw_to])
        } else {
            count_chars(&self.data[raw_from..self.gap_start])
                + count_chars(&self.data[self.gap_end..raw_to])
        }
    }

    pub fn byte_to_char(&self, byte_idx: usize) -> usize {
        self.chars_in_raw_range(0, byte_idx)
    }

    pub fn char_to_line(&self, char_idx: usize) -> usize {
        match self.try_char_to_line(char_idx) {
            Some(line_idx) => line_idx,
            None => panic!(
                "char index was {char_idx} but the buffer length is {}",
                self.len()
            ),
        }
    }

    pub fn try_char_to_line(&self, char_idx: usize) -> Option<usize> {
        // We allow setting the cursor to the end of the buffer for inserts
        if char_idx == self.len_chars() {
            return Some(self.len_lines() - 1);
        }

        let mut n = 0;

        for i in 0..self.len_lines() {
            let next_boundary = n + self.line(i).chars().count();
            if char_idx < next_boundary {
                return Some(i);
            }
            n = next_boundary;
        }

        None
    }

    pub fn line_to_char(&self, line_idx: usize) -> usize {
        match self.try_line_to_char(line_idx) {
            Some(char_idx) => char_idx,
            None => panic!(
                "line index was {line_idx} but the buffer has {} lines",
                self.len_lines()
            ),
        }
    }

    pub fn try_line_to_char(&self, line_idx: usize) -> Option<usize> {
        if line_idx > self.len_lines() - 1 {
            return None;
        }

        if line_idx == 0 {
            Some(0)
        } else {
            let k = *self.line_endings.iter().nth(line_idx - 1).unwrap().1;
            Some(k + 1)
        }
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
        self.n_chars += 1;

        if ch == '\n' {
            self.line_endings.insert(idx, char_idx);
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
        self.n_chars += s.chars().count();

        for (i, (offset, ch)) in s.char_indices().enumerate() {
            if ch == '\n' {
                self.line_endings.insert(idx + offset, char_idx + i);
            }
        }
    }

    /// Remove the requested character index from the visible region of the buffer
    pub fn remove_char(&mut self, char_idx: usize) {
        let idx = self.char_to_byte(char_idx);
        let len = self.char_len(self.char_to_raw_byte(char_idx));

        if idx != self.gap_start {
            self.move_gap_to(idx);
        }

        self.gap_end += len;
        self.n_chars -= 1;

        if self.data[self.gap_end - 1] == b'\n' {
            self.line_endings.remove(&(self.gap_end - 1));
        }

        for (_, count) in self.line_endings.iter_mut() {
            if *count >= char_idx {
                *count -= 1;
            }
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

        let n_bytes = to - from;
        let n_chars = self.chars_in_raw_range(
            self.char_to_raw_byte(char_from),
            self.char_to_raw_byte(char_to),
        );

        self.gap_end += n_bytes;
        self.n_chars -= n_chars;

        self.line_endings
            .retain(|idx, _| !((self.gap_end - n_bytes)..(self.gap_end)).contains(idx));

        for (_, count) in self.line_endings.iter_mut() {
            if *count >= char_to {
                *count -= n_chars;
            } else if *count > char_from {
                *count = char_from;
            }
        }
    }

    /// BTreeSet doesn't support iter_mut so we need to map over the existing line
    /// endings and collect into a new set.
    fn update_line_endings<F>(&mut self, f: F)
    where
        F: Fn(usize) -> usize,
    {
        self.line_endings = self
            .line_endings
            .iter()
            .map(|(&k, &v)| ((f)(k), v))
            .collect();
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

        let start = self.gap_start;
        self.update_line_endings(|idx| if idx > start { idx + gap_increase } else { idx });

        self.next_gap = clamp_gap_size(self.len(), self.next_gap * 2);
        self.data = buf.into_boxed_slice();
        self.gap_end += gap_increase;
        self.cap = cap;
    }

    /// The byte_idx argument here is an absolute position within the "live" buffer which will mark
    /// the first byte of the gap region following the move.
    ///
    /// We do not require that the data within the gap region is valid utf-8 so it is fine for this
    /// offset to land in the middle of existing multi-byte characters so long as the regions
    /// outside of the gap stay valid utf-8.
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
                let start = self.gap_start;
                self.update_line_endings(|idx| {
                    if idx >= byte_idx && idx <= start {
                        idx + gap
                    } else {
                        idx
                    }
                });

                (byte_idx..self.gap_start, byte_idx + gap)
            }

            // Gap moving right
            Ordering::Greater => {
                let end = self.gap_end;
                self.update_line_endings(|idx| {
                    if idx >= end && idx < byte_idx + gap {
                        idx - gap
                    } else {
                        idx
                    }
                });

                (self.gap_end..byte_idx + gap, self.gap_start)
            }
        };

        self.data.copy_within(src, dest);
        self.gap_end = byte_idx + gap;
        self.gap_start = byte_idx;
    }

    fn char_to_byte(&self, char_idx: usize) -> usize {
        self.char_to_byte_impl(char_idx, |raw| raw)
    }

    fn char_to_raw_byte(&self, char_idx: usize) -> usize {
        self.char_to_byte_impl(char_idx, |raw| {
            if raw > self.gap_start {
                raw + self.gap()
            } else {
                raw
            }
        })
    }

    // TODO: find a proper impl for this that doesn't just iterate through the entire buffer...!
    fn char_to_byte_impl<F>(&self, char_idx: usize, mut map_raw: F) -> usize
    where
        F: FnMut(usize) -> usize,
    {
        let all = Slice::from_raw_offsets(0, usize::MAX, self);
        let mut chars = all.chars();
        for _ in 0..char_idx {
            chars.next();
        }

        (map_raw)(chars.cur)

        // FIXME: this isn't working yet
        //
        // let mut byte_offset = 0;
        // let mut char_offset = 0;

        // for (&b, &c) in self.line_endings.iter() {
        //     match c.cmp(&char_idx) {
        //         // This line ends before our target
        //         Ordering::Less => {
        //             byte_offset = b;
        //             char_offset = c;
        //         }
        //         // Target is a line ending so map it directly
        //         Ordering::Equal => return (map_raw)(b),
        //         // This line ends after the target so the one before is
        //         // what we need to use as our starting point
        //         Ordering::Greater => break,
        //     }
        // }

        // // from_raw_offsets needs an index that is aware of the gap so we need to
        // // account for that
        // let start = if byte_offset > self.gap_start {
        //     byte_offset + self.gap()
        // } else {
        //     byte_offset
        // };

        // println!("before taking slice");
        // let slice = Slice::from_raw_offsets(start, usize::MAX, self);
        // let mut chars = slice.chars();

        // println!("counting {} chars", char_idx - char_offset);
        // for _ in 0..(char_idx - char_offset) {
        //     chars.next();
        // }

        // (map_raw)(chars.cur + byte_offset)
    }
}

/// A view on a region of the GapBuffer.
///
/// Slices will become invalidated if the gap is moved from the position they were created with
#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub struct Slice<'a> {
    from: usize,
    left: &'a [u8],
    right: &'a [u8],
}

impl<'a> Slice<'a> {
    #[inline]
    fn from_raw_offsets(from: usize, to: usize, gb: &'a GapBuffer) -> Slice<'a> {
        let to = min(to, gb.data.len());

        if to <= gb.gap_start || from >= gb.gap_end {
            return Slice {
                from,
                left: &gb.data[from..to],
                right: &[],
            };
        }

        debug_assert!(from <= gb.gap_start, "line offset sits in gap");

        Slice {
            from,
            left: &gb.data[from..gb.gap_start],
            right: &gb.data[gb.gap_end..to],
        }
    }

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

    pub fn indexed_chars(self, from: usize, rev: bool) -> IdxChars<'a> {
        let (cur, idx) = if rev {
            (
                self.left.len() + self.right.len(),
                from + count_chars(self.left) + count_chars(self.right),
            )
        } else {
            (0, from)
        };

        IdxChars {
            s: self,
            cur,
            idx,
            rev,
        }
    }

    fn cur_and_data(&self, cur: usize) -> (usize, &[u8]) {
        if cur < self.left.len() {
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
    fn to_string_works() {
        let s = "this is a test";
        let gb = GapBuffer::from(s.to_string());
        assert_eq!(gb.to_string(), s);
    }

    #[test_case("foo│foo│foo"; "interleaved multibyte and ascii")]
    #[test_case("hello, 世界!"; "blocks of multibyte and ascii")]
    #[test_case("hello, world!"; "just ascii")]
    #[test]
    fn len_chars_works(s: &str) {
        let mut gb = GapBuffer::from(s);
        let len_s = s.chars().count();

        println!("initial:          {:?}", raw_debug_buffer_content(&gb));
        assert_eq!(gb.len_chars(), len_s);
        assert_eq!(
            gb.len_chars(),
            gb.to_string().chars().count(),
            "char iter len != len_chars"
        );

        gb.insert_char(5, 'X');
        println!("after insert X:   {:?}", raw_debug_buffer_content(&gb));
        assert_eq!(gb.len_chars(), len_s + 1);
        assert_eq!(
            gb.len_chars(),
            gb.to_string().chars().count(),
            "char iter len != len_chars"
        );

        gb.insert_char(3, '界');
        println!("after insert 界:  {:?}", raw_debug_buffer_content(&gb));
        assert_eq!(gb.len_chars(), len_s + 2);
        assert_eq!(
            gb.len_chars(),
            gb.to_string().chars().count(),
            "char iter len != len_chars"
        );

        assert_eq!(gb.char(3), '界');
        gb.remove_char(3);
        println!("after remove 界:  {:?}", raw_debug_buffer_content(&gb));
        assert_eq!(gb.len_chars(), len_s + 1);
        assert_eq!(
            gb.len_chars(),
            gb.to_string().chars().count(),
            "char iter len != len_chars"
        );

        assert_eq!(gb.char(5), 'X');
        gb.remove_char(5);
        println!("after remove X:   {:?}", debug_buffer_content(&gb));
        assert_eq!(gb.len_chars(), len_s);
        assert_eq!(
            gb.len_chars(),
            gb.to_string().chars().count(),
            "char iter len != len_chars"
        );
        assert_eq!(gb.to_string(), s);
    }

    #[test_case("foo│foo│foo"; "interleaved multibyte and ascii")]
    #[test_case("hello, 世界!"; "blocks of multibyte and ascii")]
    #[test_case("hello, world!"; "just ascii")]
    #[test]
    fn move_gap_to_maintains_content(s: &str) {
        let mut gb = GapBuffer::from(s);

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(i);
            gb.move_gap_to(idx);

            // Splitting into the two sections like this allows us to verify that
            // we have valid utf-8 encoded text on either side of the gap.
            let (s1, s2) = (
                std::str::from_utf8(&gb.data[..gb.gap_start]).unwrap(),
                std::str::from_utf8(&gb.data[gb.gap_end..]).unwrap(),
            );

            assert_eq!(format!("{s1}{s2}"), s, "idx={idx}");
        }
    }

    #[test]
    fn move_gap_to_maintains_line_content() {
        let s = "hello, world!\nhow are you?\nthis is a test";
        let mut gb = GapBuffer::from(s);
        assert_eq!(gb.len_lines(), 3);

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(i);
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

    #[test_case(0, 0, 0; "BOF cur at BOF")]
    #[test_case(27, 0, 0; "BOF cur at EOF")]
    #[test_case(27, 5, 5; "in the buffer cur at EOF")]
    #[test_case(5, 5, 5; "in the buffer cur at gap")]
    #[test_case(5, 3, 3; "in the buffer cur before gap")]
    #[test_case(5, 11, 79; "in the buffer cur after gap")]
    #[test_case(5, 7, 71; "multi byte 1")]
    #[test_case(5, 8, 74; "multi byte 2")]
    #[test]
    fn char_to_raw_byte_works(cur: usize, char_idx: usize, expected: usize) {
        let s = "hello, 世界!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(s.len(), 27, "EOF case is not 0..s.len()");
        assert_eq!("世".len(), 3);
        gb.move_gap_to(cur);

        let char_idx = gb.char_to_raw_byte(char_idx);
        assert_eq!(char_idx, expected, "{:?}", debug_buffer_content(&gb));
    }

    #[test_case(0, 0, "hello, world!\n"; "first line cur at BOF")]
    #[test_case(0, 1, "how are you?"; "second line cur at BOF")]
    #[test_case(26, 0, "hello, world!\n"; "first line cur at EOF")]
    #[test_case(26, 1, "how are you?"; "second line cur at EOF")]
    #[test_case(10, 0, "hello, world!\n"; "first line cur in line")]
    #[test_case(10, 1, "how are you?"; "second line cur in line")]
    #[test]
    fn slice_to_string_works(cur: usize, line: usize, expected: &str) {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");
        gb.move_gap_to(cur);

        assert_eq!(gb.line(line).to_string(), expected);
    }

    #[test]
    fn line_to_char_works() {
        let l1 = "hello, world!\n";
        let l2 = "how are you?\n";
        let l3 = "this is a test";

        let gb = GapBuffer::from(format!("{l1}{l2}{l3}"));

        assert_eq!(gb.line_to_char(0), 0);
        assert_eq!(gb.line_to_char(1), l1.chars().count());
        assert_eq!(gb.line_to_char(2), l1.chars().count() + l2.chars().count());
    }

    #[test_case(0, 0; "start of first line")]
    #[test_case(5, 0; "in first line")]
    #[test_case(13, 0; "end of first line")]
    #[test_case(14, 1; "start of second line")]
    #[test_case(20, 1; "in second line")]
    #[test_case(26, 1; "end of second line")]
    #[test_case(27, 2; "start of third line")]
    #[test_case(30, 2; "in third line")]
    #[test_case(40, 2; "end of third line")]
    #[test]
    fn char_to_line_works(char_idx: usize, line_idx: usize) {
        let l1 = "hello, world!\n";
        let l2 = "how are you?\n";
        let l3 = "this is a test";

        let gb = GapBuffer::from(format!("{l1}{l2}{l3}"));

        assert_eq!(gb.char_to_line(char_idx), line_idx);
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
    fn insert_char_with_moving_cur() {
        let mut gb = GapBuffer::from("hello ");
        gb.insert_char(6, 'w');
        gb.insert_char(7, 'o');
        gb.insert_char(8, 'r');
        gb.insert_char(9, 'l');
        gb.insert_char(10, 'd');
        gb.insert_char(11, '!');
        gb.insert_char(5, ',');

        assert_eq!(
            gb.to_string(),
            "hello, world!",
            "{:?}",
            debug_buffer_content(&gb)
        )
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
    fn remove_range_works(from: usize, to: usize, expected: &str) {
        let s = "hello, world!";
        assert_eq!(s.len(), 13, "remove all case is not 0..s.len()");

        let mut gb = GapBuffer::from(s);
        gb.move_gap_to(6); // space before world
        gb.remove_range(from, to);

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
    }

    #[test]
    fn remove_range_w_multibyte_chars_works() {
        let s = "foo│foo│foo";
        let mut gb = GapBuffer::from(s);

        gb.remove_range(0, 3);
        assert_eq!(gb.to_string(), "│foo│foo");
        assert_eq!(gb.len_chars(), 8);

        gb.remove_range(1, 4);
        assert_eq!(gb.to_string(), "││foo");
        assert_eq!(gb.len_chars(), 5);

        gb.remove_range(2, 5);
        assert_eq!(gb.to_string(), "││");
        assert_eq!(gb.len_chars(), 2);
    }

    #[test]
    fn delete_range_for_last_line_works() {
        let s = "hello, world!\nthis is the last line";
        let mut gb = GapBuffer::from(s);
        gb.remove_range(14, s.len());
        assert_eq!(gb.to_string(), "hello, world!\n");
        assert_eq!(gb.len_lines(), 2);
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

    #[test_case('X'; "ascii")]
    #[test_case('界'; "multi-byte")]
    #[test]
    fn insert_remove_char_is_idempotent(ch: char) {
        let s = "hello, world!";
        let mut gb = GapBuffer::from(s);
        gb.insert_char(6, ch);
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
        "hello, 世界!", false,
        &[(0, 'h'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o'),
          (5, ','), (6, ' '), (7, '世'), (8, '界'), (9, '!')];
        "multi-byte block forward"
    )]
    #[test_case(
        "hello, 世界!", true,
        &[(9, '!'), (8, '界'), (7, '世'), (6, ' '), (5, ','),
          (4, 'o'), (3, 'l'), (2, 'l'), (1, 'e'), (0, 'h')];
        "multi-byte block reversed"
    )]
    #[test_case(
        "foo│foo│foo", false,
        &[(0, 'f'), (1, 'o'), (2, 'o'), (3, '│'), (4, 'f'), (5, 'o'),
          (6, 'o'), (7, '│'), (8, 'f'), (9, 'o'), (10, 'o')];
        "interleaved forward"
    )]
    #[test_case(
        "foo│foo│foo", true,
        &[(10, 'o'), (9, 'o'), (8, 'f'), (7, '│'), (6, 'o'), (5, 'o'),
          (4, 'f'), (3, '│'), (2, 'o'), (1, 'o'), (0, 'f')];
        "interleaved reversed"
    )]
    #[test]
    fn indexed_chars_works(s: &str, rev: bool, expected: &[(usize, char)]) {
        let mut gb = GapBuffer::from(s);
        let v: Vec<(usize, char)> = gb.line(0).indexed_chars(0, rev).collect();
        assert_eq!(&v, expected);

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(i);
            gb.move_gap_to(idx);

            let v: Vec<(usize, char)> = gb.line(0).indexed_chars(0, rev).collect();
            assert_eq!(&v, expected, "idx={idx}");
        }
    }

    #[test_case("foo│foo│foo"; "interleaved multibyte and ascii")]
    #[test_case("hello, 世界!"; "blocks of multibyte and ascii")]
    #[test]
    fn chars_works(s: &str) {
        let mut gb = GapBuffer::from(s);
        let chars: String = gb.line(0).chars().collect();
        assert_eq!(chars, s);

        for i in 0..gb.len_chars() {
            let idx = gb.char_to_byte(i);
            gb.move_gap_to(idx);

            let chars: String = gb.line(0).chars().collect();
            assert_eq!(chars, s, "idx={idx}");
        }
    }

    #[test]
    fn slice_works() {
        let mut gb = GapBuffer::from("hello, world!\nhow are you?");
        let slice = Slice::from_raw_offsets(0, gb.cap, &gb);
        let (s1, s2) = slice.as_strs();
        assert_eq!(s1, "");
        assert_eq!(s2, "hello, world!\nhow are you?");

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
