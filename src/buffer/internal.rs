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
fn clamp_gap_size(len: usize, min_gap: usize) -> usize {
    min(max(len / 20, min_gap), MAX_GAP)
}

// This is bit magic equivalent to: b < 128 || b >= 192
// -> taken from the impl of is_utf8_char_boundary in
//    https://doc.rust-lang.org/src/core/num/mod.rs.html
fn is_char_boundary(b: u8) -> bool {
    (b as i8) >= -0x40
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
    line_stats: LineStats,
}

impl From<String> for GapBuffer {
    fn from(s: String) -> Self {
        let gap_start = s.len();
        let cap = s.capacity();

        if cap == gap_start {
            return Self::from(s.as_str());
        }

        let line_stats = LineStats::from(s.as_str());
        let mut v = s.into_bytes();
        v.resize(cap, 0);

        let mut gb = Self {
            data: v.into_boxed_slice(),
            line_stats,
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
        let line_stats = LineStats::from(s);
        v.extend_from_slice(s.as_bytes());
        v.resize(cap, 0);

        let mut gb = Self {
            data: v.into_boxed_slice(),
            line_stats,
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

// methods to impl
//   .txt.byte_to_char(offset)
//   .txt.char(idx)
//   .txt.char_to_line(cur.idx)
//   .txt.len_chars()
//   .txt.len_lines()
//   .txt.line(y)
//   .txt.slice(rng.clone()
//   .txt.try_char_to_line(char_idx)
//   .txt.try_line_to_char(line_idx)
//
// methods done:
//   .txt.bytes()
//   .txt.insert(idx, &s)
//   .txt.insert_char(idx, ch)
//   .txt.remove(idx..(idx + 1)
//   .txt.remove(rng)
//   .txt.to_string()

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
        self.line_stats.lines.len()
    }

    pub fn len_chars(&self) -> usize {
        self.line_stats.n_chars
    }

    /// Number of bytes in the gap
    #[inline]
    fn gap(&self) -> usize {
        self.gap_end - self.gap_start
    }

    // #[inline]
    // fn is_char_boundary(&self, idx: usize) -> bool {
    //     if idx == 0 {
    //         return true;
    //     }

    //     match self.data.get(idx) {
    //         Some(&b) => is_char_boundary(b),
    //         None => idx == self.data.len(),
    //     }
    // }

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

        for ls in self.line_stats.lines.iter_mut() {
            if ls.offset > self.gap_start {
                ls.offset += gap_increase;
            }
        }

        self.next_gap = clamp_gap_size(self.len(), self.next_gap * 2);
        self.data = buf.into_boxed_slice();
        self.gap_end += gap_increase;
        self.cap = cap;
    }

    /// The byte_idx passed here must be an absolute position within the data buffer
    fn move_gap_to(&mut self, byte_idx: usize) {
        if byte_idx > self.data.len() {
            panic!("index out of bounds: {byte_idx} > {}", self.data.len());
        }

        let gap = self.gap();

        let (src, dest) = match byte_idx.cmp(&self.gap_start) {
            Ordering::Equal => return,

            // Gap moving left
            Ordering::Less => {
                for ls in self.line_stats.lines.iter_mut() {
                    if ls.offset >= byte_idx && ls.offset <= self.gap_start {
                        ls.offset += gap;
                    }
                }

                (byte_idx..self.gap_start, byte_idx + gap)
            }

            // Gap moving right
            Ordering::Greater => {
                for ls in self.line_stats.lines.iter_mut() {
                    if ls.offset >= self.gap_end && ls.offset <= byte_idx + self.gap_end {
                        ls.offset -= gap;
                    }
                }

                (self.gap_end..byte_idx + gap, self.gap_start)
            }
        };

        self.data.copy_within(src, dest);
        self.gap_end = byte_idx + gap;
        self.gap_start = byte_idx;
    }

    pub fn byte_to_line(&self, byte_idx: usize) -> usize {
        let mut n = 0;
        for (i, ls) in self.line_stats.lines.iter().enumerate() {
            n += ls.n_bytes;
            if byte_idx <= n {
                return i;
            }
        }

        panic!("out of bounds: {byte_idx} > {}", self.len());
    }

    pub fn line_to_byte(&self, line_idx: usize) -> usize {
        self.line_stats.lines[line_idx].offset
    }

    pub fn char_to_byte(&self, char_idx: usize) -> usize {
        let mut n = 0;

        for ls in self.line_stats.lines.iter() {
            let next_boundary = n + ls.n_chars;
            if char_idx > next_boundary {
                n = next_boundary;
                continue;
            }

            let mut count = char_idx - n;
            let (b1, b2) = ls.bytes(self);
            for (n_bytes, b) in b1.iter().chain(b2).enumerate() {
                if count == 0 {
                    return ls.offset + n_bytes;
                }

                if is_char_boundary(*b) {
                    count -= 1;
                }
            }

            if count == 0 {
                return self.len();
            }
        }

        panic!("out of bounds: {char_idx} > {}", self.len());
    }

    fn insert_newlines(&mut self, it: impl Iterator<Item = usize>) {}

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
        let ls = self.line_stats.current_line_mut();
        ls.n_chars += 1;
        ls.n_bytes += ch.len_utf8();

        if ch == '\n' {
            self.insert_newlines(std::iter::once(idx));
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

        self.insert_newlines(s.chars().enumerate().filter_map(|(i, ch)| {
            if ch == '\n' {
                Some(i + idx)
            } else {
                None
            }
        }));
    }

    /// Remove the requested character index from the visible region of the buffer
    pub fn remove_char(&mut self, char_idx: usize) {
        let idx = self.char_to_byte(char_idx);

        if idx == self.gap_start {
            self.gap_end += 1;
        } else {
            self.move_gap_to(idx);
            self.gap_end += 1;
        }

        // TODO: handle removal of newlines
    }

    /// Remove the requested range (from..to) from the visible region of the buffer
    pub fn remove_range(&mut self, char_from: usize, char_to: usize) {
        if char_from == char_to {
            return;
        }

        assert!(
            char_from < char_to,
            "invalid range: char_from={char_from} > char_to={char_to}"
        );

        let from = self.char_to_byte(char_from);
        let to = self.char_to_byte(char_to);

        self.move_gap_to(from);
        self.gap_end += to - from;

        // TODO: handle removal of newlines
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
struct LineStats {
    lines: Vec<LineStat>,
    n_chars: usize,
    current_line: usize,
}

impl From<&str> for LineStats {
    fn from(s: &str) -> Self {
        let mut lines = Vec::new();
        let mut ls = LineStat::default();
        let mut offset = 0;

        for ch in s.chars() {
            ls.n_bytes += ch.len_utf8();
            ls.n_chars += 1;
            offset += 1;

            if ch == '\n' {
                lines.push(ls);
                ls = LineStat {
                    offset,
                    ..Default::default()
                };
            }
        }

        if ls.n_chars > 0 {
            lines.push(ls);
        }

        Self {
            lines,
            n_chars: offset,
            current_line: 0,
        }
    }
}

impl LineStats {
    fn current_line_mut(&mut self) -> &mut LineStat {
        &mut self.lines[self.current_line]
    }
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
struct LineStat {
    offset: usize,
    n_chars: usize,
    n_bytes: usize,
}

// #[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
// struct ByteSlice<'a> {
//     left: &'a[u8],
//     right: &'a[u8],
//     cur: usize,
// }

// #[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
// struct CharSlice<'a>(ByteSlice<'a>);

impl LineStat {
    #[inline]
    fn bytes<'a>(&self, gb: &'a GapBuffer) -> (&'a [u8], &'a [u8]) {
        let mut end = self.offset + self.n_bytes;
        if end <= gb.gap_start || self.offset >= gb.gap_end || self.offset >= gb.gap_start {
            return (&gb.data[self.offset..end], &[]);
        }

        end += gb.gap_end - gb.gap_start;
        (
            &gb.data[self.offset..gb.gap_start],
            &gb.data[gb.gap_end..end],
        )
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

    #[test]
    fn to_string() {
        let s = "this is a test";
        let gb = GapBuffer::from(s.to_string());
        assert_eq!(gb.to_string(), s);
    }

    fn ls(offset: usize, n_chars: usize, n_bytes: usize) -> LineStat {
        LineStat {
            offset,
            n_chars,
            n_bytes,
        }
    }

    #[test_case(0, &[ls(64, 14, 14), ls(64+14, 12, 12)]; "BOF")]
    #[test_case(26, &[ls(0, 14, 14), ls(14, 12, 12)]; "EOF")]
    #[test]
    fn move_gap_to_cur_maintains_line_offsets(cur: usize, expected: &[LineStat]) {
        let s = "hello, world!\nhow are you?";
        let mut gb = GapBuffer::from(s);
        assert_eq!(s.len(), 26, "EOF case is not 0..s.len()");
        gb.move_gap_to(cur);
        assert_eq!(&gb.line_stats.lines, expected);
    }

    #[test_case(0, 0, 64; "BOF cur at BOF")]
    #[test_case(26, 0, 0; "BOF cur at EOF")]
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

        let (b1, b2) = gb.line_stats.lines[line].bytes(&gb);

        let mut v = Vec::with_capacity(b1.len() + b2.len());
        v.extend(b1);
        v.extend(b2);
        let s = String::from_utf8(v).expect("valid utf8");

        assert_eq!(s, expected);
    }

    #[test_case(&[(0, 'h')], "hello world"; "insert front")]
    #[test_case(&[(4, ',')], "ello, world"; "insert inner")]
    #[test_case(&[(10, '!')], "ello world!"; "insert back")]
    #[test_case(&[(10, '!'), (0, 'h'), (5, ',')], "hello, world!"; "insert all")]
    #[test]
    fn insert_char(inserts: &[(usize, char)], expected: &str) {
        let mut gb = GapBuffer::from("ello world");

        for &(idx, ch) in inserts {
            println!("{:?}", debug_buffer_content(&gb));
            gb.insert_char(idx, ch);
        }

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
    }

    #[test_case(&[(0, "hell")], "helloworl"; "insert front")]
    #[test_case(&[(1, ", ")], "o, worl"; "insert inner")]
    #[test_case(&[(5, "d!")], "oworld!"; "insert back")]
    #[test_case(&[(5, "d!"), (0, "hell"), (5, ", ")], "hello, world!"; "insert all")]
    #[test]
    fn insert_str(inserts: &[(usize, &str)], expected: &str) {
        let mut gb = GapBuffer::from("oworl");
        for &(idx, s) in inserts {
            gb.insert_str(idx, s);
        }

        assert_eq!(gb.to_string(), expected, "{:?}", debug_buffer_content(&gb))
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
    fn insert_remove_char_is_idempotent() {
        let s = "hello, world!";
        let mut gb = GapBuffer::from(s);
        gb.insert_char(6, 'X');
        gb.remove_char(6);

        assert_eq!(gb.to_string(), s, "{:?}", debug_buffer_content(&gb))
    }

    #[test]
    fn insert_remove_str_is_idempotent() {
        let s = "hello, world!";
        let mut gb = GapBuffer::from(s);
        gb.insert_str(6, "TEST");
        gb.remove_range(6, 10);

        assert_eq!(gb.to_string(), s, "{:?}", debug_buffer_content(&gb))
    }
}
