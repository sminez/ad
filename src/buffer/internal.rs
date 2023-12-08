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

    pub fn line_string(&self, line_idx: usize) -> String {
        self.line_stats.lines[line_idx].as_string(self)
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

    #[inline]
    fn current_line_is_last_line(&self) -> bool {
        self.line_stats.current_line == self.line_stats.lines.len() - 1
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
                    if ls.offset >= self.gap_end && ls.offset < byte_idx + gap {
                        ls.offset -= gap;
                    }
                }

                (self.gap_end..byte_idx + gap, self.gap_start)
            }
        };

        self.line_stats.current_line = self.byte_to_line(byte_idx);
        self.data.copy_within(src, dest);
        self.gap_end = byte_idx + gap;
        self.gap_start = byte_idx;
    }

    fn byte_to_line(&self, byte_idx: usize) -> usize {
        let mut n = 0;
        for (i, ls) in self.line_stats.lines.iter().enumerate() {
            n += ls.n_bytes;
            if byte_idx <= n {
                return i;
            }
        }

        panic!("out of bounds: {byte_idx} > {}", self.len());
    }

    fn char_to_byte(&self, char_idx: usize) -> usize {
        let mut n = 0;

        for ls in self.line_stats.lines.iter() {
            let next_boundary = n + ls.n_chars;
            if char_idx > next_boundary {
                n = next_boundary;
                continue;
            }

            let mut count = char_idx - n;
            let (b1, b2) = ls.bytes(self);

            for (n_bytes, b) in b1.iter().chain(b2.iter()).enumerate() {
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
                return self.len();
            }
        }

        panic!("out of bounds: {char_idx} > {}", self.len());
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
        self.line_stats.n_chars += 1;

        let ls = self.line_stats.current_line_mut();
        ls.n_chars += 1;
        ls.n_bytes += len;
        if idx < ls.offset {
            ls.offset = idx;
        }

        if ch == '\n' {
            self.line_stats.mark_newline(idx, &self.data);
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
        self.line_stats.n_chars += s.chars().count();

        let ls = self.line_stats.current_line_mut();
        ls.n_chars += s.chars().count();
        ls.n_bytes += len;
        if idx < ls.offset {
            ls.offset = idx;
        }

        for (i, b) in s.bytes().enumerate() {
            if b == b'\n' {
                self.line_stats.mark_newline(idx + i, &self.data);
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

        self.line_stats.n_chars -= 1;

        let ls = self.line_stats.current_line_mut();
        ls.n_chars -= 1;
        ls.n_bytes -= len;

        if self.data[idx] == b'\n' && !self.current_line_is_last_line() {
            let LineStat {
                n_chars, n_bytes, ..
            } = self
                .line_stats
                .lines
                .remove(self.line_stats.current_line + 1);

            let ls = self.line_stats.current_line_mut();
            ls.n_bytes += n_bytes;
            ls.n_chars += n_chars;
        }
    }

    /// Remove the requested range (from..to) from the visible region of the buffer
    pub fn remove_range(&mut self, char_from: usize, char_to: usize) {
        if char_from == char_to {
            return;
        }

        let from = self.char_to_byte(char_from);
        let to = self.char_to_byte(char_to);
        assert!(from < to, "invalid range: from={from} > to={to}");
        self.move_gap_to(from);

        let mut n_bytes = to - from;
        let mut n_chars = count_chars(&self.data[from..to]);

        self.gap_end += n_bytes;
        self.line_stats.n_chars -= n_chars;

        match (
            to.cmp(&self.line_stats.current_line_end()),
            self.current_line_is_last_line(),
        ) {
            (Ordering::Less, _) | (Ordering::Equal, true) => {
                let ls = self.line_stats.current_line_mut();
                ls.n_chars -= n_chars;
                ls.n_bytes -= n_bytes;
            }

            (Ordering::Equal, false) => {
                let idx = self.line_stats.current_line + 1;
                let next_ls = self.line_stats.lines.remove(idx);
                let ls = self.line_stats.current_line_mut();
                ls.n_bytes += next_ls.n_bytes;
                ls.n_chars += next_ls.n_chars;
            }

            (Ordering::Greater, false) => {
                while n_bytes > 0 {
                    let offset = self.line_stats.current_line_offset();
                    let end = self.line_stats.current_line_end();

                    if to > end && from > offset {
                        let ls = self.line_stats.current_line_mut();
                        n_bytes -= end - from;
                        ls.n_bytes = from - ls.offset;
                        ls.n_chars = count_chars(&self.data[from..end]);
                        n_chars -= ls.n_chars;
                        self.line_stats.current_line += 1;
                    } else if from < offset && to > end {
                        let ls = self.line_stats.lines.remove(self.line_stats.current_line);
                        n_bytes -= ls.n_bytes;
                        n_chars -= ls.n_chars;
                    } else {
                        let ls_last = self.line_stats.lines.remove(self.line_stats.current_line);
                        self.line_stats.current_line -= 1;
                        let ls = self.line_stats.current_line_mut();
                        ls.n_bytes += ls_last.n_bytes - n_bytes;
                        ls.n_chars += ls_last.n_chars - n_chars;
                        break;
                    }
                }
            }

            (Ordering::Greater, true) => panic!("remove_range out of bounds"),
        }
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

    #[inline]
    fn current_line_offset(&self) -> usize {
        self.lines[self.current_line].offset
    }

    #[inline]
    fn current_line_end(&self) -> usize {
        self.lines[self.current_line].end()
    }

    fn mark_newline(&mut self, idx: usize, data: &[u8]) {
        let LineStat {
            offset,
            n_chars,
            n_bytes,
        } = self.lines[self.current_line];

        let new_n_bytes = idx - offset + 1;
        let new_n_chars = count_chars(&data[offset..idx + 1]);
        let ls = self.current_line_mut();
        ls.n_bytes = new_n_bytes;
        ls.n_chars = new_n_chars;
        self.current_line += 1;
        self.lines.insert(
            self.current_line,
            LineStat::new(idx + 1, n_chars - new_n_chars, n_bytes - new_n_bytes),
        );
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
    fn bytes<'a>(&self, gb: &'a GapBuffer) -> (&'a [u8], &'a [u8]) {
        let mut end = self.offset + self.n_bytes;
        if end <= gb.gap_start || self.offset >= gb.gap_end {
            return (&gb.data[self.offset..end], &[]);
        }

        end += gb.gap_end - gb.gap_start;
        (
            &gb.data[self.offset..gb.gap_start],
            &gb.data[gb.gap_end..end],
        )
    }

    fn as_string(&self, gb: &GapBuffer) -> String {
        let (b1, b2) = self.bytes(gb);
        let mut v = Vec::with_capacity(b1.len() + b2.len());
        v.extend_from_slice(b1);
        v.extend_from_slice(b2);

        String::from_utf8(v).expect("valid utf8")
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

    #[test]
    fn move_gap_to_maintains_line_content() {
        let s = "hello, world!\nhow are you?\nthis is a test";
        assert_eq!(s.len(), 41, "EOF case is not 0..s.len()");
        let mut gb = GapBuffer::from(s);

        for idx in 0..=s.len() {
            gb.move_gap_to(idx);
            assert_eq!(gb.len_lines(), 3);

            assert_eq!(gb.line_string(0), "hello, world!\n", "idx={idx}");
            assert_eq!(gb.line_string(1), "how are you?\n", "idx={idx}");
            assert_eq!(gb.line_string(2), "this is a test", "idx={idx}");
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

        let s = gb.line_string(line);
        assert_eq!(s, expected);
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
        assert_eq!(gb.line_string(0), "hello,\n");
        assert_eq!(gb.line_string(1), " world!\n");
        assert_eq!(gb.line_string(2), "how are you?");

        for idx in 0..=gb.len_chars() {
            gb.move_gap_to(idx);
            assert_eq!(gb.len_lines(), 3);

            assert_eq!(gb.line_string(0), "hello,\n", "idx={idx}");
            assert_eq!(gb.line_string(1), " world!\n", "idx={idx}");
            assert_eq!(gb.line_string(2), "how are you?", "idx={idx}");
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

            assert_eq!(gb.line_string(0), "hello, sailor\n", "idx={idx}");
            assert_eq!(gb.line_string(1), "isn't this fun?\n", "idx={idx}");
            assert_eq!(gb.line_string(2), "what a wonderful\n", "idx={idx}");
            assert_eq!(gb.line_string(3), " world!\n", "idx={idx}");
            assert_eq!(gb.line_string(4), "how are you?", "idx={idx}");
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
        assert_eq!(gb.line_string(0), "hello, world!how are you?");
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
        assert_eq!(gb.line_string(0), "hello, worow are you?");
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
            println!("{:?}", gb.line_string(n));
        }

        gb.insert_str(6, edit);
        assert_eq!(gb.len_lines(), expected_lines);
        println!("insert:  {:?}", raw_debug_buffer_content(&gb));
        for n in 0..gb.len_lines() {
            println!("{:?}", gb.line_string(n));
        }

        gb.remove_range(6, 6 + edit.len());
        println!("remove:  {:?}", raw_debug_buffer_content(&gb));
        for n in 0..gb.len_lines() {
            println!("{:?}", gb.line_string(n));
        }

        assert_eq!(gb.to_string(), s);
    }
}
