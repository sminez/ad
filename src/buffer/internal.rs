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

/// An implementation of a gap buffer that tracks line offsets
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GapBuffer {
    data: Box<[u8]>,
    line_starts: Vec<usize>,
    cap: usize,
    gap_start: usize,
    gap_end: usize,
    next_gap: usize,
}

impl From<String> for GapBuffer {
    fn from(s: String) -> Self {
        let gap_start = s.len();
        let cap = s.capacity();

        if cap == gap_start {
            return Self::from(s.as_str());
        }

        let mut v = s.into_bytes();
        let mut line_starts: Vec<usize> = v
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if *b == b'\n' { Some(i) } else { None })
            .collect();
        line_starts.insert(0, 0);
        v.resize(cap, 0);

        Self {
            data: v.into_boxed_slice(),
            line_starts,
            cap,
            gap_start,
            gap_end: cap,
            next_gap: clamp_gap_size(gap_start, MIN_GAP),
        }
    }
}

impl From<&str> for GapBuffer {
    fn from(s: &str) -> Self {
        let gap_start = s.len();
        let next_gap = clamp_gap_size(gap_start, MIN_GAP);
        let cap = gap_start + next_gap;
        let mut v = Vec::with_capacity(cap);
        v.extend_from_slice(s.as_bytes());
        let mut line_starts: Vec<usize> = v
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if *b == b'\n' { Some(i) } else { None })
            .collect();
        line_starts.insert(0, 0);
        v.resize(cap, 0);

        Self {
            data: v.into_boxed_slice(),
            line_starts,
            cap,
            gap_start,
            gap_end: cap,
            next_gap,
        }
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

    /// Number of bytes in the gap
    #[inline]
    fn gap(&self) -> usize {
        self.gap_end - self.gap_start
    }

    /// Grow the current gap to be able to hold the text being inserted along with
    /// our normal gap growth on top.
    ///
    /// This does not alter position of the gap within the data buffer relative to
    /// the live data: it only increases the gap size.
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

        for ls in self.line_starts.iter_mut() {
            if *ls > self.gap_start {
                *ls += gap_increase;
            }
        }

        self.next_gap = clamp_gap_size(self.len(), self.next_gap * 2);
        self.data = buf.into_boxed_slice();
        self.gap_end += gap_increase;
        self.cap = cap;
    }

    fn move_gap_to(&mut self, idx: usize) {
        if idx > self.len() {
            panic!("index out of bounds: {idx} > {}", self.len());
        }

        let (src, dest) = match idx.cmp(&self.gap_start) {
            Ordering::Equal => return,
            Ordering::Less => (idx..self.gap_start, idx + self.gap()),
            Ordering::Greater => (self.gap_end..idx + self.gap(), self.gap_start),
        };

        self.data.copy_within(src, dest);
        self.gap_end = idx + self.gap();
        self.gap_start = idx;
    }

    /// Insert a single character at the specifified index.
    ///
    /// This is O(1) if idx is at the current gap start and the gap is large enough to accomodate
    /// the new text, otherwise data will need to be copied in order to relocate the gap.
    pub fn insert_char(&mut self, idx: usize, ch: char) {
        let len = ch.len_utf8();
        if len >= self.gap() {
            self.grow_gap(len);
        }
        self.move_gap_to(idx);
        ch.encode_utf8(&mut self.data[self.gap_start..]);
        self.gap_start += len;
    }

    /// Insert a string at the specifified index.
    ///
    /// This is O(1) if idx is at the current gap start and the gap is large enough to accomodate
    /// the new text, otherwise data will need to be copied in order to relocate the gap.
    pub fn insert_str(&mut self, idx: usize, s: &str) {
        let len = s.len();
        if len >= self.gap() {
            self.grow_gap(len);
        }
        self.move_gap_to(idx);
        self.data[self.gap_start..self.gap_start + len].copy_from_slice(s.as_bytes());
        self.gap_start += len;
    }

    /// Remove the requested character index from the visible region of the buffer
    pub fn remove_char(&mut self, idx: usize) {
        if idx == self.gap_start {
            self.gap_end += 1;
        } else {
            self.move_gap_to(idx);
            self.gap_end += 1;
        }
    }

    // /// Remove the requested range from the visible region of the buffer
    // pub fn remove_range(&mut self, from: usize, to: usize) {
    //     if from == to {
    //         return;
    //     }
    // }
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

    #[test]
    fn inset_remove_char_is_idempotent() {
        let s = "hello, world!";
        let mut gb = GapBuffer::from(s);
        gb.insert_char(6, 'X');
        gb.remove_char(6);

        assert_eq!(gb.to_string(), s, "{:?}", debug_buffer_content(&gb))
    }
}
