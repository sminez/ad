//! A custom data structure for storing buffer state and edits based on piece-tables
//! and zippers
use std::collections::LinkedList;

const CHUNK_LEN: usize = 128;

#[derive(Debug, Clone)]
pub struct BufferImpl {
    /// The original text as read from disk
    original: Box<[u8]>,
    /// Chunks of text with a max len to avoid having to reallocate too much
    chunks: Vec<Chunk>,
    /// The edits made to the file since it was opened
    /// Eventually this should to be a tree
    edits: LinkedList<Edit>,
    last_chunk_cap: usize,
}

#[derive(Debug, Clone, Copy)]
struct Chunk {
    data: [u8; CHUNK_LEN],
    p: usize,
}

impl Chunk {
    fn new() -> Self {
        Self {
            data: [0; CHUNK_LEN],
            p: 0,
        }
    }

    fn push(&mut self, ch: char, chunk: usize) -> Edit {
        let len = ch.len_utf8();
        ch.encode_utf8(&mut self.data[self.p..]);
        let start = self.p;
        self.p += len;

        Edit {
            chunk: Some(chunk),
            start,
            len,
        }
    }

    fn push_str(&mut self, s: &str, chunk: usize) -> Edit {
        let len = s.len();
        self.data[self.p..(self.p + len)].copy_from_slice(s.as_bytes());
        let start = self.p;
        self.p += len;

        Edit {
            chunk: Some(chunk),
            start,
            len,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Edit {
    chunk: Option<usize>, // None == original
    start: usize,
    len: usize,
}

impl From<String> for BufferImpl {
    fn from(s: String) -> Self {
        let len = s.len();
        let mut edits = LinkedList::new();
        edits.push_back(Edit {
            chunk: None,
            start: 0,
            len,
        });

        Self {
            original: s.into_bytes().into_boxed_slice(),
            chunks: vec![Chunk::new()],
            edits,
            last_chunk_cap: CHUNK_LEN,
        }
    }
}

impl From<&str> for BufferImpl {
    fn from(s: &str) -> Self {
        Self::from(s.to_string())
    }
}

impl ToString for BufferImpl {
    fn to_string(&self) -> String {
        let mut buf: Vec<u8> = Vec::with_capacity(self.len());

        for e in self.edits.iter() {
            match e.chunk {
                Some(ix) => buf.extend(&self.chunks[ix].data[e.start..(e.start + e.len)]),
                None => buf.extend(&self.original[e.start..(e.start + e.len)]),
            }
        }

        String::from_utf8(buf).expect("valid utf8")
    }
}

impl BufferImpl {
    pub fn len(&self) -> usize {
        self.edits.iter().map(|e| e.len).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }

    // TODO: Should be able to extend existing edits if they are at the end of the last edit we have
    pub fn insert_char(&mut self, idx: usize, ch: char) {
        let len = ch.len_utf8();
        if self.last_chunk_cap < len {
            self.chunks.push(Chunk::new());
            self.last_chunk_cap = CHUNK_LEN;
        }
        self.last_chunk_cap -= len;

        let chunk = self.chunks.len() - 1;
        let e = self.chunks.last_mut().unwrap().push(ch, chunk);

        if idx == 0 {
            self.edits.push_front(e);
            return;
        }

        self.insert_edit(idx, e);
    }

    // TODO: Should be able to extend existing edits if they are at the end of the last edit we have
    pub fn insert_str(&mut self, idx: usize, s: &str) {
        let len = s.len();
        if self.last_chunk_cap < len {
            self.chunks.push(Chunk::new());
            self.last_chunk_cap = CHUNK_LEN;
        }
        self.last_chunk_cap -= len;

        let chunk = self.chunks.len() - 1;
        let e = self.chunks.last_mut().unwrap().push_str(s, chunk);

        if idx == 0 {
            self.edits.push_front(e);
            return;
        }

        self.insert_edit(idx, e);
    }

    fn insert_edit(&mut self, idx: usize, e: Edit) {
        // Find insert point for the edit and split what's there
        // -> this is where we really need a B-tree
        let (edit_idx, offset) = self.split_idx(idx);
        let mut tail = self.edits.split_off(edit_idx);
        let mut e_before = tail.pop_front().unwrap();

        if e_before.len - offset == 0 {
            e_before.len = offset;
            self.edits.push_back(e_before);
            self.edits.push_back(e);
        } else {
            let mut e_after = e_before;
            e_before.len = offset;
            e_after.len -= offset;
            e_after.start += offset;
            self.edits.push_back(e_before);
            self.edits.push_back(e);
            self.edits.push_back(e_after);
        }

        self.edits.append(&mut tail);
    }

    fn split_idx(&self, idx: usize) -> (usize, usize) {
        let mut edit_idx = 0;
        for (i, e) in self.edits.iter().enumerate() {
            if idx <= edit_idx + e.len {
                return (i, idx - edit_idx);
            }
            edit_idx += e.len;
        }

        panic!("out of bounds?")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    fn orig(start: usize, len: usize) -> Edit {
        Edit {
            chunk: None,
            start,
            len,
        }
    }

    fn edit(chunk: usize, start: usize, len: usize) -> Edit {
        Edit {
            chunk: Some(chunk),
            start,
            len,
        }
    }

    #[test_case(&[(0, 'h')], &[edit(0, 0, 1), orig(0, 10)]; "insert front")]
    #[test_case(&[(4, ',')], &[orig(0, 4), edit(0, 0, 1), orig(4, 6)]; "insert inner")]
    #[test_case(&[(10, '!')], &[orig(0, 10), edit(0, 0, 1)]; "insert back")]
    #[test]
    fn insert_char(inserts: &[(usize, char)], expected: &[Edit]) {
        let mut bi = BufferImpl::from("ello world");
        for &(idx, ch) in inserts {
            bi.insert_char(idx, ch);
        }

        assert_eq!(bi.edits.into_iter().collect::<Vec<_>>(), expected)
    }

    #[test_case(&[(0, "hell")], &[edit(0, 0, 4), orig(0, 7)]; "insert front")]
    #[test_case(&[(1, ",")], &[orig(0, 1), edit(0, 0, 1), orig(1, 6)]; "insert inner")]
    #[test_case(&[(7, "!")], &[orig(0, 7), edit(0, 0, 1)]; "insert back")]
    #[test]
    fn insert_str(inserts: &[(usize, &str)], expected: &[Edit]) {
        let mut bi = BufferImpl::from("o world");
        for &(idx, s) in inserts {
            bi.insert_str(idx, s);
        }

        assert_eq!(bi.edits.into_iter().collect::<Vec<_>>(), expected)
    }

    #[test_case(&[], "ello world"; "no edits")]
    #[test_case(&[(0, 'h')], "hello world"; "insert front")]
    #[test_case(&[(4, ',')], "ello, world"; "insert inner")]
    #[test_case(&[(10, '!')], "ello world!"; "insert back")]
    #[test_case(&[(10, '!'), (0, 'h'), (5, ',')], "hello, world!"; "insert all")]
    #[test]
    fn to_string_works(inserts: &[(usize, char)], expected: &str) {
        let mut bi = BufferImpl::from("ello world");
        for &(idx, ch) in inserts {
            bi.insert_char(idx, ch);
        }

        assert_eq!(bi.to_string(), expected)
    }
}
