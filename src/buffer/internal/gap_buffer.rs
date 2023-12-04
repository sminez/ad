//! A simple gap buffer implementation

//      <--         cap         -->
// gap_start -v
//      [f,o,o,     ...     ,b,a,r]
//             <- gap_len ->
#[derive(Debug, Clone)]
pub struct GapBuffer {
    inner: Box<[u8]>,
    cap: usize,
    gap_start: usize,
    gap_len: usize,
}

impl From<String> for GapBuffer {
    fn from(s: String) -> Self {
        let gap_start = s.len();
        let cap = s.capacity();
        let mut v = s.into_bytes();
        v.resize(cap, 0);
        let inner = v.into_boxed_slice();

        Self {
            inner,
            cap,
            gap_start,
            gap_len: cap - gap_start,
        }
    }
}

impl GapBuffer {
    pub fn contents(&self) -> String {
        let mut v = Vec::with_capacity(self.cap - self.gap_len);
        v.extend(&self.inner[0..self.gap_start]);
        v.extend(&self.inner[self.gap_start + self.gap_len..]);

        String::from_utf8(v).unwrap()
    }

    pub fn len(&self) -> usize {
        self.cap - self.gap_len
    }

    pub fn is_empty(&self) -> bool {
        self.cap == self.gap_len
    }

    /// Moves the gap to idx if it's not already there and then inserts the new character.
    ///
    /// If the
    pub fn insert_char(&mut self, idx: usize, ch: char) {}

    pub fn insert(&mut self, idx: usize, s: &str) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contents_works() {
        let s = "this is a test";
        let gb = GapBuffer::from(s.to_string());
        assert_eq!(gb.contents(), s);
    }
}
