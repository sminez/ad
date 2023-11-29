use crate::{
    buffer::Dot,
    exec::{addr::Address, Edit},
};
use ropey::Rope;
use std::{
    cell::RefCell,
    io::{stdin, Stdin},
};

/// Initial length of the line buffer for when we read from stdin
const LINE_BUF_LEN: usize = 100;

/// A wrapper around stdin that buffers and caches input so that it can be manipulated
/// like a Buffer
pub struct CachedStdin {
    inner: RefCell<CachedStdinInner>,
    buf: RefCell<String>,
    r: RefCell<Rope>,
}

struct CachedStdinInner {
    stdin: Stdin,
    closed: bool,
}

impl Default for CachedStdin {
    fn default() -> Self {
        Self::new()
    }
}

impl CachedStdin {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(CachedStdinInner {
                stdin: stdin(),
                closed: false,
            }),
            buf: RefCell::new(String::with_capacity(LINE_BUF_LEN)),
            r: RefCell::new(Rope::new()),
        }
    }

    fn is_closed(&self) -> bool {
        self.inner.borrow().closed
    }

    fn get_char(&self, ix: usize) -> Option<char> {
        self.r.borrow().get_char(ix)
    }

    fn try_read_next_line(&self) {
        let mut inner = self.inner.borrow_mut();
        let mut buf = self.buf.borrow_mut();
        buf.clear();

        match inner.stdin.read_line(&mut buf) {
            Ok(n) => {
                let len = self.r.borrow().len_chars();
                self.r.borrow_mut().insert(len, &buf);
                inner.closed = n == 0;
            }
            Err(_) => inner.closed = true,
        };
    }
}

impl Address for CachedStdin {
    fn current_dot(&self) -> Dot {
        Dot::default()
    }

    fn len_chars(&self) -> usize {
        self.r.borrow().len_chars()
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        let r = self.r.borrow();
        let cur_len = r.len_lines();
        if line_idx > cur_len {
            for _ in cur_len..=line_idx {
                self.try_read_next_line();
                if self.is_closed() {
                    break;
                }
            }
        }

        r.try_line_to_char(line_idx).ok()
    }

    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        self.r.borrow().try_char_to_line(char_idx).ok()
    }

    fn char_to_line_end(&self, char_idx: usize) -> Option<usize> {
        let r = self.r.borrow();
        let line_idx = r.try_char_to_line(char_idx).ok()?;
        Some(r.line_to_char(line_idx) + r.line(line_idx).len_chars())
    }

    fn char_to_line_start(&self, char_idx: usize) -> Option<usize> {
        let r = self.r.borrow();
        let line_idx = r.try_char_to_line(char_idx).ok()?;
        Some(r.line_to_char(line_idx))
    }
}

impl Edit for CachedStdin {
    fn contents(&self) -> Rope {
        self.r.borrow().clone()
    }

    fn insert(&mut self, ix: usize, s: &str) {
        self.r.borrow_mut().insert(ix, s)
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.r.borrow_mut().remove(from..to)
    }
}

pub struct CachedStdinIter<'a> {
    pub(super) inner: &'a CachedStdin,
    pub(super) from: usize,
    pub(super) to: usize,
}

impl<'a> Iterator for CachedStdinIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // self.from == self.to - 1 is the last character so
        // we catch end of iteration on the subsequent call
        if self.from >= self.to {
            return None;
        }

        loop {
            if self.inner.is_closed() {
                return None;
            }

            match self.inner.get_char(self.from) {
                Some(ch) => {
                    let res = (self.from, ch);
                    self.from += 1;

                    return Some(res);
                }
                None => self.inner.try_read_next_line(),
            }
        }
    }
}
