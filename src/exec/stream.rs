//! An abstraction over a rope so that we can cache streaming input and apply edits
//! on the fly as needed.
use crate::util::IdxRopeChars;
use ropey::Rope;
use std::{
    cell::RefCell,
    io::{stdin, Stdin},
};

/// Initial length of the line buffer for when we read from stdin
const LINE_BUF_LEN: usize = 100;

/// An IterableStream represents something supporting both iteration over a potentially
/// lazy character stream and access to the historic text via a Rope.
pub trait IterableStream {
    fn iter_between(&self, from: usize, to: usize) -> StreamIter<'_>;
    fn contents(&self) -> String;
    fn insert(&mut self, ix: usize, s: &str);
    fn remove(&mut self, from: usize, to: usize);
    fn max_dot(&self) -> usize;
    fn len_chars(&self) -> usize;
}

pub enum StreamIter<'a> {
    Rope(IdxRopeChars<'a>),
    StdIn(CachedStdinIter<'a>),
}

impl<'a> Iterator for StreamIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Rope(it) => it.next(),
            Self::StdIn(it) => it.next(),
        }
    }
}

impl IterableStream for Rope {
    fn iter_between(&self, from: usize, to: usize) -> StreamIter<'_> {
        StreamIter::Rope(IdxRopeChars::new(self, from, to))
    }

    fn contents(&self) -> String {
        self.to_string()
    }

    fn insert(&mut self, ix: usize, s: &str) {
        self.insert(ix, s)
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.remove(from..=to)
    }

    fn max_dot(&self) -> usize {
        self.len_chars() - 1
    }

    fn len_chars(&self) -> usize {
        Rope::len_chars(self)
    }
}

pub struct CachedStdin {
    inner: RefCell<CachedStdinInner>,
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
            r: RefCell::new(Rope::new()),
        }
    }

    fn is_closed(&self) -> bool {
        self.inner.borrow().closed
    }

    fn get_char(&self, ix: usize) -> Option<char> {
        self.r.borrow().get_char(ix)
    }

    fn read_next_line(&self) -> bool {
        let mut inner = self.inner.borrow_mut();
        let mut buf = String::with_capacity(LINE_BUF_LEN);

        let closed = match inner.stdin.read_line(&mut buf) {
            Ok(_) => {
                let len = self.r.borrow().len_chars();
                self.r.borrow_mut().insert(len, &buf);
                false
            }
            Err(_) => {
                inner.closed = true;
                true
            }
        };

        closed
    }
}

pub struct CachedStdinIter<'a> {
    inner: &'a CachedStdin,
    from: usize,
    to: usize,
}

impl<'a> Iterator for CachedStdinIter<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // self.from == self.to is the last character so
        // we catch end of iteration on the subsequent call
        if self.from > self.to || self.inner.is_closed() {
            return None;
        }

        loop {
            match self.inner.get_char(self.from) {
                Some(ch) => {
                    let res = (self.from, ch);
                    self.from += 1;

                    return Some(res);
                }
                None => {
                    if self.inner.read_next_line() {
                        return None;
                    }
                }
            }
        }
    }
}

impl IterableStream for CachedStdin {
    fn iter_between(&self, from: usize, to: usize) -> StreamIter {
        StreamIter::StdIn(CachedStdinIter {
            inner: self,
            from,
            to,
        })
    }

    fn contents(&self) -> String {
        self.r.borrow().to_string()
    }

    fn insert(&mut self, ix: usize, s: &str) {
        self.r.borrow_mut().insert(ix, s)
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.r.borrow_mut().remove(from..=to)
    }

    fn max_dot(&self) -> usize {
        usize::MAX
    }

    fn len_chars(&self) -> usize {
        self.r.borrow().len_chars()
    }
}
