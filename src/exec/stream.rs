//! An abstraction over a rope so that we can cache streaming input and apply edits
//! on the fly as needed.
use crate::{
    buffer::{Buffer, Cur, Dot, Range},
    editor::Action,
    util::IdxRopeChars,
};
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
    fn contents(&self) -> Rope;
    fn insert(&mut self, ix: usize, s: &str);
    fn remove(&mut self, from: usize, to: usize);
    fn map_initial_dot(&self, line_from: usize, line_to: Option<usize>) -> (usize, usize);
    fn len_chars(&self) -> usize;

    /// This only really makes sense for use with a buffer but is supported
    /// so that don't need to special case running programs against an in-editor
    /// buffer vs stdin or a file read from disk.
    fn current_dot(&self) -> (usize, usize) {
        (0, 0)
    }
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

    fn contents(&self) -> Rope {
        self.clone()
    }

    fn insert(&mut self, ix: usize, s: &str) {
        self.insert(ix, s)
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.remove(from..=to)
    }

    fn map_initial_dot(&self, line_from: usize, line_to: Option<usize>) -> (usize, usize) {
        let from = self
            .try_line_to_char(line_from)
            .unwrap_or_else(|_| self.len_chars().saturating_sub(1));
        let to = match line_to {
            Some(n) => match self.try_line_to_char(n) {
                Ok(ix) => ix + self.line(n).len_chars().saturating_sub(1),
                Err(_) => self.len_chars().saturating_sub(1),
            },
            None => self.len_chars().saturating_sub(1),
        };

        (from, to)
    }

    fn len_chars(&self) -> usize {
        Rope::len_chars(self)
    }
}

impl IterableStream for Buffer {
    fn iter_between(&self, from: usize, to: usize) -> StreamIter<'_> {
        StreamIter::Rope(IdxRopeChars::new(&self.txt, from, to))
    }

    fn contents(&self) -> Rope {
        self.txt.clone()
    }

    fn insert(&mut self, idx: usize, s: &str) {
        self.dot = Dot::Cur { c: Cur { idx } };
        self.handle_action(Action::InsertString { s: s.to_string() });
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.dot = Dot::Range {
            r: Range::from_cursors(Cur { idx: from }, Cur { idx: to }, true),
        }
        .collapse_null_range();
        self.handle_action(Action::Delete);
    }

    fn map_initial_dot(&self, line_from: usize, line_to: Option<usize>) -> (usize, usize) {
        let from = self
            .txt
            .try_line_to_char(line_from)
            .unwrap_or_else(|_| self.txt.len_chars().saturating_sub(1));
        let to = match line_to {
            Some(n) => match self.txt.try_line_to_char(n) {
                Ok(ix) => ix + self.txt.line(n).len_chars().saturating_sub(1),
                Err(_) => self.txt.len_chars().saturating_sub(1),
            },
            None => self.txt.len_chars().saturating_sub(1),
        };

        (from, to)
    }

    fn current_dot(&self) -> (usize, usize) {
        let Range { start, end, .. } = self.dot.as_range();
        (start.idx, end.idx)
    }

    fn len_chars(&self) -> usize {
        Rope::len_chars(&self.txt)
    }
}

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
        if self.from > self.to {
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

impl IterableStream for CachedStdin {
    fn iter_between(&self, from: usize, to: usize) -> StreamIter {
        StreamIter::StdIn(CachedStdinIter {
            inner: self,
            from,
            to,
        })
    }

    fn contents(&self) -> Rope {
        self.r.borrow().clone()
    }

    fn insert(&mut self, ix: usize, s: &str) {
        self.r.borrow_mut().insert(ix, s)
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.r.borrow_mut().remove(from..=to)
    }

    fn map_initial_dot(&self, line_from: usize, line_to: Option<usize>) -> (usize, usize) {
        let n = match line_to {
            Some(n) => n,
            None => line_from,
        };

        for _ in 0..=n {
            self.try_read_next_line();
            if self.is_closed() {
                break;
            }
        }

        let r = self.contents();
        let from = r.try_line_to_char(line_from).unwrap_or(usize::MAX);
        let to = match line_to {
            Some(n) => r.try_line_to_char(n).unwrap_or(usize::MAX),
            None => usize::MAX,
        };

        (from, to)
    }

    fn len_chars(&self) -> usize {
        self.r.borrow().len_chars()
    }
}
