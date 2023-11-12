//! An abstraction over a rope so that we can cache streaming input and apply edits
//! on the fly as needed.
use crate::{
    buffer::{
        parse_dot::{CompoundAddr, DotExpression, SimpleAddr},
        Buffer, Cur, Dot, Range,
    },
    editor::Action,
    util::IdxRopeChars,
    Config,
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
    fn len_chars(&self) -> usize;

    /// This only really makes sense for use with a buffer but is supported
    /// so that don't need to special case running programs against an in-editor
    /// buffer vs stdin or a file read from disk.
    fn current_dot(&self) -> Dot {
        Dot::default()
    }

    fn map_from_to(&self, line_from: usize, line_to: Option<usize>) -> Dot;
    fn line_to_char(&self, line_idx: usize) -> Option<usize>;

    fn map_simple_addr(&self, addr: &mut SimpleAddr, cur_dot: Dot) -> Dot {
        use SimpleAddr::*;

        match addr {
            Current => cur_dot,
            Bof => Dot::Cur { c: Cur { idx: 0 } },
            Eof => Dot::Cur {
                c: Cur {
                    idx: self.len_chars(),
                },
            },
            Line(line) => self.map_from_to(*line, Some(*line)),

            LineAndColumn(line, col) => match self.line_to_char(*line) {
                Some(idx) => Dot::Cur {
                    c: Cur { idx: idx + *col },
                },
                None => Dot::from_char_indices(0, 0),
            },

            Regex(re) => {
                let from = cur_dot.last_cur().idx;
                let to = self.len_chars();

                match re.match_iter(&mut self.iter_between(from, to), from) {
                    Some(m) => {
                        let (from, to) = m.loc();
                        Dot::from_char_indices(from, to)
                    }
                    None => Dot::from_char_indices(0, 0),
                }
            }
        }
    }

    fn map_inclusive_range(&self, from: &mut SimpleAddr, to: &mut SimpleAddr) -> Dot {
        let d = self.map_simple_addr(from, self.current_dot());
        let c1 = d.first_cur();
        let c2 = self.map_simple_addr(to, d).last_cur();

        Dot::Range {
            r: Range::from_cursors(c1, c2, false),
        }
    }

    fn map_dot_expr(&self, d: &mut DotExpression) -> Dot {
        match d {
            DotExpression::Explicit(d) => *d,
            DotExpression::Simple(addr) => self.map_simple_addr(addr, self.current_dot()),
            DotExpression::Compound(CompoundAddr::InclusiveRange(from, to)) => {
                self.map_inclusive_range(from, to)
            }
        }
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
        self.remove(from..to)
    }

    fn map_from_to(&self, line_from: usize, line_to: Option<usize>) -> Dot {
        let from = self
            .try_line_to_char(line_from)
            .unwrap_or_else(|_| self.len_chars());
        let to = match line_to {
            Some(n) => match self.try_line_to_char(n) {
                Ok(ix) => ix + self.line(n).len_chars().saturating_sub(1),
                Err(_) => self.len_chars(),
            },
            None => self.len_chars(),
        };

        Dot::from_char_indices(from, to)
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        self.try_line_to_char(line_idx).ok()
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
        self.handle_action(
            Action::InsertString { s: s.to_string() },
            &Config::default(),
        );
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.dot = Dot::Range {
            r: Range::from_cursors(
                Cur { idx: from },
                Cur {
                    idx: to.saturating_sub(1),
                },
                true,
            ),
        }
        .collapse_null_range();
        self.handle_action(Action::Delete, &Config::default());
    }

    fn map_from_to(&self, line_from: usize, line_to: Option<usize>) -> Dot {
        let from = self
            .txt
            .try_line_to_char(line_from)
            .unwrap_or_else(|_| self.txt.len_chars());
        let to = match line_to {
            Some(n) => match self.txt.try_line_to_char(n) {
                Ok(ix) => ix + self.txt.line(n).len_chars().saturating_sub(1),
                Err(_) => self.txt.len_chars(),
            },
            None => self.txt.len_chars(),
        };

        Dot::from_char_indices(from, to)
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        self.txt.try_line_to_char(line_idx).ok()
    }

    fn current_dot(&self) -> Dot {
        self.dot
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
        self.r.borrow_mut().remove(from..to)
    }

    fn map_from_to(&self, line_from: usize, line_to: Option<usize>) -> Dot {
        let n = match line_to {
            Some(n) => n,
            None => line_from,
        };

        let cur_len = self.contents().len_lines();
        if n > cur_len {
            for _ in cur_len..=n {
                self.try_read_next_line();
                if self.is_closed() {
                    break;
                }
            }
        }

        let r = self.contents();
        let from = r
            .try_line_to_char(line_from)
            .unwrap_or_else(|_| r.len_chars());
        let to = match line_to {
            Some(n) => match r.try_line_to_char(n) {
                Ok(ix) => ix + r.line(n).len_chars().saturating_sub(1),
                Err(_) => r.len_chars(),
            },
            None => r.len_chars(),
        };

        Dot::from_char_indices(from, to)
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        let cur_len = self.contents().len_lines();
        if line_idx > cur_len {
            for _ in cur_len..=line_idx {
                self.try_read_next_line();
                if self.is_closed() {
                    break;
                }
            }
        }

        self.contents().try_line_to_char(line_idx).ok()
    }

    fn len_chars(&self) -> usize {
        self.r.borrow().len_chars()
    }
}
