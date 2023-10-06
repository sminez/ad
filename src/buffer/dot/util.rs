pub(super) mod iter {
    use crate::buffer::{dot::Cur, Buffer};
    use ropey::{
        iter::{Chars, Lines},
        RopeSlice,
    };
    use std::iter::Peekable;

    pub struct IdxChars<'b> {
        inner: Chars<'b>,
        idx: usize,
    }

    impl<'b> IdxChars<'b> {
        pub fn new<'a: 'b>(cur: Cur, b: &'a Buffer) -> Peekable<Self> {
            let idx = cur.as_char_idx(b);
            let inner: Chars<'b> = b.txt.chars_at(idx);

            IdxChars { inner, idx }.peekable()
        }
    }

    impl<'b> Iterator for IdxChars<'b> {
        type Item = (usize, char);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|c| {
                let res = (self.idx, c);
                self.idx += 1;

                res
            })
        }
    }

    pub struct RevIdxChars<'b> {
        inner: Chars<'b>,
        idx: usize,
    }

    impl<'b> RevIdxChars<'b> {
        pub fn new(cur: Cur, b: &'b Buffer) -> Peekable<Self> {
            let idx = cur.as_char_idx(b);
            let mut inner = b.txt.chars_at(idx);

            // crank the iterator forward one character so that when we iterate backwards the first
            // character we yeild is at the cursor position
            inner.next();

            RevIdxChars { inner, idx }.peekable()
        }
    }

    impl<'b> Iterator for RevIdxChars<'b> {
        type Item = (usize, char);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.prev().map(|c| {
                self.idx -= 1;
                (self.idx, c)
            })
        }
    }

    pub struct IdxLines<'b> {
        inner: Lines<'b>,
        idx: usize,
    }

    impl<'b> Iterator for IdxLines<'b> {
        type Item = (usize, RopeSlice<'b>);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|s| {
                let res = (self.idx, s);
                self.idx += 1;

                res
            })
        }
    }

    impl<'b> IdxLines<'b> {
        pub fn new(cur: Cur, b: &'b Buffer) -> Peekable<Self> {
            let inner = b.txt.lines_at(cur.y);

            IdxLines { inner, idx: cur.y }.peekable()
        }
    }

    pub struct RevIdxLines<'b> {
        inner: Lines<'b>,
        idx: usize,
    }

    impl<'b> Iterator for RevIdxLines<'b> {
        type Item = (usize, RopeSlice<'b>);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.prev().map(|line| {
                self.idx -= 1;
                (self.idx, line)
            })
        }
    }

    impl<'b> RevIdxLines<'b> {
        pub fn new(cur: Cur, b: &'b Buffer) -> Peekable<Self> {
            let mut inner = b.txt.lines_at(cur.y);

            // crank the iterator forward one character so that when we iterate backwards the first
            // line we yeild is at the cursor position
            inner.next();
            let idx = cur.y + 1;

            RevIdxLines { inner, idx }.peekable()
        }
    }
}

pub(super) mod cond {
    use ropey::RopeSlice;

    pub type Cond<T> = fn(&T) -> bool;

    pub fn whitespace(c: &char) -> bool {
        c.is_whitespace()
    }

    pub fn alphanumeric(c: &char) -> bool {
        c.is_alphanumeric()
    }

    pub fn non_alphanumeric(c: &char) -> bool {
        !c.is_alphanumeric()
    }

    pub fn blank_line(line: &RopeSlice<'_>) -> bool {
        line.chars().all(|c| c.is_whitespace())
    }

    pub fn non_blank_line(line: &RopeSlice<'_>) -> bool {
        line.chars().any(|c| !c.is_whitespace())
    }
}

pub(super) mod consumer {
    use super::cond::Cond;
    use std::iter::Peekable;

    pub type Consumer<I, T> = fn(Cond<T>, &mut Peekable<I>) -> Option<usize>;

    pub fn consume_until<I, T>(cond: Cond<T>, it: &mut Peekable<I>) -> Option<usize>
    where
        I: Iterator<Item = (usize, T)>,
    {
        loop {
            match it.next() {
                Some((i, c)) if cond(&c) => return Some(i),
                Some(_) => (),
                None => return None,
            }
        }
    }

    pub fn consume_while<I, T>(cond: Cond<T>, it: &mut Peekable<I>) -> Option<usize>
    where
        I: Iterator<Item = (usize, T)>,
    {
        let mut current = None;
        loop {
            match it.peek() {
                Some((_, c)) if cond(c) => current = it.next(),
                _ => return current.map(|(i, _)| i),
            }
        }
    }

    /// If the given condition doesn't currently hold or if currently holds, but the next character
    /// would break it then consume until the condition holds again.
    pub fn consume_on_boundary<I, T>(cond: Cond<T>, it: &mut Peekable<I>) -> Option<usize>
    where
        I: Iterator<Item = (usize, T)>,
    {
        match (it.next(), it.peek()) {
            // (x, x+1) => (cond holds, cond does not hold): consume until we hit cond
            (Some((_, c1)), Some((_, c2))) if cond(&c1) && !cond(c2) => consume_until(cond, it),
            // x => cond does not hold: consume until we hit cond
            (Some((_, c)), _) if !cond(&c) => consume_until(cond, it),
            // Condition is holding for this position and the next (first case covers the next item
            // breaking the condition) so return the current position.
            (Some((i, _)), _) => Some(i),
            // Out of input
            (None, _) => None,
        }
    }

    /// Run a list of consumer functions over an iterator to locate a desired point in a buffer.
    ///
    /// If the final consumer returns `None` then the base case will be returned, otherwise the index
    /// located by that consumer will be returned.
    pub fn chain_consume<const C: usize, I, T>(
        mut it: Peekable<I>,
        funcs: [(Consumer<I, T>, Cond<T>); C],
    ) -> Option<usize>
    where
        I: Iterator<Item = (usize, T)>,
    {
        let mut res = None;
        for (f, cond) in funcs.into_iter() {
            res = f(cond, &mut it);
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use super::{cond::*, consumer::*};
    use simple_test_case::test_case;

    #[test_case("a thing", Some(0), 6; "initially matching on boundary")]
    #[test_case("an item", Some(0), 6; "initially matching not on boundary")]
    #[test_case("    foo", Some(4), 2; "not initially matching")]
    #[test_case("       ", None, 0; "never matching")]
    #[test]
    fn consume_until_works(s: &str, expected: Option<usize>, remaining: usize) {
        let mut it = s.chars().enumerate().peekable();
        let res = consume_until(alphanumeric, &mut it);

        assert_eq!(res, expected);
        assert_eq!(it.count(), remaining);
    }

    #[test_case("a thing", Some(0), 6; "initially matching on boundary")]
    #[test_case("an item", Some(1), 5; "initially matching not on boundary")]
    #[test_case("    foo", None, 7; "not initially matching")]
    #[test_case("       ", None, 7; "never matching")]
    #[test]
    fn consume_while_works(s: &str, expected: Option<usize>, remaining: usize) {
        let mut it = s.chars().enumerate().peekable();
        let res = consume_while(alphanumeric, &mut it);

        assert_eq!(res, expected);
        assert_eq!(it.count(), remaining);
    }

    #[test_case("a thing", Some(2), 4; "initially matching on boundary")]
    #[test_case("an item", Some(0), 6; "initially matching not on boundary")]
    #[test_case("    foo", Some(4), 2; "not initially matching")]
    #[test_case("       ", None, 0; "never matching")]
    #[test]
    fn consume_on_boundary_works(s: &str, expected: Option<usize>, remaining: usize) {
        let mut it = s.chars().enumerate().peekable();
        let res = consume_on_boundary(alphanumeric, &mut it);

        assert_eq!(res, expected);
        assert_eq!(it.count(), remaining);
    }
}
