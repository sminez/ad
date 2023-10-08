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

            RevIdxChars {
                inner,
                idx: idx + 1,
            }
            .peekable()
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

    // pub fn consume_until<I, T>(cond: Cond<T>, it: &mut Peekable<I>) -> Option<usize>
    // where
    //     I: Iterator<Item = (usize, T)>,
    // {
    //     loop {
    //         match it.next() {
    //             Some((i, c)) if cond(&c) => return Some(i),
    //             Some(_) => (),
    //             None => return None,
    //         }
    //     }
    // }

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
}

#[cfg(test)]
mod tests {
    use super::consumer::*;
    use simple_test_case::test_case;

    pub fn alphanumeric(ch: &char) -> bool {
        ch.is_alphanumeric()
    }

    // #[test_case("a thing", Some(0), 6; "initially matching on boundary")]
    // #[test_case("an item", Some(0), 6; "initially matching not on boundary")]
    // #[test_case("    foo", Some(4), 2; "not initially matching")]
    // #[test_case("       ", None, 0; "never matching")]
    // #[test]
    // fn consume_until_works(s: &str, expected: Option<usize>, remaining: usize) {
    //     let mut it = s.chars().enumerate().peekable();
    //     let res = consume_until(alphanumeric, &mut it);

    //     assert_eq!(res, expected);
    //     assert_eq!(it.count(), remaining);
    // }

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
}
