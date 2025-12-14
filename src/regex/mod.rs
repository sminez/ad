//! A simple regex engine for operating on character streams and supporting
//! the Sam text editor's structural regular expressions.
//!
//! Thompson's original paper on writing a regex engine can be found here:
//!   <https://dl.acm.org/doi/pdf/10.1145/363347.363387>
use std::{fmt, iter::Peekable, str::Chars};

mod ast;
mod compile;
mod haystack;
mod matches;
mod stream;
mod vm;

pub use haystack::Haystack;
pub use matches::Match;
pub use stream::{CachingStream, CachingStreamIter};
pub use vm::{Regex, RevRegex};

/// Errors that can be returned by the regex engine
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// Empty parens
    EmptyParens,
    /// Empty string used when creating a [Regex]
    EmptyRegex,
    /// Invalid regex class
    InvalidClass,
    /// Invalid escape sequence
    InvalidEscape(char),
    /// Invalid repetition pattern
    InvalidRepetition,
    /// The provided regex is too long
    ReTooLong,
    /// Alternation without a right hand side
    UnbalancedAlt,
    /// Unbalanced parens
    UnbalancedParens,
    /// Group name without a closing '<'
    UnclosedGroupName(String),
    /// Invalid group qualifier following (?...)
    UnknownGroupQualifier(char),
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyParens => write!(f, "empty parens"),
            Self::EmptyRegex => write!(f, "empty regular expression"),
            Self::InvalidClass => write!(f, "invalid class"),
            Self::InvalidEscape(c) => write!(f, "invalid escaped character {c:?}"),
            Self::InvalidRepetition => write!(f, "invalid repetition"),
            Self::ReTooLong => write!(f, "regex too long"),
            Self::UnbalancedAlt => write!(f, "alternation had no right hand side"),
            Self::UnbalancedParens => write!(f, "unbalanced parens"),
            Self::UnclosedGroupName(s) => write!(f, "unclosed group name {s:?}"),
            Self::UnknownGroupQualifier(c) => write!(f, "unknown group qualifier {c:?}"),
        }
    }
}

/// Helper for converting characters to 0 based inicies for looking things up in caches.
const fn char_ix(ch: char) -> usize {
    ((ch as u16) & 0xFF) as usize
}

const fn init_escapes() -> [Option<char>; 256] {
    macro_rules! escape {
        ($escapes:expr, $($ch:expr),+) => {
            $($escapes[char_ix($ch)] = Some($ch);)+
        };
        ($escapes:expr, $($ch:expr => $esc:expr),+) => {
            $($escapes[char_ix($ch)] = Some($esc);)+
        };
    }

    let mut escapes = [None; 256];
    escape!(
        escapes, '*', '+', '?', '.', '@', '(', ')', '[', ']', '{', '}', '|'
    );
    escape!(escapes, '\\', '\'', '"', '^', '$', '-');
    escape!(escapes, 'b', 'B', 'd', 'D', 'w', 'W', 's', 'S');
    escape!(escapes, 'n'=>'\n', 'r'=>'\r', 't'=>'\t');

    escapes
}

/// Supported escape sequences
const ESCAPES: [Option<char>; 256] = init_escapes();

#[derive(Debug, Clone, PartialEq, Eq)]
struct CharClass {
    pub(crate) negated: bool,
    pub(crate) chars: Vec<char>,
    pub(crate) ranges: Vec<(char, char)>,
}

impl CharClass {
    fn try_parse(it: &mut Peekable<Chars<'_>>) -> Result<Self, Error> {
        let mut next = || next_char(it)?.ok_or(Error::InvalidClass);
        let (mut ch, _) = next()?;

        let negated = ch == '^';
        if negated {
            (ch, _) = next()?
        };
        let mut chars = vec![ch];
        let mut ranges = vec![];

        loop {
            let (ch, escaped) = next()?;
            match ch {
                ']' if !escaped => break,

                '-' if !escaped => {
                    let start = chars.pop().ok_or(Error::InvalidClass)?;
                    let (end, _) = next()?;
                    if start as u32 >= end as u32 {
                        return Err(Error::InvalidClass);
                    }

                    ranges.push((start, end));
                }

                ch => chars.push(ch),
            }
        }

        Ok(Self {
            negated,
            chars,
            ranges,
        })
    }

    // Negated classes still don't match a newline
    #[inline]
    fn matches(&self, ch: char) -> bool {
        if self.negated && ch == '\n' {
            return false;
        }

        let res = self.chars.contains(&ch)
            || self
                .ranges
                .iter()
                .any(|&(start, end)| ch >= start && ch <= end);

        if self.negated { !res } else { res }
    }

    fn size(&self) -> usize {
        self.chars.len()
            + self
                .ranges
                .iter()
                .map(|(s, e)| (*e as u32 - *s as u32 + 1) as usize)
                .sum::<usize>()
    }
}

fn next_char(it: &mut Peekable<Chars<'_>>) -> Result<Option<(char, bool)>, Error> {
    match it.next() {
        Some('\\') => (),
        Some(ch) => return Ok(Some((ch, false))),
        None => return Ok(None),
    }

    let ch = match it.next() {
        Some(ch) => ch,
        None => return Err(Error::InvalidEscape('\0')),
    };

    match ESCAPES[char_ix(ch)] {
        Some(ch) => Ok(Some((ch, true))),
        None => Err(Error::InvalidEscape(ch)),
    }
}

mod impl_structex {
    use super::*;
    use crate::buffer::{Buffer, GapBuffer, Slice};
    use std::{io, ops::Range};
    use structex::re::{Haystack, RawCaptures, RegexEngine, Sliceable, Writable};

    impl RegexEngine for Regex {
        type CompileError = Error;

        fn compile(re: &str) -> Result<Self, Self::CompileError> {
            Regex::compile(re)
        }
    }

    impl Haystack<Regex> for str {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.matches_between(&self, from, to)
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let m = re.find_between(&self, from, to)?;

            Some(RawCaptures::new(m.iter_locs()))
        }
    }

    impl Haystack<Regex> for GapBuffer {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.matches_between(self, from, to)
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let m = re.find_between(self, from, to)?;

            Some(RawCaptures::new(m.iter_locs()))
        }
    }

    impl Sliceable for GapBuffer {
        type Slice<'h>
            = Slice<'h>
        where
            Self: 'h;

        fn char_at(&self, byte_offset: usize) -> Option<char> {
            self.get_char_at(byte_offset)
        }

        fn slice(&self, range: Range<usize>) -> Self::Slice<'_> {
            self.slice_from_byte_offsets(range.start, range.end)
        }

        fn max_len(&self) -> usize {
            self.len()
        }
    }

    impl Writable for GapBuffer {
        fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
        where
            W: io::Write,
        {
            let (l, r) = self.as_byte_slices();
            w.write_all(l)?;
            w.write_all(r)?;

            Ok(self.len())
        }
    }

    impl<'a> Writable for Slice<'a> {
        fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
        where
            W: std::io::Write,
        {
            let (l, r) = self.as_slices();
            w.write_all(l)?;
            w.write_all(r)?;

            Ok(l.len() + r.len())
        }
    }

    impl Haystack<Regex> for Buffer {
        fn is_match_between(&self, re: &Regex, from: usize, to: usize) -> bool {
            re.matches_between(self, from, to)
        }

        fn captures_between(&self, re: &Regex, from: usize, to: usize) -> Option<RawCaptures> {
            let m = re.find_between(self, from, to)?;

            Some(RawCaptures::new(m.iter_locs()))
        }
    }

    impl Sliceable for Buffer {
        type Slice<'h>
            = Slice<'h>
        where
            Self: 'h;

        fn char_at(&self, byte_offset: usize) -> Option<char> {
            self.txt.get_char_at(byte_offset)
        }

        fn slice(&self, range: Range<usize>) -> Self::Slice<'_> {
            self.txt.slice_from_byte_offsets(range.start, range.end)
        }

        fn max_len(&self) -> usize {
            self.txt.len()
        }
    }

    impl Writable for Buffer {
        fn write_to<W>(&self, w: &mut W) -> io::Result<usize>
        where
            W: io::Write,
        {
            let (l, r) = self.txt.as_byte_slices();
            w.write_all(l)?;
            w.write_all(r)?;

            Ok(self.txt.len())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case("_", &['_'], &[]; "single underscore")]
    #[test_case("abc_", &['a', 'b', 'c', '_'], &[]; "chars")]
    #[test_case("a-z", &[], &[('a', 'z')]; "single range")]
    #[test_case("a-zA-Z", &[], &[('a', 'z'), ('A', 'Z')]; "multi range")]
    #[test_case("a-z_./]", &['_', '.', '/'], &[('a', 'z')]; "compound")]
    #[test_case("a-zA-Z_\\-.]", &['_', '-', '.'], &[('a', 'z'), ('A', 'Z')]; "compound escaped dash")]
    #[test]
    fn parsing_classes_works(raw: &str, chars: &[char], ranges: &[(char, char)]) {
        // The outer regex parser consumes the initial '[' before passing through so test cases
        // look a little lopsided due to missing this.
        for (s, negated) in [(format!("{raw}]"), false), (format!("^{raw}]"), true)] {
            let cls = CharClass::try_parse(&mut s.chars().peekable()).unwrap();
            let expected = CharClass {
                negated,
                chars: chars.to_vec(),
                ranges: ranges.to_vec(),
            };

            assert_eq!(cls, expected, "negated={negated}");
        }
    }

    // Each of the inputs here is missing the opening '[' as that is consumed prior to calling
    // `CharClass::try_parse`
    #[test_case("z-a]"; "backwards alpha")]
    #[test_case("Z-A]"; "backwards upper alpha")]
    #[test_case("9-0]"; "backwards numeric")]
    #[test_case("a-a]"; "equal alpha endpoints")]
    #[test_case("5-5]"; "equal numeric endpoints")]
    #[test_case("a-z9-0]"; "valid then invalid range")]
    #[test_case("\u{00FF}-\u{0000}]"; "backwards unicode")]
    #[test]
    fn invalid_character_ranges_error(s: &str) {
        assert!(CharClass::try_parse(&mut s.chars().peekable()).is_err());
    }
}
