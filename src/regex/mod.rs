//! A simple regex engine for operating on character streams and supporting
//! the Sam text editor's structural regular expressions.
//!
//! Thompson's original paper on writing a regex engine can be found here:
//!   https://dl.acm.org/doi/pdf/10.1145/363347.363387
use std::{iter::Peekable, str::Chars};

mod ast;
mod compile;
mod matches;
mod vm;

pub use matches::{Match, MatchIter};
pub use vm::Regex;

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
    /// Too many parens in the provided regex
    TooManyParens,
    /// Alternation without a right hand side
    UnbalancedAlt,
    /// Unbalanced parens
    UnbalancedParens,
    /// Group name without a closing paren
    UnclosedGroupName(String),
    /// Invalid group qualifier following (?...)
    UnknownGroupQualifier(char),
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
    escape!(escapes, '*', '+', '?', '.', '@', '(', ')', '[', ']', '{', '}', '|');
    escape!(escapes, '\\', '\'', '"', '^', '$', '-');
    escape!(escapes, 'b', 'B', 'd', 'D', 'w', 'W', 's', 'S');
    escape!(escapes, 'n'=>'\n', 'r'=>'\r', 't'=>'\t');

    escapes
}

/// Supported escape sequences
const ESCAPES: [Option<char>; 256] = init_escapes();

#[derive(Debug, Clone, PartialEq, Eq)]
struct CharClass {
    negated: bool,
    chars: Vec<char>,
    ranges: Vec<(char, char)>,
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

        if self.negated {
            !res
        } else {
            res
        }
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
}
