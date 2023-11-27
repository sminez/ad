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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    EmptyParens,
    EmptyRegex,
    InvalidClass,
    InvalidEscape(char),
    InvalidRepetition,
    ReTooLong,
    TooManyParens,
    UnbalancedAlt,
    UnbalancedParens,
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
    escape!(escapes, '\\', '\'', '"', '^', '$');
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
    fn new(negated: bool, chars: Vec<char>, ranges: Vec<(char, char)>) -> Self {
        Self {
            negated,
            chars,
            ranges,
        }
    }

    fn try_parse(it: &mut Peekable<Chars>) -> Result<Self, Error> {
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

                '-' => {
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

fn next_char(it: &mut Peekable<Chars>) -> Result<Option<(char, bool)>, Error> {
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
