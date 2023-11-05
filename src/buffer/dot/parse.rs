//! Parsing of dot expressions so that they can be used to set Dot for a given buffer.
//!
//! A `DotExpression` can be parsed from a valid address string. The syntax for these expressions
//! is adapted from the syntax supported by the Sam text editor from Rob Pike and supports both
//! absolute and relative addressing based on the current `Buffer` and `Dot`.
//!
//! Addresses (and by extension DotExpressions) identify substrings within a larger string. The
//! `Dot` for a given buffer is simply the currently selected address to which editing actions will
//! be applied.
//!
//! ## Address syntax
//!
//! ### Simple addresses
//!
//!```text
//! .      => current dot
//! e1     => set dot to e1
//! e1,    => set dot to e1_start..=EOF
//! e1,e2  => set dot to e1_start..=e2_end
//! ```
use super::Dot;
use crate::{
    regex::{self, Regex},
    util::parse_num,
};
use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ParseError {
    InvalidRegex(regex::Error),
    NotADotExpression,
    UnclosedDelimiter,
    UnexpectedCharacter(char),
}

/// A DotExpression can be evaluated by a Buffer to produce a valid Dot for using in future editing
/// actions. The `Explicit` variant is used to handle internal operations that need to provide a
/// DotExpression (as opposed to parsed user input) where we already have a fully evaluated Dot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DotExpression {
    Simple(SimpleAddr),
    Compound(CompoundAddr),
    Explicit(Dot),
}

// Primatives for building out addresses
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleAddr {
    Current,
    Bof,
    Eof,
    Line(usize),
    LineAndColumn(usize, usize),
    Regex(Regex),
}

impl SimpleAddr {
    pub(crate) fn parse(it: &mut Peekable<Chars>) -> Result<Self, ParseError> {
        match it.peek() {
            Some('.') => {
                it.next();
                Ok(Self::Current)
            }

            Some('0') => {
                it.next();
                Ok(Self::Bof)
            }

            Some('$') => {
                it.next();
                Ok(Self::Eof)
            }

            Some(&c) if c.is_ascii_digit() => {
                it.next();
                let line = parse_num(c, it);
                match it.peek() {
                    Some(':') => {
                        it.next();
                        match it.next() {
                            Some(c) if c.is_ascii_digit() => {
                                let col = parse_num(c, it);
                                Ok(Self::LineAndColumn(
                                    line.saturating_sub(1),
                                    col.saturating_sub(1),
                                ))
                            }
                            Some(c) => Err(ParseError::UnexpectedCharacter(c)),
                            None => Err(ParseError::NotADotExpression),
                        }
                    }
                    _ => Ok(Self::Line(line.saturating_sub(1))),
                }
            }

            Some('/') => {
                it.next();
                Ok(Self::Regex(parse_delimited_regex(it)?))
            }

            _ => Err(ParseError::NotADotExpression),
        }
    }
}

/// Sam supports a variety of ways for setting the dot based on the current Dot by using offsets or
/// regext searches forward and backwards. I'm avoiding implementing these for now as they add some
/// additional complexity that I'm not sure I'll make use of yet.
/// As and when I need the relative addressing I'll get them added in.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompoundAddr {
    InclusiveRange(SimpleAddr, SimpleAddr),
    // ForwardFrom(SimpleAddr, SimpleAddr),
    // BackwardFrom(SimpleAddr, SimpleAddr),
}

impl DotExpression {
    pub(crate) fn full() -> Self {
        DotExpression::Compound(CompoundAddr::InclusiveRange(
            SimpleAddr::Bof,
            SimpleAddr::Eof,
        ))
    }

    /// Attempt to parse a valid dot expression from a character stream
    pub(crate) fn parse(it: &mut Peekable<Chars>) -> Result<Self, ParseError> {
        let start = match SimpleAddr::parse(it) {
            Ok(exp) => Some(exp),
            // If the following char is a ',' we substitute BOF for a missing start
            Err(ParseError::NotADotExpression) => None,
            Err(e) => return Err(e),
        };

        match it.peek() {
            // If we didn't have an starting addr then this expression is invalid, otherwise
            // we just have 'start' as a simple addr
            Some(' ') | None => Ok(DotExpression::Simple(
                start.ok_or(ParseError::NotADotExpression)?,
            )),

            // Compound addrs default their first element to Bof and last to Eof
            Some(',') => {
                it.next();
                let start = start.unwrap_or(SimpleAddr::Bof);
                let end = match SimpleAddr::parse(it) {
                    Ok(exp) => exp,
                    Err(ParseError::NotADotExpression) => SimpleAddr::Eof,
                    Err(e) => return Err(e),
                };

                Ok(DotExpression::Compound(CompoundAddr::InclusiveRange(
                    start, end,
                )))
            }

            _ => Err(ParseError::NotADotExpression),
        }
    }
}

fn parse_delimited_regex(it: &mut Peekable<Chars>) -> Result<Regex, ParseError> {
    let mut s = String::new();
    let mut prev = '/';

    for ch in it {
        if ch == '/' && prev != '\\' {
            return Regex::compile(&s).map_err(ParseError::InvalidRegex);
        }
        s.push(ch);
        prev = ch;
    }

    Err(ParseError::UnclosedDelimiter)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::{CompoundAddr::*, DotExpression::*, SimpleAddr::*};
    use crate::regex::Regex;
    use simple_test_case::test_case;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    #[test_case("3", Simple(Line(2)); "single line")]
    #[test_case(".", Simple(Current); "current dot")]
    #[test_case("0", Simple(Bof); "begining of file")]
    #[test_case("$", Simple(Eof); "end of file")]
    #[test_case("3:9", Simple(LineAndColumn(2, 8)); "line and column cursor")]
    #[test_case("/foo/", Simple(Regex(re("foo"))); "regex")]
    #[test_case(",", Compound(InclusiveRange(Bof, Eof)); "full")]
    #[test_case("5,", Compound(InclusiveRange(Line(4), Eof)); "from n")]
    #[test_case("50,", Compound(InclusiveRange(Line(49), Eof)); "from n multi digit")]
    #[test_case("5,9", Compound(InclusiveRange(Line(4), Line(8))); "from n to m")]
    #[test_case("25,90", Compound(InclusiveRange(Line(24), Line(89))); "from n to m multi digit")]
    #[test_case("/foo/,/bar/", Compound(InclusiveRange(Regex(re("foo")), Regex(re("bar")))); "regex range")]
    #[test]
    fn parse_works(s: &str, expected: DotExpression) {
        let dot = DotExpression::parse(&mut s.chars().peekable()).expect("valid input");
        assert_eq!(dot, expected);
    }
}
