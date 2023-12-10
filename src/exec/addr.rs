//! Parsing of addresses so that they can be used to set Dot for a given buffer.
//!
//! An `Addr` can be parsed from a valid address string. The syntax for these expressions
//! is adapted from the syntax supported by the Sam text editor from Rob Pike and supports both
//! absolute and relative addressing based on the current `Buffer` and `Dot`.
//!
//! Addresses identify substrings within a larger string. The `Dot` for a given buffer is simply
//! the currently selected address to which editing actions will be applied.
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
use crate::{
    buffer::Buffer,
    dot::{Cur, Dot, Range},
    exec::char_iter::IterBoundedChars,
    regex::{self, Regex},
    util::parse_num,
};
use ropey::Rope;
use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    InvalidRegex(regex::Error),
    InvalidSuffix,
    NotAnAddress,
    UnclosedDelimiter,
    UnexpectedCharacter(char),
}

/// An Addr can be evaluated by a Buffer to produce a valid Dot for using in future editing
/// actions. The `Explicit` variant is used to handle internal operations that need to provide a
/// Addr (as opposed to parsed user input) where we already have a fully evaluated Dot.
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Addr {
    Explicit(Dot),
    Simple(SimpleAddr),
    Compound(SimpleAddr, SimpleAddr),
}

impl Addr {
    pub fn full() -> Self {
        Addr::Compound(AddrBase::Bof.into(), AddrBase::Eof.into())
    }

    /// Attempt to parse a valid dot expression from a character stream
    pub fn parse(it: &mut Peekable<Chars>) -> Result<Self, ParseError> {
        let start = match SimpleAddr::parse(it) {
            Ok(exp) => Some(exp),
            // If the following char is a ',' we substitute BOF for a missing start
            Err(ParseError::NotAnAddress) => None,
            Err(e) => return Err(e),
        };

        match it.peek() {
            // If we didn't have an starting addr then this expression is invalid, otherwise
            // we just have 'start' as a simple addr
            Some(' ') | None => Ok(Addr::Simple(start.ok_or(ParseError::NotAnAddress)?)),

            // Compound addrs default their first element to Bof and last to Eof
            Some(',') => {
                it.next();
                let start = start.unwrap_or(AddrBase::Bof.into());
                let end = match AddrBase::parse(it) {
                    Ok(exp) => exp,
                    Err(ParseError::NotAnAddress) => AddrBase::Eof,
                    Err(e) => return Err(e),
                };

                Ok(Addr::Compound(start, end.into()))
            }

            _ => Err(ParseError::NotAnAddress),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleAddr {
    base: AddrBase,
    to_eol: bool,
    to_bol: bool,
}

impl SimpleAddr {
    fn parse(it: &mut Peekable<Chars>) -> Result<Self, ParseError> {
        let base = AddrBase::parse(it)?;
        let mut suffix = String::with_capacity(2);

        for _ in 0..2 {
            if let Some(c @ '-' | c @ '+') = it.peek() {
                suffix.push(*c);
                it.next();
            }
        }

        let (to_eol, to_bol) = match suffix.as_str() {
            "++" | "--" => return Err(ParseError::InvalidSuffix),
            "-+" | "+-" => (true, true),
            "-" => (false, true),
            "+" => (true, false),
            _ => (false, false),
        };

        Ok(Self {
            base,
            to_eol,
            to_bol,
        })
    }
}

/// Primatives for building out addresses
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddrBase {
    /// .
    Current,
    /// 0
    Bof,
    /// $
    Eof,
    /// n
    Line(usize),
    /// -/+n
    RelativeLine(isize),
    /// #n
    Char(usize),
    /// -/+#n
    RelativeChar(isize),
    /// n:m
    LineAndColumn(usize, usize),
    /// /re/ or +/re/
    Regex(Regex),
    /// -/re/
    RegexBack(Regex),
}

impl From<AddrBase> for SimpleAddr {
    fn from(base: AddrBase) -> Self {
        Self {
            base,
            to_eol: false,
            to_bol: false,
        }
    }
}

enum Dir {
    Fwd,
    Bck,
}

impl AddrBase {
    pub(crate) fn parse(it: &mut Peekable<Chars>) -> Result<Self, ParseError> {
        let dir = match it.peek() {
            Some('-') => {
                it.next();
                Some(Dir::Bck)
            }
            Some('+') => {
                it.next();
                Some(Dir::Fwd)
            }
            _ => None,
        };

        match (it.peek(), dir) {
            (Some('.' | '0' | '$'), Some(_)) => Err(ParseError::NotAnAddress),

            (Some('.'), None) => {
                it.next();
                Ok(Self::Current)
            }

            (Some('0'), None) => {
                it.next();
                Ok(Self::Bof)
            }

            (Some('$'), None) => {
                it.next();
                Ok(Self::Eof)
            }

            (Some('#'), dir) => {
                it.next();
                let ix = match it.peek() {
                    Some(&c) if c.is_ascii_digit() => {
                        it.next();
                        parse_num(c, it)
                    }
                    _ => return Err(ParseError::NotAnAddress),
                };

                match dir {
                    None => Ok(Self::Char(ix)),
                    Some(Dir::Fwd) => Ok(Self::RelativeChar(ix as isize)),
                    Some(Dir::Bck) => Ok(Self::RelativeChar(-(ix as isize))),
                }
            }

            (Some(&c), dir) if c.is_ascii_digit() => {
                it.next();
                let line = parse_num(c, it);

                match (it.peek(), dir) {
                    (Some(':'), Some(_)) => Err(ParseError::NotAnAddress),

                    (Some(':'), None) => {
                        it.next();
                        match it.next() {
                            Some(c) if c.is_ascii_digit() => {
                                let col = parse_num(c, it).saturating_sub(1);
                                Ok(Self::LineAndColumn(line.saturating_sub(1), col))
                            }
                            Some(c) => Err(ParseError::UnexpectedCharacter(c)),
                            None => Err(ParseError::NotAnAddress),
                        }
                    }

                    (_, None) => Ok(Self::Line(line.saturating_sub(1))),
                    (_, Some(Dir::Fwd)) => Ok(Self::RelativeLine(line as isize)),
                    (_, Some(Dir::Bck)) => Ok(Self::RelativeLine(-(line as isize))),
                }
            }

            (Some('/'), dir) => {
                it.next();
                parse_delimited_regex(it, dir.unwrap_or(Dir::Fwd))
            }

            _ => Err(ParseError::NotAnAddress),
        }
    }
}

fn parse_delimited_regex(it: &mut Peekable<Chars>, dir: Dir) -> Result<AddrBase, ParseError> {
    let mut s = String::new();
    let mut prev = '/';

    for ch in it {
        if ch == '/' && prev != '\\' {
            return match dir {
                Dir::Fwd => Ok(AddrBase::Regex(
                    Regex::compile(&s).map_err(ParseError::InvalidRegex)?,
                )),
                Dir::Bck => Ok(AddrBase::RegexBack(
                    Regex::compile_reverse(&s).map_err(ParseError::InvalidRegex)?,
                )),
            };
        }
        s.push(ch);
        prev = ch;
    }

    Err(ParseError::UnclosedDelimiter)
}

/// Something that is capable of resolving an Addr to a Dot
pub trait Address: IterBoundedChars {
    /// This only really makes sense for use with a buffer but is supported
    /// so that don't need to special case running programs against an in-editor
    /// buffer vs stdin or a file read from disk.
    fn current_dot(&self) -> Dot;
    fn len_chars(&self) -> usize;
    fn line_to_char(&self, line_idx: usize) -> Option<usize>;
    fn char_to_line(&self, char_idx: usize) -> Option<usize>;
    fn char_to_line_end(&self, char_idx: usize) -> Option<usize>;
    fn char_to_line_start(&self, char_idx: usize) -> Option<usize>;

    fn map_addr(&self, a: &mut Addr) -> Dot {
        let dot = match a {
            Addr::Explicit(d) => Some(*d),
            Addr::Simple(a) => self.map_simple_addr(a, self.current_dot()),
            Addr::Compound(from, to) => self.map_compound_addr(from, to),
        };

        dot.unwrap_or_default()
    }

    fn full_line(&self, line_idx: usize) -> Option<Dot> {
        let from = self.line_to_char(line_idx)?;
        let to = self.char_to_line_end(from)?.saturating_sub(1);

        Some(Dot::from_char_indices(from, to))
    }

    fn map_simple_addr(&self, addr: &mut SimpleAddr, cur_dot: Dot) -> Option<Dot> {
        use AddrBase::*;

        let mut dot = match &mut addr.base {
            Current => cur_dot,
            Bof => Cur { idx: 0 }.into(),
            Eof => Cur::new(self.len_chars()).into(),

            Line(line_idx) => self.full_line(*line_idx)?,
            RelativeLine(offset) => {
                let mut line_idx = self.char_to_line(cur_dot.active_cur().idx)?;
                line_idx = (line_idx as isize + *offset) as usize;
                self.full_line(line_idx)?
            }

            Char(idx) => Cur { idx: *idx }.into(),
            RelativeChar(offset) => {
                let mut c = cur_dot.active_cur();
                c.idx = (c.idx as isize + *offset) as usize;
                c.into()
            }

            LineAndColumn(line, col) => {
                let idx = self.line_to_char(*line)?;
                Cur { idx: idx + *col }.into()
            }

            Regex(re) => {
                let from = cur_dot.last_cur().idx;
                let to = self.len_chars();
                println!("IN {from} {to}");
                let m = re.match_iter(&mut self.iter_between(from, to), from)?;
                let (from, to) = m.loc();
                println!("OUT {from} {to}");
                Dot::from_char_indices(from, to)
            }

            RegexBack(re) => {
                let from = cur_dot.first_cur().idx;
                let m = re.match_iter(&mut self.rev_iter_between(from, 0), from)?;
                let (from, to) = m.loc();
                Dot::from_char_indices(from, to)
            }
        };

        if addr.to_eol || addr.to_bol {
            let Range { start, end, .. } = dot.as_range();

            let from = if addr.to_bol {
                self.char_to_line_start(start.idx)?
            } else {
                start.idx
            };

            let to = if addr.to_eol {
                self.char_to_line_end(end.idx)?
            } else {
                end.idx
            };

            dot = Dot::from_char_indices(from, to);
        }

        Some(dot)
    }

    fn map_compound_addr(&self, from: &mut SimpleAddr, to: &mut SimpleAddr) -> Option<Dot> {
        let d = self.map_simple_addr(from, self.current_dot())?;
        let c1 = d.first_cur();
        let c2 = self.map_simple_addr(to, d)?.last_cur();

        Some(Range::from_cursors(c1, c2, false).into())
    }
}

impl Address for Rope {
    fn current_dot(&self) -> Dot {
        Dot::default()
    }

    fn len_chars(&self) -> usize {
        Rope::len_chars(self)
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        self.try_line_to_char(line_idx).ok()
    }

    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        self.try_char_to_line(char_idx).ok()
    }

    fn char_to_line_end(&self, char_idx: usize) -> Option<usize> {
        let line_idx = self.try_char_to_line(char_idx).ok()?;
        Some(self.line_to_char(line_idx) + self.line(line_idx).len_chars())
    }

    fn char_to_line_start(&self, char_idx: usize) -> Option<usize> {
        let line_idx = self.try_char_to_line(char_idx).ok()?;
        Some(self.line_to_char(line_idx))
    }
}

impl Address for Buffer {
    fn current_dot(&self) -> Dot {
        self.dot
    }

    fn len_chars(&self) -> usize {
        self.txt.len_chars()
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        self.txt.try_line_to_char(line_idx)
    }

    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        self.txt.try_char_to_line(char_idx)
    }

    fn char_to_line_end(&self, char_idx: usize) -> Option<usize> {
        let line_idx = self.txt.try_char_to_line(char_idx)?;
        Some(self.txt.line_to_char(line_idx) + self.txt.line(line_idx).chars().count())
    }

    fn char_to_line_start(&self, char_idx: usize) -> Option<usize> {
        let line_idx = self.txt.try_char_to_line(char_idx)?;
        Some(self.txt.line_to_char(line_idx))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::{Addr::*, AddrBase::*};
    use crate::regex::Regex;
    use simple_test_case::test_case;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    //  Simple
    #[test_case(".", Simple(Current.into()); "current dot")]
    #[test_case("0", Simple(Bof.into()); "begining of file")]
    #[test_case("$", Simple(Eof.into()); "end of file")]
    #[test_case("3", Simple(Line(2).into()); "single line")]
    #[test_case("+42", Simple(RelativeLine(42).into()); "relative line forward")]
    #[test_case("-12", Simple(RelativeLine(-12).into()); "relative line backward")]
    #[test_case("#3", Simple(Char(3).into()); "char")]
    #[test_case("+#42", Simple(RelativeChar(42).into()); "relative char forward")]
    #[test_case("-#12", Simple(RelativeChar(-12).into()); "relative char backward")]
    #[test_case("3:9", Simple(LineAndColumn(2, 8).into()); "line and column cursor")]
    #[test_case("/foo/", Simple(Regex(re("foo")).into()); "regex")]
    #[test_case("+/baz/", Simple(Regex(re("baz")).into()); "regex explicit forward")]
    #[test_case("-/bar/", Simple(RegexBack(Regex::compile_reverse("bar").unwrap()).into()); "regex back")]
    // Simple with suffix
    #[test_case(
        "#3+",
        Simple(SimpleAddr { base: Char(3), to_eol: true, to_bol: false });
        "char to eol"
    )]
    #[test_case(
        "#3-",
        Simple(SimpleAddr { base: Char(3), to_eol: false, to_bol: true });
        "char to bol"
    )]
    #[test_case(
        "#3-+",
        Simple(SimpleAddr { base: Char(3), to_eol: true, to_bol: true });
        "char full line minus plus"
    )]
    #[test_case(
        "#3+-",
        Simple(SimpleAddr { base: Char(3), to_eol: true, to_bol: true });
        "char full line plus minus"
    )]
    // Compound
    #[test_case(",", Compound(Bof.into(), Eof.into()); "full")]
    #[test_case("5,", Compound(Line(4).into(), Eof.into()); "from n")]
    #[test_case("50,", Compound(Line(49).into(), Eof.into()); "from n multi digit")]
    #[test_case("5,9", Compound(Line(4).into(), Line(8).into()); "from n to m")]
    #[test_case("25,90", Compound(Line(24).into(), Line(89).into()); "from n to m multi digit")]
    #[test_case("/foo/,/bar/", Compound(Regex(re("foo")).into(), Regex(re("bar")).into()); "regex range")]
    #[test]
    fn parse_works(s: &str, expected: Addr) {
        let addr = Addr::parse(&mut s.chars().peekable()).expect("valid input");
        assert_eq!(addr, expected);
    }

    #[test_case("0", Dot::default(); "bof")]
    #[test_case("2", Dot::from_char_indices(15, 26); "line 2")]
    #[test_case("-1", Dot::from_char_indices(0, 14); "line 1 relative to 2")]
    #[test_case("/something/", Dot::from_char_indices(33, 42); "regex forward")]
    #[test_case("-/line/", Dot::from_char_indices(10, 14); "regex back")]
    #[test_case("-/his/", Dot::from_char_indices(1, 4); "regex back 2")]
    #[test]
    fn map_addr_works(s: &str, expected: Dot) {
        let mut b = Buffer::new_unnamed(0, "this is a line\nand another\n- [ ] something to do\n");
        b.dot = Cur::new(16).into();

        let mut addr = Addr::parse(&mut s.chars().peekable()).expect("valid addr");
        b.dot = b.map_addr(&mut addr);

        assert_eq!(b.dot, expected, ">{}<", b.dot_contents());
    }
}
