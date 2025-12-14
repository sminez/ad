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
    buffer::{Buffer, GapBuffer},
    dot::{Cur, Dot, Range},
    parse::{self, ParseInput},
    regex::{self, Haystack, Regex, RevRegex},
};
use std::fmt;

pub type Error = parse::Error<ErrorKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
    InvalidRegex(regex::Error),
    InvalidSuffix,
    NotAnAddress,
    UnclosedDelimiter,
    UnexpectedCharacter(char),
    UnexpectedEof,
    ZeroIndexedLineOrColumn,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRegex(err) => write!(f, "invalid regular expression: {err}"),
            Self::InvalidSuffix => write!(f, "invalid suffix"),
            Self::NotAnAddress => write!(f, "not an address"),
            Self::UnclosedDelimiter => write!(f, "unclosed delimiter"),
            Self::UnexpectedCharacter(c) => write!(f, "unexpected character {c:?}"),
            Self::UnexpectedEof => write!(f, "unexpecterd EOF"),
            Self::ZeroIndexedLineOrColumn => write!(f, "zero indexed line or column"),
        }
    }
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
    pub fn from_dot(dot: Dot, b: &Buffer) -> Self {
        match dot {
            Dot::Cur { c } => {
                let (y, x) = c.as_yx(b);
                Self::Simple(AddrBase::LineAndColumn(y, x).into())
            }

            Dot::Range { r } => {
                let (y1, x1) = r.start.as_yx(b);
                let (y2, x2) = r.end.as_yx(b);

                Self::Compound(
                    AddrBase::LineAndColumn(y1, x1).into(),
                    AddrBase::LineAndColumn(y2, x2).into(),
                )
            }
        }
    }

    pub fn full() -> Self {
        Addr::Compound(AddrBase::Bof.into(), AddrBase::Eof.into())
    }

    /// Attempt to parse a valid dot expression from a string
    pub fn parse(s: &str) -> Result<Self, Error> {
        Parser::new(&ParseInput::new(s)).parse()
    }

    /// Attempt to parse a valid dot expression from an existing [ParseInput].
    pub(crate) fn parse_from_input(input: &ParseInput<'_>) -> Result<Self, Error> {
        Parser::new(input).parse()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleAddr {
    base: AddrBase,
    suffixes: Vec<AddrBase>, // restricted to variants that return true for is_valid_suffix
}

/// Primitives for building out addresses.
///
/// Line and column indices are 1-based to match the editor line numbering and common output
/// formats from popular compilers and other command line tooling. Internally these indices are
/// converted to be 0-based to match the internal representation of ad's [Dot].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddrBase {
    /// .
    Current,
    /// -+ | +-
    CurrentLine,
    /// -
    Bol,
    /// +
    Eol,
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
    RegexBack(RevRegex),
}

impl From<AddrBase> for SimpleAddr {
    fn from(base: AddrBase) -> Self {
        Self {
            base,
            suffixes: Vec::new(),
        }
    }
}

enum Dir {
    Fwd,
    Bck,
}

impl AddrBase {
    fn is_valid_suffix(&self) -> bool {
        use AddrBase::*;
        matches!(
            self,
            Bol | Eol | CurrentLine | RelativeLine(_) | RelativeChar(_) | Regex(_) | RegexBack(_)
        )
    }
}

/// This is a slightly odd setup but we take a reference to a [ParseInput] in order to support
/// being run with an existing input when parsing addresses as part of an exec script.
#[derive(Debug)]
struct Parser<'a> {
    input: &'a ParseInput<'a>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a ParseInput<'a>) -> Self {
        Self { input }
    }

    /// Attempt to parse a full address out of the input in the form [from],[to].
    ///
    /// Must be called without leading whitespace.
    fn parse(&self) -> Result<Addr, Error> {
        let start = match self.parse_simple() {
            Ok(addr) => Some(addr),
            Err(e) if self.input.at_bof() && self.input.try_char() == Some(',') => None,
            Err(e) => return Err(e),
        };

        if self.input.at_eof() || self.input.char() == ' ' {
            // If we didn't have an starting addr then this expression is invalid, otherwise
            // we just have 'start' as a simple addr
            Ok(Addr::Simple(
                start.ok_or_else(|| self.error(ErrorKind::NotAnAddress))?,
            ))
        } else if self.input.char() == ',' {
            // Compound addrs default their first element to Bof and last to Eof
            self.input.advance(); // consume the ','
            let start = start.unwrap_or(AddrBase::Bof.into());
            let next_is_eof_or_whitespace = self
                .input
                .try_char()
                .map(|ch| ch.is_whitespace())
                .unwrap_or(true);

            let end = if next_is_eof_or_whitespace {
                AddrBase::Eof.into()
            } else {
                self.parse_simple()?
            };

            Ok(Addr::Compound(start, end))
        } else {
            Err(self.error(ErrorKind::NotAnAddress))
        }
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error::new(kind, self.input.text(), self.input.span())
    }

    fn parse_simple(&self) -> Result<SimpleAddr, Error> {
        let base = self.parse_base()?;
        let mut suffixes = Vec::new();

        while !self.input.at_eof() {
            if !"-+".contains(self.input.char()) {
                break;
            }
            let addr = self.parse_base()?;
            if !addr.is_valid_suffix() {
                return Err(self.error(ErrorKind::InvalidSuffix));
            }
            suffixes.push(addr);
        }

        Ok(SimpleAddr { base, suffixes })
    }

    fn parse_base(&self) -> Result<AddrBase, Error> {
        if self.input.at_eof() {
            return Err(self.error(ErrorKind::UnexpectedEof));
        }

        let dir = match self.input.char() {
            '-' => {
                self.input.advance();
                if self.input.at_eof() {
                    return Ok(AddrBase::Bol);
                }
                Some(Dir::Bck)
            }

            '+' => {
                self.input.advance();
                if self.input.at_eof() {
                    return Ok(AddrBase::Eol);
                }
                Some(Dir::Fwd)
            }

            _ => None,
        };

        match (self.input.char(), dir) {
            ('.' | '0' | '$', Some(_)) => Err(self.error(ErrorKind::NotAnAddress)),

            ('-', Some(Dir::Fwd)) | ('+', Some(Dir::Bck)) => {
                self.input.advance();
                Ok(AddrBase::CurrentLine)
            }

            ('.', None) => {
                self.input.advance();
                Ok(AddrBase::Current)
            }

            ('0', None) => {
                self.input.advance();
                Ok(AddrBase::Bof)
            }

            ('$', None) => {
                self.input.advance();
                Ok(AddrBase::Eof)
            }

            ('#', dir) => {
                self.input.advance();
                if self.input.at_eof() || !self.input.char().is_ascii_digit() {
                    return Err(self.error(ErrorKind::NotAnAddress));
                }

                let ix = self.try_parse_num()?;
                match dir {
                    None => Ok(AddrBase::Char(ix)),
                    Some(Dir::Fwd) => Ok(AddrBase::RelativeChar(ix as isize)),
                    Some(Dir::Bck) => Ok(AddrBase::RelativeChar(-(ix as isize))),
                }
            }

            (c, dir) if c.is_ascii_digit() => {
                let line = self.try_parse_num()?;
                if line == 0 {
                    return Err(self.error(ErrorKind::ZeroIndexedLineOrColumn));
                }

                match (self.input.try_char(), dir) {
                    (Some(':'), Some(_)) => Err(self.error(ErrorKind::NotAnAddress)),

                    (Some(':'), None) => {
                        self.input.advance();
                        if self.input.at_eof() {
                            Err(self.error(ErrorKind::NotAnAddress))
                        } else if !self.input.char().is_ascii_digit() {
                            Err(self.error(ErrorKind::UnexpectedCharacter(self.input.char())))
                        } else {
                            match self.try_parse_num()? {
                                0 => Err(self.error(ErrorKind::ZeroIndexedLineOrColumn)),
                                col => Ok(AddrBase::LineAndColumn(line - 1, col - 1)),
                            }
                        }
                    }

                    (_, None) => Ok(AddrBase::Line(line - 1)),
                    (_, Some(Dir::Fwd)) => Ok(AddrBase::RelativeLine(line as isize)),
                    (_, Some(Dir::Bck)) => Ok(AddrBase::RelativeLine(-(line as isize))),
                }
            }

            ('/', dir) => self.parse_delimited_regex(dir.unwrap_or(Dir::Fwd)),

            (_, Some(Dir::Fwd)) => Ok(AddrBase::Eol),
            (_, Some(Dir::Bck)) => Ok(AddrBase::Bol),

            _ => Err(self.error(ErrorKind::NotAnAddress)),
        }
    }

    fn try_parse_num(&self) -> Result<usize, Error> {
        assert!(self.input.char().is_ascii_digit());
        let mut s = self.input.char().to_string();
        self.input.advance();

        loop {
            if self.input.at_eof() || !self.input.char().is_ascii_digit() {
                break;
            }
            s.push(self.input.char());
            self.input.advance();
        }

        s.parse().map_err(|_| self.error(ErrorKind::NotAnAddress))
    }

    fn parse_delimited_regex(&self, dir: Dir) -> Result<AddrBase, Error> {
        assert_eq!(self.input.char(), '/');
        let mut s = String::new();
        let mut prev = '/';
        self.input.advance(); // consume the '/'

        while !self.input.at_eof() {
            let ch = self.input.char();
            if ch == '/' && prev != '\\' {
                self.input.advance(); // consume the '/'
                return match dir {
                    Dir::Fwd => Ok(AddrBase::Regex(
                        Regex::compile(&s).map_err(|e| self.error(ErrorKind::InvalidRegex(e)))?,
                    )),
                    Dir::Bck => Ok(AddrBase::RegexBack(
                        RevRegex::compile(&s)
                            .map_err(|e| self.error(ErrorKind::InvalidRegex(e)))?,
                    )),
                };
            }

            self.input.advance();
            s.push(ch);
            prev = ch;
        }

        Err(self.error(ErrorKind::UnclosedDelimiter))
    }
}

/// Something that is capable of resolving an Addr to a Dot
pub trait Address: Haystack + Sized {
    /// This only really makes sense for use with a buffer but is supported
    /// so that don't need to special case running programs against an in-editor
    /// buffer vs stdin or a file read from disk.
    fn current_dot(&self) -> Dot;
    fn len_bytes(&self) -> usize;
    fn len_chars(&self) -> usize;
    fn line_to_char(&self, line_idx: usize) -> Option<usize>;
    fn char_to_line(&self, char_idx: usize) -> Option<usize>;
    fn char_to_line_end(&self, char_idx: usize) -> Option<usize>;
    fn char_to_line_start(&self, char_idx: usize) -> Option<usize>;

    fn max_iter(&self) -> usize {
        self.len_chars()
    }

    fn map_addr(&self, a: &Addr) -> Dot {
        let maybe_dot = match a {
            Addr::Explicit(d) => Some(*d),
            Addr::Simple(a) => self.map_simple_addr(a, self.current_dot()),
            Addr::Compound(from, to) => self.map_compound_addr(from, to),
        };

        let mut dot = maybe_dot.unwrap_or_default();
        dot.clamp_idx(self.max_iter());

        dot
    }

    fn full_line(&self, line_idx: usize) -> Option<Dot> {
        let from = self.line_to_char(line_idx)?;
        let to = self.char_to_line_end(from)?.saturating_sub(1);

        Some(Dot::from_char_indices(from, to))
    }

    fn map_addr_base(&self, addr_base: &AddrBase, cur_dot: Dot) -> Option<Dot> {
        use AddrBase::*;

        let dot = match addr_base {
            Current => cur_dot,
            Bof => Cur { idx: 0 }.into(),
            Eof => Cur::new(self.max_iter()).into(),

            Bol => {
                let Range { start, end, .. } = cur_dot.as_range();
                let from = self.char_to_line_start(start.idx)?;
                Dot::from_char_indices(from, end.idx)
            }

            Eol => {
                let Range { start, end, .. } = cur_dot.as_range();
                let to = self.char_to_line_end(end.idx)?;
                Dot::from_char_indices(start.idx, to)
            }

            CurrentLine => {
                let Range { start, end, .. } = cur_dot.as_range();
                let from = self.char_to_line_start(start.idx)?;
                let to = self.char_to_line_end(end.idx)?;
                Dot::from_char_indices(from, to)
            }

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
                let m = re.find_from(self, from)?;
                let (byte_from, byte_to) = m.loc();
                let from = self.byte_to_char(byte_from).unwrap();
                let to = self.byte_to_char(byte_to).unwrap();

                Dot::from_char_indices(from, to.saturating_sub(1))
            }

            RegexBack(re) => {
                let from = cur_dot.first_cur().idx;
                let m = re.find_rev_from(self, from)?;
                let (byte_from, byte_to) = m.loc();
                let from = self.byte_to_char(byte_from).unwrap();
                let to = self.byte_to_char(byte_to).unwrap();

                Dot::from_char_indices(from, to.saturating_sub(1))
            }
        };

        Some(dot)
    }

    fn map_simple_addr(&self, addr: &SimpleAddr, cur_dot: Dot) -> Option<Dot> {
        let mut dot = self.map_addr_base(&addr.base, cur_dot)?;

        for suffix in addr.suffixes.iter() {
            dot = self.map_addr_base(suffix, dot)?;
        }

        Some(dot)
    }

    fn map_compound_addr(&self, from: &SimpleAddr, to: &SimpleAddr) -> Option<Dot> {
        let c1 = self.map_simple_addr(from, self.current_dot())?.first_cur();
        let c2 = self.map_simple_addr(to, self.current_dot())?.last_cur();

        Some(Range::from_cursors(c1, c2, false).into())
    }
}

impl Address for GapBuffer {
    fn current_dot(&self) -> Dot {
        Dot::from_char_indices(0, self.len_chars().saturating_sub(1))
    }

    fn len_bytes(&self) -> usize {
        self.len()
    }

    fn len_chars(&self) -> usize {
        self.len_chars()
    }

    fn line_to_char(&self, line_idx: usize) -> Option<usize> {
        self.try_line_to_char(line_idx)
    }

    fn char_to_line(&self, char_idx: usize) -> Option<usize> {
        self.try_char_to_line(char_idx)
    }

    fn char_to_line_end(&self, char_idx: usize) -> Option<usize> {
        let line_idx = self.try_char_to_line(char_idx)?;
        match self.try_line_to_char(line_idx + 1) {
            None => Some(self.len_chars() - 1),
            Some(idx) => Some(idx),
        }
    }

    fn char_to_line_start(&self, char_idx: usize) -> Option<usize> {
        let line_idx = self.try_char_to_line(char_idx)?;
        Some(self.line_to_char(line_idx))
    }
}

impl Address for Buffer {
    fn current_dot(&self) -> Dot {
        self.dot
    }

    fn len_bytes(&self) -> usize {
        self.txt.len()
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
        match self.txt.try_line_to_char(line_idx + 1) {
            None => Some(self.txt.len_chars() - 1),
            Some(idx) => Some(idx),
        }
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
    use crate::regex::{Regex, RevRegex};
    use simple_test_case::test_case;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    fn re_rev(s: &str) -> RevRegex {
        RevRegex::compile(s).unwrap()
    }

    //  Simple
    #[test_case(".", Simple(Current.into()); "current dot")]
    #[test_case("-", Simple(Bol.into()); "beginning of line")]
    #[test_case("+", Simple(Eol.into()); "end of line")]
    #[test_case("-+", Simple(CurrentLine.into()); "current line minus plus")]
    #[test_case("+-", Simple(CurrentLine.into()); "current line plus minus")]
    #[test_case("0", Simple(Bof.into()); "beginning of file")]
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
    #[test_case("-/bar/", Simple(RegexBack(re_rev("bar")).into()); "regex back")]
    // Simple with suffix
    #[test_case(
        "#5+",
        Simple(SimpleAddr { base: Char(5), suffixes: vec![Eol] });
        "char to eol"
    )]
    #[test_case(
        "#5-",
        Simple(SimpleAddr { base: Char(5), suffixes: vec![Bol] });
        "char to bol"
    )]
    #[test_case(
        "5+#3",
        Simple(SimpleAddr { base: Line(4), suffixes: vec![RelativeChar(3)] });
        "line plus char"
    )]
    #[test_case(
        "5-#3",
        Simple(SimpleAddr { base: Line(4), suffixes: vec![RelativeChar(-3)] });
        "line minus char"
    )]
    // Compound
    #[test_case(",", Compound(Bof.into(), Eof.into()); "full")]
    #[test_case("5,", Compound(Line(4).into(), Eof.into()); "from n")]
    #[test_case("50,", Compound(Line(49).into(), Eof.into()); "from n multi digit")]
    #[test_case("5,9", Compound(Line(4).into(), Line(8).into()); "from n to m")]
    #[test_case("25,90", Compound(Line(24).into(), Line(89).into()); "from n to m multi digit")]
    #[test_case("/foo/,/bar/", Compound(Regex(re("foo")).into(), Regex(re("bar")).into()); "regex range")]
    // Compound with suffix
    #[test_case(
        "-/\\s/+#1,/\\s/-#1",
        Compound(
            SimpleAddr { base: RegexBack(re_rev("\\s")), suffixes: vec![RelativeChar(1)] },
            SimpleAddr { base: Regex(re("\\s")), suffixes: vec![RelativeChar(-1)] },
        );
        "regex range with suffixes"
    )]
    #[test]
    fn parse_works(s: &str, expected: Addr) {
        let addr = Addr::parse(s).expect("valid input");
        assert_eq!(addr, expected);
    }

    #[test_case("0", Dot::default(), "t"; "bof")]
    #[test_case("2", Dot::from_char_indices(15, 26), "and another\n"; "line 2")]
    #[test_case("2:1", Cur { idx: 15 }.into(), "a"; "line 2 col 1")]
    #[test_case("2:2", Cur { idx: 16 }.into(), "n"; "line 2 col 2")]
    #[test_case("-1", Dot::from_char_indices(0, 14), "this is a line\n"; "line 1 relative to 2")]
    #[test_case("/something/", Dot::from_char_indices(33, 41), "something"; "regex forward")]
    #[test_case("-/line/", Dot::from_char_indices(10, 13), "line"; "regex back")]
    #[test_case("-/his/", Dot::from_char_indices(1, 3), "his"; "regex back 2")]
    #[test_case("-/a/,/a/", Dot::from_char_indices(15, 19), "and a"; "regex range")]
    #[test_case("-/\\s/+#1,/\\s/-#1", Dot::from_char_indices(15, 17), "and"; "regex range boundaries")]
    #[test]
    fn map_addr_works(s: &str, expected: Dot, expected_contents: &str) {
        let mut b = Buffer::new_unnamed(
            0,
            "this is a line\nand another\n- [ ] something to do\n",
            Default::default(),
        );
        b.dot = Cur::new(16).into();

        let addr = Addr::parse(s).expect("valid addr");
        b.dot = b.map_addr(&addr);

        assert_eq!(b.dot, expected, ">{}<", b.dot_contents());
        assert_eq!(b.dot_contents(), expected_contents);
    }

    #[test_case("99999999999999999999"; "line number overflow")]
    #[test_case("#99999999999999999999"; "char index overflow")]
    #[test_case("+#99999999999999999999"; "relative char forward overflow")]
    #[test_case("-#99999999999999999999"; "relative char back overflow")]
    #[test_case("5:99999999999999999999"; "column number overflow")]
    #[test_case("99999999999999999999,100"; "range start overflow")]
    #[test_case("1,99999999999999999999"; "range end overflow")]
    #[test_case("99999999999999999999:5"; "line in line col overflow")]
    #[test]
    fn giant_address_integers_error(s: &str) {
        let res = Addr::parse(s);
        assert!(res.is_err(), "expected error, got {res:?}");
    }

    #[test_case("#"; "char address at eof")]
    #[test_case("1,#"; "compound with eof after hash")]
    #[test_case("#,5"; "compound with incomplete char start")]
    #[test_case("+#"; "relative forward at eof")]
    #[test_case("-#"; "relative back at eof")]
    #[test]
    fn incomplete_char_addresses_error(s: &str) {
        let res = Addr::parse(s);
        assert!(res.is_err(), "expected error, got {res:?}");
    }
}
