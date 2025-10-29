//! Helpers for writing minimal parsers that report error positions
use std::{cell::Cell, error, fmt};

/// A parser error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error<K: fmt::Display> {
    pub kind: K,
    pub source: String,
    pub span: Span,
}

impl<K> Error<K>
where
    K: fmt::Debug + fmt::Display,
{
    pub fn new(kind: impl Into<K>, source: &str, span: Span) -> Self {
        let source = source[span.as_range()].to_string();

        Self {
            kind: kind.into(),
            source,
            span,
        }
    }
}

impl<K> error::Error for Error<K> where K: fmt::Debug + fmt::Display {}

impl<K> fmt::Display for Error<K>
where
    K: fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error ({:?}): {} {:?}",
            self.span, self.kind, self.source
        )
    }
}

/// A single position within the input text.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Position {
    /// starts at 0
    pub offset: usize,
    /// starts at 1
    pub line: usize,
    /// starts at 1
    pub column: usize,
}

impl Position {
    pub const START: Self = Position::new(0, 1, 1);

    pub const fn new(offset: usize, line: usize, column: usize) -> Position {
        Position {
            offset,
            line,
            column,
        }
    }
}

/// The range within the input being parsed.
///
/// All span positions are absolute byte offsets that can be used on the
/// original string that was parsed.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }

    pub fn at(pos: Position) -> Span {
        Span::new(pos, pos)
    }

    pub fn with_start(self, pos: Position) -> Span {
        Span { start: pos, ..self }
    }

    pub fn with_end(self, pos: Position) -> Span {
        Span { end: pos, ..self }
    }

    pub fn as_range(&self) -> std::ops::Range<usize> {
        self.start.offset..self.end.offset
    }
}

#[derive(Debug)]
pub struct ParseInput<'a> {
    input: &'a str,
    pos: Cell<Position>,
}

impl<'a> ParseInput<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: Cell::new(Position::START),
        }
    }

    pub fn text(&self) -> &str {
        self.input
    }

    pub fn span_text(&self, span: &Span) -> &str {
        &self.input[span.as_range()]
    }

    pub fn reset(&self) {
        self.pos.set(Position::START);
    }

    pub fn at_eof(&self) -> bool {
        self.offset() == self.input.len()
    }

    /// The character at the current parser position
    pub fn char(&self) -> char {
        self.char_at(self.offset())
    }

    pub fn try_char(&self) -> Option<char> {
        if self.at_eof() {
            None
        } else {
            Some(self.char_at(self.offset()))
        }
    }

    /// The remaining text following the current parser position
    pub fn remaining(&self) -> &str {
        &self.input[self.offset()..]
    }

    /// The character at the given position
    pub fn char_at(&self, i: usize) -> char {
        self.input[i..]
            .chars()
            .next()
            .unwrap_or_else(|| panic!("expected char at offset {i} {:?}", self.pos()))
    }

    /// The current position of the parser within the input text
    pub fn pos(&self) -> Position {
        self.pos.get()
    }

    /// Crate a null span pointing at the current parser position
    pub fn span(&self) -> Span {
        Span::at(self.pos())
    }

    /// Create a span covering the current character.
    pub fn span_char(&self) -> Span {
        let ch = self.char();
        let mut end = Position {
            offset: self.offset().checked_add(ch.len_utf8()).unwrap(),
            line: self.line(),
            column: self.column().checked_add(1).unwrap(),
        };
        if ch == '\n' {
            end.line += 1;
            end.column = 1;
        }

        Span::new(self.pos(), end)
    }

    pub fn offset(&self) -> usize {
        self.pos.get().offset
    }

    pub fn line(&self) -> usize {
        self.pos.get().line
    }

    pub fn column(&self) -> usize {
        self.pos.get().column
    }

    /// Advance to the next Unicode scalar value.
    ///
    /// If EOF has been reached then `false` is returned.
    pub fn advance(&self) -> bool {
        if self.at_eof() {
            return false;
        }

        let Position {
            mut offset,
            mut line,
            mut column,
        } = self.pos();
        if self.char() == '\n' {
            line = line.checked_add(1).unwrap();
            column = 1;
        } else {
            column = column.checked_add(1).unwrap();
        }
        offset += self.char().len_utf8();
        self.pos.set(Position {
            offset,
            line,
            column,
        });

        self.input[self.offset()..].chars().next().is_some()
    }

    /// Advance until the provided target is located or we hit EOF, returning
    /// the substring that was consumed from the input.
    pub fn read_until(&self, target: char) -> Option<&str> {
        let start = self.offset();
        self.advance();

        while !self.at_eof() {
            let end = self.offset();
            if self.char() == target {
                return Some(&self.input[start..end]);
            }
            self.advance();
        }

        None
    }

    pub fn consume_until(&self, delim: char) {
        self.read_until(delim);
    }

    /// If the substring starting at the current parser position has the given
    /// prefix then advance to the character after the prefix and return true.
    /// Otherwise, return false.
    pub fn try_consume(&self, prefix: &str) -> bool {
        if self.remaining().starts_with(prefix) {
            for _ in 0..prefix.chars().count() {
                self.advance();
            }
            true
        } else {
            false
        }
    }

    pub fn consume_whitespace(&self) {
        while !self.at_eof() {
            if self.char().is_whitespace() {
                self.advance();
            } else {
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test]
    fn advance_works() {
        let p = ParseInput::new("bar");

        assert_eq!(p.char(), 'b');
        assert!(p.advance());
        assert_eq!(p.char(), 'a');
        assert!(p.advance());
        assert_eq!(p.char(), 'r');
        assert!(!p.advance());
    }

    #[test_case(' ', Some("this"); "target found")]
    #[test_case('\n', None; "target missing")]
    #[test]
    fn read_until_works(target: char, expected: Option<&str>) {
        let p = ParseInput::new("this and that.");
        let res = p.read_until(target);

        assert_eq!(res, expected);
        if expected.is_some() {
            assert_eq!(p.char(), target, "should be sat on target");
        } else {
            assert!(p.at_eof(), "should be at EOF");
        }
    }

    #[test]
    fn consume_until_works() {
        let p = ParseInput::new("this is line one\nthis is line 2");
        p.consume_until('\n');
        assert_eq!(p.remaining(), "\nthis is line 2");
    }

    #[test]
    fn consume_whitespace_works() {
        let p = ParseInput::new("   \t\r\nthis is line 2");
        p.consume_whitespace();
        assert_eq!(p.remaining(), "this is line 2");
    }
}
