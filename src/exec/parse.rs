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

    pub fn at_bof(&self) -> bool {
        self.offset() == 0
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

    pub fn offset(&self) -> usize {
        self.pos.get().offset
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
}
