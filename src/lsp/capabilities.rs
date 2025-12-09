//! Capability based logic
use crate::{
    buffer::Buffer,
    exec::{Addr, AddrBase},
    lsp::Pos,
};
use lsp_types::{InitializeResult, Location, Position, PositionEncodingKind, ServerCapabilities};
use tracing::warn;

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct Capabilities {
    inner: ServerCapabilities,
    pub(super) position_encoding: PositionEncoding,
}

impl Capabilities {
    pub(crate) fn try_new(res: InitializeResult) -> Option<Self> {
        let position_encoding = match &res.capabilities.position_encoding {
            Some(p) if *p == PositionEncodingKind::UTF8 => PositionEncoding::Utf8,
            Some(p) if *p == PositionEncodingKind::UTF16 => PositionEncoding::Utf16,
            Some(p) if *p == PositionEncodingKind::UTF32 => PositionEncoding::Utf32,
            None => PositionEncoding::Utf16, // see quote from the spec below

            Some(p) => {
                warn!(
                    "LSP provided unknown position encoding: {p:?} {:?}",
                    res.server_info
                );
                return None;
            }
        };

        Some(Self {
            inner: res.capabilities,
            position_encoding,
        })
    }

    pub(crate) fn as_pretty_json(&self) -> Option<String> {
        serde_json::to_string_pretty(&self.inner).ok()
    }
}

// NOTE: The LSP spec explicitly calls out needing to support \n, \r and \r\n line
//       endings which ad doesn't do. Files using \r or \r\n will likely result in
//       malformed positions.

/// From the LSP 3.17 spec:
///
/// The position encodings supported by the client. Client and server
/// have to agree on the same position encoding to ensure that offsets
/// (e.g. character position in a line) are interpreted the same on both
/// side.
///
/// To keep the protocol backwards compatible the following applies: if
/// the value 'utf-16' is missing from the array of position encodings
/// servers can assume that the client supports UTF-16. UTF-16 is
/// therefore a mandatory encoding.
///
/// If omitted it defaults to ['utf-16'].
///
/// Implementation considerations: since the conversion from one encoding
/// into another requires the content of the file / line the conversion
/// is best done where the file is read which is usually on the server
/// side.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum PositionEncoding {
    /// Raw bytes
    Utf8,
    /// Javascript / MS
    Utf16,
    /// Unicode code points
    Utf32,
}

impl PositionEncoding {
    pub(crate) fn parse_lsp_position(&self, b: &Buffer, pos: Position) -> (usize, usize) {
        let pos_line = pos.line as usize;
        if pos_line > b.len_lines() - 1 {
            warn!("LSP position out of bounds, clamping to EOF");
            return (b.len_lines().saturating_sub(1), b.len_chars());
        }

        match self {
            Self::Utf8 => {
                let line_start = b.txt.line_to_byte(pos.line as usize);
                let col = b.txt.chars_in_raw_range(
                    b.txt.byte_to_raw_byte(line_start),
                    b.txt.byte_to_raw_byte(line_start + pos.character as usize),
                );

                (pos.line as usize, col)
            }

            Self::Utf16 => {
                let slice = b.txt.line(pos.line as usize);
                let mut remaining = pos.character as usize;
                let mut col = 0;
                for ch in slice.chars() {
                    if remaining == 0 {
                        break;
                    }
                    remaining = remaining.saturating_sub(ch.len_utf16());
                    col += 1;
                }
                if remaining > 0 {
                    col = slice.chars().count(); // clamp to EOL
                }

                (pos.line as usize, col)
            }

            Self::Utf32 => (pos.line as usize, pos.character as usize),
        }
    }

    pub(super) fn buffer_pos(&self, b: &Buffer) -> Pos {
        let file = b.full_name();
        let (y, x) = b.dot.active_cur().as_yx(b);
        let (line, character) = self.lsp_position(b, y, x);

        Pos::new(file, line, character)
    }

    fn lsp_position(&self, b: &Buffer, line: usize, col: usize) -> (u32, u32) {
        match self {
            Self::Utf8 => {
                let line_start = b.txt.line_to_char(line);
                let start_idx = b.txt.char_to_byte(line_start);
                let character = b.txt.char_to_byte(line_start + col) - start_idx;

                (line as u32, character as u32)
            }

            Self::Utf16 => {
                let slice = b.txt.line(line);
                let mut character = 0;
                for ch in slice.chars().take(col) {
                    character += ch.len_utf16();
                }

                (line as u32, character as u32)
            }

            Self::Utf32 => (line as u32, col as u32),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Coords {
    pub(crate) start: Position,
    pub(crate) end: Position,
    pub(crate) encoding: PositionEncoding,
}

impl Coords {
    pub(crate) fn new(loc: Location, encoding: PositionEncoding) -> (String, Self) {
        let filepath = loc
            .uri
            .to_string()
            .strip_prefix("file://")
            .unwrap()
            .to_owned();

        let coords = Coords {
            start: loc.range.start,
            end: loc.range.end,
            encoding,
        };

        (filepath, coords)
    }

    pub(crate) fn new_from_range(r: lsp_types::Range, encoding: PositionEncoding) -> Self {
        Coords {
            start: r.start,
            end: r.end,
            encoding,
        }
    }

    pub(crate) fn new_from_pos(pos: Pos, encoding: PositionEncoding) -> Self {
        Coords {
            start: lsp_types::Position::new(pos.line, pos.character),
            end: lsp_types::Position::new(pos.line, pos.character),
            encoding,
        }
    }

    pub fn line(&self) -> u32 {
        self.start.line
    }

    pub(crate) fn as_addr(&self, b: &Buffer) -> Addr {
        let (row_start, col_start) = self.encoding.parse_lsp_position(b, self.start);
        let (mut row_end, mut col_end) = self.encoding.parse_lsp_position(b, self.end);

        if (row_start, col_start) == (row_end, col_end) {
            // LSP insert at this position within the buffer
            Addr::Simple(AddrBase::LineAndColumn(row_start, col_start).into())
        } else if row_start == row_end && col_end == col_start + 1 {
            // LSP delete of a single character
            Addr::Compound(
                AddrBase::LineAndColumn(row_start, col_start).into(),
                AddrBase::LineAndColumn(row_start, col_start).into(),
            )
        } else {
            // From the LSP spec on Ranges:
            //   https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#range
            //
            // "If you want to specify a range that contains a line including the line ending
            // character(s) then use an end position denoting the start of the next line."
            //
            // This idea of targeting the line ending character(s) by specifying the first
            // character of the following line doesn't seem self consistent given that the range is
            // inclusive? Not sure how an LSP server is supposed to genuinely target the first
            // character of a given line then...
            // With that in mind, we need to check for this case and filter for when the line is
            // actually a blank line otherwise removing full lines doesn't work.
            if col_end == 0 && !b.txt.line_is_blank(row_end) {
                row_end = row_end.saturating_sub(1);
                col_end = b.txt.line(row_end).chars().count();
            }

            Addr::Compound(
                AddrBase::LineAndColumn(row_start, col_start).into(),
                AddrBase::LineAndColumn(row_end, col_end.saturating_sub(1)).into(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;
    use simple_test_case::test_case;

    // LSP positions are _between_ characters (like a cursor).
    // Position 0 = before first char, position N = after Nth char.
    //
    // For ASCII text, UTF-16 positions equal character indices.
    //
    // Multi-byte UTF-16: emoji like 😀 uses 2 UTF-16 code units (surrogate pair)
    // String "a😀b" is 3 chars but 4 UTF-16 units: a(1) + 😀(2) + b(1)

    #[test_case("hello", 0, 0; "ascii position 0")]
    #[test_case("hello", 1, 1; "ascii position 1")]
    #[test_case("hello", 5, 5; "ascii position 5")]
    #[test_case("a😀b", 0, 0; "emoji position 0")]
    #[test_case("a😀b", 1, 1; "emoji position 1 after a before emoji")]
    #[test_case("a😀b", 3, 2; "emoji position 3 after emoji before b")]
    #[test_case("a😀b", 4, 3; "emoji position 4 after b")]
    #[test]
    fn parse_lsp_position_utf16_ascii(content: &str, lsp_char: u32, expected_col: usize) {
        let b = Buffer::new_virtual(0, "test", content, Default::default());
        let pos = Position {
            line: 0,
            character: lsp_char,
        };

        let (line, col) = PositionEncoding::Utf16.parse_lsp_position(&b, pos);

        assert_eq!(line, 0);
        assert_eq!(col, expected_col);
    }
}
