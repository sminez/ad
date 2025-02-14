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
                let line_start = b.txt.line_to_char(pos.line as usize);
                let byte_idx = b.txt.char_to_byte(line_start + pos.character as usize);
                let col = b.txt.raw_byte_to_char(byte_idx);

                (pos.line as usize, col)
            }

            Self::Utf16 => {
                let slice = b.txt.line(pos.line as usize);
                let mut character = pos.character as usize;
                let mut col = 0;
                for (idx, ch) in slice.chars().enumerate() {
                    let n = ch.len_utf16();
                    col = idx;
                    character -= n;
                    if character == 0 {
                        break;
                    }
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
    encoding: PositionEncoding,
    start: Position,
    end: Position,
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
            encoding,
            start: loc.range.start,
            end: loc.range.end,
        };

        (filepath, coords)
    }

    pub fn line(&self) -> u32 {
        self.start.line
    }

    pub fn as_addr(&self, b: &Buffer) -> Addr {
        let (sr, sc) = self.encoding.parse_lsp_position(b, self.start);
        let (er, ec) = self.encoding.parse_lsp_position(b, self.end);

        Addr::Compound(
            AddrBase::LineAndColumn(sr, sc).into(),
            AddrBase::LineAndColumn(er, ec).into(),
        )
    }
}
