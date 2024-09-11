//! Filetype detection and behaviour.
use std::path::Path;

pub mod lex;

use lex::{LangSpec, StringDelims, Tokenizer};

pub(crate) const RUST_SPEC: LangSpec = LangSpec {
    single_line_comment: Some("//"),
    multi_line_comment: Some(("/*", "*/")),
    string_delimiters: StringDelims::Both,
    keywords: &[
        "pub", "return", "crate", "super", "use", "mod", "where", "mut", "unsafe", "self", "async",
        "await", "ref",
    ],
    control_flow: &[
        "if", "else", "for", "in", "while", "loop", "match", "break", "continue",
    ],
    definitions: &[
        "let", "enum", "struct", "trait", "impl", "fn", "type", "const",
    ],
    punctuation: &[
        "::", "&", "%", "||", "|", "->", "=>", "==", "!=", ">>", "<<", "=", "?", "^", "..", "+",
        "-", "*", "/", "<", ">", "+=", "-=", "*=", "/=", "%=", ">=", "<=", "&=", "|=", "^=",
    ],
};

/// Determine the appropriate [Tokenizer] for the provided [Path].
///
/// Returns None is no Tokenizer is available.
pub(crate) fn try_tokenizer_for_path(path: &Path) -> Option<Tokenizer> {
    match path.extension() {
        Some(ext) if ext == "rs" => Some(Tokenizer::new(RUST_SPEC)),
        _ => None,
    }
}
