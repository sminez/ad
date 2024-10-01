//! Filetype detection and behaviour.
use std::path::Path;

pub mod lex;

use lex::{LangSpec, StringDelims, Tokenizer};

pub(crate) const RUST_SPEC: LangSpec = LangSpec {
    single_line_comment: Some("//"),
    multi_line_comment: Some(("/*", "*/")),
    string_delimiters: StringDelims::Double,
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

pub(crate) const SHELL_SPEC: LangSpec = LangSpec {
    single_line_comment: Some("#"),
    multi_line_comment: None,
    string_delimiters: StringDelims::Both,
    keywords: &["echo", "source", "shift", "read"],
    control_flow: &[
        "if", "fi", "elif", "else", "for", "then", "do", "done", "case", "esac", "in", "break",
        "continue", "while",
    ],
    definitions: &[],
    punctuation: &[
        "&", "|", "&&", "||", "[", "]", "=", "()", "{", "}", "(", ")", "$", ";",
    ],
};

pub(crate) const PLUMBING_SPEC: LangSpec = LangSpec {
    single_line_comment: Some("#"),
    multi_line_comment: None,
    string_delimiters: StringDelims::Both,
    keywords: &["src", "dst", "wdir", "attrs", "attr", "arg", "data"],
    control_flow: &[
        "matches", "is", "isfile", "isdir", "from", "set", "add", "delete", "start", "to",
    ],
    definitions: &["plumb"],
    punctuation: &["=", "$"],
};

/// Determine the appropriate [Tokenizer] for the provided [Path].
///
/// Returns None is no Tokenizer is available.
pub(crate) fn try_tokenizer_for_path(path: &Path, first_line: Option<&str>) -> Option<Tokenizer> {
    if let Some(line) = first_line {
        if line.starts_with("#!") && (line.ends_with("sh") || line.ends_with("bash")) {
            return Some(Tokenizer::new(SHELL_SPEC));
        }
    }

    match path.extension() {
        Some(ext) if ext == "rs" => Some(Tokenizer::new(RUST_SPEC)),
        Some(ext) if ext == "sh" => Some(Tokenizer::new(SHELL_SPEC)),
        Some(ext) if ext == "rules" => Some(Tokenizer::new(PLUMBING_SPEC)),
        _ => None,
    }
}
