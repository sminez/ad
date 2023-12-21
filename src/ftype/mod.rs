pub mod lex;

use lex::{LangSpec, StringDelims};

pub const RUST_SPEC: LangSpec = LangSpec {
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
