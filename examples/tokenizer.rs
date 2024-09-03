//! A simple attempt at a generic tokenizer
use ad_editor::{regex::Regex, term::Color};
use std::collections::HashMap;

struct Face {
    fg: Color,
    bg: Option<Color>,
    italic: bool,
    bold: bool,
}

type ColorScheme = HashMap<String, Face>;

struct Tag {
    name: String,
    re: String,
}

impl Tag {
    pub fn new(name: impl Into<String>, re: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            re: re.into(),
        }
    }

    pub fn new_from_literals(name: impl Into<String>, lits: &[impl AsRef<str>]) -> Self {
        let mut re = String::new();
        re.push_str("^(");
        for lit in lits {
            push_escaped(&mut re, lit.as_ref());
            re.push('|');
        }
        re.pop(); // the final |
        re.push(')');

        Self {
            name: name.into(),
            re,
        }
    }
}

struct Token<'a> {
    tag: &'a str,
    txt: &'a str,
}

struct Tokenizer {
    re: Regex,
}

impl Tokenizer {
    pub fn new(tags: Vec<Tag>) -> Self {
        let mut s = String::new();

        for Tag { name, re } in tags.into_iter() {
            s.push_str(&format!("(?<{name}>{re})|"));
        }
        s.pop(); // last '|'

        Self {
            re: Regex::compile(&s).unwrap(),
        }
    }

    pub fn tokenize(&mut self, mut input: &str) {
        while !input.is_empty() {
            if let Some(m) = self.re.match_str(input) {
                println!(
                    "tag={:?} match={:?}",
                    m.named_matches()[0],
                    m.str_match_text_ref(input)
                );
                let (_, end) = m.loc();
                input = &input[end..];
            }
        }
    }
}

#[inline]
fn push_escaped(buf: &mut String, s: &str) {
    for ch in s.chars() {
        match ch {
            '.' | '@' | '?' | '+' | '*' | '{' | '}' | '[' | ']' | '(' | ')' | '^' | '$' | '|' => {
                buf.push('\\');
            }
            _ => (),
        }
        buf.push(ch);
    }
}

fn main() {
    let tags = vec![
        // This needs to handle r, r#, r##... prefixes
        Tag::new("STRING", r#"^"(\\.|[^\"\\])*"?"#),
        Tag::new("COMMENT", r"^(//.*\n|/\*@*\*/)"),
        Tag::new("LIFETIME", r"^'\w+\b"),
        Tag::new_from_literals(
            "KEYWORD",
            &[
                "pub", "return", "crate", "super", "use", "mod", "where", "mut", "self", "async",
                "await",
            ],
        ),
        Tag::new_from_literals(
            "CONTROL_FLOW",
            &[
                "if", "else", "for", "in", "while", "loop", "match", "break", "continue",
            ],
        ),
        Tag::new_from_literals(
            "DEFINITION",
            &[
                "let", "enum", "struct", "trait", "impl", "fn", "type", "const",
            ],
        ),
        Tag::new_from_literals(
            "OPERATOR",
            &[
                "::", "&", "%", "||", "|", "->", "=>", "==", "!=", ">>", "<<", "=", "?", "^", "..",
                "+", "-", "*", "/", "<", ">", "+=", "-=", "*=", "/=", "%=", ">=", "<=", "&=", "|=",
                "^=",
            ],
        ),
        Tag::new_from_literals(
            "PUNCTUATION",
            &["[", "]", "(", ")", "{", "}", ",", ".", ";", ":"],
        ),
        Tag::new("IDENTIFIER", r"^[a-z_][a-z0-9_]+"),
        Tag::new("CONST", r"^[A-Z_][A-Z0-9_]+"),
        Tag::new("TYPE", r"^[A-Z][a-zA-Z0-9]+"),
        Tag::new("WHITESPACE", r"^\s+"),
        Tag::new("DEFAULT", r"^\S+"),
    ];

    let mut tk = Tokenizer::new(tags);

    // tk.tokenize("let foo: FooBar = BAZ * 10;");
    tk.tokenize(include_str!("tokenizer.rs"));
}
