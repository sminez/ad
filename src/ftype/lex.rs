//! Simple lexing of file content to support syntax highlighting
use crate::{config::ColorScheme, regex::Regex, term::Style};
use std::{cell::RefCell, cmp::min};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenType {
    Dot,
    Load,
    Execute,
    Default,
    Comment,
    String,
    Keyword,
    ControlFlow,
    Definition,
    Punctuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Token<'a> {
    pub ty: TokenType,
    pub s: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn render(&self, cs: &ColorScheme) -> String {
        match self.ty {
            TokenType::Dot => format!("{}{}", Style::Bg(cs.dot_bg), self.s),
            TokenType::Load => format!("{}{}", Style::Bg(cs.load_bg), self.s),
            TokenType::Execute => format!("{}{}", Style::Bg(cs.exec_bg), self.s),
            TokenType::Default => format!("{}{}{}", Style::Bg(cs.bg), Style::Fg(cs.fg), self.s),
            TokenType::Comment => format!(
                "{}{}{}{}{}",
                Style::Italic,
                Style::Bg(cs.bg),
                Style::Fg(cs.comment),
                self.s,
                Style::NoItalic,
            ),
            TokenType::String => format!("{}{}{}", Style::Bg(cs.bg), Style::Fg(cs.string), self.s),
            TokenType::Keyword => format!(
                "{}{}{}{}{}",
                Style::Italic,
                Style::Bg(cs.bg),
                Style::Fg(cs.keyword),
                self.s,
                Style::NoItalic,
            ),
            TokenType::ControlFlow => format!(
                "{}{}{}{}{}",
                Style::Bold,
                Style::Bg(cs.bg),
                Style::Fg(cs.control_flow),
                self.s,
                Style::NoBold,
            ),
            TokenType::Definition => {
                format!(
                    "{}{}{}{}{}",
                    Style::Italic,
                    Style::Bg(cs.bg),
                    Style::Fg(cs.definition),
                    self.s,
                    Style::NoItalic
                )
            }
            TokenType::Punctuation => format!(
                "{}{}{}",
                Style::Bg(cs.bg),
                Style::Fg(cs.punctuation),
                self.s
            ),
        }
    }

    fn with_highlighted_dot(self, start: usize, end: usize, ty: TokenType) -> Vec<Token<'a>> {
        let (byte_start, _) = self.s.char_indices().nth(start).unwrap_or((0, 'a'));
        let (byte_end, _) = self.s.char_indices().nth(end).unwrap_or((usize::MAX, 'a'));
        let clamped_end = min(byte_end, self.s.len());
        let mut tks = Vec::with_capacity(3);

        if byte_start > 0 {
            tks.push(Token {
                ty: self.ty,
                s: &self.s[..byte_start],
            });
        }

        tks.push(Token {
            ty,
            s: &self.s[byte_start..clamped_end],
        });

        if clamped_end < self.s.len() {
            tks.push(Token {
                ty: self.ty,
                s: &self.s[clamped_end..],
            });
        }

        tks
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Tokens<'a> {
    Single(Token<'a>),
    Multi(Vec<Token<'a>>),
}

impl<'a> Tokens<'a> {
    fn push(&mut self, other: Token<'a>) {
        match self {
            Self::Single(tk) => *self = Tokens::Multi(vec![*tk, other]),
            Self::Multi(tks) => tks.push(other),
        }
    }

    pub fn with_highlighted_dot(self, start: usize, end: usize, ty: TokenType) -> Vec<Token<'a>> {
        match self {
            Self::Single(tk) => tk.with_highlighted_dot(start, end, ty),

            Self::Multi(tks) => {
                let mut new_tks = Vec::new();
                let mut offset = 0;

                for tk in tks.into_iter() {
                    let len = tk.s.chars().count();
                    let token_end = offset + len;

                    if start >= token_end || end <= offset {
                        new_tks.push(tk);
                    } else {
                        new_tks.extend_from_slice(&tk.with_highlighted_dot(
                            start.saturating_sub(offset),
                            end - offset,
                            ty,
                        ));
                    }
                    offset += len;
                }

                new_tks
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum StringDelims {
    Single,
    Double,
    Both,
}

impl StringDelims {
    const RE_DOUBLE: &'static str = r#""(\\.|[^\"\\])*"?"#;
    const RE_SINGLE: &'static str = r#"'(\\.|[^\'\\])*'?"#;

    fn as_re_string(&self) -> String {
        match self {
            Self::Double => Self::RE_DOUBLE.to_string(),
            Self::Single => Self::RE_SINGLE.to_string(),
            Self::Both => format!("{}|{}", Self::RE_DOUBLE, Self::RE_SINGLE),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LangSpec {
    pub single_line_comment: Option<&'static str>,
    pub multi_line_comment: Option<(&'static str, &'static str)>,
    pub string_delimiters: StringDelims,
    pub keywords: &'static [&'static str],
    pub control_flow: &'static [&'static str],
    pub definitions: &'static [&'static str],
    pub punctuation: &'static [&'static str],
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

impl LangSpec {
    fn re_from_all_literals(&self) -> Regex {
        let mut re = String::new();
        re.push_str("(\\b(");
        for words in [self.keywords, self.control_flow, self.definitions] {
            for word in words {
                push_escaped(&mut re, word);
                re.push('|');
            }
        }
        re.pop(); // the final |
        re.push_str(")\\b)|(");

        for word in self.punctuation {
            push_escaped(&mut re, word);
            re.push('|');
        }
        re.pop();
        re.push(')');

        Regex::compile(&re).expect("valid regex to be build from re_from_words")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Tokenizer {
    ls: LangSpec,
    re: RefCell<Regex>,
    re_str: RefCell<Regex>,
}

impl Tokenizer {
    pub(crate) fn new(ls: LangSpec) -> Self {
        let re = RefCell::new(ls.re_from_all_literals());
        let re_str = RefCell::new(
            Regex::compile(&ls.string_delimiters.as_re_string())
                .expect("valid regex to be build from string_delimiters"),
        );

        Self { ls, re, re_str }
    }

    /// FIXME: doing this by line with no way to look ahead or back won't work for multiline constructs
    pub(crate) fn tokenize<'a>(&self, line: &'a str) -> Tokens<'a> {
        match self.split_off_comments(line) {
            ("", Some(tk)) => Tokens::Single(tk),
            (s, None) => self.regex_tokenize(s),
            (s, Some(tk)) => {
                let mut tks = self.regex_tokenize(s);
                tks.push(tk);
                tks
            }
        }
    }

    fn split_off_comments<'a>(&self, line: &'a str) -> (&'a str, Option<Token<'a>>) {
        if let Some(pat) = self.ls.single_line_comment {
            if let Some(comment_start) = line.find(pat) {
                let (s, comment) = line.split_at(comment_start);

                return (
                    s,
                    Some(Token {
                        ty: TokenType::Comment,
                        s: comment,
                    }),
                );
            }
        }

        (line, None)
    }

    fn regex_tokenize<'a>(&self, line: &'a str) -> Tokens<'a> {
        let mut tks = Vec::new();
        let mut offset = 0;

        for m in self.re_str.borrow_mut().match_str_all(line) {
            let (start, end, s) = m.str_match_text_ref_with_byte_offsets(line);
            if start > offset {
                tks.extend_from_slice(&self.regex_tokenize_literals(&line[offset..start]));
            }
            tks.push(Token {
                ty: TokenType::String,
                s,
            });
            offset = end + 1;
        }

        if offset < line.len() {
            tks.extend_from_slice(&self.regex_tokenize_literals(&line[offset..]));
        }

        if tks.len() == 1 {
            Tokens::Single(tks[0])
        } else {
            Tokens::Multi(tks)
        }
    }

    fn regex_tokenize_literals<'a>(&self, line: &'a str) -> Vec<Token<'a>> {
        let mut tks = Vec::new();
        let mut offset = 0;

        for m in self.re.borrow_mut().match_str_all(line) {
            let (start, end, s) = m.str_match_text_ref_with_byte_offsets(line);
            if start > offset {
                tks.push(Token {
                    ty: TokenType::Default,
                    s: &line[offset..start],
                });
            }

            let ty = if self.ls.keywords.contains(&s) {
                TokenType::Keyword
            } else if self.ls.control_flow.contains(&s) {
                TokenType::ControlFlow
            } else if self.ls.definitions.contains(&s) {
                TokenType::Definition
            } else if self.ls.punctuation.contains(&s) {
                TokenType::Punctuation
            } else {
                panic!("broken parse: s={s:?}")
            };

            tks.push(Token { ty, s });
            offset = end + 1;
        }

        if offset < line.len() {
            tks.push(Token {
                ty: TokenType::Default,
                s: &line[offset..],
            });
        }

        tks
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;
    use crate::ftype::RUST_SPEC;
    use simple_test_case::test_case;

    fn t_def(s: &str) -> Token<'_> {
        Token { ty: Default, s }
    }

    fn t_dot(s: &str) -> Token<'_> {
        Token { ty: Dot, s }
    }

    fn t_dfn(s: &str) -> Token<'_> {
        Token { ty: Definition, s }
    }

    fn t_cfl(s: &str) -> Token<'_> {
        Token { ty: ControlFlow, s }
    }

    fn t_str(s: &str) -> Token<'_> {
        Token { ty: String, s }
    }

    #[test]
    fn with_highlighted_dot_works_for_single_token() {
        let tk = t_def("hello, world!");
        let tks = tk.with_highlighted_dot(5, 9, TokenType::Dot);

        assert_eq!(tks, vec![t_def("hello"), t_dot(", wo"), t_def("rld!"),]);
    }

    #[test]
    fn with_highlighted_dot_works_on_boundaries() {
        let tks = vec![t_def("hello"), t_cfl(","), t_def(" world!")];
        let tks = Tokens::Multi(tks).with_highlighted_dot(5, 9, TokenType::Dot);

        assert_eq!(
            tks,
            vec![t_def("hello"), t_dot(","), t_dot(" wo"), t_def("rld!"),]
        );
    }

    #[test]
    fn with_highlighted_dot_works_with_multibyte() {
        let tks = vec![t_def("hello, world! [a-z"), t_cfl("¡-￿0-9_\\-./@]+")];
        let tks = Tokens::Multi(tks).with_highlighted_dot(18, 21, TokenType::Dot);

        assert_eq!(
            tks,
            vec![
                t_def("hello, world! [a-z"),
                t_dot("¡-￿"),
                t_cfl("0-9_\\-./@]+"),
            ]
        );
    }

    #[test_case("find_forward_wrapping"; "start of word")]
    #[test_case("min"; "end of word")]
    #[test_case("submatching"; "in middle of word")]
    #[test]
    fn tokens_inside_words_do_not_match(line: &str) {
        let tokenizer = Tokenizer::new(RUST_SPEC);
        let tks = tokenizer.regex_tokenize(line);

        assert_eq!(tks, Tokens::Single(t_def(line)));
    }

    #[test_case(
        "impl Default for Foo {",
        vec![t_dfn("impl"), t_def(" Default "), t_cfl("for"), t_def(" Foo {")];
        "ascii with keywords and punctuation"
    )]
    #[test_case(
        r#"#[test_case("/oo.fo/ d", "fo│foo"; "regex dot delete")]"#,
        vec![
            t_def("#[test_case("), t_str("\"/oo.fo/ d\""), t_def(", "), t_str("\"fo│foo\""),
            t_def("; "), t_str("\"regex dot delete\""), t_def(")]")
        ];
        "utf8"
    )]
    #[test]
    fn tokenize_works(s: &'static str, expected: Vec<Token<'static>>) {
        let tokenizer = Tokenizer::new(RUST_SPEC);
        let tks = tokenizer.regex_tokenize(s);

        assert_eq!(tks, Tokens::Multi(expected));
    }
}
