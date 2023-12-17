//! Simple lexing of file content to support syntax highlighting
use crate::{config::ColorScheme, regex::Regex, term::Style};
use std::{cell::RefCell, cmp::min};

pub const RUST_SPEC: LangSpec = LangSpec {
    single_line_comment: Some("//"),
    multi_line_comment: Some(("/*", "*/")),
    string_delimiters: StringDelims::Double,
    keywords: &[
        "pub", "return", "crate", "super", "use", "mod", "where", "mut", "self",
    ],
    control_flow: &["if", "else", "for", "in", "loop", "match"],
    definitions: &["let", "enum", "struct", "trait", "impl", "fn"],
    punctuation: &[
        "::", "&", "->", "=>", "==", "=", "?", "..", "+", "-", "*", "<", ">", "+=", "-=", "*=",
        ">=", "<=",
    ],
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    Dot,
    Default,
    Comment,
    String,
    Keyword,
    ControlFlow,
    Definition,
    Punctuation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub s: &'a str,
}

impl<'a> Token<'a> {
    pub fn render(&self, cs: &ColorScheme) -> String {
        match self.ty {
            TokenType::Dot => format!("{}{}", Style::Bg(cs.dot_bg), self.s),
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
                format!("{}{}{}", Style::Bg(cs.bg), Style::Fg(cs.definition), self.s)
            }
            TokenType::Punctuation => format!(
                "{}{}{}",
                Style::Bg(cs.bg),
                Style::Fg(cs.punctuation),
                self.s
            ),
        }
    }

    pub fn with_highlighted_dot(self, start: usize, end: usize) -> Vec<Token<'a>> {
        let clamped_end = min(end, self.s.len());
        let mut tks = Vec::with_capacity(3);

        if start > 0 {
            tks.push(Token {
                ty: self.ty,
                s: &self.s[..start],
            });
        }

        tks.push(Token {
            ty: TokenType::Dot,
            s: &self.s[start..clamped_end],
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
pub enum Tokens<'a> {
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

    pub fn with_highlighted_dot(self, start: usize, end: usize) -> Vec<Token<'a>> {
        match self {
            Self::Single(tk) => tk.with_highlighted_dot(start, end),

            Self::Multi(tks) => {
                let mut new_tks = Vec::new();
                let mut offset = 0;

                for tk in tks.into_iter() {
                    let len = tk.s.len();
                    let token_end = offset + len;

                    if start > token_end || end <= offset {
                        new_tks.push(tk);
                    } else {
                        new_tks.extend_from_slice(
                            &tk.with_highlighted_dot(start.saturating_sub(offset), end - offset),
                        );
                    }
                    offset += len;
                }

                new_tks
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StringDelims {
    Single,
    Double,
    Both,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangSpec {
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
            '.' | '@' | '?' | '+' | '*' | '{' | '}' | '[' | ']' | '(' | ')' | '^' | '$' => {
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
pub struct Tokenizer {
    ls: LangSpec,
    re: RefCell<Regex>,
}

impl Tokenizer {
    pub fn new(ls: LangSpec) -> Self {
        let re = RefCell::new(ls.re_from_all_literals());

        Self { ls, re }
    }

    /// FIXME: doing this by line with no way to look ahead or back won't work for multiline constructs
    pub fn tokenize<'a>(&self, line: &'a str) -> Tokens<'a> {
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

    // FIXME: this is horrifyingly inefficient right now
    fn regex_tokenize<'a>(&self, line: &'a str) -> Tokens<'a> {
        let mut tks = Vec::new();
        let mut offset = 0;

        for m in self.re.borrow_mut().match_str_all(line) {
            let (start, end) = m.loc();
            let s = m.str_match_text_ref(line);
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
            offset = end;
        }

        if offset < line.len() {
            tks.push(Token {
                ty: TokenType::Default,
                s: &line[offset..],
            });
        }

        if tks.len() == 1 {
            Tokens::Single(tks[0])
        } else {
            Tokens::Multi(tks)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TokenType::*;
    use super::*;
    use simple_test_case::test_case;

    #[test]
    fn with_highlighted_dot_works_for_single_token() {
        let tk = Token {
            ty: TokenType::Default,
            s: "hello, world!",
        };
        let tks = tk.with_highlighted_dot(2, 5);

        assert_eq!(
            tks,
            vec![
                Token {
                    ty: Default,
                    s: "he"
                },
                Token { ty: Dot, s: "llo" },
                Token {
                    ty: Default,
                    s: ", world!"
                }
            ]
        );
    }

    #[test]
    fn with_highlighted_dot_works_for_tokens() {
        let tks = vec![
            Token {
                ty: TokenType::Default,
                s: "hello, world!",
            },
            Token {
                ty: TokenType::Comment,
                s: " // this is a comment",
            },
        ];
        let tks = Tokens::Multi(tks).with_highlighted_dot(9, 21);

        assert_eq!(
            tks,
            vec![
                Token {
                    ty: Default,
                    s: "hello, wo"
                },
                Token { ty: Dot, s: "rld!" },
                Token {
                    ty: Dot,
                    s: " // this"
                },
                Token {
                    ty: Comment,
                    s: " is a comment"
                }
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

        assert_eq!(
            tks,
            Tokens::Single(Token {
                ty: TokenType::Default,
                s: line
            })
        );
    }
}
