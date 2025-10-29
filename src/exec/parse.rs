//! Parsing for the ad-exec language
use crate::{
    exec::addr::Addr,
    parse::{self, ParseInput, Span},
};

pub type Error = parse::Error<String>;

#[derive(Debug, PartialEq, Eq)]
pub(super) enum Ast {
    /// Explicitly set the current dot to the provided address.
    SetAddr(SetAddr),

    /// Extract all non-overlapping matches of a regex from dot and
    /// then run the the provided nodes over each match.
    Extract(Extract),
    /// Extract all non-overlapping matches of a regex from dot and
    /// then run the the provided nodes over the regions between
    /// each match.
    ExtractBetween(Extract),

    /// Run each node in parallel over the current dot.
    Parallel(Sequence),
    /// Run each node in series over the current dot.
    Series(Sequence),

    /// If the current dot matches the given regex run the given
    /// nodes in parallel over dot.
    Guard(Guard),
    /// If the current dot doesn't matche the given regex run the given
    /// nodes in parallel over dot.
    InvGuard(Guard),

    /// Template and insert the given string before dot.
    Insert(Template),
    /// Template and insert the given string after dot.
    Append(Template),
    /// Replace dot with the templated string.
    Change(Template),
    /// Template and print the given string
    Print(Template),

    /// Delete the contents of dot.
    Delete(Span),

    /// Comments are captured to correctly track spans but are
    /// dropped when compiling
    Comment(Span),
}

impl Ast {
    fn is_comment(&self) -> bool {
        matches!(self, Ast::Comment(_))
    }

    pub fn span(&self) -> &Span {
        match self {
            Ast::SetAddr(sa) => &sa.span,
            Ast::Extract(e) | Ast::ExtractBetween(e) => &e.span,
            Ast::Parallel(s) | Ast::Series(s) => &s.span,
            Ast::Guard(g) | Ast::InvGuard(g) => &g.span,
            Ast::Insert(t) | Ast::Append(t) | Ast::Change(t) | Ast::Print(t) => &t.span,
            Ast::Delete(s) | Ast::Comment(s) => s,
        }
    }
}

// Spanned Ast nodes
#[derive(Debug, PartialEq, Eq)]
pub(super) struct SetAddr {
    pub span: Span,
    pub addr: Box<Addr>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Extract {
    pub span: Span,
    pub re: String,
    pub nodes: Vec<Ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Sequence {
    pub span: Span,
    pub nodes: Vec<Ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Guard {
    pub span: Span,
    pub re: String,
    pub nodes: Vec<Ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Template {
    pub span: Span,
    pub s: String,
}

#[derive(Debug)]
pub(super) struct Parser<'a> {
    input: ParseInput<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(prog: &'a str) -> Self {
        Self {
            input: ParseInput::new(prog),
        }
    }

    pub fn parse(&self) -> Result<Ast, Error> {
        self.reset();
        let mut seq = Sequence {
            span: self.input.span(),
            nodes: vec![],
        };

        while !self.input.at_eof() {
            let node = self.parse1(false)?;
            if !node.is_comment() {
                seq.nodes.push(node);
            }
            self.input.consume_whitespace();
        }

        match seq.nodes.len() {
            0 => Err(self.error("empty program")),
            1 => Ok(seq.nodes.remove(0)),
            _ => {
                seq.span = seq.span.with_end(self.input.pos());
                Ok(Ast::Series(seq))
            }
        }
    }

    /// Must be called after consuming whitespace and comments
    fn parse1(&self, in_group: bool) -> Result<Ast, Error> {
        self.input.consume_whitespace();

        let ast = match self.input.char() {
            '#' => Ast::Comment(self.parse_comment()),

            // Braced group
            '{' => Ast::Parallel(self.parse_group()?),

            // Extract
            'x' => Ast::Extract(self.parse_extract(in_group)?),
            'y' => Ast::ExtractBetween(self.parse_extract(in_group)?),

            // Guard
            'g' => Ast::Guard(self.parse_guard(in_group)?),
            'v' => Ast::InvGuard(self.parse_guard(in_group)?),

            // Template
            'i' => Ast::Insert(self.parse_template()?),
            'a' => Ast::Append(self.parse_template()?),
            'c' => Ast::Change(self.parse_template()?),
            'p' => Ast::Print(self.parse_template()?),

            // Special cases
            'X' => {
                let start = self.input.pos();
                self.input.advance();
                let nodes = self.parse_seq(in_group)?;

                Ast::Extract(Extract {
                    span: self.span().with_start(start),
                    re: ".*\n".to_string(),
                    nodes,
                })
            }

            'P' => {
                let ast = Ast::Print(Template {
                    span: self.span(),
                    s: "$0\n".to_string(),
                });
                self.input.advance();
                ast
            }

            'd' => {
                let ast = Ast::Delete(self.span());
                self.input.advance();
                ast
            }

            ';' => return Err(self.error("unexpected ';' in input")),
            '}' => return Err(self.error("unexpected '}' in input")),

            // Default to trying to parse an Addr
            _ => {
                let span = self.span();
                Ast::SetAddr(SetAddr {
                    span: span.with_end(self.input.pos()),
                    addr: Box::new(
                        Addr::parse_from_input(&self.input)
                            .map_err(|e| self.error(e.to_string()))?,
                    ),
                })
            }
        };

        Ok(ast)
    }

    fn reset(&self) {
        self.input.reset();
    }

    /// Crate a null span at the current parser position
    fn span(&self) -> Span {
        self.input.span()
    }

    fn error(&self, kind: impl Into<String>) -> Error {
        Error::new(kind, self.input.text(), self.input.span())
    }

    fn parse_comment(&self) -> Span {
        assert_eq!(self.input.char(), '#');
        let span = self.input.span();
        let mut end = self.input.pos();

        loop {
            if self.input.try_consume("#") {
                self.input.consume_until('\n');
                end = self.input.pos();
                self.input.consume_whitespace();
            } else {
                break;
            }
        }

        span.with_end(end)
    }

    fn parse_template(&self) -> Result<Template, Error> {
        assert!("iacp".contains(self.input.char()));

        let start = self.input.pos();
        self.input.advance();
        let s = self.parse_delimited_str()?;

        Ok(Template {
            span: self.span().with_start(start),
            s,
        })
    }

    fn parse_extract(&self, in_group: bool) -> Result<Extract, Error> {
        assert!("xy".contains(self.input.char()));

        let start = self.input.pos();
        self.input.advance();
        let re = self.parse_delimited_str()?;
        let nodes = self.parse_seq(in_group)?;
        let end = nodes.last().unwrap().span().end;

        Ok(Extract {
            span: Span::new(start, end),
            re,
            nodes,
        })
    }

    fn parse_seq(&self, in_group: bool) -> Result<Vec<Ast>, Error> {
        let mut nodes = Vec::new();
        while !self.input.at_eof() {
            let node = self.parse1(in_group)?;
            if !node.is_comment() {
                nodes.push(node);
            }
            self.input.consume_whitespace();
            if !self.input.at_eof() && in_group && self.input.char() == ';' {
                break;
            }
        }

        if nodes.is_empty() {
            return Err(self.error("empty sequence"));
        }

        Ok(nodes)
    }

    fn parse_guard(&self, in_group: bool) -> Result<Guard, Error> {
        assert!("gv".contains(self.input.char()));

        let start = self.input.pos();
        let mut end = start;
        self.input.advance();
        let re = self.parse_delimited_str()?;
        self.input.consume_whitespace();

        let mut nodes = Vec::new();

        while !self.input.at_eof() {
            let node = self.parse1(in_group)?;
            end = self.input.pos();
            if !node.is_comment() {
                nodes.push(node);
            }

            self.input.consume_whitespace();
            if self.input.at_eof() {
                break;
            }

            if in_group && self.input.char() == ';' {
                end = self.input.pos();
                break;
            }
        }

        if nodes.is_empty() {
            return Err(self.error("empty sequence"));
        }

        Ok(Guard {
            span: Span::new(start, end),
            re,
            nodes,
        })
    }

    fn parse_group(&self) -> Result<Sequence, Error> {
        assert_eq!(self.input.char(), '{');
        let mut seq = Sequence {
            span: self.input.span(),
            nodes: vec![],
        };
        let mut branch = Vec::new();
        let mut start = self.input.pos();
        self.input.advance();

        loop {
            self.input.consume_whitespace();
            if self.input.at_eof() {
                return Err(self.error("unclosed group"));
            } else if self.input.char() == '}' {
                self.input.advance(); // consume the '}'

                return if seq.nodes.is_empty() {
                    Err(self.error("empty group"))
                } else {
                    seq.span = seq.span.with_end(self.input.pos());
                    Ok(seq)
                };
            }
            let node = self.parse1(true)?;
            if !node.is_comment() {
                branch.push(node);
            }
            self.input.consume_whitespace();
            if self.input.char() == ';' {
                let mut span = self.span().with_start(start);
                self.input.advance(); // consume the ';'

                let node = match branch.len() {
                    0 => return Err(self.error("empty group branch")),
                    1 => branch.remove(0),
                    _ => {
                        span = span.with_end(self.input.pos());
                        Ast::Series(Sequence {
                            span,
                            nodes: branch,
                        })
                    }
                };

                seq.nodes.push(node);
                branch = Vec::new();
                start = self.input.pos();
            }
        }
    }

    /// Can't use ParseInput::read_until for this (or return &str) as we need to handle
    /// escaping the delimiter inside of the string.
    fn parse_delimited_str(&self) -> Result<String, Error> {
        let delim = self.input.char();
        if !self.input.advance() {
            return Err(self.error("EOF"));
        }

        let mut s = String::new();
        let mut prev = delim;

        while !self.input.at_eof() {
            let ch = self.input.char();
            if ch == delim {
                if prev == '\\' {
                    s.push(ch);
                } else {
                    self.input.advance(); // consume the delimiter
                    return Ok(s);
                }
            } else {
                if prev == '\\' {
                    s.push('\\');
                }
                if ch != '\\' {
                    s.push(ch);
                }
            }
            prev = ch;
            self.input.advance();
        }

        Err(self.error(format!("unclosed delimiter '{delim}'")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::Position;
    use simple_test_case::test_case;

    fn span_at(offset: usize, line: usize, column: usize) -> Span {
        Span::at(Position::new(offset, line, column))
    }

    fn span(start: (usize, usize, usize), end: (usize, usize, usize)) -> Span {
        Span::new(
            Position::new(start.0, start.1, start.2),
            Position::new(end.0, end.1, end.2),
        )
    }

    #[test_case("/foo/", Ok("foo"); "slash")]
    #[test_case(":foo:", Ok("foo"); "colon")]
    #[test_case("|foo|", Ok("foo"); "pipe")]
    #[test_case("/foo\\/bar/", Ok("foo/bar"); "with escaped delimiter")]
    #[test_case("/foo\nbar/", Ok("foo\nbar"); "with escaped non-delimiter")]
    #[test_case(
        "/",
        Err(Error::new("EOF", "/", span_at(1, 1, 2)));
        "unexpected EOF"
    )]
    #[test_case(
        "/foo",
        Err(Error::new("unclosed delimiter '/'", "/foo", span_at(4, 1, 5)));
        "unclosed delimiter"
    )]
    #[test]
    fn parse_delimited_str_works(input: &str, expected: Result<&str, Error>) {
        let p = Parser::new(input);
        let res = p.parse_delimited_str();
        assert_eq!(res, expected.map(String::from));
    }

    #[test]
    fn parse_template_returns_correct_span() {
        let p = Parser::new("c/bar/  # comment");
        let template = p.parse_template().unwrap();
        let substr = p.input.span_text(&template.span);

        assert_eq!(substr, "c/bar/");
    }

    #[test]
    fn parse_extract_retuns_correct_span() {
        let p = Parser::new("x/foo/ c/bar/ a/baz/  # comment");
        let extract = p.parse_extract(false).unwrap();
        let substr = p.input.span_text(&extract.span);

        assert_eq!(substr, "x/foo/ c/bar/ a/baz/");
    }

    #[test]
    fn parse_guard_retuns_correct_span() {
        let p = Parser::new("g/foo/ c/bar/   ");
        let guard = p.parse_guard(false).unwrap();
        let substr = p.input.span_text(&guard.span);

        assert_eq!(substr, "g/foo/ c/bar/");
    }

    #[test]
    fn parse_group_retuns_correct_span() {
        let p = Parser::new("  { x/foo/ c/bar/;\nx/bar/ c/foo/; }    ");
        p.input.consume_whitespace();
        let seq = p.parse_group().unwrap();
        let substr = p.input.span_text(&seq.span);

        assert_eq!(substr, "{ x/foo/ c/bar/;\nx/bar/ c/foo/; }");
    }

    #[test_case("d", Ast::Delete(span_at(0, 1, 1)); "delete")]
    #[test_case("i/this/",
        Ast::Insert(Template {
            span: span((0, 1, 1), (7, 1, 8)),
            s: "this".to_string()
        });
        "insert"
    )]
    #[test_case("a/this/",
        Ast::Append(Template {
            span: span((0, 1, 1), (7, 1, 8)),
            s: "this".to_string()
        });
        "append"
    )]
    #[test_case("c/this/",
        Ast::Change(Template {
            span: span((0, 1, 1), (7, 1, 8)),
            s: "this".to_string()
        });
        "change"
    )]
    #[test_case("p/this/",
        Ast::Print(Template {
            span: span((0, 1, 1), (7, 1, 8)),
            s: "this".to_string()
        });
        "print"
    )]
    #[test_case("P",
        Ast::Print(Template {
            span: span_at(0, 1, 1),
            s: "$0\n".to_string()
        });
        "print line"
    )]
    #[test_case("x/foo/ d",
        Ast::Extract(Extract {
            span: span((0, 1, 1), (7, 1, 8)),
            re: "foo".to_string(),
            nodes: vec![Ast::Delete(span_at(7, 1, 8))],
        });
        "extract"
    )]
    #[test_case("y/foo/ d",
        Ast::ExtractBetween(Extract {
            span: span((0, 1, 1), (7, 1, 8)),
            re: "foo".to_string(),
            nodes: vec![Ast::Delete(span_at(7, 1, 8))],
        });
        "extract between"
    )]
    #[test_case("X d",
        Ast::Extract(Extract {
            span: span((0, 1, 1), (3, 1, 4)),
            re: ".*\n".to_string(),
            nodes: vec![Ast::Delete(span_at(2, 1, 3))],
        });
        "extract lines"
    )]
    #[test_case("g/foo/ d",
        Ast::Guard(Guard {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            nodes: vec![Ast::Delete(span_at(7, 1, 8))],
        });
        "guard"
    )]
    #[test_case("v/foo/ d",
        Ast::InvGuard(Guard {
            span: span((0, 1, 1), (8, 1, 9)),
            re: "foo".to_string(),
            nodes: vec![Ast::Delete(span_at(7, 1, 8))],
        });
        "inv-guard"
    )]
    #[test_case("{ d; P; }",
        Ast::Parallel(Sequence {
            span: span((0, 1, 1), (9, 1, 10)),
            nodes: vec![
                Ast::Delete(span_at(2, 1, 3)),
                Ast::Print(Template {
                    span: span_at(5, 1, 6),
                    s: "$0\n".to_string()
                })
            ],
        });
        "group"
    )]
    #[test]
    fn parse1_works(input: &str, expected: Ast) {
        let p = Parser::new(input);
        let ast = p.parse1(false).unwrap();
        assert_eq!(ast, expected);
    }
}
