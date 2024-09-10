use super::{consume_whitespace, Error};
use crate::regex::Regex;
use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Expr {
    LoopMatches(Regex),
    LoopBetweenMatches(Regex),

    IfContains(Regex),
    IfNotContains(Regex),

    Insert(String),
    Append(String),
    Change(String),
    Sub(Regex, String),
    Print(String),
    Delete,

    Group(Vec<Vec<Expr>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum ParseOutput {
    Single(Expr),
    Pair(Expr, Expr),
}

impl Expr {
    pub(super) fn try_parse(it: &mut Peekable<Chars<'_>>) -> Result<ParseOutput, Error> {
        use Expr::*;
        use ParseOutput::*;

        match it.next() {
            Some('x') => Ok(Single(LoopMatches(parse_delimited_regex(it, "x")?))),
            Some('X') => Ok(Single(LoopMatches(Regex::compile(".*")?))),

            Some('y') => Ok(Single(LoopBetweenMatches(parse_delimited_regex(it, "y")?))),
            Some('Y') => Ok(Single(LoopBetweenMatches(Regex::compile(".*")?))),

            Some('g') => Ok(Single(IfContains(parse_delimited_regex(it, "g")?))),
            Some('v') => Ok(Single(IfNotContains(parse_delimited_regex(it, "v")?))),

            Some('i') => Ok(Single(Insert(parse_delimited_str(it, "i")?))),
            Some('a') => Ok(Single(Append(parse_delimited_str(it, "a")?))),
            Some('c') => Ok(Single(Change(parse_delimited_str(it, "c")?))),
            Some('s') => parse_sub(it),
            Some('d') => Ok(Single(Delete)),

            Some('p') => Ok(Single(Print(parse_delimited_str(it, "p")?))),
            Some('P') => Ok(Single(Print("$0".to_string()))),

            Some('{') => Ok(Single(Group(parse_group(it)?))),

            // Comments run until the end of the current line
            Some('#') => loop {
                match it.next() {
                    Some('\n') => {
                        consume_whitespace(it);
                        return Expr::try_parse(it);
                    }
                    None => return Err(Error::Eof),
                    _ => (),
                }
            },

            Some(ch) => Err(Error::UnexpectedCharacter(ch)),
            None => Err(Error::Eof),
        }
    }
}

fn parse_delimited_regex(it: &mut Peekable<Chars<'_>>, kind: &'static str) -> Result<Regex, Error> {
    let s = parse_delimited_str(it, kind)?;
    Ok(Regex::compile(&s)?)
}

fn parse_delimited_str(it: &mut Peekable<Chars<'_>>, kind: &'static str) -> Result<String, Error> {
    let delim = it.next().ok_or(Error::MissingDelimiter(kind))?;
    read_until(delim, it, kind)
}

fn read_until(
    delim: char,
    it: &mut Peekable<Chars<'_>>,
    kind: &'static str,
) -> Result<String, Error> {
    let mut s = String::new();
    let mut prev = delim;

    for ch in it {
        if ch == delim && prev != '\\' {
            return Ok(s);
        }
        s.push(ch);
        prev = ch;
    }

    Err(Error::UnclosedDelimiter(kind, delim))
}

fn parse_sub(it: &mut Peekable<Chars<'_>>) -> Result<ParseOutput, Error> {
    let delim = it.next().ok_or(Error::MissingDelimiter("s"))?;
    let re = Regex::compile(&read_until(delim, it, "s")?)?;
    let s = read_until(delim, it, "s")?;
    if let Some('g') = it.peek() {
        it.next();
        Ok(ParseOutput::Pair(Expr::LoopMatches(re), Expr::Change(s)))
    } else {
        Ok(ParseOutput::Single(Expr::Sub(re, s)))
    }
}

fn parse_group(it: &mut Peekable<Chars<'_>>) -> Result<Vec<Vec<Expr>>, Error> {
    let mut group = Vec::new();
    let mut branch = Vec::new();
    loop {
        consume_whitespace(it);
        match it.peek() {
            Some(';' | '\n') => {
                it.next();
                if branch.is_empty() {
                    return Err(Error::EmptyExpressionGroupBranch);
                }
                group.push(branch);
                branch = Vec::new();
            }
            Some('}') => {
                it.next();
                return if group.is_empty() {
                    Err(Error::EmptyExpressionGroup)
                } else if !branch.is_empty() {
                    Err(Error::UnclosedExpressionGroupBranch)
                } else {
                    Ok(group)
                };
            }
            // Comments need to be consumed inside of our parse of the group
            // so that the closing '}' is treated correctly
            Some('#') => loop {
                match it.next() {
                    Some('\n') => break,
                    None => return Err(Error::Eof),
                    _ => (),
                }
            },

            Some(_) => match Expr::try_parse(it)? {
                ParseOutput::Single(e) => branch.push(e),
                ParseOutput::Pair(e1, e2) => branch.extend([e1, e2]),
            },
            None => return Err(Error::UnclosedExpressionGroup),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;
    use Expr::*;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    fn s(e: Expr) -> ParseOutput {
        ParseOutput::Single(e)
    }

    fn p(e1: Expr, e2: Expr) -> ParseOutput {
        ParseOutput::Pair(e1, e2)
    }

    #[test_case("x/.*/", s(LoopMatches(re(".*"))); "x loop")]
    #[test_case("y/.*/", s(LoopBetweenMatches(re(".*"))); "y loop")]
    #[test_case("g/.*/", s(IfContains(re(".*"))); "g filter")]
    #[test_case("v/.*/", s(IfNotContains(re(".*"))); "v filter")]
    #[test_case("i/foo/", s(Insert("foo".to_string())); "insert")]
    #[test_case("a/foo/", s(Append("foo".to_string())); "append")]
    #[test_case("c/foo/", s(Change("foo".to_string())); "change")]
    #[test_case("s/.*/foo/", s(Sub(re(".*"), "foo".to_string())); "substitute")]
    #[test_case("s/.*/foo/g", p(LoopMatches(re(".*")), Change("foo".to_string())); "substitute all")]
    #[test_case("p/$0/", s(Print("$0".to_string())); "print")]
    #[test_case("P", s(Print("$0".to_string())); "print full match")]
    #[test_case("d", s(Delete); "delete")]
    #[test_case(
        "{P; g/bar/ a/foo/;}",
        s(Group(vec![
            vec![Print("$0".to_string())],
            vec![IfContains(re("bar")), Append("foo".to_string())]
        ]));
        "group"
    )]
    #[test]
    fn parse_expr_works(input: &str, expected: ParseOutput) {
        let a = Expr::try_parse(&mut input.chars().peekable()).expect("valid input");
        assert_eq!(a, expected);
    }
}
