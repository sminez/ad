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
    Sub(Regex, String, bool),
    Print(String),
    Delete,

    Group(Vec<Vec<Expr>>),
}

impl Expr {
    pub(super) fn try_parse(it: &mut Peekable<Chars>) -> Result<Self, Error> {
        match it.next() {
            Some('x') => Ok(Expr::LoopMatches(parse_delimited_regex(it, "x")?)),
            Some('y') => Ok(Expr::LoopBetweenMatches(parse_delimited_regex(it, "y")?)),
            Some('g') => Ok(Expr::IfContains(parse_delimited_regex(it, "g")?)),
            Some('v') => Ok(Expr::IfNotContains(parse_delimited_regex(it, "v")?)),
            Some('i') => Ok(Expr::Insert(parse_delimited_str(it, "i")?)),
            Some('a') => Ok(Expr::Append(parse_delimited_str(it, "a")?)),
            Some('c') => Ok(Expr::Change(parse_delimited_str(it, "c")?)),
            Some('s') => parse_sub(it),
            Some('p') => Ok(Expr::Print(parse_delimited_str(it, "p")?)),
            Some('d') => Ok(Expr::Delete),
            Some('{') => Ok(Expr::Group(parse_group(it)?)),

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

fn parse_delimited_regex(it: &mut Peekable<Chars>, kind: &'static str) -> Result<Regex, Error> {
    let s = parse_delimited_str(it, kind)?;
    Ok(Regex::compile(&s)?)
}

fn parse_delimited_str(it: &mut Peekable<Chars>, kind: &'static str) -> Result<String, Error> {
    let delim = it.next().ok_or(Error::MissingDelimiter(kind))?;
    read_until(delim, it, kind)
}

fn read_until(delim: char, it: &mut Peekable<Chars>, kind: &'static str) -> Result<String, Error> {
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

fn parse_sub(it: &mut Peekable<Chars>) -> Result<Expr, Error> {
    let delim = it.next().ok_or(Error::MissingDelimiter("s"))?;
    let re = Regex::compile(&read_until(delim, it, "s")?)?;
    let s = read_until(delim, it, "s")?;
    if let Some('g') = it.peek() {
        it.next();
        Ok(Expr::Sub(re, s, true))
    } else {
        Ok(Expr::Sub(re, s, false))
    }
}

fn parse_group(it: &mut Peekable<Chars>) -> Result<Vec<Vec<Expr>>, Error> {
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

            Some(_) => branch.push(Expr::try_parse(it)?),
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

    #[test_case("x/.*/", LoopMatches(re(".*")); "x loop")]
    #[test_case("y/.*/", LoopBetweenMatches(re(".*")); "y loop")]
    #[test_case("g/.*/", IfContains(re(".*")); "g filter")]
    #[test_case("v/.*/", IfNotContains(re(".*")); "v filter")]
    #[test_case("i/foo/", Insert("foo".to_string()); "insert")]
    #[test_case("a/foo/", Append("foo".to_string()); "append")]
    #[test_case("c/foo/", Change("foo".to_string()); "change")]
    #[test_case("s/.*/foo/", Sub(re(".*"), "foo".to_string(), false); "substitute")]
    #[test_case("s/.*/foo/g", Sub(re(".*"), "foo".to_string(), true); "substitute all")]
    #[test_case("p/$0/", Print("$0".to_string()); "print")]
    #[test_case("d", Delete; "delete")]
    #[test_case(
        "{p/$0/; g/bar/ a/foo/;}",
        Group(vec![
            vec![Print("$0".to_string())],
            vec![IfContains(re("bar")), Append("foo".to_string())]
        ]);
        "group"
    )]
    #[test]
    fn parse_expr_works(s: &str, expected: Expr) {
        let a = Expr::try_parse(&mut s.chars().peekable()).expect("valid input");
        assert_eq!(a, expected);
    }
}
