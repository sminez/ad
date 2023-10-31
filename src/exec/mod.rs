//! Sam style language for running edit commands using structural regular expressions
use crate::{
    regex::{self, Match},
    util::parse_num,
};
use std::{cmp::Ordering, io::Write, iter::Peekable, str::Chars};

mod expr;
mod stream;

use expr::Expr;
pub use stream::{CachedStdin, IterableStream};

/// Variable usable in templates for injecting the current filename.
/// (Following the naming convention used in Awk)
const FNAME_VAR: &str = "$FILENAME";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    EmptyExpressionGroup,
    EmptyExpressionGroupBranch,
    EmptyProgram,
    Eof,
    InvalidRegex(regex::Error),
    InvalidSubstitution(usize),
    MissingAction,
    MissingDelimiter(&'static str),
    UnclosedDelimiter(&'static str, char),
    UnclosedExpressionGroup,
    UnclosedExpressionGroupBranch,
    UnexpectedCharacter(char),
}

impl From<regex::Error> for Error {
    fn from(err: regex::Error) -> Self {
        Error::InvalidRegex(err)
    }
}

/// A parsed and compiled program that can be executed against an input
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    initial_dot: (usize, Option<usize>),
    exprs: Vec<Expr>,
}

impl Program {
    /// Execute this program against a given Rope
    pub fn execute<S, W>(
        &mut self,
        stream: &mut S,
        fname: &str,
        out: &mut W,
    ) -> Result<(usize, usize), Error>
    where
        S: IterableStream,
        W: Write,
    {
        let (from, to) = self.initial_dot;
        let initial = Match::synthetic(from, to.unwrap_or_else(|| stream.max_dot()));
        self.step(stream, initial, 0, fname, out)
    }

    fn step<S, W>(
        &mut self,
        stream: &mut S,
        m: Match,
        pc: usize,
        fname: &str,
        out: &mut W,
    ) -> Result<(usize, usize), Error>
    where
        S: IterableStream,
        W: Write,
    {
        let (mut from, mut to) = m.loc();

        match self.exprs[pc].clone() {
            Expr::Group(g) => {
                let mut res = (from, to);
                for exprs in g {
                    let mut p = Program {
                        initial_dot: (from, Some(to)),
                        exprs: exprs.clone(),
                    };
                    res = p.step(stream, m, 0, fname, out)?;
                }
                Ok(res)
            }

            Expr::LoopMatches(mut re) => loop {
                match re.match_iter(&mut stream.iter_between(from, to), from) {
                    Some(m) => {
                        let cur_len = stream.len_chars();
                        (_, from) = self.step(stream, m, pc + 1, fname, out)?;
                        let new_len = stream.len_chars();
                        from += 1;

                        match new_len.cmp(&cur_len) {
                            Ordering::Greater => to += new_len - cur_len,
                            Ordering::Less => to -= cur_len - new_len,
                            _ => (),
                        }

                        if from > to {
                            return Ok((from, to));
                        }
                    }
                    None => return Ok((from, to)),
                }
            },

            Expr::LoopBetweenMatches(mut re) => loop {
                match re.match_iter(&mut stream.iter_between(from, to), from) {
                    Some(m) => {
                        let (initial_to, new_from) = m.loc();
                        if initial_to == 0 {
                            // skip matches of the null string at the start of input
                            from = new_from + 1;
                            continue;
                        }

                        let m = Match::synthetic(from, initial_to - 1);
                        let cur_len = stream.len_chars();
                        (_, _) = self.step(stream, m, pc + 1, fname, out)?;
                        let new_len = stream.len_chars();

                        match new_len.cmp(&cur_len) {
                            Ordering::Greater => to += new_len - cur_len,
                            Ordering::Less => to -= cur_len - new_len,
                            _ => (),
                        }

                        from = new_from + new_len - cur_len + 1;

                        if from > to {
                            return Ok((from, to));
                        }
                    }
                    None => return Ok((from, to)),
                }
            },

            Expr::IfContains(mut re) => {
                if re.matches_iter(&mut stream.iter_between(from, to), from) {
                    self.step(stream, m, pc + 1, fname, out)
                } else {
                    Ok((from, to))
                }
            }

            Expr::IfNotContains(mut re) => {
                if !re.matches_iter(&mut stream.iter_between(from, to), from) {
                    self.step(stream, m, pc + 1, fname, out)
                } else {
                    Ok((from, to))
                }
            }

            Expr::Print(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                writeln!(out, "{s}").expect("to be able to write");
                Ok((from, to))
            }

            Expr::Insert(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                stream.insert(from, &s);
                Ok((from, to + s.chars().count()))
            }

            Expr::Append(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                stream.insert(to + 1, &s);
                Ok((from, to + s.chars().count()))
            }

            Expr::Change(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                stream.remove(from, to);
                stream.insert(from, &s);
                Ok((from, from + s.chars().count()))
            }

            Expr::Delete => {
                stream.remove(from, to);
                Ok((from, from))
            }

            Expr::Sub(mut re, pat, false) => {
                match re.match_iter(&mut stream.iter_between(from, to), from) {
                    Some(m) => {
                        let (mfrom, mto) = m.loc();
                        let s = template_match(&pat, m, stream.contents(), fname)?;
                        stream.remove(mfrom, mto);
                        stream.insert(mfrom, &s);
                        Ok((from, to + mto - mfrom + s.chars().count()))
                    }
                    None => Ok((from, to)),
                }
            }

            Expr::Sub(mut re, pat, true) => loop {
                match re.match_iter(&mut stream.iter_between(from, to), from) {
                    Some(m) => {
                        let cur_len = stream.len_chars();
                        let (mfrom, mto) = m.loc();
                        let s = template_match(&pat, m, stream.contents(), fname)?;
                        stream.remove(mfrom, mto);
                        stream.insert(mfrom, &s);
                        let new_len = stream.len_chars();

                        from = to + mto - mfrom + s.chars().count() - 1;

                        match new_len.cmp(&cur_len) {
                            Ordering::Greater => to += new_len - cur_len,
                            Ordering::Less => to -= cur_len - new_len,
                            _ => (),
                        }

                        if from > to {
                            return Ok((from, to));
                        }
                    }
                    None => return Ok((from, to)),
                }
            },
        }
    }

    /// Attempt to parse a given program input using a known max dot position
    pub fn try_parse(s: &str) -> Result<Self, Error> {
        let mut exprs = vec![];
        let mut it = s.trim().chars().peekable();

        if it.peek().is_none() {
            return Err(Error::EmptyProgram);
        }

        let initial_dot = parse_initial_dot(&mut it)?;
        consume_whitespace(&mut it);

        loop {
            if it.peek().is_none() {
                break;
            }

            match Expr::try_parse(&mut it) {
                Ok(expr) => {
                    exprs.push(expr);
                    consume_whitespace(&mut it);
                }
                Err(Error::Eof) => break,
                Err(e) => return Err(e),
            }
        }

        validate(&exprs)?;

        Ok(Self { initial_dot, exprs })
    }
}

fn consume_whitespace(it: &mut Peekable<Chars>) {
    loop {
        match it.peek() {
            Some(ch) if ch.is_whitespace() => {
                it.next();
            }
            _ => break,
        }
    }
}

fn validate(exprs: &Vec<Expr>) -> Result<(), Error> {
    use Expr::*;

    if exprs.is_empty() {
        return Err(Error::EmptyProgram);
    }

    // Groups branches must be valid sub-programs
    for e in exprs.iter() {
        if let Group(branches) = e {
            for branch in branches.iter() {
                validate(branch)?;
            }
        }
    }

    // Must end with an action
    if !matches!(
        exprs[exprs.len() - 1],
        Group(_) | Insert(_) | Append(_) | Change(_) | Sub(_, _, _) | Print(_) | Delete
    ) {
        return Err(Error::MissingAction);
    }

    Ok(())
}

fn parse_initial_dot(it: &mut Peekable<Chars>) -> Result<(usize, Option<usize>), Error> {
    match it.peek() {
        Some(',') => {
            it.next();
            Ok((0, None))
        }

        // n,m | n,
        Some(&c) if c.is_ascii_digit() => {
            it.next();
            let n = parse_num(c, it);
            match it.next() {
                Some(',') => match it.next() {
                    Some(c) if c.is_ascii_digit() => {
                        let m = parse_num(c, it);
                        Ok((n, Some(m)))
                    }
                    Some(' ') | None => Ok((n, None)),
                    Some(ch) => Err(Error::UnexpectedCharacter(ch)),
                },
                Some(ch) => Err(Error::UnexpectedCharacter(ch)),
                None => Err(Error::Eof),
            }
        }

        // Allow omitting the initial dot
        _ => Ok((0, None)),
    }
}

// FIXME: if a previous sub-match replacement injects a valid var name for a subsequent one
// then we end up attempting to template THAT in a later iteration of the loop.
fn template_match(s: &str, m: Match, txt: String, fname: &str) -> Result<String, Error> {
    let mut output = if s.contains(FNAME_VAR) {
        s.replace(FNAME_VAR, fname)
    } else {
        s.to_string()
    };

    // replace newline and tab escapes with their literal equivalents
    output = output.replace("\\n", "\n").replace("\\t", "\t");

    let vars = ["$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9"];
    for (n, var) in vars.iter().enumerate() {
        if !s.contains(var) {
            continue;
        }
        match m.str_submatch_text(n, &txt) {
            Some(sm) => output = output.replace(var, &sm),
            None => return Err(Error::InvalidSubstitution(n)),
        }
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regex::Regex;
    use ropey::Rope;
    use simple_test_case::test_case;
    use Expr::*;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    #[test_case(",", (0, None); "full")]
    #[test_case("5,", (5, None); "from n")]
    #[test_case("50,", (50, None); "from n multi digit")]
    #[test_case("5,9", (5, Some(9)); "from n to m")]
    #[test_case("25,90", (25, Some(90)); "from n to m multi digit")]
    #[test]
    fn parse_initial_dot_works(s: &str, expected: (usize, Option<usize>)) {
        let dot = parse_initial_dot(&mut s.chars().peekable()).expect("valid input");
        assert_eq!(dot, expected);
    }

    #[test_case(", p/$0/", vec![Print("$0".to_string())]; "print all")]
    #[test_case(", x/^.*$/ s/foo/bar/", vec![LoopMatches(re("^.*$")), Sub(re("foo"), "bar".to_string(), false)]; "simple loop")]
    #[test_case(", x/^.*$/ g/emacs/ d", vec![LoopMatches(re("^.*$")), IfContains(re("emacs")), Delete]; "loop filter")]
    #[test]
    fn parse_program_works(s: &str, expected: Vec<Expr>) {
        let p = Program::try_parse(s).expect("valid input");
        assert_eq!(
            p,
            Program {
                initial_dot: (0, None),
                exprs: expected
            }
        );
    }

    #[test_case("", Error::EmptyProgram; "empty program")]
    #[test_case(", x/.*/", Error::MissingAction; "missing action")]
    #[test]
    fn parse_program_errors_correctly(s: &str, expected: Error) {
        let res = Program::try_parse(s);
        assert_eq!(res, Err(expected));
    }

    #[test_case(Insert("X".to_string()), "Xfoo foo foo"; "insert")]
    #[test_case(Append("X".to_string()), "fooX foo foo"; "append")]
    #[test_case(Change("X".to_string()), "X foo foo"; "change")]
    #[test_case(Delete, " foo foo"; "delete")]
    #[test]
    fn step_works(expr: Expr, expected: &str) {
        let mut prog = Program {
            initial_dot: (0, None),
            exprs: vec![expr],
        };
        let mut r = Rope::from_str("foo foo foo");
        // matching the first 'foo'
        prog.step(&mut r, Match::synthetic(0, 2), 0, "test", &mut vec![])
            .unwrap();

        assert_eq!(&r.to_string(), expected);
    }

    #[test_case(", x/foo/ p/$0/", "foo│foo│foo"; "x print")]
    #[test_case(", x/foo/ i/X/", "Xfoo│Xfoo│Xfoo"; "x insert")]
    #[test_case(", x/foo/ a/X/", "fooX│fooX│fooX"; "x append")]
    #[test_case(", x/foo/ c/X/", "X│X│X"; "x change")]
    #[test_case(", x/foo/ s/o/X/", "fXo│fXo│fXo"; "x substitute")]
    #[test_case(", x/foo/ s/o/X/g", "fXX│fXX│fXX"; "x substitute all")]
    #[test_case(", y/foo/ p/>$0</", "foo│foo│foo"; "y print")]
    #[test_case(", y/foo/ i/X/", "fooX│fooX│foo"; "y insert")]
    #[test_case(", y/foo/ a/X/", "foo│Xfoo│Xfoo"; "y append")]
    #[test_case(", y/foo/ c/X/", "fooXfooXfoo"; "y change")]
    #[test]
    fn execute_produces_the_correct_string(s: &str, expected: &str) {
        let mut prog = Program::try_parse(s).unwrap();
        let mut r = Rope::from_str("foo│foo│foo");
        prog.execute(&mut r, "test", &mut vec![]).unwrap();

        assert_eq!(&r.to_string(), expected);
    }
}
