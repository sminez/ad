//! Parsing of the command line syntax for exec commands
use crate::{
    regex::{self, Match, Regex},
    util::{parse_num, IdxRopeChars},
};
use ropey::Rope;
use std::{cmp::Ordering, iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    EmptyProgram,
    InvalidExpr,
    InvalidRegex(regex::Error),
    InvalidSubstitution(usize),
    MissingInitialDot,
    MissingAction,
}

impl From<regex::Error> for Error {
    fn from(err: regex::Error) -> Self {
        Error::InvalidRegex(err)
    }
}

/// A parsed and compiled program that can be executed against an input
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    initial_dot: (usize, usize),
    exprs: Vec<Expr>,
}

impl Program {
    /// Execute this program against a given Rope
    pub fn execute(&mut self, r: &mut Rope) -> Result<(usize, usize), Error> {
        let (from, to) = self.initial_dot;
        let initial = Match::synthetic(from, to);
        self.step(r, initial, 0)
    }

    fn step(&mut self, r: &mut Rope, m: Match, pc: usize) -> Result<(usize, usize), Error> {
        let (mut from, mut to) = m.loc();

        match self.exprs[pc].clone() {
            Expr::LoopMatches(mut re) => loop {
                let mut it = IdxRopeChars::new(r, from, to);
                match re.match_iter(&mut it, from) {
                    Some(m) => {
                        let cur_len = r.len_chars();
                        (_, from) = self.step(r, m, pc + 1)?;
                        let new_len = r.len_chars();
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
                let mut it = IdxRopeChars::new(r, from, to);
                match re.match_iter(&mut it, from) {
                    Some(m) => {
                        let (initial_to, new_from) = m.loc();
                        if initial_to == 0 {
                            // skip matches of the null string at the start of input
                            from = new_from + 1;
                            continue;
                        }

                        let m = Match::synthetic(from, initial_to - 1);
                        let cur_len = r.len_chars();
                        (_, _) = self.step(r, m, pc + 1)?;
                        let new_len = r.len_chars();

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
                let mut it = IdxRopeChars::new(r, from, to);
                if re.match_iter(&mut it, from).is_some() {
                    self.step(r, m, pc + 1)
                } else {
                    Ok((from, to))
                }
            }

            Expr::IfNotContains(mut re) => {
                let mut it = IdxRopeChars::new(r, from, to);
                if re.match_iter(&mut it, from).is_none() {
                    self.step(r, m, pc + 1)
                } else {
                    Ok((from, to))
                }
            }

            // FIXME: should be using something that impls Write so we can redirect output to a buffer
            Expr::Print(pat) => {
                println!("{}", template_match(pat, m, r)?);
                Ok((from, to))
            }

            Expr::Insert(pat) => {
                let s = template_match(pat, m, r)?;
                r.insert(from, &s);
                Ok((from, to + s.len()))
            }

            Expr::Append(pat) => {
                let s = template_match(pat, m, r)?;
                r.insert(to + 1, &s);
                Ok((from, to + s.len()))
            }

            Expr::Change(pat) => {
                let s = template_match(pat, m, r)?;
                r.remove(from..=to);
                r.insert(from, &s);
                Ok((from, from + s.len()))
            }

            Expr::Delete => {
                r.remove(from..=to);
                Ok((from, from))
            }

            Expr::Sub(mut re, pat, false) => {
                let mut it = IdxRopeChars::new(r, from, to);
                match re.match_iter(&mut it, from) {
                    Some(m) => {
                        let (mfrom, mto) = m.loc();
                        let s = template_match(pat, m, r)?;
                        r.remove(mfrom..=mto);
                        r.insert(mfrom, &s);
                        Ok((from, to + mto - mfrom + s.len()))
                    }
                    None => Ok((from, to)),
                }
            }

            Expr::Sub(mut re, pat, true) => loop {
                let mut it = IdxRopeChars::new(r, from, to);
                match re.match_iter(&mut it, from) {
                    Some(m) => {
                        let cur_len = r.len_chars();
                        let (mfrom, mto) = m.loc();
                        let s = template_match(pat.clone(), m, r)?;
                        r.remove(mfrom..=mto);
                        r.insert(mfrom, &s);
                        let new_len = r.len_chars();

                        from = to + mto - mfrom + s.len() - 1;

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
    pub fn try_parse(s: &str, max_dot: usize) -> Result<Self, Error> {
        let mut exprs = vec![];
        let mut it = s.trim().chars().peekable();

        if it.peek().is_none() {
            return Err(Error::EmptyProgram);
        }

        let initial_dot = parse_initial_dot(&mut it, max_dot)?;
        consume_whitespace(&mut it);

        loop {
            if it.peek().is_none() {
                break;
            }

            exprs.push(Expr::try_parse(&mut it)?);
            consume_whitespace(&mut it);
        }

        validate(&mut exprs)?;

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

fn validate(exprs: &mut Vec<Expr>) -> Result<(), Error> {
    use Expr::*;

    if exprs.is_empty() {
        return Err(Error::EmptyProgram);
    }

    // Must end with an action
    if !matches!(
        exprs[exprs.len() - 1],
        Insert(_) | Append(_) | Change(_) | Sub(_, _, _) | Print(_) | Delete
    ) {
        return Err(Error::MissingAction);
    }

    Ok(())
}

fn parse_initial_dot(it: &mut Peekable<Chars>, max_dot: usize) -> Result<(usize, usize), Error> {
    match it.next() {
        Some(',') => Ok((0, max_dot)),
        // n,m | n,
        Some(c) if c.is_ascii_digit() => {
            let n = parse_num(c, it);
            match it.next() {
                Some(',') => match it.next() {
                    Some(c) if c.is_ascii_digit() => {
                        let m = parse_num(c, it);
                        Ok((n, m))
                    }
                    Some(' ') | None => Ok((n, max_dot)),
                    _ => Err(Error::InvalidExpr),
                },
                _ => Err(Error::InvalidExpr),
            }
        }

        _ => Err(Error::MissingInitialDot),
    }
}

fn template_match(mut s: String, m: Match, r: &Rope) -> Result<String, Error> {
    let vars = ["$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9"];
    for (n, var) in vars.iter().enumerate() {
        if !s.contains(var) {
            continue;
        }
        match m.rope_submatch_text(n, r) {
            Some(txt) => s = s.replace(var, &txt.to_string()),
            None => return Err(Error::InvalidSubstitution(n)),
        }
    }

    Ok(s)
}

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
}

impl Expr {
    fn try_parse(it: &mut Peekable<Chars>) -> Result<Self, Error> {
        match it.next() {
            Some('x') => Ok(Expr::LoopMatches(try_parse_delimited_regex(it)?)),
            Some('y') => Ok(Expr::LoopBetweenMatches(try_parse_delimited_regex(it)?)),
            Some('g') => Ok(Expr::IfContains(try_parse_delimited_regex(it)?)),
            Some('v') => Ok(Expr::IfNotContains(try_parse_delimited_regex(it)?)),
            Some('i') => Ok(Expr::Insert(try_parse_delimited_str(it)?)),
            Some('a') => Ok(Expr::Append(try_parse_delimited_str(it)?)),
            Some('c') => Ok(Expr::Change(try_parse_delimited_str(it)?)),
            Some('s') => {
                let delim = it.next().ok_or(Error::InvalidExpr)?;
                let re = Regex::compile(&try_read_until(delim, it)?)?;
                let s = try_read_until(delim, it)?;
                if let Some('g') = it.peek() {
                    it.next();
                    Ok(Expr::Sub(re, s, true))
                } else {
                    Ok(Expr::Sub(re, s, false))
                }
            }
            Some('p') => Ok(Expr::Print(try_parse_delimited_str(it)?)),
            Some('d') => Ok(Expr::Delete),

            _ => Err(Error::InvalidExpr),
        }
    }
}

fn try_parse_delimited_regex(it: &mut Peekable<Chars>) -> Result<Regex, Error> {
    let s = try_parse_delimited_str(it)?;
    Ok(Regex::compile(&s)?)
}

fn try_parse_delimited_str(it: &mut Peekable<Chars>) -> Result<String, Error> {
    let delim = it.next().ok_or(Error::InvalidExpr)?;
    try_read_until(delim, it)
}

fn try_read_until(delim: char, it: &mut Peekable<Chars>) -> Result<String, Error> {
    let mut s = String::new();
    let mut prev = delim;

    for ch in it {
        if ch == delim && prev != '\\' {
            return Ok(s);
        }
        s.push(ch);
        prev = ch;
    }

    Err(Error::InvalidExpr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;
    use Expr::*;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    #[test_case(",", (0, 100); "full")]
    #[test_case("5,", (5, 100); "from n")]
    #[test_case("50,", (50, 100); "from n multi digit")]
    #[test_case("5,9", (5, 9); "from n to m")]
    #[test_case("25,90", (25, 90); "from n to m multi digit")]
    #[test]
    fn parse_initial_dot_works(s: &str, expected: (usize, usize)) {
        let dot = parse_initial_dot(&mut s.chars().peekable(), 100).expect("valid input");
        assert_eq!(dot, expected);
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
    #[test]
    fn parse_expr_works(s: &str, expected: Expr) {
        let a = Expr::try_parse(&mut s.chars().peekable()).expect("valid input");
        assert_eq!(a, expected);
    }

    #[test_case(", p/$0/", vec![Print("$0".to_string())]; "print all")]
    #[test_case(", x/^.*$/ s/foo/bar/", vec![LoopMatches(re("^.*$")), Sub(re("foo"), "bar".to_string(), false)]; "simple loop")]
    #[test_case(", x/^.*$/ g/emacs/ d", vec![LoopMatches(re("^.*$")), IfContains(re("emacs")), Delete]; "loop filter")]
    #[test]
    fn parse_program_works(s: &str, expected: Vec<Expr>) {
        let p = Program::try_parse(s, 100).expect("valid input");
        assert_eq!(
            p,
            Program {
                initial_dot: (0, 100),
                exprs: expected
            }
        );
    }

    #[test_case("", Error::EmptyProgram; "empty program")]
    #[test_case("x/.*/ d", Error::MissingInitialDot; "missing initial dot")]
    #[test_case(", x/.*/", Error::MissingAction; "missing action")]
    #[test]
    fn parse_program_errors_correctly(s: &str, expected: Error) {
        let res = Program::try_parse(s, 100);
        assert_eq!(res, Err(expected));
    }

    #[test_case(Insert("X".to_string()), "Xfoo foo foo"; "insert")]
    #[test_case(Append("X".to_string()), "fooX foo foo"; "append")]
    #[test_case(Change("X".to_string()), "X foo foo"; "change")]
    #[test_case(Delete, " foo foo"; "delete")]
    #[test]
    fn step_works(expr: Expr, expected: &str) {
        let mut prog = Program {
            initial_dot: (0, 10),
            exprs: vec![expr],
        };
        let mut r = Rope::from_str("foo foo foo");
        // matching the first 'foo'
        prog.step(&mut r, Match::synthetic(0, 2), 0).unwrap();

        assert_eq!(&r.to_string(), expected);
    }

    #[test_case(", x/foo/ p/$0/", "foo foo foo"; "x print")]
    #[test_case(", x/foo/ i/X/", "Xfoo Xfoo Xfoo"; "x insert")]
    #[test_case(", x/foo/ a/X/", "fooX fooX fooX"; "x append")]
    #[test_case(", x/foo/ c/X/", "X X X"; "x change")]
    #[test_case(", x/foo/ s/o/X/", "fXo fXo fXo"; "x substitute")]
    #[test_case(", x/foo/ s/o/X/g", "fXX fXX fXX"; "x substitute all")]
    #[test_case(", y/foo/ p/>$0</", "foo foo foo"; "y print")]
    #[test_case(", y/foo/ i/X/", "fooX fooX foo"; "y insert")]
    #[test_case(", y/foo/ a/X/", "foo Xfoo Xfoo"; "y append")]
    #[test_case(", y/foo/ c/X/", "fooXfooXfoo"; "y change")]
    #[test]
    fn execute_produces_the_correct_string(s: &str, expected: &str) {
        let mut prog = Program::try_parse(s, 10).unwrap();
        let mut r = Rope::from_str("foo foo foo");
        prog.execute(&mut r).unwrap();

        assert_eq!(&r.to_string(), expected);
    }
}
