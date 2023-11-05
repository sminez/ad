//! Sam style language for running edit commands using structural regular expressions
use crate::{
    buffer::{
        parse_dot::{DotExpression, ParseError},
        Dot,
    },
    regex::{self, Match},
};
use ropey::Rope;
use std::{cmp::min, io::Write, iter::Peekable, str::Chars};

mod expr;
mod stream;

use expr::{Expr, ParseOutput};
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
    initial_dot: DotExpression,
    exprs: Vec<Expr>,
}

impl Program {
    /// Execute this program against a given IterableStream
    pub fn execute<S, W>(&mut self, stream: &mut S, fname: &str, out: &mut W) -> Result<Dot, Error>
    where
        S: IterableStream,
        W: Write,
    {
        let initial_dot = stream.map_dot_expr(&mut self.initial_dot);

        if self.exprs.is_empty() {
            return Ok(initial_dot);
        }

        let (from, to) = initial_dot.as_char_indices();
        let initial = Match::synthetic(from, to);

        // In the case of running against a lazy stream our initial `to` will be a sential value of
        // usize::MAX which needs to be clamped to the size of the input. For Buffers and Ropes
        // where we know that we should already be in bounds this is not required but the overhead
        // of always doing it is fairly minimal.
        let (from, to) = self.step(stream, initial, 0, fname, out)?.as_char_indices();
        let ix_max = stream.len_chars();

        Ok(Dot::from_char_indices(min(from, ix_max), min(to, ix_max)))
    }

    fn step<S, W>(
        &mut self,
        stream: &mut S,
        m: Match,
        pc: usize,
        fname: &str,
        out: &mut W,
    ) -> Result<Dot, Error>
    where
        S: IterableStream,
        W: Write,
    {
        let (mut from, to) = m.loc();

        match self.exprs[pc].clone() {
            Expr::Group(g) => {
                let mut dot = Dot::from_char_indices(from, to);
                for exprs in g {
                    let mut p = Program {
                        initial_dot: DotExpression::Explicit(dot),
                        exprs: exprs.clone(),
                    };
                    dot = p.step(stream, m, 0, fname, out)?;
                }

                Ok(dot)
            }

            Expr::LoopMatches(mut re) => {
                let mut initial_matches = Vec::new();
                while let Some(m) = re.match_iter(&mut stream.iter_between(from, to), from) {
                    initial_matches.push(m);

                    // It's possible for the Regex we're using to match a 0-length string which
                    // would cause us to get stuck trying to advance to the next match position.
                    // If this happens we advance from by a character to ensure that we search
                    // further in the input.
                    let mut new_from = m.loc().1;
                    if new_from == from {
                        new_from += 1;
                    }
                    from = new_from;

                    if from >= to {
                        break;
                    }
                }

                self.apply_matches(initial_matches, stream, m, pc, fname, out)
            }

            Expr::LoopBetweenMatches(mut re) => {
                let mut initial_matches = Vec::new();

                while let Some(m) = re.match_iter(&mut stream.iter_between(from, to), from) {
                    let (new_from, new_to) = m.loc();
                    if from < new_from {
                        initial_matches.push(Match::synthetic(from, new_from));
                    }
                    from = new_to;
                    if from > to {
                        break;
                    }
                }

                if from < to {
                    initial_matches.push(Match::synthetic(from, to));
                }

                self.apply_matches(initial_matches, stream, m, pc, fname, out)
            }

            Expr::IfContains(mut re) => {
                if re.matches_iter(&mut stream.iter_between(from, to), from) {
                    self.step(stream, m, pc + 1, fname, out)
                } else {
                    Ok(Dot::from_char_indices(from, to))
                }
            }

            Expr::IfNotContains(mut re) => {
                if !re.matches_iter(&mut stream.iter_between(from, to), from) {
                    self.step(stream, m, pc + 1, fname, out)
                } else {
                    Ok(Dot::from_char_indices(from, to))
                }
            }

            Expr::Print(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                writeln!(out, "{s}").expect("to be able to write");
                Ok(Dot::from_char_indices(from, to))
            }

            Expr::Insert(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                stream.insert(from, &s);
                Ok(Dot::from_char_indices(from, to + s.chars().count()))
            }

            Expr::Append(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                stream.insert(to, &s);
                Ok(Dot::from_char_indices(from, to + s.chars().count()))
            }

            Expr::Change(pat) => {
                let s = template_match(&pat, m, stream.contents(), fname)?;
                stream.remove(from, to);
                stream.insert(from, &s);
                Ok(Dot::from_char_indices(from, from + s.chars().count()))
            }

            Expr::Delete => {
                stream.remove(from, to);
                Ok(Dot::from_char_indices(from, from))
            }

            Expr::Sub(mut re, pat) => {
                match re.match_iter(&mut stream.iter_between(from, to), from) {
                    Some(m) => {
                        let (mfrom, mto) = m.loc();
                        let s = template_match(&pat, m, stream.contents(), fname)?;
                        stream.remove(mfrom, mto);
                        stream.insert(mfrom, &s);
                        Ok(Dot::from_char_indices(
                            from,
                            to - (mto - mfrom) + s.chars().count(),
                        ))
                    }
                    None => Ok(Dot::from_char_indices(from, to)),
                }
            }
        }
    }

    /// When looping over disjoint matches in the input we need to determin all of the initial
    /// match points before we start making any edits as the edits may alter the semantics of
    /// future matches.
    fn apply_matches<S, W>(
        &mut self,
        initial_matches: Vec<Match>,
        stream: &mut S,
        m: Match,
        pc: usize,
        fname: &str,
        out: &mut W,
    ) -> Result<Dot, Error>
    where
        S: IterableStream,
        W: Write,
    {
        let (mut from, to) = m.loc();
        let mut offset: isize = 0;

        for mut m in initial_matches.into_iter() {
            m.apply_offset(offset);

            let cur_len = stream.len_chars();
            from = self.step(stream, m, pc + 1, fname, out)?.last_cur().idx;
            let new_len = stream.len_chars();

            offset += new_len as isize - cur_len as isize;
        }

        Ok(Dot::from_char_indices(
            from,
            (to as isize + offset) as usize,
        ))
    }

    /// Attempt to parse a given program input using a known max dot position
    pub fn try_parse(s: &str) -> Result<Self, Error> {
        let mut exprs = vec![];
        let mut it = s.trim().chars().peekable();

        if it.peek().is_none() {
            return Err(Error::EmptyProgram);
        }

        let initial_dot = match DotExpression::parse(&mut it) {
            Ok(dot_expr) => dot_expr,
            // If the start of input is not a dot expression we default to Full and
            // attempt to parse the rest of the program
            Err(ParseError::NotADotExpression) => DotExpression::full(),
            // All other errors are parse errors for us
            Err(ParseError::InvalidRegex(e)) => return Err(Error::InvalidRegex(e)),
            Err(ParseError::UnclosedDelimiter) => {
                return Err(Error::UnclosedDelimiter("dot expr regex", '/'))
            }
            Err(ParseError::UnexpectedCharacter(c)) => return Err(Error::UnexpectedCharacter(c)),
        };

        consume_whitespace(&mut it);

        loop {
            if it.peek().is_none() {
                break;
            }

            match Expr::try_parse(&mut it) {
                Ok(ParseOutput::Single(expr)) => {
                    exprs.push(expr);
                    consume_whitespace(&mut it);
                }
                Ok(ParseOutput::Pair(e1, e2)) => {
                    exprs.extend([e1, e2]);
                    consume_whitespace(&mut it);
                }
                Err(Error::Eof) => break,
                Err(e) => return Err(e),
            }
        }

        if exprs.is_empty() {
            return Ok(Self { initial_dot, exprs });
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
        Group(_) | Insert(_) | Append(_) | Change(_) | Sub(_, _) | Print(_) | Delete
    ) {
        return Err(Error::MissingAction);
    }

    Ok(())
}

// FIXME: if a previous sub-match replacement injects a valid var name for a subsequent one
// then we end up attempting to template THAT in a later iteration of the loop.
fn template_match(s: &str, m: Match, r: Rope, fname: &str) -> Result<String, Error> {
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
        match m.rope_submatch_text(n, &r) {
            Some(sm) => output = output.replace(var, &sm.to_string()),
            None => return Err(Error::InvalidSubstitution(n)),
        }
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{buffer::Buffer, editor::Action, regex::Regex};
    use ropey::Rope;
    use simple_test_case::test_case;
    use Expr::*;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    #[test_case(", p/$0/", vec![Print("$0".to_string())]; "print all")]
    #[test_case(", x/^.*$/ s/foo/bar/", vec![LoopMatches(re("^.*$")), Sub(re("foo"), "bar".to_string())]; "simple loop")]
    #[test_case(", x/^.*$/ g/emacs/ d", vec![LoopMatches(re("^.*$")), IfContains(re("emacs")), Delete]; "loop filter")]
    #[test]
    fn parse_program_works(s: &str, expected: Vec<Expr>) {
        let p = Program::try_parse(s).expect("valid input");
        assert_eq!(
            p,
            Program {
                initial_dot: DotExpression::full(),
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

    #[test_case(Insert("X".to_string()), "Xfoo foo foo", (0, 12); "insert")]
    #[test_case(Append("X".to_string()), "foo foo fooX", (0, 12); "append")]
    #[test_case(Change("X".to_string()), "X", (0, 1); "change")]
    #[test_case(Delete, "", (0, 0); "delete")]
    #[test_case(Sub(re("oo"), "X".to_string()), "fX foo foo", (0, 10); "sub single")]
    #[test_case(LoopMatches(re("foo")), "  ", (2, 2); "loop delete")]
    #[test_case(LoopBetweenMatches(re("foo")), "foofoofoo", (6, 9); "loop between delete")]
    #[test]
    fn step_works(expr: Expr, expected: &str, expected_dot: (usize, usize)) {
        let mut prog = Program {
            initial_dot: DotExpression::full(),
            exprs: vec![expr, Delete],
        };
        let mut r = Rope::from_str("foo foo foo");
        let dot = prog
            .step(&mut r, Match::synthetic(0, 11), 0, "test", &mut vec![])
            .unwrap();

        assert_eq!(&r.to_string(), expected);
        assert_eq!(dot.as_char_indices(), expected_dot);
    }

    #[test_case(", x/(t.)/ c/$1X/", "thXis is a teXst XstrXing"; "x c")]
    #[test_case(", x/(t.)/ i/$1/", "ththis is a tetest t strtring"; "x i")]
    #[test_case(", x/(t.)/ a/$1/", "ththis is a tetest t strtring"; "x a")]
    #[test]
    fn substitution_of_submatches_works(s: &str, expected: &str) {
        let mut prog = Program::try_parse(s).unwrap();

        let mut r = Rope::from_str("this is a test string");
        prog.execute(&mut r, "test", &mut vec![]).unwrap();
        assert_eq!(&r.to_string(), expected);
    }

    #[test_case("/oo.fo/ d", "fo│foo"; "regex dot delete")]
    #[test_case("/oo/,/oo/ d", "f│foo"; "regex dot range delete")]
    #[test_case(", x/foo/ p/$0/", "foo│foo│foo"; "x print")]
    #[test_case(", x/foo/ i/X/", "Xfoo│Xfoo│Xfoo"; "x insert")]
    #[test_case(", x/foo/ a/X/", "fooX│fooX│fooX"; "x append")]
    #[test_case(", x/foo/ c/X/", "X│X│X"; "x change")]
    #[test_case(", x/foo/ s/o/X/", "fXo│fXo│fXo"; "x substitute")]
    #[test_case(", y/foo/ p/>$0</", "foo│foo│foo"; "y print")]
    #[test_case(", y/foo/ i/X/", "fooX│fooX│foo"; "y insert")]
    #[test_case(", y/foo/ a/X/", "foo│Xfoo│Xfoo"; "y append")]
    #[test_case(", y/foo/ c/X/", "fooXfooXfoo"; "y change")]
    #[test_case(", s/oo/X/", "fX│foo│foo"; "sub single")]
    #[test_case(", s/\\w+/X/", "X│foo│foo"; "sub word single")]
    #[test_case(", s/oo/X/g", "fX│fX│fX"; "sub all")]
    #[test_case(", s/.*/X/g", "X"; "sub all dot star")]
    #[test_case(", x/\\b\\w+\\b/ c/X/", "X│X│X"; "change each word")]
    #[test_case(", x/foo/ s/o/X/g", "fXX│fXX│fXX"; "nested loop x substitute all")]
    #[test_case(", x/oo/ s/.*/X/g", "fX│fX│fX"; "nested loop x sub all dot star")]
    #[test]
    fn execute_produces_the_correct_string(s: &str, expected: &str) {
        let mut prog = Program::try_parse(s).unwrap();

        let mut r = Rope::from_str("foo│foo│foo");
        prog.execute(&mut r, "test", &mut vec![]).unwrap();
        assert_eq!(&r.to_string(), expected, "rope");

        let mut b = Buffer::new_unnamed(0, "foo│foo│foo");
        prog.execute(&mut b, "test", &mut vec![]).unwrap();
        assert_eq!(&b.txt.to_string(), expected, "buffer");
    }

    #[test]
    fn multiline_file_dot_star_works() {
        let mut prog = Program::try_parse(", x/.*/ c/foo/").unwrap();

        let mut r = Rope::from_str("this is\na multiline\nfile");
        prog.execute(&mut r, "test", &mut vec![]).unwrap();

        // '.*' will match the null string at the end of lines containing a newline as well
        assert_eq!(&r.to_string(), "foofoo\nfoofoo\nfoo");
    }

    #[test]
    fn multiline_file_dot_plus_works() {
        let mut prog = Program::try_parse(", x/.+/ c/foo/").unwrap();

        let mut r = Rope::from_str("this is\na multiline\nfile");
        prog.execute(&mut r, "test", &mut vec![]).unwrap();
        assert_eq!(&r.to_string(), "foo\nfoo\nfoo");
    }

    #[test_case(", d"; "delete buffer")]
    #[test_case(", x/th/ d"; "delete each th")]
    #[test_case(", x/ / d"; "delete spaces")]
    #[test_case(", s/ //g"; "sub remove spaces")]
    #[test_case(", x/\\b\\w+\\b/ d"; "delete each word")]
    #[test_case(", x/. / d"; "delete things before a space")]
    #[test_case(", x/\\b\\w+\\b/ c/buffalo/"; "change each word")]
    #[test_case(", x/\\b\\w+\\b/ a/buffalo/"; "append to each word")]
    #[test_case(", x/\\b\\w+\\b/ i/buffalo/"; "insert before each word")]
    #[test]
    fn buffer_execute_undo_all_is_a_noop(s: &str) {
        let mut prog = Program::try_parse(s).unwrap();
        let initial_content = "this is a line\nand another\n- [ ] something to do\n";
        let mut b = Buffer::new_unnamed(0, initial_content);

        prog.execute(&mut b, "test", &mut vec![]).unwrap();
        while b.handle_action(Action::Undo).is_none() {}
        let final_content = String::from_utf8(b.contents()).unwrap();

        assert_eq!(&final_content, initial_content);
    }
}
