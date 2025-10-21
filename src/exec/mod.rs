//! Sam style language for running edit commands using structural regular expressions
use crate::{
    buffer::{Buffer, GapBuffer},
    dot::{Cur, Dot},
    editor::Action,
    regex::{self, Match},
};
use ad_event::Source;
use aho_corasick::AhoCorasick;
use std::{
    borrow::Cow,
    cmp::min,
    fmt::Write as _,
    io::Write,
    iter::Peekable,
    mem,
    ops::{Deref, DerefMut},
    str::Chars,
    sync::{LazyLock, Mutex, OnceLock},
};

mod addr;
mod cached_stdin;
mod char_iter;
mod expr;

use addr::ParseError;
pub(crate) use addr::{Addr, AddrBase, Address};
pub use cached_stdin::{CachedStdin, CachedStdinIter};
pub(crate) use char_iter::IterBoundedChars;
use expr::{Expr, ParseOutput};

/// Variable usable in templates for injecting the current filename.
/// (Following the naming convention used in Awk)
const FNAME_VAR: &str = "$FILENAME";
/// Variable usable in templates for injecting the row that the current match starts at
const ROW_VAR: &str = "$ROW";
/// Variable usable in templates for injecting the column that the current match starts at
const COL_VAR: &str = "$COL";

static TEMPLATE_AC: OnceLock<AhoCorasick> = OnceLock::new();
const TEMPLATE_PATTERNS: [&str; 15] = [
    "$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9", FNAME_VAR, ROW_VAR, COL_VAR, "\\n",
    "\\t",
];

/// A shared pool of scratch buffers for tracking initial matches for loop-matches and
/// loop-between-matches instructions.
static INITIAL_MATCHES_POOL: LazyLock<Mutex<Vec<Vec<Match>>>> =
    LazyLock::new(|| Mutex::new((0..4).map(|_| Vec::with_capacity(10)).collect::<Vec<_>>()));

/// Errors that can be returned by the exec engine
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// Empty expression group
    EmptyExpressionGroup,
    /// Empty branch for an expression group
    EmptyExpressionGroupBranch,
    /// Empty program
    EmptyProgram,
    /// Unexpected end of file
    Eof,
    /// Invalid match generated (indices out of bounds)
    InvalidMatchIndices,
    /// Invalid regex
    InvalidRegex(regex::Error),
    /// Invalid substitution
    InvalidSubstitution(usize),
    /// Invalid suffix
    InvalidSuffix,
    /// Missing action
    MissingAction,
    /// Missing delimiter
    MissingDelimiter(&'static str),
    /// Unclosed delimiter
    UnclosedDelimiter(&'static str, char),
    /// Unclosed expression group
    UnclosedExpressionGroup,
    /// Unclosed expression group branch
    UnclosedExpressionGroupBranch,
    /// Unexpected character
    UnexpectedCharacter(char),
    /// A 0 was provided as a line or column index
    ZeroIndexedLineOrColumn,
}

impl From<regex::Error> for Error {
    fn from(err: regex::Error) -> Self {
        Error::InvalidRegex(err)
    }
}

/// Something that can be edited by a Program
pub trait Edit: Address {
    /// Insert a string at the specified index
    fn insert(&mut self, ix: usize, s: &str);

    /// Remove all characters from (from..to)
    fn remove(&mut self, from: usize, to: usize);

    /// Mark the start of an edit transaction
    fn begin_edit_transaction(&mut self) {}

    /// Mark the end of an edit transaction
    fn end_edit_transaction(&mut self) {}
}

impl Edit for GapBuffer {
    fn insert(&mut self, idx: usize, s: &str) {
        self.insert_str(idx, s)
    }

    fn remove(&mut self, from: usize, to: usize) {
        self.remove_range(from, to);
    }
}

impl Edit for Buffer {
    fn insert(&mut self, idx: usize, s: &str) {
        self.dot = Dot::Cur { c: Cur { idx } };
        self.handle_action(Action::InsertString { s: s.to_string() }, Source::Fsys);
    }

    fn remove(&mut self, from: usize, to: usize) {
        if from == to {
            return;
        }
        self.dot = Dot::from_char_indices(from, to.saturating_sub(1)).collapse_null_range();
        self.handle_action(Action::Delete, Source::Fsys);
    }

    fn begin_edit_transaction(&mut self) {
        self.new_edit_log_transaction()
    }

    fn end_edit_transaction(&mut self) {
        self.new_edit_log_transaction()
    }
}

/// A parsed and compiled program that can be executed against an input
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    initial_dot: Addr,
    exprs: Vec<Expr>,
    runner: Runner,
}

impl Program {
    /// Attempt to parse a given program input
    pub fn try_parse(s: &str) -> Result<Self, Error> {
        let mut exprs = vec![];
        let mut it = s.trim().chars().peekable();

        if it.peek().is_none() {
            return Err(Error::EmptyProgram);
        }

        let initial_dot = match Addr::parse(&mut it) {
            Ok(dot_expr) => dot_expr,

            // If the start of input is not an address we default to Full and attempt to parse the
            // rest of the program. We need to reconstruct the iterator here as we may have
            // advanced through the string while we attempt to parse the initial address.
            Err(ParseError::NotAnAddress) => {
                it = s.trim().chars().peekable();
                Addr::full()
            }

            Err(ParseError::InvalidRegex(e)) => return Err(Error::InvalidRegex(e)),
            Err(ParseError::UnclosedDelimiter) => {
                return Err(Error::UnclosedDelimiter("dot expr regex", '/'));
            }
            Err(ParseError::UnexpectedCharacter(c)) => return Err(Error::UnexpectedCharacter(c)),
            Err(ParseError::InvalidSuffix) => return Err(Error::InvalidSuffix),
            Err(ParseError::ZeroIndexedLineOrColumn) => return Err(Error::ZeroIndexedLineOrColumn),
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
            return Ok(Self::new(initial_dot, exprs));
        }

        validate(&exprs)?;

        Ok(Self::new(initial_dot, exprs))
    }

    fn new(initial_dot: Addr, exprs: Vec<Expr>) -> Self {
        Self {
            initial_dot,
            exprs,
            runner: Runner::new(),
        }
    }

    /// Execute this program against a given [Edit].
    pub fn execute<E, W>(&mut self, ed: &mut E, fname: &str, out: &mut W) -> Result<Dot, Error>
    where
        E: Edit,
        W: Write,
    {
        ed.try_make_contiguous();
        let initial_dot = ed.map_addr(&mut self.initial_dot);

        if self.exprs.is_empty() {
            return Ok(initial_dot);
        }

        let (from, to) = initial_dot.as_char_indices();
        let initial = &Match::synthetic(from, to.saturating_add(1));

        ed.begin_edit_transaction();
        let (from, to) = self
            .runner
            .step(&mut self.exprs, ed, initial, 0, fname, out)?
            .as_char_indices();
        ed.end_edit_transaction();

        // In the case of running against a lazy stream our initial `to` will be a sential value of
        // usize::MAX which needs to be clamped to the size of the input. For Buffers and GapBuffers
        // where we know that we should already be in bounds this is not required but the overhead
        // of always doing it is minimal as checking the number of chars in the buffer is O(1) due
        // to us caching the value.
        let ix_max = ed.len_chars();

        Ok(Dot::from_char_indices(min(from, ix_max), min(to, ix_max)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Runner {
    template_buf: GapBuffer,
    row_buf: String,
    col_buf: String,
}

impl Runner {
    fn new() -> Self {
        Self {
            template_buf: GapBuffer::new(),
            row_buf: String::with_capacity(4),
            col_buf: String::with_capacity(4),
        }
    }

    fn step<E, W>(
        &mut self,
        exprs: &mut [Expr],
        ed: &mut E,
        m: &Match,
        pc: usize,
        fname: &str,
        out: &mut W,
    ) -> Result<Dot, Error>
    where
        E: Edit,
        W: Write,
    {
        let (mut from, to) = m.loc();

        match &mut exprs[pc] {
            Expr::Group(g) => {
                let mut dot = Dot::from_char_indices(from, to);
                for sub_exprs in g.iter_mut() {
                    dot = self.step(sub_exprs, ed, m, 0, fname, out)?;
                }

                Ok(dot)
            }

            Expr::LoopMatches(re) => {
                let mut initial_matches = InitialMatches::get_from_pool();
                while let Some(m) = re.find_between(ed, from, to) {
                    // It's possible for the Regex we're using to match a 0-length string which
                    // would cause us to get stuck trying to advance to the next match position.
                    // If this happens we advance from by a character to ensure that we search
                    // further in the input.
                    let mut new_from = m.loc().1;
                    if new_from == from {
                        new_from += 1;
                    }
                    from = new_from;

                    initial_matches.push(m);

                    if from >= to || from >= ed.max_iter() {
                        break;
                    }
                }

                self.apply_matches(exprs, initial_matches, ed, m, pc, fname, out)
            }

            Expr::LoopBetweenMatches(re) => {
                let mut initial_matches = InitialMatches::get_from_pool();
                while let Some(m) = re.find_between(ed, from, to) {
                    let (new_from, new_to) = m.loc();
                    if from < new_from {
                        initial_matches.push(Match::synthetic(from, new_from));
                    }
                    from = new_to;
                    if from > to || from >= ed.max_iter() {
                        break;
                    }
                }

                if from < to {
                    initial_matches.push(Match::synthetic(from, to));
                }

                self.apply_matches(exprs, initial_matches, ed, m, pc, fname, out)
            }

            Expr::IfContains(re) => {
                if re.matches_between(ed, from, to) {
                    self.step(exprs, ed, m, pc + 1, fname, out)
                } else {
                    Ok(Dot::from_char_indices(from, to))
                }
            }

            Expr::IfNotContains(re) => {
                if !re.matches_between(ed, from, to) {
                    self.step(exprs, ed, m, pc + 1, fname, out)
                } else {
                    Ok(Dot::from_char_indices(from, to))
                }
            }

            Expr::Print(pat) => {
                self.template_match(pat, m, ed, fname)?;
                write!(out, "{}", self.template_buf.as_str()).expect("to be able to write");
                Ok(Dot::from_char_indices(from, to))
            }

            Expr::Insert(pat) => {
                self.template_match(pat, m, ed, fname)?;
                ed.insert(from, self.template_buf.as_str());
                Ok(Dot::from_char_indices(
                    from,
                    to + self.template_buf.len_chars(),
                ))
            }

            Expr::Append(pat) => {
                self.template_match(pat, m, ed, fname)?;
                ed.insert(to, self.template_buf.as_str());
                Ok(Dot::from_char_indices(
                    from,
                    to + self.template_buf.len_chars(),
                ))
            }

            Expr::Change(pat) => {
                self.template_match(pat, m, ed, fname)?;
                ed.remove(from, to);
                ed.insert(from, self.template_buf.as_str());
                Ok(Dot::from_char_indices(
                    from,
                    from + self.template_buf.len_chars(),
                ))
            }

            Expr::Delete => {
                ed.remove(from, to);
                Ok(Dot::from_char_indices(from, from))
            }

            Expr::Sub(re, pat) => match re.find_between(ed, from, to) {
                Some(m) => {
                    let (mfrom, mto) = m.loc();
                    self.template_match(pat, &m, ed, fname)?;
                    ed.remove(mfrom, mto);
                    ed.insert(mfrom, self.template_buf.as_str());
                    Ok(Dot::from_char_indices(
                        from,
                        to - (mto - mfrom) + self.template_buf.len_chars(),
                    ))
                }
                None => Ok(Dot::from_char_indices(from, to)),
            },
        }
    }

    /// When looping over disjoint matches in the input we need to determine all of the initial
    /// match points before we start making any edits as the edits may alter the semantics of
    /// future matches.
    #[allow(clippy::too_many_arguments)]
    fn apply_matches<E, W>(
        &mut self,
        exprs: &mut [Expr],
        mut initial_matches: InitialMatches,
        ed: &mut E,
        m: &Match,
        pc: usize,
        fname: &str,
        out: &mut W,
    ) -> Result<Dot, Error>
    where
        E: Edit,
        W: Write,
    {
        let mut offset: isize = 0;
        let (from, to) = m.loc();
        let mut dot = Dot::from_char_indices(from, to);

        for m in initial_matches.iter_mut() {
            m.apply_offset(offset);

            let cur_len = ed.len_chars();
            dot = self.step(exprs, ed, m, pc + 1, fname, out)?;
            let new_len = ed.len_chars();
            offset += new_len as isize - cur_len as isize;
        }

        Ok(dot)
    }

    fn template_match<E>(&mut self, s: &str, m: &Match, ed: &E, fname: &str) -> Result<(), Error>
    where
        E: Edit,
    {
        self.template_buf.clear();
        self.template_buf.insert_str(0, s);

        let mut matches: Vec<aho_corasick::Match> = TEMPLATE_AC
            .get_or_init(|| {
                AhoCorasick::new(TEMPLATE_PATTERNS)
                    .expect("using auto builder so no errors possible")
            })
            .find_iter(s)
            .collect();

        if matches.is_empty() {
            return Ok(());
        }

        // process the matches in reverse order so we don't need to update the match positions as
        // we alter the contents of the buffer.
        matches.reverse();

        self.row_buf.clear();
        self.col_buf.clear();
        let mut seen_row_col = false;

        for mat in matches {
            let (from, to) = (mat.start(), mat.end());
            let pat = mat.pattern().as_u32() as usize;
            let pattern = TEMPLATE_PATTERNS[pat];

            let new_s = match pattern {
                "\\n" => Cow::Borrowed("\n"),
                "\\t" => Cow::Borrowed("\t"),
                FNAME_VAR => Cow::Borrowed(fname),

                ROW_VAR | COL_VAR => {
                    if !seen_row_col {
                        let (i, _) = m.loc();
                        let row = ed.char_to_line(i).ok_or(Error::InvalidMatchIndices)?;
                        let col = i - ed.line_to_char(row).ok_or(Error::InvalidMatchIndices)?;
                        _ = write!(&mut self.row_buf, "{row}");
                        _ = write!(&mut self.col_buf, "{col}");
                        seen_row_col = true;
                    }
                    if pattern == ROW_VAR {
                        Cow::Borrowed(self.row_buf.as_str())
                    } else {
                        Cow::Borrowed(self.col_buf.as_str())
                    }
                }

                _ => {
                    debug_assert_eq!(
                        TEMPLATE_PATTERNS[0], "$0",
                        "submatch patterns must be first"
                    );

                    match m.submatch_text(pat, ed) {
                        Some(sm) => sm,
                        None => return Err(Error::InvalidSubstitution(pat)),
                    }
                }
            };

            let char_from = self.template_buf.byte_to_char(from);
            let char_to = self.template_buf.byte_to_char(to);
            self.template_buf.remove_range(char_from, char_to);
            self.template_buf.insert_str(char_from, new_s.as_ref());
        }

        Ok(())
    }
}

fn consume_whitespace(it: &mut Peekable<Chars<'_>>) {
    loop {
        match it.peek() {
            Some(ch) if ch.is_whitespace() => {
                it.next();
            }
            _ => break,
        }
    }
}

fn validate(exprs: &[Expr]) -> Result<(), Error> {
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

/// A reusable scratch buffer for holding initial matches when executing loop-matches and
/// loop-between-matches instructions.
/// Automatically released back to the shared pool when dropped.
struct InitialMatches(Vec<Match>);

impl InitialMatches {
    fn get_from_pool() -> Self {
        let mut guard = INITIAL_MATCHES_POOL.lock().unwrap();
        match guard.pop() {
            Some(mut v) => {
                v.clear();
                Self(v)
            }
            None => Self(Vec::with_capacity(10)),
        }
    }
}

impl Deref for InitialMatches {
    type Target = Vec<Match>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for InitialMatches {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Drop for InitialMatches {
    fn drop(&mut self) {
        let mut cache = Vec::new();
        mem::swap(&mut self.0, &mut cache);
        INITIAL_MATCHES_POOL.lock().unwrap().push(cache);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{buffer::Buffer, editor::Action, regex::Regex};
    use Expr::*;
    use simple_test_case::test_case;

    fn re(s: &str) -> Regex {
        Regex::compile(s).unwrap()
    }

    #[test_case(", p/$0/", vec![Print("$0".to_string())]; "print all")]
    #[test_case(", x/^.*$/ s/foo/bar/", vec![LoopMatches(re("^.*$")), Sub(re("foo"), "bar".to_string())]; "simple loop")]
    #[test_case(", x/^.*$/ g/emacs/ d", vec![LoopMatches(re("^.*$")), IfContains(re("emacs")), Delete]; "loop filter")]
    #[test]
    fn parse_program_works(s: &str, expected: Vec<Expr>) {
        let p = Program::try_parse(s).expect("valid input");
        assert_eq!(p, Program::new(Addr::full(), expected));
    }

    #[test_case("", Error::EmptyProgram; "empty program")]
    #[test_case(", x/.*/", Error::MissingAction; "missing action")]
    #[test]
    fn parse_program_errors_correctly(s: &str, expected: Error) {
        let res = Program::try_parse(s);
        assert_eq!(res, Err(expected));
    }

    #[test_case(vec![Insert("X".to_string())], "Xfoo foo foo", (0, 12); "insert")]
    #[test_case(vec![Append("X".to_string())], "foo foo fooX", (0, 12); "append")]
    #[test_case(vec![Change("X".to_string())], "X", (0, 1); "change")]
    #[test_case(vec![Delete], "", (0, 0); "delete")]
    #[test_case(vec![Sub(re("oo"), "X".to_string())], "fX foo foo", (0, 10); "sub single")]
    #[test_case(vec![LoopMatches(re("foo")), Delete], "  ", (2, 2); "loop delete")]
    #[test_case(vec![LoopBetweenMatches(re("foo")), Delete], "foofoofoo", (6, 6); "loop between delete")]
    #[test_case(vec![LoopMatches(re("foo")), Append("X".to_string())], "fooX fooX fooX", (10, 14); "loop change")]
    #[test_case(vec![LoopBetweenMatches(re("foo")), Append("X".to_string())], "foo Xfoo Xfoo", (8, 10); "loop between change")]
    #[test]
    fn step_works(exprs: Vec<Expr>, expected: &str, expected_dot: (usize, usize)) {
        let mut prog = Program::new(Addr::full(), exprs);
        let mut b = Buffer::new_unnamed(0, "foo foo foo", Default::default());
        let dot = prog
            .runner
            .step(
                &mut prog.exprs,
                &mut b,
                &Match::synthetic(0, 11),
                0,
                "test",
                &mut vec![],
            )
            .unwrap();

        assert_eq!(&b.txt.to_string(), expected);
        assert_eq!(dot.as_char_indices(), expected_dot);
    }

    #[test_case(", x/(t.)/ c/$1X/", "thXis is a teXst XstrXing"; "x c")]
    #[test_case(", x/(t.)/ i/$1/", "ththis is a tetest t strtring"; "x i")]
    #[test_case(", x/(t.)/ a/$1/", "ththis is a tetest t strtring"; "x a")]
    #[test]
    fn substitution_of_submatches_works(s: &str, expected: &str) {
        let mut prog = Program::try_parse(s).unwrap();

        let mut b = Buffer::new_unnamed(0, "this is a test string", Default::default());
        prog.execute(&mut b, "test", &mut vec![]).unwrap();
        assert_eq!(&b.txt.to_string(), expected);
    }

    #[test]
    fn loop_between_generates_the_correct_blocks() {
        let mut prog = Program::try_parse(", y/ / p/>$0<\n/").unwrap();
        let mut b = Buffer::new_unnamed(0, "this and that", Default::default());
        let mut output = Vec::new();
        let dot = prog.execute(&mut b, "test", &mut output).unwrap();

        let s = String::from_utf8(output).unwrap();
        assert_eq!(s, ">this<\n>and<\n>that<\n");

        let dot_content = dot.content(&b);
        assert_eq!(dot_content, "that");
    }

    #[test_case(0, "/oo.fo/ d", "fo│foo"; "regex dot delete")] // typos:ignore
    #[test_case(2, "-/f/,/f/ d", "oo│foo"; "regex dot range delete")]
    #[test_case(0, ", x/foo/ p/$0/", "foo│foo│foo"; "x print")]
    #[test_case(0, ", x/foo/ i/X/", "Xfoo│Xfoo│Xfoo"; "x insert")]
    #[test_case(0, ", x/foo/ a/X/", "fooX│fooX│fooX"; "x append")]
    #[test_case(0, ", x/foo/ c/X/", "X│X│X"; "x change")]
    #[test_case(0, ", x/foo/ c/XX/", "XX│XX│XX"; "x change 2")]
    #[test_case(0, ", x/foo/ d", "││"; "x delete")]
    #[test_case(0, ", x/foo/ s/o/X/", "fXo│fXo│fXo"; "x substitute")]
    #[test_case(0, ", y/foo/ p/>$0</", "foo│foo│foo"; "y print")]
    #[test_case(0, ", y/foo/ i/X/", "fooX│fooX│fooX"; "y insert")]
    #[test_case(0, ", y/foo/ a/X/", "foo│Xfoo│XfooX"; "y append")]
    #[test_case(0, ", y/foo/ c/X/", "fooXfooXfooX"; "y change")]
    #[test_case(0, ", y/foo/ d", "foofoofoo"; "y delete")]
    #[test_case(0, ", y/│/ d", "││"; "y delete 2")]
    #[test_case(0, ", s/oo/X/", "fX│foo│foo"; "sub single")]
    #[test_case(0, ", s/\\w+/X/", "X│foo│foo"; "sub word single")]
    #[test_case(0, ", s/oo/X/g", "fX│fX│fX"; "sub all")]
    #[test_case(0, ", s/.*/X/g", "X"; "sub all dot star")]
    #[test_case(0, ", x/\\b\\w+\\b/ c/X/", "X│X│X"; "change each word")]
    #[test_case(0, ", x/foo/ s/o/X/g", "fXX│fXX│fXX"; "nested loop x substitute all")]
    #[test_case(0, ", x/oo/ s/.*/X/g", "fX│fX│fX"; "nested loop x sub all dot star")]
    #[test]
    fn execute_produces_the_correct_string(idx: usize, s: &str, expected: &str) {
        let mut prog = Program::try_parse(s).unwrap();
        let mut b = Buffer::new_unnamed(0, "foo│foo│foo", Default::default());
        b.dot = Cur::new(idx).into();
        prog.execute(&mut b, "test", &mut vec![]).unwrap();

        assert_eq!(&b.txt.to_string(), expected, "buffer");
    }

    #[test]
    fn multiline_file_dot_star_works() {
        let mut prog = Program::try_parse(", x/.*/ c/foo/").unwrap();
        let mut b = Buffer::new_unnamed(0, "this is\na multiline\nfile", Default::default());
        prog.execute(&mut b, "test", &mut vec![]).unwrap();

        // '.*' will match the null string at the end of lines containing a newline as well
        assert_eq!(&b.txt.to_string(), "foofoo\nfoofoo\nfoo");
    }

    #[test]
    fn multiline_file_dot_plus_works() {
        let mut prog = Program::try_parse(", x/.+/ c/foo/").unwrap();
        let mut b = Buffer::new_unnamed(0, "this is\na multiline\nfile", Default::default());
        prog.execute(&mut b, "test", &mut vec![]).unwrap();

        assert_eq!(&b.txt.to_string(), "foo\nfoo\nfoo");
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
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, "test", &mut vec![]).unwrap();
        while b.handle_action(Action::Undo, Source::Keyboard).is_none() {}
        let final_content = b.str_contents();

        assert_eq!(&final_content, initial_content);
    }

    #[test_case("$FILENAME\\n\\t", ".", "test.txt\n\t"; "direct replacements")]
    #[test_case("$ROW", "/", "1"; "row")]
    #[test_case("$COL", "/p", "5"; "col")]
    #[test_case("$ROW:$COL", "/p", "1:5"; "row and col")]
    #[test_case("$0", "/\\w+/", "/some/"; "full match")]
    #[test_case("$1", "/(\\w+)/", "some"; "first submatch")]
    #[test_case(
        "$1$2$3$4$5$6$7$8$9",
        "(.)(.)(.)(.)(.)(.)(.)(.)(.)",
        "foo and 1";
        "all nine submatches"
    )]
    #[test]
    fn template_match_works(s: &str, re: &str, expected: &str) {
        let mut runner = Runner::new();
        let mut gb = GapBuffer::from("foo and 123\n/some/path");
        gb.make_contiguous();

        let mut re = Regex::compile(re).unwrap();
        let m = re.find(&gb).unwrap();

        runner.template_match(s, &m, &gb, "test.txt").unwrap();

        assert_eq!(runner.template_buf.as_str(), expected);
    }
}
