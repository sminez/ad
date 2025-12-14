//! Sam style language for executing structural regular expressions against ad Buffers
use crate::{
    buffer::{Buffer, GapBuffer},
    dot::{Cur, Dot},
    editor::Action,
    parse::ParseInput,
    regex::{self, Regex},
};
use ad_event::Source;
use std::{
    borrow::Cow,
    cell::RefCell,
    cmp::min,
    collections::BTreeMap,
    fmt,
    io::{self, Write},
};
use structex::{
    Structex, StructexBuilder,
    re::{Haystack, Sliceable},
    template::{self, Context, Template},
};

mod addr;
mod runner;

pub use runner::SystemRunner;

pub(crate) use addr::Address;
pub use addr::{Addr, AddrBase, SimpleAddr};
pub(crate) use runner::{EditorRunner, Runner};

use addr::ErrorKind;

/// Errors that can be returned by the exec engine
#[derive(Debug)]
pub enum Error {
    /// Format error
    Format,
    /// Invalid regex
    InvalidRegex(regex::Error),
    /// Invalid structex
    InvalidStructex(structex::Error),
    /// Invalid structex template
    InvalidTemplate(template::Error),
    /// Invalid suffix
    InvalidSuffix,
    /// IO error
    Io(io::ErrorKind, String),
    /// Error rendering a structex template
    Render(template::RenderError),
    /// Unclosed delimiter
    UnclosedDelimiter(&'static str, char),
    /// Unexpected character
    UnexpectedCharacter(char),
    /// Unexpected end of file
    UnexpectedEof,
    /// A 0 was provided as a line or column index
    ZeroIndexedLineOrColumn,
}

impl From<fmt::Error> for Error {
    fn from(_: fmt::Error) -> Self {
        Error::Format
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err.kind(), err.to_string())
    }
}

impl From<regex::Error> for Error {
    fn from(err: regex::Error) -> Self {
        Error::InvalidRegex(err)
    }
}

impl From<structex::Error> for Error {
    fn from(err: structex::Error) -> Self {
        Error::InvalidStructex(err)
    }
}

impl From<template::Error> for Error {
    fn from(err: template::Error) -> Self {
        Error::InvalidTemplate(err)
    }
}

impl From<template::RenderError> for Error {
    fn from(err: template::RenderError) -> Self {
        Error::Render(err)
    }
}

/// Something that can be edited by a Program
pub trait Edit: Address + Haystack<Regex> {
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
#[derive(Debug, Clone)]
pub struct Program {
    initial_addr: Option<Addr>,
    se: Option<Structex<Regex>>,
    templates: BTreeMap<usize, Template>,
}

impl Program {
    /// Attempt to parse a given program input
    pub fn try_parse(s: &str) -> Result<Self, Error> {
        let s = s.trim();

        let input = ParseInput::new(s);
        let (initial_addr, remaining_input) = match Addr::parse_from_input(&input) {
            Ok(dot_expr) => (Some(dot_expr), input.remaining()),

            // If the start of input is not an address we fall back to requesting the current dot
            // from the Edit we are running over during execution and attempt to parse the rest of
            // the program. We need to reconstruct the iterator here as we may have advanced
            // through the string while we attempt to parse the initial address.
            Err(e) => match e.kind {
                ErrorKind::NotAnAddress => (None, s),
                ErrorKind::InvalidRegex(e) => return Err(Error::InvalidRegex(e)),
                ErrorKind::InvalidSuffix => return Err(Error::InvalidSuffix),
                ErrorKind::UnclosedDelimiter => {
                    return Err(Error::UnclosedDelimiter("dot expr regex", '/'));
                }
                ErrorKind::UnexpectedCharacter(c) => {
                    return Err(Error::UnexpectedCharacter(c));
                }
                ErrorKind::UnexpectedEof => return Err(Error::UnexpectedEof),
                ErrorKind::ZeroIndexedLineOrColumn => {
                    return Err(Error::ZeroIndexedLineOrColumn);
                }
            },
        };

        let se: Option<Structex<Regex>> = match StructexBuilder::new(remaining_input)
            .with_allowed_argless_tags("d")
            .with_allowed_single_arg_tags("acip$<>|") // typos:ignore
            .allow_top_level_actions()
            .require_actions()
            .build()
        {
            Ok(se) => Some(se),
            Err(structex::Error::Syntax(e)) if e.kind == structex::ErrorKind::EmptyExpression => {
                None
            }
            Err(e) => return Err(e.into()),
        };

        let mut templates = BTreeMap::new();
        if let Some(se) = se.as_ref() {
            for action in se.actions() {
                if let Some(arg) = action.arg() {
                    let t = Template::parse(arg)?;
                    templates.insert(action.id(), t);
                }
            }
        }

        Ok(Self {
            initial_addr,
            se,
            templates,
        })
    }

    /// Execute this program against a given [Edit].
    pub fn execute<'a, E, R, W>(
        &self,
        ed: &'a mut E,
        runner: &mut R,
        fname: &str,
        out: &mut W,
    ) -> Result<Dot, Error>
    where
        E: Edit,
        for<'s> <E as Sliceable>::Slice<'s>: Into<Cow<'s, str>>,
        R: Runner,
        W: Write,
    {
        let mut dot = match self.initial_addr.as_ref() {
            Some(addr) => ed.map_addr(addr),
            None => ed.current_dot(),
        };

        if self.se.is_none() {
            return Ok(dot);
        };

        let (char_from, char_to) = dot.as_char_indices();
        let byte_from = ed.char_to_byte(char_from).unwrap();
        let byte_to = ed
            .char_to_byte(char_to.saturating_add(1))
            .unwrap_or_else(|| ed.len_bytes());

        let mut edit_actions = self.gather_actions(byte_from, byte_to, ed, runner, fname, out)?;

        ed.begin_edit_transaction();

        // Determine the dot we need to set by applying the last action. All other actions update
        // this selection based on how they manipulate the buffer so we need to special case this
        // final action in order to not double-count the delta it would generate.
        let last_action = edit_actions.pop();
        let mut delta = 0;
        if let Some(action) = last_action {
            dot = action.as_dot(ed);
            action.apply(ed);
        }

        // apply remaining actions in reverse order, updating the final dot position accordingly
        for action in edit_actions.into_iter().rev() {
            delta += action.apply(ed);
        }

        ed.end_edit_transaction();

        // In the case of running against a lazy stream our initial `to` will be a sential value of
        // usize::MAX which needs to be clamped to the size of the input. For Buffers and GapBuffers
        // where we know that we should already be in bounds this is not required but the overhead
        // of always doing it is minimal as checking the number of chars in the buffer is O(1) due
        // to us caching the value.
        let ix_max = ed.len_chars();

        // Apply the cumulative delta from all edit actions to the final dot position to account
        // for changes made to the buffer state.
        let (from, to) = dot.as_char_indices();
        let from = (from as isize + delta) as usize;
        let to = (to as isize + delta) as usize;

        Ok(Dot::from_char_indices(min(from, ix_max), min(to, ix_max)))
    }

    fn gather_actions<'a, E, R, W>(
        &self,
        byte_from: usize,
        byte_to: usize,
        ed: &'a E,
        runner: &mut R,
        fname: &str,
        out: &mut W,
    ) -> Result<Vec<EditAction>, Error>
    where
        E: Edit,
        for<'s> <E as Sliceable>::Slice<'s>: Into<Cow<'s, str>>,
        R: Runner,
        W: Write,
    {
        let se = self.se.as_ref().unwrap();
        let mut edit_actions = Vec::new();
        let mut ctx = Ctx {
            fname,
            byte_from: 0,
            ed,
            row_col: RefCell::new(None),
        };

        for caps in se.iter_tagged_captures_between(byte_from, byte_to, ed) {
            let action = caps.action.as_ref().unwrap();
            let id = action.id();
            ctx.byte_from = caps.from();
            ctx.row_col.borrow_mut().take();

            match action.tag() {
                // Immediate actions

                // Print rendered template
                'p' => {
                    self.templates[&id].render_with_context_to(out, &caps, &ctx)?;
                }

                // Run rendered template as shell command
                '$' => {
                    let cmd = self.templates[&id].render_with_context(&caps, &ctx)?;
                    out.write_all(runner.run_shell_command(&cmd, None)?.as_bytes())?;
                }

                // Run template as shell command with match as input
                '>' => {
                    let cmd = self.templates[&id].render_with_context(&caps, &ctx)?;
                    let slice = caps.as_slice();
                    out.write_all(
                        runner
                            .run_shell_command(&cmd, Some(slice.into().as_ref()))?
                            .as_bytes(),
                    )?;
                }

                // Edit actions

                // Delete matched text
                'd' => edit_actions.push(EditAction::Remove(caps.from(), caps.to())),

                // Change matched text to rendered template
                'c' => {
                    edit_actions.push(EditAction::Replace(
                        caps.from(),
                        caps.to(),
                        self.templates[&id].render_with_context(&caps, &ctx)?,
                    ));
                }

                // Insert rendered template before match
                'i' => {
                    edit_actions.push(EditAction::Insert(
                        caps.from(),
                        self.templates[&id].render_with_context(&caps, &ctx)?,
                    ));
                }

                // Append rendered template after match
                'a' => {
                    edit_actions.push(EditAction::Insert(
                        caps.to(),
                        self.templates[&id].render_with_context(&caps, &ctx)?,
                    ));
                }

                // Replace matched text with output from running rendered template as shell command
                '<' => {
                    let cmd = self.templates[&id].render_with_context(&caps, &ctx)?;
                    edit_actions.push(EditAction::Replace(
                        caps.from(),
                        caps.to(),
                        runner.run_shell_command(&cmd, None)?,
                    ));
                }

                // Pipe matched text through running rendered template as a shell command
                '|' => {
                    let cmd = self.templates[&id].render_with_context(&caps, &ctx)?;
                    let slice = caps.as_slice();
                    edit_actions.push(EditAction::Replace(
                        caps.from(),
                        caps.to(),
                        runner.run_shell_command(&cmd, Some(slice.into().as_ref()))?,
                    ));
                }

                _ => unreachable!(),
            }
        }

        Ok(edit_actions)
    }
}

#[derive(Debug)]
enum EditAction {
    Insert(usize, String),
    Remove(usize, usize),
    Replace(usize, usize, String),
}

impl EditAction {
    fn as_dot<E>(&self, ed: &mut E) -> Dot
    where
        E: Edit,
    {
        match self {
            Self::Insert(from, s) | Self::Replace(from, _, s) => {
                let from = ed.byte_to_char(*from).unwrap();
                let n_chars = s.chars().count();

                Dot::from_char_indices(from, from + n_chars - 1)
            }

            Self::Remove(from, _) => {
                let from = ed.byte_to_char(*from).unwrap();
                Dot::from_char_indices(from, from)
            }
        }
    }

    fn apply<E>(self, ed: &mut E) -> isize
    where
        E: Edit,
    {
        match self {
            Self::Insert(from, s) => {
                let from = ed.byte_to_char(from).unwrap();
                ed.insert(from, &s);
                s.chars().count() as isize
            }

            Self::Remove(from, to) => {
                let from = ed.byte_to_char(from).unwrap();
                let to = ed.byte_to_char(to).unwrap();
                ed.remove(from, to);
                -((to - from) as isize)
            }

            Self::Replace(from, to, s) => {
                Self::Remove(from, to).apply(ed);
                let n_chars = Self::Insert(from, s).apply(ed);
                n_chars - (to - from) as isize
            }
        }
    }
}

struct Ctx<'a, E>
where
    E: Edit,
{
    fname: &'a str,
    byte_from: usize,
    ed: &'a E,
    row_col: RefCell<Option<(String, String)>>,
}

impl<'a, E> Ctx<'a, E>
where
    E: Edit,
{
    fn ensure_row_col(&self) {
        if self.row_col.borrow().is_some() {
            return;
        }

        let char_from = self.ed.byte_to_char(self.byte_from).unwrap();
        let row = self.ed.char_to_line(char_from).unwrap();
        let col = char_from - self.ed.line_to_char(row).unwrap();

        *self.row_col.borrow_mut() = Some((row.to_string(), col.to_string()));
    }
}

impl<'a, E> Context for Ctx<'a, E>
where
    E: Edit,
{
    fn render_var<W>(&self, var: &str, w: &mut W) -> Option<io::Result<usize>>
    where
        W: Write,
    {
        match var {
            "FILENAME" => Some(w.write_all(self.fname.as_bytes()).map(|_| self.fname.len())),

            "ROW" => {
                self.ensure_row_col();
                let rc = self.row_col.borrow();
                let row = &rc.as_ref().unwrap().0;

                Some(w.write_all(row.as_bytes()).map(|_| row.len()))
            }

            "COL" => {
                self.ensure_row_col();
                let rc = self.row_col.borrow();
                let col = &rc.as_ref().unwrap().1;

                Some(w.write_all(col.as_bytes()).map(|_| col.len()))
            }

            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{buffer::Buffer, editor::Action};
    use simple_test_case::test_case;
    use std::{collections::HashMap, env, io};

    #[test_case(", x/(t.)/ c/{1}X/", "thXis is a teXst XstrXing"; "x c")]
    #[test_case(", x/(t.)/ i/{1}/", "ththis is a tetest t strtring"; "x i")]
    #[test_case(", x/(t.)/ a/{1}/", "ththis is a tetest t strtring"; "x a")]
    #[test]
    fn substitution_of_submatches_works(s: &str, expected: &str) {
        let prog = Program::try_parse(s).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());

        let mut b = Buffer::new_unnamed(0, "this is a test string", Default::default());
        prog.execute(&mut b, &mut runner, "test", &mut Vec::new())
            .unwrap();

        assert_eq!(&b.txt.to_string(), expected);
    }

    #[test]
    fn templating_context_vars_works() {
        // FILENAME, ROW, and COL all need to be worked out from the buffer being run against
        let prog = Program::try_parse(", x/line/ a/ ({FILENAME} {ROW}:{COL})/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());

        let mut b = Buffer::new_unnamed(
            0,
            " │  line one\n世 line two\n   🦊  line three",
            Default::default(),
        );

        prog.execute(&mut b, &mut runner, "test", &mut Vec::new())
            .unwrap();

        assert_eq!(
            &b.txt.to_string(),
            // the column offsets here should be in terms of characters, not bytes
            " │  line (test 0:4) one\n世 line (test 1:2) two\n   🦊  line (test 2:6) three"
        );
    }

    #[test]
    fn loop_between_generates_the_correct_blocks() {
        let prog = Program::try_parse(", y/ / p/>{0}<\n/").unwrap();
        let mut b = Buffer::new_unnamed(0, "this and that", Default::default());
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut output = Vec::new();
        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut output)
            .unwrap();

        let s = String::from_utf8(output).unwrap();
        assert_eq!(s, ">this<\n>and<\n>that<\n");

        let dot_content = dot.content(&b);
        assert_eq!(dot_content, "this and that");
    }

    #[test_case(0, "/oo.fo/ d", "fo│foo"; "regex dot delete")] // typos:ignore
    #[test_case(2, "-/f/,/f/ d", "oo│foo"; "regex dot range delete")]
    #[test_case(0, ", x/foo/ p/{0}/", "foo│foo│foo"; "x print")]
    #[test_case(0, ", x/foo/ i/X/", "Xfoo│Xfoo│Xfoo"; "x insert")]
    #[test_case(0, ", x/foo/ a/X/", "fooX│fooX│fooX"; "x append")]
    #[test_case(0, ", x/foo/ c/X/", "X│X│X"; "x change")]
    #[test_case(0, ", x/foo/ c/XX/", "XX│XX│XX"; "x change 2")]
    #[test_case(0, ", x/foo/ d", "││"; "x delete")]
    #[test_case(0, ", y/foo/ p/>{0}</", "foo│foo│foo"; "y print")]
    #[test_case(0, ", y/foo/ i/X/", "fooX│fooX│foo"; "y insert")]
    #[test_case(0, ", y/foo/ a/X/", "foo│Xfoo│Xfoo"; "y append")]
    #[test_case(0, ", y/foo/ c/X/", "fooXfooXfoo"; "y change")]
    #[test_case(0, ", y/foo/ d", "foofoofoo"; "y delete")]
    #[test_case(0, ", y/│/ d", "││"; "y delete 2")]
    #[test_case(0, ", x/\\b\\w+\\b/ c/X/", "X│X│X"; "change each word")]
    #[test]
    fn execute_produces_the_correct_string(idx: usize, s: &str, expected: &str) {
        let prog = Program::try_parse(s).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());

        let mut b = Buffer::new_unnamed(0, "foo│foo│foo", Default::default());
        b.dot = Cur::new(idx).into();
        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.txt.to_string(), expected, "buffer");
    }

    #[test]
    fn multiline_file_dot_star_works() {
        let prog = Program::try_parse(", x/.*/ c/foo/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, "this is\na multiline\nfile", Default::default());
        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        // '.*' will match the null string at the end of lines containing a newline as well
        assert_eq!(&b.txt.to_string(), "foofoo\nfoofoo\nfoo");
    }

    #[test]
    fn multiline_file_dot_plus_works() {
        let prog = Program::try_parse(", x/.+/ c/foo/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, "this is\na multiline\nfile", Default::default());
        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.txt.to_string(), "foo\nfoo\nfoo");
    }

    #[test]
    fn buffer_current_dot_is_used_when_there_is_no_leading_addr() {
        // The only thing this program does is delete the selection which should be the current
        // buffer dot rather than the entire buffer.
        let prog = Program::try_parse("d").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());

        let initial_content = "this is a FOO line\nand another";
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());
        b.dot = Dot::from_char_indices(9, 12);
        assert_eq!(b.dot_contents(), " FOO");

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();
        assert_eq!(&b.str_contents(), "this is a line\nand another");
    }

    #[test_case(", x/a/ d", "foo br bz", "z", (8, 8); "extract delete")]
    #[test_case(", x/a/ i/12/", "foo b12ar b12az", "12", (11, 12); "extract insert")]
    #[test_case(", x/o/ a/XYZ/", "foXYZoXYZ bar baz", "XYZ", (6, 8); "extract append")] // typos:ignore
    #[test_case(", x/b/ c/B/", "foo Bar Baz", "B", (8, 8); "extract change same length")]
    #[test_case(", x/b./ c/X/", "foo Xr Xz", "X", (7, 7); "extract change shorter")]
    #[test_case(", x/b/ c/Bee/", "foo Beear Beeaz", "Bee", (10, 12); "extract change longer")]
    #[test_case(", x/b../ p/{0}/", "foo bar baz", "foo bar baz", (0, 11); "print should keep original")]
    #[test]
    fn returned_dot_should_hold_the_final_edit(
        s: &str,
        expected_content: &str,
        expected_dot_content: &str,
        expected_dot: (usize, usize),
    ) {
        let prog = Program::try_parse(s).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());

        let initial_content = "foo bar baz";
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content);
        assert_eq!(&dot.content(&b), expected_dot_content);
        assert_eq!(dot.as_char_indices(), expected_dot);
    }

    #[test_case(", d"; "delete buffer")]
    #[test_case(", x/th/ d"; "delete each th")]
    #[test_case(", x/ / d"; "delete spaces")]
    #[test_case(", x/\\b\\w+\\b/ d"; "delete each word")]
    #[test_case(", x/. / d"; "delete things before a space")]
    #[test_case(", x/\\b\\w+\\b/ c/buffalo/"; "change each word")]
    #[test_case(", x/\\b\\w+\\b/ a/buffalo/"; "append to each word")]
    #[test_case(", x/\\b\\w+\\b/ i/buffalo/"; "insert before each word")]
    #[test]
    fn buffer_execute_undo_all_is_a_noop(s: &str) {
        let prog = Program::try_parse(s).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let initial_content = "this is a line\nand another\n- [ ] something to do\n";
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();
        while b.handle_action(Action::Undo, Source::Keyboard).is_none() {}
        let final_content = b.str_contents();

        assert_eq!(&final_content, initial_content);
    }

    struct MockRunner {
        responses: HashMap<(String, Option<String>), io::Result<String>>,
    }

    impl MockRunner {
        fn new() -> Self {
            Self {
                responses: HashMap::new(),
            }
        }

        fn with_response(mut self, cmd: &str, output: &str) -> Self {
            self.responses
                .insert((cmd.to_string(), None), Ok(output.to_string()));
            self
        }

        fn with_response_for_input(mut self, cmd: &str, input: &str, output: &str) -> Self {
            self.responses.insert(
                (cmd.to_string(), Some(input.to_string())),
                Ok(output.to_string()),
            );
            self
        }

        fn with_failure_for_input(mut self, cmd: &str, input: &str, error_msg: &str) -> Self {
            self.responses.insert(
                (cmd.to_string(), Some(input.to_string())),
                Err(io::Error::other(error_msg)),
            );
            self
        }
    }

    impl Runner for MockRunner {
        fn run_shell_command(&mut self, cmd: &str, input: Option<&str>) -> io::Result<String> {
            let key = (cmd.to_string(), input.map(|s| s.to_string()));

            match self.responses.get(&key) {
                Some(Ok(output)) => Ok(output.clone()),
                Some(Err(e)) => Err(io::Error::new(e.kind(), e.to_string())),
                None => panic!(
                    "MockRunner: no response configured for command {cmd:?} with input {input:?}"
                ),
            }
        }
    }

    #[test]
    fn runner_based_action_errors_are_returned() {
        let prog = Program::try_parse(", x/foo/ >/fail/").unwrap();
        let mut b = Buffer::new_unnamed(0, "foo bar", Default::default());
        let mut runner = MockRunner::new().with_failure_for_input("fail", "foo", "error");

        let result = prog.execute(&mut b, &mut runner, "test", &mut Vec::new());

        assert!(result.is_err());
        assert_eq!(b.str_contents(), "foo bar");
    }

    #[test_case("foo", ", x/foo/ $/cmd/", "cmd", "X", "foo", "X"; "simple output")]
    #[test_case(" foo", ", x/foo/ $/cmd {FILENAME} {ROW} {COL}/", "cmd test 0 1", "ok\n", " foo", "ok\n"; "context variables")]
    #[test_case("foo", ", x/foo/ $/empty/", "empty", "", "foo", ""; "empty output")]
    #[test_case("foo", ", x/foo/ $/multi/", "multi", "line1\nline2\n", "foo", "line1\nline2\n"; "multiline output")]
    #[test_case("foo bar foo", ", x/foo/ $/cmd/", "cmd", "X", "foo bar foo", "XX"; "multiple matches")]
    #[test]
    fn shell_dollar_action_works(
        initial: &str,
        program: &str,
        mock_cmd: &str,
        mock_output: &str,
        expected_buffer: &str,
        expected_output: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut b = Buffer::new_unnamed(0, initial, Default::default());
        let mut runner = MockRunner::new().with_response(mock_cmd, mock_output);
        let mut output = Vec::new();

        prog.execute(&mut b, &mut runner, "test", &mut output)
            .unwrap();

        assert_eq!(b.str_contents(), expected_buffer);
        assert_eq!(String::from_utf8(output).unwrap(), expected_output);
    }

    #[test_case("foo", ", x/foo/ >/cat/", "cat", "foo", "FOO", "foo", "FOO"; "simple")]
    #[test_case("foo\nbar\nbaz", ", x/foo\\nbar/ >/process/", "process", "foo\nbar", "processed", "foo\nbar\nbaz", "processed"; "multiline input")]
    #[test_case("word", ", x/(\\w+)/ >/process {1}/", "process word", "word", "result", "word", "result"; "template in command")]
    #[test]
    fn shell_redirect_in_action(
        initial: &str,
        program: &str,
        mock_cmd: &str,
        mock_input: &str,
        mock_output: &str,
        expected_buffer: &str,
        expected_output: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut b = Buffer::new_unnamed(0, initial, Default::default());
        let mut runner =
            MockRunner::new().with_response_for_input(mock_cmd, mock_input, mock_output);
        let mut output = Vec::new();

        prog.execute(&mut b, &mut runner, "test", &mut output)
            .unwrap();

        assert_eq!(b.str_contents(), expected_buffer);
        assert_eq!(String::from_utf8(output).unwrap(), expected_output);
    }

    #[test_case("this foo that", ", x/foo/ </cmd/", "cmd", "bar", "this bar that"; "simple replacement")]
    #[test_case("foo foo foo", ", x/foo/ </cmd/", "cmd", "X", "X X X"; "multiple matches")]
    #[test_case("word", ", x/(\\w+)/ </process {1}/", "process word", "WORD", "WORD"; "template in command")]
    #[test_case("foo bar foo", ", x/foo/ </cmd/", "cmd", "", " bar "; "empty output deletes")]
    #[test_case("foo", ", x/foo/ </cmd/", "cmd", "line1\nline2\n", "line1\nline2\n"; "multiline output")]
    #[test]
    fn shell_redirect_out_action(
        initial: &str,
        program: &str,
        mock_cmd: &str,
        mock_output: &str,
        expected_buffer: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut b = Buffer::new_unnamed(0, initial, Default::default());
        let mut runner = MockRunner::new().with_response(mock_cmd, mock_output);
        let mut output = Vec::new();

        prog.execute(&mut b, &mut runner, "test", &mut output)
            .unwrap();

        assert_eq!(b.str_contents(), expected_buffer);
    }

    #[test]
    fn shell_redirect_out_action_sets_dot() {
        let prog = Program::try_parse(", x/foo/ </cmd/").unwrap();
        let mut runner = MockRunner::new().with_response("cmd", "REPLACEMENT");
        let mut b = Buffer::new_unnamed(0, "foo bar foo", Default::default());
        let mut output = Vec::new();

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut output)
            .unwrap();

        assert_eq!(dot.content(&b), "REPLACEMENT");
    }

    #[test_case("this foo that", ", x/foo/ |/upper/", "upper", "foo", "FOO", "this FOO that"; "simple transformation")]
    #[test_case("foo\nbar\nbaz", ", x/foo\\nbar/ |/transform/", "transform", "foo\nbar", "transformed", "transformed\nbaz"; "multiline match")]
    #[test_case("word", ", x/(\\w+)/ |/process {1}/", "process word", "word", "WORD", "WORD"; "template in command")]
    #[test_case("foo\tbar\n", ", |/cmd/", "cmd", "foo\tbar\n", "transformed", "transformed"; "special chars")]
    #[test]
    fn shell_pipe_action(
        initial: &str,
        program: &str,
        mock_cmd: &str,
        mock_input: &str,
        mock_output: &str,
        expected_buffer: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut b = Buffer::new_unnamed(0, initial, Default::default());
        let mut runner =
            MockRunner::new().with_response_for_input(mock_cmd, mock_input, mock_output);

        prog.execute(&mut b, &mut runner, "test", &mut Vec::new())
            .unwrap();

        assert_eq!(b.str_contents(), expected_buffer);
    }

    #[test]
    fn shell_pipe_action_sets_dot() {
        let prog = Program::try_parse(", x/foo/ |/expand/").unwrap();
        let mut b = Buffer::new_unnamed(0, "foo bar", Default::default());
        let mut runner =
            MockRunner::new().with_response_for_input("expand", "foo", "much longer replacement");

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut Vec::new())
            .unwrap();

        assert_eq!(b.str_contents(), "much longer replacement bar");
        assert_eq!(dot.content(&b), "much longer replacement");
    }

    #[test_case("/[unclosed/"; "invalid regex forward")]
    #[test_case("-/[unclosed/"; "invalid regex backward")]
    #[test_case("/foo/,/[bad/"; "invalid regex in compound")]
    #[test_case("/(?P<invalid>/"; "invalid regex special chars")]
    #[test]
    fn try_parse_invalid_regex_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::InvalidRegex(_))));
    }

    #[test_case("/foo"; "forward regex no closing")]
    #[test_case("-/bar"; "backward regex no closing")]
    #[test_case("/foo/,/bar"; "compound second unclosed")]
    #[test]
    fn try_parse_unclosed_delimiter_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::UnclosedDelimiter(_, '/'))));
    }

    #[test_case("5:@"; "unexpected char after colon")]
    #[test_case("5:@10"; "unexpected char before column")]
    #[test]
    fn try_parse_unexpected_character_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::UnexpectedCharacter(_))));
    }

    #[test_case("1:0"; "zero column")]
    #[test_case("2:00"; "zero column with double zero")]
    #[test_case("10:000"; "zero column with triple zero")]
    #[test]
    fn try_parse_zero_indexed_line_or_column_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::ZeroIndexedLineOrColumn)));
    }

    #[test_case("#,5"; "incomplete char addr in compound start")]
    #[test_case("+#,10"; "incomplete relative char in compound start")]
    #[test_case("-#,"; "incomplete relative char back in compound")]
    #[test_case("#"; "char addr at eof")]
    #[test_case("+#"; "relative char forward at eof")]
    #[test_case("-#"; "relative char back at eof")]
    #[test]
    fn try_parse_malformed_leading_address_returns_error(addr: &str) {
        let res = Program::try_parse(&format!("{addr} x/../ d"));
        assert!(res.is_err(), "expected error, got {res:?}");
    }

    #[test_case(","; "omitted start defaults to bof")]
    #[test_case(",5"; "omitted start with line end")]
    #[test_case(",/foo/"; "omitted start with regex end")]
    #[test_case(",$"; "omitted start with eof")]
    #[test]
    fn try_parse_omitted_leading_address_works(addr: &str) {
        let res = Program::try_parse(&format!("{addr} x/../ d"));
        assert!(res.is_ok(), "expected OK, got {res:?}");
        assert!(res.unwrap().initial_addr.is_some());
    }

    #[test_case("#"; "char addr incomplete")]
    #[test_case("+#"; "relative char forward incomplete")]
    #[test_case("-#"; "relative char back incomplete")]
    #[test]
    fn try_parse_unexpected_eof_returns_error(addr: &str) {
        let res = Program::try_parse(&format!("{addr} x/../ d"));
        // The exact error we get here depends on how the address is malformed. We deliberately
        // swallow "NotAnAddress" and try to parse the full input as a Structex if possible.
        assert!(res.is_err(), "expected error, got {res:?}");
    }

    #[test_case("x/foo/ d"; "x")]
    #[test_case("y/bar/ c/X/"; "y")]
    #[test_case("d"; "d")]
    #[test]
    fn try_parse_no_leading_address_with_action_works(input: &str) {
        let res = Program::try_parse(input);
        assert!(res.is_ok(), "expected OK, got {res:?}");
        assert!(res.unwrap().initial_addr.is_none());
    }

    #[test_case("5,#"; "incomplete char end")]
    #[test_case("/foo/,+#"; "incomplete relative char end")]
    #[test_case("1,#abc"; "char with non-digit")]
    #[test]
    fn try_parse_malformed_trailing_address_returns_error(addr: &str) {
        let res = Program::try_parse(&format!("{addr} x/../ d"));
        assert!(res.is_err(), "expected error, got {res:?}");
    }

    #[test_case(", x/foo/ c/{0/"; "change action unclosed submatch")]
    #[test_case(", x/foo/ i/{1/"; "insert action unclosed submatch")]
    #[test_case(", x/foo/ a/{FILENAME/"; "append action unclosed variable")]
    #[test_case(", x/foo/ p/{ROW/"; "print action unclosed variable")]
    #[test_case(", x/foo/ $/cmd {0/"; "shell dollar action unclosed")]
    #[test_case(", x/foo/ >/cmd {1/"; "shell redirect in unclosed")]
    #[test_case(", x/foo/ </cmd {2/"; "shell redirect out unclosed")]
    #[test_case(", x/foo/ |/cmd {COL/"; "shell pipe unclosed")]
    #[test]
    fn try_parse_template_unclosed_brace_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::InvalidTemplate(_))));
    }

    #[test_case(", x/foo/ c/\\x/"; "escape x not valid")]
    #[test_case(", x/foo/ i/\\z/"; "escape z not valid")]
    #[test_case(", x/foo/ a/\\r/"; "escape r not valid")]
    #[test_case(", x/foo/ p/\\b/"; "escape b not valid")]
    #[test_case(", x/foo/ c/foo\\qbar/"; "escape q in middle")]
    #[test_case(", x/foo/ i/\\d/"; "escape d not valid")]
    #[test_case(", x/foo/ $/cmd \\w/"; "escape w in shell command")]
    #[test]
    fn try_parse_template_invalid_escape_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::InvalidTemplate(_))));
    }

    // Note: Some apparent template EOF errors are actually caught by structex as
    // MissingDelimiter errors. For example:
    // - ", x/foo/ i/\\/" - backslash escapes the closing '/', causing structex error
    // - ", x/foo/ a/{0" - missing closing '/', caught by structex before template parsing
    #[test_case(", x/foo/ c/{/"; "eof after opening brace")]
    #[test_case(", x/foo/ p/{F/"; "eof in middle of variable")]
    #[test_case(", x/foo/ $/cmd {/"; "shell command eof after brace")]
    #[test]
    fn try_parse_template_unexpected_eof_returns_error(input: &str) {
        let res = Program::try_parse(input);
        assert!(matches!(res, Err(Error::InvalidTemplate(_))));
    }

    #[test_case(", x/foo/ p/{UNKNOWN}/"; "print with unknown variable")]
    #[test_case(", x/foo/ c/{INVALID_VAR}/"; "change with unknown variable")]
    #[test_case(", x/foo/ i/{NOTDEFINED}/"; "insert with unknown variable")]
    #[test_case(", x/foo/ a/{BADVAR}/"; "append with unknown variable")]
    #[test]
    fn execute_with_unknown_variable_returns_error(input: &str) {
        let program = Program::try_parse(input).unwrap();
        let mut buffer = Buffer::new_unnamed(0, "foo bar", Default::default());
        let mut runner = MockRunner::new();
        let mut output = Vec::new();

        let res = program.execute(&mut buffer, &mut runner, "test.txt", &mut output);
        assert!(matches!(res, Err(Error::Render(_))));
    }

    /// A Writer that always returns IO errors
    struct FailingWriter;

    impl io::Write for FailingWriter {
        fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
            Err(io::Error::other("mock write failure"))
        }

        fn flush(&mut self) -> io::Result<()> {
            Err(io::Error::other("mock flush failure"))
        }
    }

    #[test]
    fn execute_print_with_failing_writer_returns_io_error() {
        let program = Program::try_parse(", x/foo/ p/{0}/").unwrap();
        let mut buffer = Buffer::new_unnamed(0, "foo bar foo", Default::default());
        let mut runner = MockRunner::new();

        let res = program.execute(&mut buffer, &mut runner, "test.txt", &mut FailingWriter);
        assert!(matches!(res, Err(Error::Render(_))));
    }

    #[test]
    fn execute_shell_dollar_with_failing_writer_returns_io_error() {
        let program = Program::try_parse(", x/foo/ $/echo test/").unwrap();
        let mut buffer = Buffer::new_unnamed(0, "foo", Default::default());
        let mut runner = MockRunner::new().with_response("echo test", "output");

        let res = program.execute(&mut buffer, &mut runner, "test.txt", &mut FailingWriter);
        assert!(matches!(res, Err(Error::Io(..))));
    }

    #[test_case(0, ".,. d", "foo│bar│baz", "oo│bar│baz", (0, 0); "dot current position")]
    #[test_case(5, ".,. d", "foo│bar│baz", "foo│br│baz", (5, 5); "dot at delimiter")]
    #[test_case(0, "0,0 d", "foo│bar│baz", "oo│bar│baz", (0, 0); "bof beginning of file")]
    #[test_case(0, "$,$ d", "foo│bar│baz", "foo│bar│baz", (11, 11); "eof end of file")]
    #[test_case(0, "-,- d", "line1\nline2\nline3", "ine1\nline2\nline3", (0, 0); "bol at line start")] // typos:ignore
    #[test_case(3, "-,- d", "line1\nline2\nline3", "1\nline2\nline3", (0, 0); "bol from mid line")]
    #[test_case(0, "+,+ d", "line1\nline2", "ine2", (0, 0); "eol from line start")] // typos:ignore
    #[test_case(2, "+,+ d", "line1\nline2", "liine2", (2, 2); "eol from mid line")]
    #[test_case(0, "-,+ d", "line1\nline2", "ine2", (0, 0); "current line from start")] // typos:ignore
    #[test_case(3, "-,+ d", "line1\nline2", "ine2", (0, 0); "current line from middle")] // typos:ignore
    #[test_case(0, "2,2 d", "line1\nline2\nline3", "line1\nline3", (6, 6); "absolute line 2")]
    #[test_case(0, "1,1 d", "line1\nline2", "line2", (0, 0); "absolute line 1")]
    #[test_case(0, "#5,#5 d", "0123456789", "012346789", (5, 5); "absolute char offset")]
    #[test_case(0, "#0,#0 d", "hello world", "ello world", (0, 0); "char offset at start")]
    #[test_case(5, "+2,+2 d", "L1\nL2\nL3\nL4", "L1\nL2\nL3\n4", (9, 9); "relative line forward")]
    #[test_case(10, "-2,-2 d", "L1\nL2\nL3\nL4", "L1\nL3\nL4", (3, 3); "relative line backward")]
    #[test_case(5, "+#3,+#3 d", "hello world", "hello wold", (8, 8); "relative char forward")]
    #[test_case(8, "-#3,-#3 d", "hello world", "helloworld", (5, 5); "relative char backward")]
    #[test_case(0, "2:3,2:3 d", "L1\nL2\nL3", "L1\nL2L3", (5, 5); "line and column")]
    #[test_case(0, "1:1,1:1 d", "hello\nworld", "ello\nworld", (0, 0); "line 1 col 1")]
    #[test_case(0, "2:1,2:1 d", "hello\nworld", "hello\norld", (6, 6); "second line first col")]
    #[test]
    fn address_simple_positions_work(
        initial_dot_idx: usize,
        program: &str,
        initial_content: &str,
        expected_content: &str,
        expected_dot: (usize, usize),
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());
        b.dot = Cur::new(initial_dot_idx).into();

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content, "buffer content");
        assert_eq!(dot.as_char_indices(), expected_dot, "returned dot");
    }

    #[test_case("0,/foo/ d", "start\nfoo bar", " bar"; "bof to regex")]
    #[test_case("0,$ d", "hello\nworld", ""; "bof to eof entire buffer")]
    #[test_case("/foo/,$ d", "hello\nfoo\nbar", "hello\n"; "regex to eof")]
    #[test_case("2,4 d", "L1\nL2\nL3\nL4\nL5", "L1\nL5"; "line range")]
    #[test_case("#5,#10 d", "0123456789abc", "01234bc"; "char range")]
    #[test_case("1:2,2:3 d", "hello\nworld", "hld"; "line:col range")]
    #[test_case("-,+ d", "L1\nL2\nL3", "2\nL3"; "bol to eol entire line")]
    #[test_case(".,$ d", "foo\nbar\nbaz", ""; "dot to eof")]
    #[test_case("0,#10 d", "hello world test", " test"; "bof to char offset")]
    #[test_case("2,/end/ d", "start\nL2\nend here", "start\n here"; "line to regex")]
    #[test_case("#10,$ d", "0123456789rest", "0123456789"; "char to eof")]
    #[test_case("/start/,3 d", "foo\nstart\nL3\nbar", "foo\nbar"; "regex to line")]
    #[test]
    fn address_compound_ranges_work(program: &str, initial_content: &str, expected_content: &str) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content);
    }

    #[test_case(", d", "", ""; "delete on empty buffer")]
    #[test_case(", x/foo/ c/bar/", "", ""; "x on empty buffer")]
    #[test_case(", y/foo/ c/bar/", "", ""; "y on empty buffer")]
    #[test_case("0,$ d", "", ""; "full buffer delete on empty")]
    #[test_case(", x/foo/ d", "bar baz qux", "bar baz qux"; "x no matches")]
    #[test_case(", x/foo/ c/replacement/", "bar baz", "bar baz"; "x no matches change")]
    #[test_case(", y/foo/ i/X/", "bar baz", "Xbar baz"; "y no matches inserts once")]
    #[test_case("/foo/ d", "bar baz", "ar baz"; "regex address no match")]
    #[test]
    fn edge_case_empty_and_no_matches_work(
        program: &str,
        initial_content: &str,
        expected_content: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content, "buffer content");
    }

    #[test_case(", x/.*/ i/X/", "foo", "Xfoo"; "zero-length dot star insert")]
    #[test_case(", x/.*/ p/{0}/", "a\nb", "a\nb"; "zero-length dot star print")]
    #[test_case(", x/^/ i/> /", "foo\nbar", "> f> o> o> \n> b> a> r"; "zero-length line start")]
    #[test_case(", x/$/ a/ </", "foo\nbar", " <f <o <o <\n <b <a <r"; "zero-length line end")]
    #[test_case(", x/\\b/ i/|/", "foo bar", "|f|o|o| |b|a|r"; "zero-length word boundary")]
    #[test_case(", y/.*/ i/X/", "foo", "foo"; "y with zero-length")]
    #[test]
    fn edge_case_zero_length_matches_work(
        program: &str,
        initial_content: &str,
        expected_content: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content, "buffer content");
    }

    #[test]
    fn edge_case_very_large_replacement() {
        let large_replacement = "X".repeat(10000);
        let prog = Program::try_parse(&format!(", x/foo/ c/{}/", large_replacement)).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, "foo bar foo", Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        let expected = format!("{} bar {}", large_replacement, large_replacement);
        assert_eq!(&b.str_contents(), &expected);
    }

    #[test]
    fn edge_case_large_buffer_with_many_matches() {
        let initial = "foo ".repeat(1000);
        let prog = Program::try_parse(", x/foo/ c/bar/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, &initial, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        let expected = "bar ".repeat(1000);
        assert_eq!(&b.str_contents(), &expected);
    }

    #[test_case("0,$", "hello world", "hello world", (0, 11); "full buffer address only")]
    #[test_case("/foo/", "bar foo baz", "bar foo baz", (4, 6); "regex address only")]
    #[test_case("2", "L1\nL2\nL3", "L1\nL2\nL3", (3, 5); "line address only")]
    #[test_case("#5", "hello world", "hello world", (5, 5); "char address only")]
    #[test]
    fn edge_case_address_only_programs_work(
        program: &str,
        initial_content: &str,
        expected_content: &str,
        expected_dot: (usize, usize),
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(
            &b.str_contents(),
            expected_content,
            "buffer should not change"
        );
        assert_eq!(dot.as_char_indices(), expected_dot, "dot should be set");
    }

    #[test_case(", x/./ c/X/", "hello", "XXXXX"; "ascii single char")]
    #[test_case(", x/./ c/X/", "世界", "XX"; "multibyte chars")]
    #[test_case(", x/./ c/X/", "🦊🐕", "XX"; "emoji")]
    #[test_case(", x/./ c/X/", "é", "X"; "e with acute literal")]
    #[test_case("#2,#4 d", "世界你好", "世界"; "char offset with multibyte")]
    #[test_case(", x/\\w+/ c/X/", "hello世界", "X世界"; "word with mixed scripts")]
    #[test_case("1:2,1:4 d", "世界你好", "世"; "line:col with multibyte")]
    #[test]
    fn edge_case_unicode_works(program: &str, initial_content: &str, expected_content: &str) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content, "buffer content");
    }

    // This is a regression test for a panic found when testing the program being run here. This
    // should replace each character in the buffer with an "X": originally this incorrectly treated
    // the "é" character as two distinct characters and ended up positioning the GapBuffer gap in
    // an invalid position.
    #[test]
    fn multibyte_unicode_combining_characters_are_handled_correctly() {
        let prog = Program::try_parse(", x/./ c/X/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut gb = GapBuffer::from("é");

        prog.execute(&mut gb, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&gb.to_string(), "X");
    }

    #[test]
    fn edge_case_buffer_grows_significantly() {
        let prog = Program::try_parse(", x/x/ c/REPLACEMENT/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, "x x x", Default::default());

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), "REPLACEMENT REPLACEMENT REPLACEMENT");
        assert!(dot.as_char_indices().1 > 5);
    }

    #[test]
    fn edge_case_buffer_shrinks_significantly() {
        let prog = Program::try_parse(", x/LONGWORD/ c/x/").unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, "LONGWORD LONGWORD LONGWORD", Default::default());

        let dot = prog
            .execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), "x x x");
        assert_eq!(dot.as_char_indices(), (4, 4));
    }

    #[test_case(", x/foo/ x/o/ c/X/", "foo bar", "fXX bar"; "nested x")]
    #[test_case(", x/\\w+/ y/o/ c/X/", "foo boo", "Xoo Xoo"; "x containing y")]
    #[test_case(", y/foo/ x/o/ c/X/", "foo bar foo", "foo bar foo"; "y containing x")]
    #[test]
    fn edge_case_complex_structex_works(
        program: &str,
        initial_content: &str,
        expected_content: &str,
    ) {
        let prog = Program::try_parse(program).unwrap();
        let mut runner = SystemRunner::new(env::current_dir().unwrap());
        let mut b = Buffer::new_unnamed(0, initial_content, Default::default());

        prog.execute(&mut b, &mut runner, "test", &mut vec![])
            .unwrap();

        assert_eq!(&b.str_contents(), expected_content, "buffer content");
    }
}
