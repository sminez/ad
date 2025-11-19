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

pub(crate) use addr::{Addr, AddrBase, Address};
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
                ErrorKind::UnclosedDelimiter => {
                    return Err(Error::UnclosedDelimiter("dot expr regex", '/'));
                }
                ErrorKind::UnexpectedCharacter(c) => {
                    return Err(Error::UnexpectedCharacter(c));
                }
                ErrorKind::InvalidSuffix => return Err(Error::InvalidSuffix),
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
        for<'e> &'e E: Haystack<Regex>,
        for<'s> <&'s E as Sliceable>::Slice<'s>: Into<Cow<'s, str>>,
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
        for<'e> &'e E: Haystack<Regex>,
        for<'s> <&'s E as Sliceable>::Slice<'s>: Into<Cow<'s, str>>,
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
    use std::env;

    use super::*;
    use crate::{buffer::Buffer, editor::Action};
    use simple_test_case::test_case;

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
}
