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
    cell::RefCell,
    cmp::min,
    collections::BTreeMap,
    fmt,
    io::{self, Write},
};
use structex::{
    Structex, StructexBuilder,
    template::{Context, Template},
};

mod addr;

use addr::ErrorKind;
pub(crate) use addr::{Addr, AddrBase, Address};

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
    InvalidTemplate(structex::template::Error),
    /// Invalid suffix
    InvalidSuffix,
    /// IO error
    Io(io::ErrorKind, String),
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
impl From<structex::template::Error> for Error {
    fn from(err: structex::template::Error) -> Self {
        Error::InvalidTemplate(err)
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
    initial_dot: Addr,
    se: Option<Structex<Regex>>,
    templates: BTreeMap<usize, Template>,
}

impl Program {
    /// Attempt to parse a given program input
    pub fn try_parse(s: &str) -> Result<Self, Error> {
        let s = s.trim();

        let input = ParseInput::new(s);
        let (initial_dot, remaining_input) = match Addr::parse_from_input(&input) {
            Ok(dot_expr) => (dot_expr, input.remaining()),

            // If the start of input is not an address we default to Full and attempt to parse the
            // rest of the program. We need to reconstruct the iterator here as we may have
            // advanced through the string while we attempt to parse the initial address.
            Err(e) => match e.kind {
                ErrorKind::NotAnAddress => (Addr::full(), s),
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
            .with_allowed_single_arg_tags("acip") // typos:ignore
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
            initial_dot,
            se,
            templates,
        })
    }

    /// Execute this program against a given [Edit].
    pub fn execute<E, W>(&mut self, ed: &mut E, fname: &str, out: &mut W) -> Result<Dot, Error>
    where
        E: Edit,
        W: Write,
    {
        let dot = ed.map_addr(&mut self.initial_dot);
        let se = match self.se.as_ref() {
            Some(se) => se,
            None => return Ok(dot),
        };

        let (char_from, char_to) = dot.as_char_indices();
        let byte_from = ed.char_to_byte(char_from).unwrap();
        let byte_to = ed
            .char_to_byte(char_to.saturating_add(1))
            .unwrap_or_else(|| ed.len_bytes());

        let initial = ed.substr(byte_from, byte_to);

        let mut edit_actions = Vec::new();
        let mut ctx = Ctx {
            fname,
            byte_from: 0,
            ed,
            row_col: RefCell::new(None),
        };

        for caps in se.iter_tagged_captures(initial.as_ref()) {
            let action = caps.action.as_ref().unwrap();
            let id = action.id();
            ctx.byte_from = caps.from();
            ctx.row_col.borrow_mut().take();

            match action.tag() {
                'p' => {
                    // Handle print actions immediately
                    self.templates[&id].render_with_context_to(out, &caps, &ctx)?;
                }

                'd' => edit_actions.push(EditAction::Remove(caps.from(), caps.to())),
                'c' => {
                    // order flipped as we reverse before running
                    edit_actions.push(EditAction::Insert(
                        caps.from(),
                        self.templates[&id].render_with_context(&caps, &ctx)?,
                    ));
                    edit_actions.push(EditAction::Remove(caps.from(), caps.to()));
                }
                'i' => {
                    edit_actions.push(EditAction::Insert(
                        caps.from(),
                        self.templates[&id].render_with_context(&caps, &ctx)?,
                    ));
                }
                'a' => {
                    edit_actions.push(EditAction::Insert(
                        caps.to(),
                        self.templates[&id].render_with_context(&caps, &ctx)?,
                    ));
                }

                _ => unreachable!(),
            }
        }

        edit_actions.reverse();
        ed.begin_edit_transaction();
        for action in edit_actions {
            match action {
                EditAction::Insert(from, s) => {
                    let from = ed.byte_to_char(from + byte_from).unwrap();
                    ed.insert(from, &s);
                }
                EditAction::Remove(from, to) => {
                    let from = ed.byte_to_char(from + byte_from).unwrap();
                    let to = ed.byte_to_char(to + byte_from).unwrap();
                    ed.remove(from, to);
                }
            };
        }
        ed.end_edit_transaction();

        // In the case of running against a lazy stream our initial `to` will be a sential value of
        // usize::MAX which needs to be clamped to the size of the input. For Buffers and GapBuffers
        // where we know that we should already be in bounds this is not required but the overhead
        // of always doing it is minimal as checking the number of chars in the buffer is O(1) due
        // to us caching the value.
        let ix_max = ed.len_chars();
        let (from, to) = dot.as_char_indices();

        Ok(Dot::from_char_indices(min(from, ix_max), min(to, ix_max)))
    }
}

enum EditAction {
    Insert(usize, String),
    Remove(usize, usize),
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

        let row = self.ed.char_to_line(self.byte_from).unwrap();
        let col = self.byte_from - self.ed.line_to_char(row).unwrap();

        *self.row_col.borrow_mut() = Some((row.to_string(), col.to_string()));
    }
}

impl<'a, E> Context for Ctx<'a, E>
where
    E: Edit,
{
    fn render_var<W>(&self, var: &str, w: &mut W) -> io::Result<usize>
    where
        W: Write,
    {
        match var {
            "FILENAME" => {
                w.write_all(self.fname.as_bytes())?;
                Ok(self.fname.len())
            }

            "ROW" => {
                self.ensure_row_col();
                let rc = self.row_col.borrow();
                let row = &rc.as_ref().unwrap().0;
                w.write_all(row.as_bytes())?;
                Ok(row.len())
            }

            "COL" => {
                self.ensure_row_col();
                let rc = self.row_col.borrow();
                let col = &rc.as_ref().unwrap().1;
                w.write_all(col.as_bytes())?;
                Ok(col.len())
            }

            _ => {
                let s = format!("{{{var}}}");
                w.write_all(s.as_bytes())?;
                Ok(s.len())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{buffer::Buffer, editor::Action};
    use simple_test_case::test_case;

    #[test_case(", x/(t.)/ c/{1}X/", "thXis is a teXst XstrXing"; "x c")]
    #[test_case(", x/(t.)/ i/{1}/", "ththis is a tetest t strtring"; "x i")]
    #[test_case(", x/(t.)/ a/{1}/", "ththis is a tetest t strtring"; "x a")]
    #[test]
    fn substitution_of_submatches_works(s: &str, expected: &str) {
        let mut prog = Program::try_parse(s).unwrap();

        let mut b = Buffer::new_unnamed(0, "this is a test string", Default::default());
        prog.execute(&mut b, "test", &mut vec![]).unwrap();
        assert_eq!(&b.txt.to_string(), expected);
    }

    #[test]
    fn loop_between_generates_the_correct_blocks() {
        let mut prog = Program::try_parse(", y/ / p/>{0}<\n/").unwrap();
        let mut b = Buffer::new_unnamed(0, "this and that", Default::default());
        let mut output = Vec::new();
        let dot = prog.execute(&mut b, "test", &mut output).unwrap();

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
}
