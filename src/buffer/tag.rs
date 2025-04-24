//! The 'tag' for a given UI window is a buffer with additional semantics such as automatic line
//! wrapping and enforcement over the structure of its content.
//!
//! See ../ui/layout.rs:/struct.Window/
use crate::{
    buffer::{ActionOutcome, Buffer, GapBuffer},
    dot::{find::find_forward, Cur, Dot, Range},
    editor::Action,
    fsys::InputFilter,
    ts::LineIter,
};
use ad_event::Source;
use std::{iter::repeat_n, mem::swap};
use unicode_width::UnicodeWidthChar;

const SPACE_PIPE: &str = " |";
const TAB_PIPE: &str = "\t|";
const PUT: &str = " Put";
const GET: &str = " Get";

/// A tag is a [Buffer] attached to given [Window] that contains specific semantics around how it is
/// rendered and edited in order to enforce that the window's name is always present
///
/// Tag content is always of the form "<filename> | <user defined content>"
///                                              ... <- uneditable
#[derive(Debug)]
pub(crate) struct Tag {
    /// The full editable tag content including filename and | separator
    pub(crate) b: Buffer,
    /// Pre-computed, wrapped UI lines
    pub(crate) gb: GapBuffer,
    /// Number of UI tag lines
    pub(crate) n_lines: usize,
}

impl Tag {
    /// Construct a new [Tag] associated with a given buffer.
    pub(super) fn new(parent: &Buffer, id: usize, tabstop: usize, n_cols: usize) -> Self {
        let content = format!("{} | ", parent.full_name());
        let gb = GapBuffer::from(content.as_str());
        let mut tag = Self {
            b: Buffer::new_unnamed(id, content),
            gb,
            n_lines: 0,
        };
        tag.set_parent(parent, tabstop, n_cols);

        tag
    }

    /// Update the current state of the tag based on what the current [Buffer] is.
    ///
    /// Handling of invariants in the tag is done through reparsing the raw tag buffer content
    /// and attempting to preserve any user edited portion of the tag rather than attempting to
    /// modify edits being made to the tag in order to ensure that the invariants are upheld.
    /// This is a similar approach to the one used in Acme.
    pub(crate) fn set_parent(&mut self, b: &Buffer, tabstop: usize, n_cols: usize) {
        // parse the current tag content in order to see if we need to change anything
        let (eofname, mut sotag) = parse_tag(&self.b);
        let old_fname = self.b.txt.slice(0, eofname);

        // If the filename has changed then we need to copy over the one from the buffer
        // -> This is based on the logic in winsettag1
        let fname = b.full_name();
        if old_fname == fname {
            if sotag.is_some() {
                sotag = Some(self.b.txt.len_chars() - eofname + fname.chars().count());
            }
            self.b.xdot = Dot::from_char_indices(0, eofname);
            self.b.insert_xdot(fname.to_string()); // using xdot handles updating dot for us
            self.b.input_filter = b.input_filter.as_ref().map(|f| f.paired_tag_filter());
        }

        self.update_content(fname, sotag, b, tabstop, n_cols);
    }

    /// Update the filname of the parent buffer if the state within the Tag has modified it and then recompute the
    /// the full tag content.
    fn update_parent(&mut self, b: &mut Buffer, tabstop: usize, n_cols: usize) {
        let (eofname, sotag) = parse_tag(&self.b);
        let fname = self.b.txt.slice(0, eofname + 1).to_string();
        if fname != b.full_name() {
            b.set_filename(&fname);
        }

        self.update_content(&fname, sotag, b, tabstop, n_cols);
    }

    fn update_content(
        &mut self,
        fname: &str,
        sotag: Option<usize>,
        b: &Buffer,
        tabstop: usize,
        n_cols: usize,
    ) {
        // Determine the new tag content and then replace the existing tag if they differ
        let mut new_tag = fname.to_string();
        new_tag.reserve(self.b.txt.len().saturating_sub(fname.len())); // avoid repeated realloc

        if b.dirty && !b.kind.is_dir() {
            new_tag.push_str(PUT);
        }
        if b.kind.is_dir() {
            new_tag.push_str(GET);
        }

        // End of the non-user editable portion of the tag
        new_tag.push_str(SPACE_PIPE);
        if let Some(i) = sotag {
            new_tag.extend(self.b.txt.slice(i, self.b.len_chars()).chars());
        }

        if self.b.txt != new_tag.as_str() {
            self.b.txt.clear();
            self.b.append(new_tag, Source::Fsys);
        }

        self.compute_ui_lines(tabstop, n_cols);
    }

    pub(crate) fn line_iter(&self, load_exec_range: Option<(bool, Range)>) -> LineIter<'_> {
        LineIter::new(
            0,
            &self.gb,
            self.b.dot.as_range(),
            load_exec_range,
            &[],
            &[],
        )
    }

    pub(crate) fn handle_action(
        &mut self,
        b: &mut Buffer,
        a: Action,
        source: Source,
        tabstop: usize,
        n_cols: usize,
    ) -> Option<ActionOutcome> {
        let outcome = self.b.handle_action(a, source);
        self.update_parent(b, tabstop, n_cols);

        outcome
    }

    #[inline]
    pub(crate) fn set_input_filter(&mut self, filter: &InputFilter) {
        self.b.input_filter = Some(filter.paired_tag_filter());
    }

    #[inline]
    pub(crate) fn clear_input_filter(&mut self) {
        self.b.input_filter = None;
    }

    pub(crate) fn compute_ui_lines(&mut self, tabstop: usize, n_cols: usize) {
        if n_cols == 0 {
            return;
        }

        let mut lines = Vec::new();
        let mut buf = String::new();
        let mut cols = 0;

        for ch in self.b.txt.chars() {
            if ch == '\n' {
                buf.extend(repeat_n(' ', n_cols - buf.len()));
                let mut s = String::new();
                swap(&mut buf, &mut s);
                lines.push(s);
                cols = 0;
                continue;
            }

            let w = if ch == '\t' {
                tabstop
            } else {
                UnicodeWidthChar::width(ch).unwrap_or(1)
            };

            if cols + w <= n_cols {
                if ch == '\t' {
                    buf.extend(repeat_n(' ', tabstop));
                } else {
                    buf.push(ch);
                }
                cols += w;
            } else {
                let mut s = String::new();
                swap(&mut buf, &mut s);
                lines.push(s);
                buf.push(ch);
                cols = w;
                continue;
            }
        }

        if !buf.is_empty() {
            debug_assert!(cols <= n_cols, "{cols} vs {n_cols}");
            buf.extend(repeat_n(' ', n_cols - cols));
            lines.push(buf);
        }

        self.gb.clear();
        self.gb.insert_str(0, &lines.join("\n"));
        self.n_lines = lines.len();
    }
}

/// Parse the current tag content to determine:
///   - the end of the filname portion of the tag
///   - the start of the user defined tag
fn parse_tag(b: &Buffer) -> (usize, Option<usize>) {
    // filename portion of the tag ends on the first space or tab encountered
    let eofname = match find_forward(&|c| c == ' ' || c == '\t', Cur::new(0), b) {
        Some(dot) => dot.first_cur().idx,
        None => return (b.len_chars(), None), // entire tag is a filename
    };

    // the tag is split by either " |" or "\t|"
    let sotag = find_forward(&SPACE_PIPE, Cur::new(0), b)
        .or_else(|| find_forward(&TAB_PIPE, Cur::new(0), b))
        .map(|dot: Dot| dot.last_cur().idx + 1); // starts the char after |

    (eofname.saturating_sub(1), sotag)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::{BufferKind, Buffers};
    use simple_test_case::test_case;
    use std::sync::mpsc::channel;

    #[test]
    fn initial_tag_state_is_correct() {
        let (tx, _rx) = channel();
        let mut buffers = Buffers::new_stubbed(&[0], tx);
        let parent_id = buffers.open_virtual("/home/foo/bar.txt".to_string(), String::new());
        let tag = buffers.new_tag_buffer(parent_id, 4, 10);

        assert_eq!(tag.b.txt.to_string(), "/home/foo/bar.txt | ");
    }

    #[test_case(
        "this is a test",
        &[
            "/home/foo/",
            "bar.txt | ",
            "this is a ",
            "test      "
        ];
        "simple tag"
    )]
    #[test_case(
        "this is\na test",
        &[
            "/home/foo/",
            "bar.txt | ",
            "this is   ",
            "a test    "
        ];
        "tag with explicit newline"
    )]
    #[test_case(
        "this 🦊 is a test",
        &[
            "/home/foo/",
            "bar.txt | ",
            "this 🦊 is",
            " a test   "
        ];
        "tag with multibyte character"
    )]
    #[test]
    fn compute_ui_lines_append_content(s: &str, expected: &[&str]) {
        let (tx, _rx) = channel();
        let mut buffers = Buffers::new_stubbed(&[0], tx);
        let parent_id = buffers.open_virtual("/home/foo/bar.txt".to_string(), String::new());
        let mut tag = buffers.new_tag_buffer(parent_id, 4, 10);

        tag.b.append(s.to_string(), Source::Fsys);
        tag.compute_ui_lines(4, 10);

        let string_lines: Vec<String> = tag.gb.iter_lines().map(|l| l.to_string()).collect();
        let str_lines: Vec<&str> = string_lines.iter().map(|s| s.as_str()).collect();

        assert_eq!(&str_lines, expected);
    }

    #[test_case("/home/foo/bar.txt | ", "/home/foo/bar.txt", Some(" "); "default tag")]
    #[test_case("+errors | ", "+errors", Some(" "); "default tag non-filepath")]
    #[test_case("/home/foo/bar.txt | foo bar", "/home/foo/bar.txt", Some(" foo bar"); "with tag")]
    #[test_case("+errors | foo bar", "+errors", Some(" foo bar"); "non-filepath with tag")]
    #[test_case("/foo\t|", "/foo", Some(""); "tab pipe no trailing space")]
    #[test_case("/foo", "/foo", None; "no pipe")]
    #[test_case("/foo | this 🦊 is a test", "/foo", Some(" this 🦊 is a test"); "unicode tag")]
    // This one is weird...do we want to do the acme thing and discard input in this instance
    // or do we want to inject / move the pipe?
    #[test_case("/foo bar baz | quux", "/foo", Some(" quux"); "text between fname and pipe")]
    #[test]
    fn parse_tag(s: &str, expected_fname: &str, expected_tag: Option<&str>) {
        let b = Buffer::new_virtual(usize::MAX, "test", s);
        let (eofname, sotag) = parse_tag(&b);
        let fname = b.txt.slice(0, eofname + 1);
        let tag = sotag.map(|i| b.txt.slice(i, b.txt.len_chars()).to_string());

        assert_eq!(&fname.to_string(), expected_fname);
        assert_eq!(tag.as_deref(), expected_tag);
    }

    #[test_case("", false, false, ""; "no user tag")]
    #[test_case("foo", false, false, ""; "simple user tag")]
    #[test_case("", true, false, " Get"; "no user tag dir")]
    #[test_case("foo", true, false, " Get"; "simple user tag dir")]
    #[test_case("", false, true, " Put"; "no user tag while dirty")]
    #[test_case("foo", false, true, " Put"; "simple user tag while dirty")]
    #[test_case("", true, false, " Get"; "no user tag dir while")]
    #[test_case("foo", true, false, " Get"; "simple user tag dir while")]
    #[test_case("", true, true, " Get"; "no user tag dir while dirty")]
    #[test_case("foo", true, true, " Get"; "simple user tag dir while dirty")]
    #[test]
    fn setting_a_new_buffer_works(current: &str, is_dir: bool, dirty: bool, pre: &str) {
        let (tx, _rx) = channel();
        let mut buffers = Buffers::new_stubbed(&[0], tx);
        let parent_id = buffers.open_virtual("/home/foo/bar.txt".to_string(), String::new());
        let mut tag = buffers.new_tag_buffer(parent_id, 4, 10);

        // set the initial user tag
        tag.b.append(current.to_string(), Source::Fsys);
        tag.compute_ui_lines(4, 10);

        let s_tag = tag.b.txt.to_string();
        assert_eq!(s_tag, format!("/home/foo/bar.txt | {current}"));

        let new_parent_id = buffers.open_virtual("/home/bar/baz.json".to_string(), String::new());
        let b = buffers.with_id_mut(new_parent_id).unwrap();
        if is_dir {
            // Not an actual directory but used for testing
            b.kind = BufferKind::Directory("/home/bar/baz.json".into());
        }
        b.dirty = dirty;
        tag.set_parent(b, 4, 10);

        let s_tag = tag.b.txt.to_string();
        assert_eq!(s_tag, format!("/home/bar/baz.json{pre} | {current}"));
    }

    #[test]
    fn update_parent_modifies_parent_filename() {
        let (tx, _rx) = channel();
        let mut buffers = Buffers::new_stubbed(&[0], tx);
        let parent_id = buffers.open_virtual("/home/foo/bar.txt".to_string(), String::new());
        let mut tag = buffers.new_tag_buffer(parent_id, 4, 10);
        let b = buffers.with_id_mut(parent_id).unwrap();

        // set an entirely new tag
        tag.b.txt = "/home/bar/baz".into();
        tag.update_parent(b, 4, 10);

        assert_eq!(b.full_name(), "/home/bar/baz");
    }
}
