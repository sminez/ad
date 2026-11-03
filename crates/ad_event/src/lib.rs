//! A shared event message format between ad and clients
use serde::{Deserialize, Serialize};
use std::io;

/// The maximum number of characters included in a serialized message.
///
/// [FsysEvent::truncated] will be set to true if the source text was larger than this. The byte
/// and character offsets included in the event can be used to fetch the full text from an `ad`
/// control file.
pub const MAX_CHARS: usize = 256;

/// The source of an [FsysEvent].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Source {
    /// The user provided input via the keyboard
    #[serde(rename = "K")]
    Keyboard,
    /// The user provided input via the mouse
    #[serde(rename = "M")]
    Mouse,
    /// The event was received via `ad`'s virtual filesystem
    #[serde(rename = "F")]
    Fsys,
}

/// The editor action that occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Kind {
    /// Text was inserted into the buffer's body.
    #[serde(rename = "I")]
    InsertBody,
    /// Text was deleted from the buffer's body.
    #[serde(rename = "D")]
    DeleteBody,
    /// Text was executed. If there was a [ChordedArgument][Kind::ChordedArgument] then it will
    /// have been sent immediately before this event.
    #[serde(rename = "X")]
    ExecuteBody,
    /// Text was loaded.
    #[serde(rename = "L")]
    LoadBody,
    /// Text was inserted into the scratch buffer while this buffer was focused.
    #[serde(rename = "i")]
    InsertScratch,
    /// Text was deleted from the scratch buffer while this buffer was focused.
    #[serde(rename = "d")]
    DeleteScratch,
    /// Text was executed in the scratch buffer while this buffer was focused. If there was a
    /// [ChordedArgument][Kind::ChordedArgument] then it will have been sent immediately before
    /// this event.
    #[serde(rename = "x")]
    ExecuteScratch,
    /// Text was loaded in the scratch buffer while this buffer was focused.
    #[serde(rename = "l")]
    LoadScratch,
    /// The text of this event was a chorded argument for the `execute` event that follows.
    #[serde(rename = "A")]
    ChordedArgument,
}

/// A buffer level event emitted from `ad` via a buffer's `event` file.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FsysEvent {
    /// The source of the event
    pub source: Source,
    /// The event kind, denoting the action taken within the editor
    pub kind: Kind,
    /// The starting byte offset of the text in this event
    pub byte_from: usize,
    /// The ending byte offset of the text in this event
    pub byte_to: usize,
    /// The starting character offset of the text in this event
    pub ch_from: usize,
    /// The ending character offset of the text in this event
    pub ch_to: usize,
    /// Whether or not `txt` has been truncated to [MAX_CHARS].
    pub truncated: bool,
    /// The text of the event
    pub txt: String,
}

impl FsysEvent {
    /// Construct a new [FsysEvent].
    ///
    /// The `txt` field of events is limited to [MAX_CHARS] or up until the first newline character
    /// and will be truncated if larger. Delete events are always truncated to zero length.
    pub fn new(
        source: Source,
        kind: Kind,
        ch_from: usize,
        byte_from: usize,
        raw_txt: &str,
    ) -> Self {
        let n_bytes = raw_txt.len();
        let n_chars = raw_txt.chars().count();

        let (txt, truncated) = match kind {
            Kind::DeleteScratch | Kind::DeleteBody => (String::new(), true),
            _ if n_chars <= MAX_CHARS => (raw_txt.to_string(), false),
            _ => (raw_txt.chars().take(MAX_CHARS).collect(), true),
        };

        Self {
            source,
            kind,
            byte_from,
            byte_to: byte_from + n_bytes,
            ch_from,
            ch_to: ch_from + n_chars,
            truncated,
            txt,
        }
    }

    pub fn as_event_file_line(&self) -> String {
        format!("{}\n", serde_json::to_string(self).unwrap())
    }

    pub fn try_from_str(s: &str) -> io::Result<Self> {
        let evt: Self = serde_json::from_str(s.trim())
            .map_err(|e| invalid_data(format!("invalid event: {e}")))?;
        if evt.txt.chars().count() > MAX_CHARS {
            return Err(invalid_data(format!(
                "txt field too long: max chars = {MAX_CHARS}"
            )));
        }

        Ok(evt)
    }
}

/// A single entry in `ad`'s `buffers/index` file.
///
/// Each line of the index file is a buffer id and the full filename of the buffer, separated by
/// a tab character.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexEntry {
    /// The id of the buffer.
    pub id: usize,
    /// The full filename of the buffer.
    pub filename: String,
}

impl IndexEntry {
    pub fn as_index_file_line(&self) -> String {
        format!("{}\t{}\n", self.id, self.filename)
    }

    pub fn try_from_str(s: &str) -> io::Result<Self> {
        let s = s.strip_suffix('\n').unwrap_or(s);
        let (id, filename) = s
            .split_once('\t')
            .ok_or_else(|| invalid_data(format!("invalid index line: {s:?}")))?;

        Ok(Self {
            id: parse_id(id)?,
            filename: filename.to_string(),
        })
    }

    /// Parse the full contents of an index file.
    pub fn parse_index_file(s: &str) -> io::Result<Vec<Self>> {
        s.lines().map(Self::try_from_str).collect()
    }
}

/// A single event from `ad`'s `log` file.
///
/// Each line of the log file is a buffer id and the action that occurred, separated by a space.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogEvent {
    /// A newly created buffer
    Open(usize),
    /// A buffer that has now been closed
    Close(usize),
    /// A change to the currently active buffer
    Focus(usize),
    /// A buffer was saved
    Save(usize),
}

impl LogEvent {
    pub fn as_log_file_line(&self) -> String {
        match self {
            Self::Open(id) => format!("{id} open\n"),
            Self::Close(id) => format!("{id} close\n"),
            Self::Focus(id) => format!("{id} focus\n"),
            Self::Save(id) => format!("{id} save\n"),
        }
    }

    pub fn try_from_str(s: &str) -> io::Result<Self> {
        let s = s.strip_suffix('\n').unwrap_or(s);
        let (id, action) = s
            .split_once(' ')
            .ok_or_else(|| invalid_data(format!("malformed log line: {s:?}")))?;
        let id = parse_id(id)?;

        match action {
            "open" => Ok(Self::Open(id)),
            "close" => Ok(Self::Close(id)),
            "focus" => Ok(Self::Focus(id)),
            "save" => Ok(Self::Save(id)),
            _ => Err(invalid_data(format!("unknown log action {action:?}"))),
        }
    }
}

/// The result of asking the user to provide input via the minibuffer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MiniBufferSelection {
    /// The user selected one of the provided input lines
    Line {
        /// The index of the selected line within the input
        cy: usize,
        /// The content of the selected line
        line: String,
    },
    /// The user provided a custom response that was not in the input
    UserInput {
        /// The content provided by the user
        input: String,
    },
    /// The user dismissed the minibuffer without providing input
    Cancelled,
}

impl MiniBufferSelection {
    /// Discard any information around which line was selected by the user and only
    /// return the content of the selected line itself.
    pub fn into_content(self) -> Option<String> {
        match self {
            Self::Line { line, .. } => Some(line),
            Self::UserInput { input } => Some(input),
            Self::Cancelled => None,
        }
    }

    /// Interpret the content of `ad`'s `minibuffer` file after offering `lines` to the user.
    pub fn from_minibuffer_file_content(lines: &[String], mut s: String) -> Self {
        if s.ends_with('\n') {
            s.pop();
        }

        if s.is_empty() {
            return Self::Cancelled;
        }

        match lines.iter().position(|l| l == &s) {
            Some(cy) => Self::Line { cy, line: s },
            None => Self::UserInput { input: s },
        }
    }
}

fn invalid_data(msg: String) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, msg)
}

fn parse_id(s: &str) -> io::Result<usize> {
    s.parse()
        .map_err(|_| invalid_data(format!("expected integer ID, got {s:?}")))
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case("", MiniBufferSelection::Cancelled; "empty is cancelled")]
    #[test_case("\n", MiniBufferSelection::Cancelled; "newline is cancelled")]
    #[test_case("b\n", MiniBufferSelection::Line { cy: 1, line: "b".to_string() }; "line")]
    #[test_case("x", MiniBufferSelection::UserInput { input: "x".to_string() }; "user input")]
    #[test_case("b\n\n", MiniBufferSelection::UserInput { input: "b\n".to_string() }; "only one newline stripped")]
    #[test]
    fn minibuffer_selection_from_file_content_works(s: &str, expected: MiniBufferSelection) {
        let lines = vec!["a".to_string(), "b".to_string()];
        let sel = MiniBufferSelection::from_minibuffer_file_content(&lines, s.to_string());

        assert_eq!(sel, expected);
    }

    #[test_case(MiniBufferSelection::Line { cy: 1, line: "b".to_string() }; "line")]
    #[test_case(MiniBufferSelection::UserInput { input: "x".to_string() }; "user input")]
    #[test_case(MiniBufferSelection::Cancelled; "cancelled")]
    #[test]
    fn minibuffer_selection_round_trip_works(sel: MiniBufferSelection) {
        let lines = vec!["a".to_string(), "b".to_string()];
        let s = sel.clone().into_content().unwrap_or_default();
        let parsed = MiniBufferSelection::from_minibuffer_file_content(&lines, s);

        assert_eq!(parsed, sel);
    }

    #[test_case(LogEvent::Open(1); "open")]
    #[test_case(LogEvent::Close(2); "close")]
    #[test_case(LogEvent::Focus(3); "focus")]
    #[test_case(LogEvent::Save(4); "save")]
    #[test]
    fn log_event_round_trip_works(evt: LogEvent) {
        let line = evt.as_log_file_line();
        let parsed = LogEvent::try_from_str(&line).expect("to parse");

        assert_eq!(parsed, evt);
    }

    #[test_case("1open"; "missing separator")]
    #[test_case("x open"; "non-integer id")]
    #[test_case("1 explode"; "unknown action")]
    #[test_case("1 open\n2 close\n"; "multiple lines")]
    #[test]
    fn invalid_log_lines_are_rejected(s: &str) {
        let err = LogEvent::try_from_str(s).expect_err("to be rejected");
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
    }

    #[test_case("foo.rs"; "simple")]
    #[test_case("/home/user/my file.rs"; "with spaces")]
    #[test_case("/tmp/a\tb"; "with tab")]
    #[test_case(""; "empty")]
    #[test]
    fn index_entry_round_trip_works(filename: &str) {
        let entry = IndexEntry {
            id: 42,
            filename: filename.to_string(),
        };
        let line = entry.as_index_file_line();
        let parsed = IndexEntry::try_from_str(&line).expect("to parse");

        assert_eq!(parsed, entry);
    }

    #[test]
    fn parse_index_file_works() {
        let s = "1\tfoo.rs\n2\t/my dir/bar.rs\n";
        let entries = IndexEntry::parse_index_file(s).unwrap();

        assert_eq!(
            entries,
            vec![
                IndexEntry {
                    id: 1,
                    filename: "foo.rs".to_string()
                },
                IndexEntry {
                    id: 2,
                    filename: "/my dir/bar.rs".to_string()
                },
            ]
        );
    }

    fn evt(s: &str) -> FsysEvent {
        FsysEvent::new(Source::Keyboard, Kind::InsertBody, 17, 17, s)
    }

    #[test]
    fn as_event_file_line_works() {
        let line = evt("a").as_event_file_line();
        assert_eq!(
            line,
            "{\"source\":\"K\",\"kind\":\"I\",\"byte_from\":17,\"byte_to\":18,\"ch_from\":17,\"ch_to\":18,\"truncated\":false,\"txt\":\"a\"}\n"
        );
    }

    #[test]
    fn as_event_file_line_works_for_newline() {
        let line = evt("\n").as_event_file_line();
        assert_eq!(
            line,
            "{\"source\":\"K\",\"kind\":\"I\",\"byte_from\":17,\"byte_to\":18,\"ch_from\":17,\"ch_to\":18,\"truncated\":false,\"txt\":\"\\n\"}\n"
        );
    }

    #[test]
    fn txt_length_is_truncated_in_new() {
        let long_txt = "a".repeat(MAX_CHARS + 10);
        let e = FsysEvent::new(Source::Keyboard, Kind::InsertBody, 17, 17, &long_txt);
        assert!(e.truncated);
    }

    #[test_case(Kind::DeleteBody; "delete in body")]
    #[test_case(Kind::DeleteScratch; "delete in tag")]
    #[test]
    fn txt_is_removed_for_delete_events_if_provided(kind: Kind) {
        let e = FsysEvent::new(Source::Keyboard, kind, 42, 42, "some deleted text");
        assert!(e.truncated);
        assert!(e.txt.is_empty());
    }

    #[test]
    fn txt_length_is_checked_on_parse() {
        let long_txt = "a".repeat(MAX_CHARS + 10);
        let line = format!("K I 17 283 266 | {long_txt}");
        let res = FsysEvent::try_from_str(&line);
        assert!(res.is_err(), "expected error, got {res:?}");
    }

    #[test_case("a"; "single char")]
    #[test_case("testing"; "multi char")]
    #[test_case("testing testing 1 2 3"; "multi char with spaces")]
    #[test_case("Hello, 世界"; "multi char with spaces and multi byte chars")]
    #[test_case("testing testing\n1 2 3"; "multi char with spaces and internal newline")]
    #[test_case("testing testing 1 2 3\n"; "multi char with spaces and trailing newline")]
    #[test]
    fn round_trip_single_works(s: &str) {
        let e = evt(s);
        let line = e.as_event_file_line();
        let parsed = FsysEvent::try_from_str(&line).expect("to parse");

        assert_eq!(parsed, e);
    }
}
