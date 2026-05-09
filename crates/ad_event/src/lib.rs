//! A shared event message format between ad and clients
use serde::{Deserialize, Serialize};

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

    pub fn try_from_str(s: &str) -> Result<Self, String> {
        let evt: Self =
            serde_json::from_str(s.trim()).map_err(|e| format!("invalid event: {e}"))?;
        if evt.txt.chars().count() > MAX_CHARS {
            return Err(format!("txt field too long: max chars = {MAX_CHARS}"));
        }

        Ok(evt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

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
