//! A shared event message format between ad and clients
use serde::{Deserialize, Serialize};

type Result<T> = std::result::Result<T, String>;

pub const MAX_CHARS: usize = 256;

/// acme makes a distinction between direct writes to /body and /tag vs
/// text entering the buffer via one of the other fsys files but I'm not
/// sure if I need that initially? As and when it looks useful I can add it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Source {
    #[serde(rename = "K")]
    Keyboard,
    #[serde(rename = "M")]
    Mouse,
    #[serde(rename = "F")]
    Fsys,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Kind {
    #[serde(rename = "I")]
    InsertBody,
    #[serde(rename = "D")]
    DeleteBody,
    #[serde(rename = "X")]
    ExecuteBody,
    #[serde(rename = "L")]
    LoadBody,
    #[serde(rename = "i")]
    InsertScratch,
    #[serde(rename = "d")]
    DeleteScratch,
    #[serde(rename = "x")]
    ExecuteScratch,
    #[serde(rename = "l")]
    LoadScratch,
    #[serde(rename = "A")]
    ChordedArgument,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FsysEvent {
    pub source: Source,
    pub kind: Kind,
    pub ch_from: usize,
    pub ch_to: usize,
    pub truncated: bool,
    pub txt: String,
}

impl FsysEvent {
    /// Construct a new [FsysEvent].
    ///
    /// The `txt` field of events is limited to [MAX_CHARS] or up until the first newline character
    /// and will be truncated if larger. Delete events are always truncated to zero length.
    pub fn new(source: Source, kind: Kind, ch_from: usize, ch_to: usize, raw_txt: &str) -> Self {
        let (txt, truncated) = match kind {
            Kind::DeleteScratch | Kind::DeleteBody => (String::new(), true),
            _ => {
                let txt = raw_txt.chars().take(MAX_CHARS).collect();
                let truncated = txt != raw_txt;

                (txt, truncated)
            }
        };

        Self {
            source,
            kind,
            ch_from,
            ch_to,
            truncated,
            txt,
        }
    }

    pub fn as_event_file_line(&self) -> String {
        format!("{}\n", serde_json::to_string(self).unwrap())
    }

    pub fn try_from_str(s: &str) -> Result<Self> {
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
        let n_chars = s.chars().count();
        FsysEvent::new(Source::Keyboard, Kind::InsertBody, 17, 17 + n_chars, s)
    }

    #[test]
    fn as_event_file_line_works() {
        let line = evt("a").as_event_file_line();
        assert_eq!(
            line,
            "{\"source\":\"K\",\"kind\":\"I\",\"ch_from\":17,\"ch_to\":18,\"truncated\":false,\"txt\":\"a\"}\n"
        );
    }

    #[test]
    fn as_event_file_line_works_for_newline() {
        let line = evt("\n").as_event_file_line();
        assert_eq!(
            line,
            "{\"source\":\"K\",\"kind\":\"I\",\"ch_from\":17,\"ch_to\":18,\"truncated\":false,\"txt\":\"\\n\"}\n"
        );
    }

    #[test]
    fn txt_length_is_truncated_in_new() {
        let long_txt = "a".repeat(MAX_CHARS + 10);
        let e = FsysEvent::new(Source::Keyboard, Kind::InsertBody, 17, 283, &long_txt);
        assert!(e.truncated);
    }

    #[test_case(Kind::DeleteBody; "delete in body")]
    #[test_case(Kind::DeleteScratch; "delete in tag")]
    #[test]
    fn txt_is_removed_for_delete_events_if_provided(kind: Kind) {
        let e = FsysEvent::new(Source::Keyboard, kind, 42, 42 + 17, "some deleted text");
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
