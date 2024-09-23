//! A shared event message format between ad and clients
type Result<T> = std::result::Result<T, String>;

const EOF: &str = "unexpected EOF";
const MAX_CHARS: usize = 256;

macro_rules! impl_charconv {
    ($name:ident, $($variant:ident <=> $ch:expr,)+) => {
        impl $name {
            fn to_char(self) -> char {
                match self {
                    $($name::$variant => $ch,)+
                }
            }

            fn try_from_iter<I>(it: &mut I) -> Result<Self>
            where
                I: Iterator<Item = char>,
            {
                let ch = it.next().ok_or_else(|| EOF.to_string())?;
                match ch {
                    $($ch => Ok($name::$variant),)+
                    ch => Err(format!("unknown {} variant: {}", stringify!($name), ch)),
                }
            }

        }
    }
}

/// acme makes a distinction between direct writes to /body and /tag vs
/// text entering the buffer via one of the other fsys files but I'm not
/// sure if I need that initially? As and when it looks useful I can add it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    Keyboard,
    Mouse,
    Fsys,
}

impl_charconv! {
    Source,
    Keyboard <=> 'K',
    Mouse <=> 'M',
    Fsys <=> 'F',
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    InsertBody,
    DeleteBody,
    ExecuteBody,
    LoadBody,
    InsertTag,
    DeleteTag,
    ExecuteTag,
    LoadTag,
}

impl_charconv! {
    Kind,
    InsertBody <=> 'I',
    DeleteBody <=> 'D',
    ExecuteBody <=> 'X',
    LoadBody <=> 'L',
    InsertTag <=> 'i',
    DeleteTag <=> 'd',
    ExecuteTag <=> 'x',
    LoadTag <=> 'l',
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FsysEvent {
    pub source: Source,
    pub kind: Kind,
    pub ch_from: usize,
    pub ch_to: usize,
    pub n_chars: usize,
    pub txt: String,
}

impl FsysEvent {
    /// Construct a new [FsysEvent].
    ///
    /// The `txt` field of events is limited to [MAX_CHARS] and will be truncated if larger. Delete
    /// events are always truncated to zero length.
    pub fn new(source: Source, kind: Kind, ch_from: usize, ch_to: usize, txt: &str) -> Self {
        let txt = match kind {
            Kind::DeleteTag | Kind::DeleteBody => String::new(),
            _ => txt.chars().take(MAX_CHARS).collect(),
        };
        let n_chars = txt.chars().count();

        Self {
            source,
            kind,
            ch_from,
            ch_to,
            n_chars,
            txt,
        }
    }

    pub fn as_event_file_line(&self) -> String {
        let (txt, n_chars) = match self.kind {
            Kind::DeleteTag | Kind::DeleteBody => ("", 0),
            _ => (self.txt.as_str(), self.n_chars),
        };

        format!(
            "{} {} {} {} {} | {}\n",
            self.source.to_char(),
            self.kind.to_char(),
            self.ch_from,
            self.ch_to,
            n_chars,
            txt
        )
    }

    pub fn try_from_str(s: &str) -> Result<Vec<Self>> {
        let mut it = s.chars().peekable();
        let mut events = Vec::new();
        loop {
            if it.peek().is_none() {
                return Ok(events);
            }
            let evt = Self::try_single_from_iter(&mut it)?;
            events.push(evt);
        }
    }

    pub fn try_single_from_iter<I>(it: &mut I) -> Result<Self>
    where
        I: Iterator<Item = char>,
    {
        let source = Source::try_from_iter(it)?;
        it.next(); // consume the space
        let kind = Kind::try_from_iter(it)?;
        it.next(); // consume the space
        let ch_from = read_usize(it)?;
        let ch_to = read_usize(it)?;
        let n_chars = read_usize(it)?;

        if n_chars > MAX_CHARS {
            return Err(format!("txt field too long: max chars = {MAX_CHARS}"));
        }

        // skip the '| ' delimiter before the text body starts
        let txt: Vec<char> = it.skip(2).take(n_chars).collect();
        it.next(); // consume the trailing newline

        if txt.len() != n_chars {
            Err(format!(
                "expected {n_chars} chars in txt but got {}",
                txt.len()
            ))
        } else {
            Ok(Self {
                source,
                kind,
                ch_from,
                ch_to,
                n_chars,
                txt: txt.into_iter().collect(),
            })
        }
    }
}

fn read_usize<I>(it: &mut I) -> Result<usize>
where
    I: Iterator<Item = char>,
{
    let mut buf = String::new();
    for ch in it {
        if ch == ' ' {
            break;
        }
        buf.push(ch);
    }

    buf.parse::<usize>().map_err(|e| e.to_string())
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
        assert_eq!(line, "K I 17 18 1 | a\n");
    }

    #[test]
    fn txt_length_is_truncated_in_new() {
        let long_txt = "a".repeat(MAX_CHARS + 10);
        let e = FsysEvent::new(Source::Keyboard, Kind::InsertBody, 17, 283, &long_txt);
        assert_eq!(e.n_chars, MAX_CHARS);
    }

    #[test_case(Kind::DeleteBody; "delete in body")]
    #[test_case(Kind::DeleteTag; "delete in tag")]
    #[test]
    fn txt_is_removed_for_delete_events_if_provided(kind: Kind) {
        let e = FsysEvent::new(Source::Keyboard, kind, 42, 42 + 17, "some deleted text");
        assert_eq!(e.n_chars, 0);
    }

    #[test]
    fn txt_length_is_checked_on_parse() {
        let long_txt = "a".repeat(MAX_CHARS + 10);
        let line = format!("K I 17 283 266 | {long_txt}");
        let res = FsysEvent::try_single_from_iter(&mut line.chars());
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
        let parsed = FsysEvent::try_single_from_iter(&mut line.chars()).expect("to parse");

        assert_eq!(parsed, e);
    }

    #[test]
    fn round_trip_multi_works() {
        let events = vec![
            evt("a"),
            evt("testing"),
            evt("testing testing 1 2 3"),
            evt("Hello, 世界"),
            evt("testing testing\n1 2 3"),
            evt("testing testing 1 2 3\n"),
        ];
        let s: String = events.iter().map(|e| e.as_event_file_line()).collect();
        let parsed = FsysEvent::try_from_str(&s).expect("to parse");

        assert_eq!(parsed, events);
    }
}
