//! A simple 9p based client for interacting with ad
#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::style,
    future_incompatible,
    missing_debug_implementations,
    missing_docs,
    rust_2018_idioms,
    rustdoc::all,
    clippy::undocumented_unsafe_blocks
)]
use ad_event::{FsysEvent, Kind};
use ninep::{sansio::server::socket_dir, sync::client::Client};
use std::{fs, io, str::FromStr};

pub mod sync;
#[cfg(feature = "tokio")]
pub mod tokio;

#[cfg(test)]
pub(crate) mod test_util;

pub use ad_event::Source;
pub use ninep::sansio::client::{Error, Result};

/// The result of asking the user to provide input via the minibuffer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MiniBufferSelection {
    /// The user selected one of the provided input lines
    Line {
        /// The index of the selected line within the input
        index: usize,
        /// The content of the selected line
        content: String,
    },
    /// The user provided a custom response that was not in the input
    UserInput {
        /// The content provided by the user
        content: String,
    },
    /// The user dismissed the minibuffer without providing input
    Cancelled,
}

/// Event data received from an ad buffer's `events` file.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EventData<'a> {
    /// The source of the event
    pub source: Source,
    /// The starting character offset within the buffer of `txt`
    pub ch_from: usize,
    /// The ending character offset within the buffer of `txt`
    pub ch_to: usize,
    /// The text content for the event (truncated if `ch_from`...`ch_to` exceeds 256 characters)
    pub txt: &'a str,
    /// Whether or not `txt` is truncated.
    pub truncated: bool,
    /// Whether or not this event originated from the scratch buffer
    pub from_scratch: bool,
}

impl<'a> From<&'a FsysEvent> for EventData<'a> {
    fn from(evt: &'a FsysEvent) -> Self {
        Self {
            source: evt.source,
            ch_from: evt.ch_from,
            ch_to: evt.ch_to,
            txt: &evt.txt,
            truncated: evt.truncated,
            from_scratch: matches!(
                evt.kind,
                Kind::LoadScratch
                    | Kind::ExecuteScratch
                    | Kind::InsertScratch
                    | Kind::DeleteScratch
            ),
        }
    }
}

impl<'a> EventData<'a> {
    /// Attempt to read the full text of this event from the underlying buffer
    pub fn try_full_text(&self, client: &sync::BufferClient) -> Result<String> {
        if self.txt.len() < ad_event::MAX_CHARS {
            return Ok(self.txt.to_string());
        }

        client.write_xaddr(&format!("#{},#{}", self.ch_from, self.ch_to))?;

        client.read_xdot()
    }

    #[cfg(feature = "tokio")]
    /// Attempt to read the full text of this event from the underlying buffer
    pub async fn try_full_text_async(&self, client: &tokio::BufferClient) -> Result<String> {
        if self.txt.len() < ad_event::MAX_CHARS {
            return Ok(self.txt.to_string());
        }

        client
            .write_xaddr(&format!("#{},#{}", self.ch_from, self.ch_to))
            .await?;

        client.read_xdot().await
    }
}

/// Outcome of handling an event within an event filter
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventOutcome {
    /// The event should be passed back to ad
    Passthrough,
    /// The event should not be passed back to ad
    Handled,
    /// The event should be passed back to ad and then the filter should exit
    PassthroughAndExit,
    /// The event should not be passed back to ad and the filter should exit
    Exit,
}

pub(crate) fn parse_bufid(str_id: &str) -> io::Result<usize> {
    str_id.parse().map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("expected integer ID, got {str_id:?}"),
        )
    })
}

/// A message sent by the main editor thread to notify the fs thread that
/// the current buffer list has changed.
#[derive(Debug, Clone, Copy)]
pub enum LogEvent {
    /// A newly created buffer
    Open(usize),
    /// A buffer that has now been closed and needs removing from state
    Close(usize),
    /// A change to the currently active buffer
    Focus(usize),
    /// A buffer was saved
    Save(usize),
}

impl FromStr for LogEvent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let s = s.trim();
        if s.contains('\n') {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "expected single line").into());
        }

        let (str_id, action) = s.split_once(' ').ok_or(io::Error::new(
            io::ErrorKind::InvalidData,
            "malformed log line: {s:?}",
        ))?;

        let id = parse_bufid(str_id)?;
        let evt = match action {
            "open" => Self::Open(id),
            "close" => Self::Close(id),
            "focus" => Self::Focus(id),
            "save" => Self::Save(id),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unknown log action {action:?}",
                )
                .into());
            }
        };

        Ok(evt)
    }
}

fn open_9p_sockets() -> io::Result<Vec<String>> {
    let mut ad_sockets = Vec::new();
    for entry in fs::read_dir(socket_dir())? {
        let entry = entry?;
        let fname = entry.file_name();
        if let Some(s) = fname.to_str()
            && s.starts_with("ad-")
        {
            ad_sockets.push(s.to_string());
        }
    }

    Ok(ad_sockets)
}

/// Metadata for an ad editor session.
#[derive(Debug)]
pub struct SessionMeta {
    /// The socket name within [socket_dir] for this session.
    pub socket_name: String,
    /// Whether or not the session is currently unresponsive.
    ///
    /// A session can become unresponsive when it crashes before successfully
    /// removing it's filesystem socket.
    pub is_unresponsive: bool,
    /// The id of the currently active buffer.
    pub active_buffer_id: String,
    /// Metadata for the buffers open in this session.
    pub buffers: Vec<BufferMeta>,
}

impl SessionMeta {
    /// Remove this session's filesystem socket.
    pub fn remove_socket(&self) -> Result<()> {
        fs::remove_file(socket_dir().join(&self.socket_name))?;

        Ok(())
    }
}

/// Metadata for an open buffer within an ad editor session.
#[derive(Debug)]
pub struct BufferMeta {
    /// The id of the buffer.
    pub id: usize,
    /// The full filename of the buffer.
    pub filename: String,
}

/// Call [SessionMeta::remove_socket] for all currently unresponsive editor sessions.
pub fn remove_unresponsive_sessions() -> Result<()> {
    for session in list_open_sessions()?.into_iter() {
        if session.is_unresponsive {
            session.remove_socket()?;
        }
    }

    Ok(())
}

/// List open `ad` editor sessions and their current state.
pub fn list_open_sessions() -> Result<Vec<SessionMeta>> {
    let mut sessions = Vec::new();

    for ns in open_9p_sockets()?.into_iter() {
        let client = match Client::new_unix(&ns, "") {
            Ok(client) => client,
            Err(_) => {
                sessions.push(SessionMeta {
                    socket_name: ns,
                    is_unresponsive: true,
                    active_buffer_id: String::new(),
                    buffers: Vec::new(),
                });
                continue;
            }
        };
        let active_buffer_id = client.read_str("buffers/current")?;
        let buffers = client
            .read_str("buffers/index")?
            .lines()
            .map(|line| {
                let mut it = line.split_whitespace();
                let id = parse_bufid(it.next().unwrap_or_default())?;
                let filename = it.next().map(String::from).unwrap_or_default();

                Ok(BufferMeta { id, filename })
            })
            .collect::<Result<Vec<_>>>()?;

        sessions.push(SessionMeta {
            socket_name: ns,
            is_unresponsive: false,
            active_buffer_id,
            buffers,
        });
    }

    Ok(sessions)
}
