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
use ninep::{
    sansio::server::socket_dir,
    sync::client::{ReadLineIter, UnixClient},
};
use std::{env, fs, io, io::Write, os::unix::net::UnixStream, str::FromStr};

mod event;

pub use ad_event::Source;
pub use event::{EventFilter, Outcome};

/// A simple 9p client for ad
#[derive(Debug, Clone)]
pub struct Client {
    inner: UnixClient,
    ns: String,
}

impl Client {
    /// Create a new client connected to `ad` over it's 9p unix socket
    pub fn new() -> io::Result<Self> {
        let ns = match env::var("AD_PID") {
            Ok(pid) => format!("ad-{pid}"),
            Err(_) => "ad".to_string(),
        };

        Ok(Self {
            inner: UnixClient::new_unix(&ns, "")?,
            ns,
        })
    }

    /// Create a new client connected to the `ad` session with the given pid
    /// over it's 9p unix socket.
    ///
    /// When running under ad, the [Client::new] method will automatically find
    /// and connect to it's parent session.
    pub fn new_for_pid(pid: &str) -> io::Result<Self> {
        let ns = format!("ad-{pid}");

        Ok(Self {
            inner: UnixClient::new_unix(&ns, "")?,
            ns,
        })
    }

    pub(crate) fn event_lines(&mut self, buffer: &str) -> io::Result<ReadLineIter<UnixStream>> {
        self.inner.iter_lines(format!("buffers/{buffer}/event"))
    }

    pub(crate) fn write_event(&mut self, buffer: &str, event_line: &str) -> io::Result<()> {
        self.inner
            .write_str(format!("buffers/{buffer}/event"), 0, event_line)?;
        Ok(())
    }

    /// Iterate over the log events emitted by ad
    pub fn log_events(&mut self) -> io::Result<impl Iterator<Item = io::Result<LogEvent>> + use<>> {
        Ok(self
            .inner
            .iter_lines("log")?
            .map(|line| LogEvent::from_str(&line)))
    }

    /// Get the currently active buffer id.
    pub fn current_buffer(&mut self) -> io::Result<String> {
        self.inner.read_str("buffers/current")
    }

    fn _read_buffer_file(&mut self, buffer: &str, file: &str) -> io::Result<String> {
        self.inner.read_str(format!("buffers/{buffer}/{file}"))
    }

    /// Read the contents of the dot of the given buffer
    pub fn read_dot(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "dot")
    }

    /// Read the body of the given buffer.
    pub fn read_body(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "body")
    }

    /// Read the current dot address of the given buffer.
    pub fn read_addr(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "addr")
    }

    /// Read the filename of the given buffer
    pub fn read_filename(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "filename")
    }

    /// Read the x-address of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xaddr(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "xaddr")
    }

    /// Read the x-dot of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xdot(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "xdot")
    }

    fn _write_buffer_file(
        &mut self,
        buffer: &str,
        file: &str,
        offset: u64,
        content: &[u8],
    ) -> io::Result<usize> {
        self.inner
            .write(format!("buffers/{buffer}/{file}"), offset, content)
    }

    /// Replace the dot of the given buffer with the provided string.
    pub fn write_dot(&mut self, buffer: &str, content: &str) -> io::Result<usize> {
        self._write_buffer_file(buffer, "dot", 0, content.as_bytes())
    }

    /// Append the provided string to the given buffer.
    pub fn append_to_body(&mut self, buffer: &str, content: &str) -> io::Result<usize> {
        self._write_buffer_file(buffer, "body", 0, content.as_bytes())
    }

    /// Set the addr of the given buffer.
    pub fn write_addr(&mut self, buffer: &str, addr: &str) -> io::Result<usize> {
        self._write_buffer_file(buffer, "addr", 0, addr.as_bytes())
    }

    /// Replace the xdot of the given buffer with the provided string.
    pub fn write_xdot(&mut self, buffer: &str, content: &str) -> io::Result<usize> {
        self._write_buffer_file(buffer, "xdot", 0, content.as_bytes())
    }

    /// Set the xaddr of the given buffer.
    pub fn write_xaddr(&mut self, buffer: &str, content: &str) -> io::Result<usize> {
        self._write_buffer_file(buffer, "xaddr", 0, content.as_bytes())
    }

    /// Send a control message to ad.
    pub fn ctl(&mut self, command: &str, args: &str) -> io::Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub fn echo(&mut self, msg: impl AsRef<str>) -> io::Result<()> {
        self.ctl("echo", msg.as_ref())
    }

    /// Open the requested file.
    pub fn open(&mut self, path: impl AsRef<str>) -> io::Result<()> {
        self.ctl("open", path.as_ref())
    }

    /// Open the requested file in a new window.
    pub fn open_in_new_window(&mut self, path: impl AsRef<str>) -> io::Result<()> {
        self.ctl("open-in-new-window", path.as_ref())
    }

    /// Reload the currently active buffer.
    pub fn reload_current_buffer(&mut self) -> io::Result<()> {
        self.ctl("reload", "")
    }

    /// Run a provided [EventFilter] until it exits or errors
    pub fn run_event_filter<F>(&mut self, buffer: &str, filter: F) -> io::Result<()>
    where
        F: EventFilter,
    {
        event::run_filter(buffer, filter, self)
    }

    /// Create a [Write] impl that can be used to continuously write to the given path
    pub fn body_writer(&self, bufid: &str) -> io::Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{bufid}/body"),
            client: UnixClient::new_unix(&self.ns, "")?,
        })
    }
}

/// A writer for appending to the body of a buffer
#[derive(Debug)]
pub struct BodyWriter {
    path: String,
    client: UnixClient,
}

impl BodyWriter {
    /// Mark the buffer as being clean
    pub fn mark_clean(&mut self) -> io::Result<()> {
        self.client.write("ctl", 0, "mark-clean".as_bytes())?;

        Ok(())
    }
}

impl Write for BodyWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.client.write(&self.path, 0, buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
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
    type Err = io::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if s.contains('\n') {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "expected single line",
            ));
        }

        let (str_id, action) = s.split_once(' ').ok_or(io::Error::new(
            io::ErrorKind::InvalidData,
            "malformed log line: {s:?}",
        ))?;

        let id: usize = str_id.parse().map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidData,
                "expected integer ID, got {str_id:?}",
            )
        })?;

        let evt = match action {
            "open" => Self::Open(id),
            "close" => Self::Close(id),
            "focus" => Self::Focus(id),
            "save" => Self::Save(id),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unknown log action {action:?}",
                ));
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
    /// Create a new [Client] for this session.
    pub fn client_for_session(&self) -> io::Result<Client> {
        Ok(Client {
            inner: UnixClient::new_unix(&self.socket_name, "")?,
            ns: self.socket_name.clone(),
        })
    }

    /// Remove this session's filesystem socket.
    pub fn remove_socket(&self) -> io::Result<()> {
        fs::remove_file(socket_dir().join(&self.socket_name))
    }
}

/// Metadata for an open buffer within an ad editor session.
#[derive(Debug)]
pub struct BufferMeta {
    /// The id of the buffer.
    pub id: String,
    /// The full filename of the buffer.
    pub filename: String,
}

/// Call [SessionMeta::remove_socket] for all currently unresponsive editor sessions.
pub fn remove_unresponsive_sessions() -> io::Result<()> {
    for session in list_open_sessions()?.into_iter() {
        if session.is_unresponsive {
            session.remove_socket()?;
        }
    }

    Ok(())
}

/// List open `ad` editor sessions and their current state.
pub fn list_open_sessions() -> io::Result<Vec<SessionMeta>> {
    let mut sessions = Vec::new();

    for ns in open_9p_sockets()?.into_iter() {
        let mut client = match UnixClient::new_unix(&ns, "") {
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
                let id = it.next().map(String::from).unwrap_or_default();
                let filename = it.next().map(String::from).unwrap_or_default();

                BufferMeta { id, filename }
            })
            .collect();

        sessions.push(SessionMeta {
            socket_name: ns,
            is_unresponsive: false,
            active_buffer_id,
            buffers,
        });
    }

    Ok(sessions)
}
