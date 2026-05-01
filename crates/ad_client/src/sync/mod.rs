//! A synchronous client implementation.
use crate::{LogEvent, MiniBufferSelection, SessionMeta};
use ninep::sync::client::{ReadLineIter, Result, UnixClient};
use std::{
    env,
    io::{self, Write},
    os::unix::net::UnixStream,
    str::FromStr,
};

mod event;

pub use event::EventFilter;

/// A simple synchronous 9p client for ad
#[derive(Debug, Clone)]
pub struct Client {
    inner: UnixClient,
    ns: String,
}

impl Client {
    /// Create a new client connected to `ad` over it's 9p unix socket
    pub fn new() -> Result<Self> {
        let ns = match env::var("AD_PID") {
            Ok(pid) => format!("ad-{pid}"),
            Err(_) => "ad".to_string(),
        };

        Ok(Self {
            inner: UnixClient::new_unix(&ns, "/")?,
            ns,
        })
    }

    /// Create a new client connected to the `ad` session with the given pid
    /// over it's 9p unix socket.
    ///
    /// When running under ad, the [Client::new] method will automatically find
    /// and connect to it's parent session.
    pub fn new_for_pid(pid: &str) -> Result<Self> {
        let ns = format!("ad-{pid}");

        Ok(Self {
            inner: UnixClient::new_unix(&ns, "/")?,
            ns,
        })
    }

    pub(crate) fn event_lines(&mut self, buffer_id: &str) -> Result<ReadLineIter<UnixStream>> {
        self.inner.iter_lines(format!("buffers/{buffer_id}/event"))
    }

    pub(crate) fn write_event(&mut self, buffer_id: &str, event_line: &str) -> Result<()> {
        self.inner
            .write_str(format!("buffers/{buffer_id}/event"), 0, event_line)?;

        Ok(())
    }

    /// Iterate over the log events emitted by ad
    pub fn log_events(&mut self) -> Result<impl Iterator<Item = Result<LogEvent>> + use<>> {
        Ok(self
            .inner
            .iter_lines("log")?
            .map(|line| LogEvent::from_str(&line)))
    }

    /// Get the currently active buffer id.
    pub fn current_buffer(&mut self) -> Result<String> {
        self.inner.read_str("buffers/current")
    }

    fn _read_buffer_file(&mut self, buffer_id: &str, file: &str) -> Result<String> {
        self.inner.read_str(format!("buffers/{buffer_id}/{file}"))
    }

    /// Read the contents of the dot of the given buffer
    pub fn read_dot(&mut self, buffer_id: &str) -> Result<String> {
        self._read_buffer_file(buffer_id, "dot")
    }

    /// Read the body of the given buffer.
    pub fn read_body(&mut self, buffer_id: &str) -> Result<String> {
        self._read_buffer_file(buffer_id, "body")
    }

    /// Read the current dot address of the given buffer.
    pub fn read_addr(&mut self, buffer_id: &str) -> Result<String> {
        self._read_buffer_file(buffer_id, "addr")
    }

    /// Read the filename of the given buffer
    pub fn read_filename(&mut self, buffer_id: &str) -> Result<String> {
        self._read_buffer_file(buffer_id, "filename")
    }

    /// Read the x-address of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xaddr(&mut self, buffer_id: &str) -> Result<String> {
        self._read_buffer_file(buffer_id, "xaddr")
    }

    /// Read the x-dot of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xdot(&mut self, buffer_id: &str) -> Result<String> {
        self._read_buffer_file(buffer_id, "xdot")
    }

    fn _write_buffer_file(
        &mut self,
        buffer_id: &str,
        file: &str,
        offset: u64,
        content: &[u8],
    ) -> Result<usize> {
        self.inner
            .write(format!("buffers/{buffer_id}/{file}"), offset, content)
    }

    /// Replace the dot of the given buffer with the provided string.
    pub fn write_dot(&mut self, buffer_id: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "dot", 0, content.as_bytes())
    }

    /// Append the provided string to the given buffer.
    pub fn append_to_body(&mut self, buffer_id: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "body", 0, content.as_bytes())
    }

    /// Set the addr of the given buffer.
    pub fn write_addr(&mut self, buffer_id: &str, addr: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "addr", 0, addr.as_bytes())
    }

    /// Replace the xdot of the given buffer with the provided string.
    pub fn write_xdot(&mut self, buffer_id: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "xdot", 0, content.as_bytes())
    }

    /// Set the xaddr of the given buffer.
    pub fn write_xaddr(&mut self, buffer_id: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "xaddr", 0, content.as_bytes())
    }

    /// Clear the contents of the given buffer
    pub fn clear(&mut self, buffer_id: &str) -> Result<()> {
        self.write_xaddr(buffer_id, ",")?;
        self.write_xdot(buffer_id, "")?;

        Ok(())
    }

    /// Focus the given buffer
    pub fn focus_buffer(&mut self, buffer_id: &str) -> Result<()> {
        self.inner.write_str("buffers/current", 0, buffer_id)?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the beginning of the file
    pub fn cur_to_bof(&mut self, buffer_id: &str) -> Result<()> {
        self.write_addr(buffer_id, "0")?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the end of the file
    pub fn cur_to_eof(&mut self, buffer_id: &str) -> Result<()> {
        self.write_addr(buffer_id, "$")?;

        Ok(())
    }

    /// Send a control message to ad.
    pub fn ctl(&mut self, command: &str, args: &str) -> Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub fn echo(&mut self, msg: impl AsRef<str>) -> Result<()> {
        self.ctl("echo", msg.as_ref())
    }

    /// Open the requested file.
    pub fn open(&mut self, path: impl AsRef<str>) -> Result<()> {
        self.ctl("open", path.as_ref())
    }

    /// Open the requested file in a new window.
    pub fn open_in_new_window(&mut self, path: impl AsRef<str>) -> Result<()> {
        self.ctl("open-in-new-window", path.as_ref())
    }

    /// Reload the currently active buffer.
    pub fn reload_current_buffer(&mut self) -> Result<()> {
        self.ctl("reload", "")
    }

    /// Mark the currently active buffer as being clean.
    pub fn mark_clean(&mut self) -> Result<()> {
        self.ctl("mark-clean", "")
    }

    /// Run the provided ad Edit script against the current buffer
    pub fn run_edit_script(&mut self, script: impl AsRef<str>) -> Result<()> {
        self.ctl("Edit", script.as_ref())
    }

    /// Run a provided [EventFilter] until it exits or errors
    pub fn run_event_filter<F>(&mut self, buffer_id: &str, filter: F) -> Result<()>
    where
        F: EventFilter,
    {
        event::run_filter(buffer_id, filter, self)
    }

    /// Create a [Write] impl that can be used to continuously write to the given path
    pub fn body_writer(&self, buffer_id: &str) -> Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{buffer_id}/body"),
            client: UnixClient::new_unix(&self.ns, "/")?,
        })
    }

    /// Open the minibuffer with the provided `prompt` showing `lines`.
    ///
    /// If the user makes a selection (either from the provided lines or
    pub fn minibuffer_select<I, S>(&mut self, prompt: &str, lines: I) -> Result<MiniBufferSelection>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let lines: Vec<String> = lines
            .into_iter()
            .map(|elem| elem.as_ref().to_string())
            .collect();
        self.inner.write_str("minibuffer", 0, &lines.join("\n"))?;
        self.ctl("minibuffer-prompt", prompt)?;
        let s = self.inner.read_str("minibuffer")?;

        if s.is_empty() {
            Ok(MiniBufferSelection::Cancelled)
        } else if let Some(index) = lines.iter().position(|elem| elem == &s) {
            Ok(MiniBufferSelection::Line { index, content: s })
        } else {
            Ok(MiniBufferSelection::UserInput { content: s })
        }
    }

    /// Prompt the user for input via the minibuffer.
    ///
    /// Returns `Ok(None)` if the user dismisses the minibuffer without input.
    pub fn minibuffer_prompt(&mut self, prompt: &str) -> Result<Option<String>> {
        self.inner.write_str("minibuffer", 0, "")?;
        self.ctl("minibuffer-prompt", prompt)?;
        let s = self.inner.read_str("minibuffer")?;

        if s.is_empty() { Ok(None) } else { Ok(Some(s)) }
    }
}

impl SessionMeta {
    /// Create a new [Client] for this session.
    pub fn client_for_session(&self) -> Result<Client> {
        Ok(Client {
            inner: UnixClient::new_unix(&self.socket_name, "/")?,
            ns: self.socket_name.clone(),
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
    pub fn mark_clean(&mut self) -> Result<()> {
        self.client.write("ctl", 0, "mark-clean".as_bytes())?;

        Ok(())
    }
}

impl Write for BodyWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(self.client.write(&self.path, 0, buf)?)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}
