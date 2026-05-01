//! An asynchronous client implementation.
use crate::{LogEvent, MiniBufferSelection, SessionMeta};
use ninep::tokio::client::{ReadLineStream, Result, UnixClient};
use std::{env, io, str::FromStr};
use tokio::net::UnixStream;

mod event;

pub use event::AsyncEventFilter;

/// A simple synchronous 9p client for ad
#[derive(Debug, Clone)]
pub struct Client {
    inner: UnixClient,
    ns: String,
}

impl Client {
    /// Create a new client connected to `ad` over it's 9p unix socket
    pub async fn new() -> Result<Self> {
        let ns = match env::var("AD_PID") {
            Ok(pid) => format!("ad-{pid}"),
            Err(_) => "ad".to_string(),
        };

        Ok(Self {
            inner: UnixClient::new_unix(&ns, "/").await?,
            ns,
        })
    }

    /// Create a new client connected to the `ad` session with the given pid
    /// over it's 9p unix socket.
    ///
    /// When running under ad, the [Client::new] method will automatically find
    /// and connect to it's parent session.
    pub async fn new_for_pid(pid: &str) -> Result<Self> {
        let ns = format!("ad-{pid}");

        Ok(Self {
            inner: UnixClient::new_unix(&ns, "/").await?,
            ns,
        })
    }

    pub(crate) async fn event_lines(&mut self, buffer: &str) -> Result<ReadLineStream<UnixStream>> {
        self.inner
            .stream_lines(format!("buffers/{buffer}/event"))
            .await
    }

    pub(crate) async fn write_event(&mut self, buffer: &str, event_line: &str) -> Result<()> {
        self.inner
            .write_str(format!("buffers/{buffer}/event"), 0, event_line)
            .await?;

        Ok(())
    }

    /// Iterate over the log events emitted by ad
    pub async fn log_events(&mut self) -> Result<LogStream> {
        Ok(LogStream {
            inner: self.inner.stream_lines("log").await?,
        })
    }

    /// Get the currently active buffer id.
    pub async fn current_buffer(&mut self) -> Result<String> {
        self.inner.read_str("buffers/current").await
    }

    async fn _read_buffer_file(&mut self, buffer: &str, file: &str) -> Result<String> {
        self.inner
            .read_str(format!("buffers/{buffer}/{file}"))
            .await
    }

    /// Read the contents of the dot of the given buffer
    pub async fn read_dot(&mut self, buffer: &str) -> Result<String> {
        self._read_buffer_file(buffer, "dot").await
    }

    /// Read the body of the given buffer.
    pub async fn read_body(&mut self, buffer: &str) -> Result<String> {
        self._read_buffer_file(buffer, "body").await
    }

    /// Read the current dot address of the given buffer.
    pub async fn read_addr(&mut self, buffer: &str) -> Result<String> {
        self._read_buffer_file(buffer, "addr").await
    }

    /// Read the filename of the given buffer
    pub async fn read_filename(&mut self, buffer: &str) -> Result<String> {
        self._read_buffer_file(buffer, "filename").await
    }

    /// Read the x-address of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub async fn read_xaddr(&mut self, buffer: &str) -> Result<String> {
        self._read_buffer_file(buffer, "xaddr").await
    }

    /// Read the x-dot of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub async fn read_xdot(&mut self, buffer: &str) -> Result<String> {
        self._read_buffer_file(buffer, "xdot").await
    }

    async fn _write_buffer_file(
        &mut self,
        buffer: &str,
        file: &str,
        offset: u64,
        content: &[u8],
    ) -> Result<usize> {
        self.inner
            .write(format!("buffers/{buffer}/{file}"), offset, content)
            .await
    }

    /// Replace the dot of the given buffer with the provided string.
    pub async fn write_dot(&mut self, buffer: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer, "dot", 0, content.as_bytes())
            .await
    }

    /// Append the provided string to the given buffer.
    pub async fn append_to_body(&mut self, buffer: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer, "body", 0, content.as_bytes())
            .await
    }

    /// Set the addr of the given buffer.
    pub async fn write_addr(&mut self, buffer: &str, addr: &str) -> Result<usize> {
        self._write_buffer_file(buffer, "addr", 0, addr.as_bytes())
            .await
    }

    /// Replace the xdot of the given buffer with the provided string.
    pub async fn write_xdot(&mut self, buffer: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer, "xdot", 0, content.as_bytes())
            .await
    }

    /// Set the xaddr of the given buffer.
    pub async fn write_xaddr(&mut self, buffer: &str, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer, "xaddr", 0, content.as_bytes())
            .await
    }

    /// Send a control message to ad.
    pub async fn ctl(&mut self, command: &str, args: &str) -> Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())
            .await?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub async fn echo(&mut self, msg: impl AsRef<str>) -> Result<()> {
        self.ctl("echo", msg.as_ref()).await
    }

    /// Open the requested file.
    pub async fn open(&mut self, path: impl AsRef<str>) -> Result<()> {
        self.ctl("open", path.as_ref()).await
    }

    /// Open the requested file in a new window.
    pub async fn open_in_new_window(&mut self, path: impl AsRef<str>) -> Result<()> {
        self.ctl("open-in-new-window", path.as_ref()).await
    }

    /// Reload the currently active buffer.
    pub async fn reload_current_buffer(&mut self) -> Result<()> {
        self.ctl("reload", "").await
    }

    /// Run a provided [AsyncEventFilter] until it exits or errors.
    pub async fn run_event_filter<F>(&mut self, buffer: &str, filter: F) -> Result<()>
    where
        F: AsyncEventFilter,
    {
        event::run_filter(buffer, filter, self).await
    }

    /// Create a [BodyWriter] impl that can be used to continuously write to the given path
    pub async fn body_writer(&self, bufid: &str) -> Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{bufid}/body"),
            client: UnixClient::new_unix(&self.ns, "/").await?,
        })
    }

    /// Open the minibuffer with the provided `prompt` showing `lines`.
    ///
    /// If the user makes a selection (either from the provided lines or
    pub async fn minibuffer_select<I, S>(
        &mut self,
        prompt: &str,
        lines: I,
    ) -> Result<MiniBufferSelection>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let lines: Vec<String> = lines
            .into_iter()
            .map(|elem| elem.as_ref().to_string())
            .collect();
        self.inner
            .write_str("minibuffer", 0, &lines.join("\n"))
            .await?;
        self.ctl("minibuffer-prompt", prompt).await?;
        let s = self.inner.read_str("minibuffer").await?;

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
    pub async fn minibuffer_prompt(&mut self, prompt: &str) -> Result<Option<String>> {
        self.inner.write_str("minibuffer", 0, "").await?;
        self.ctl("minibuffer-prompt", prompt).await?;
        let s = self.inner.read_str("minibuffer").await?;

        if s.is_empty() { Ok(None) } else { Ok(Some(s)) }
    }
}

/// An asynchronous stream of [LogEvent]s from an `ad` instance.
#[derive(Debug)]
pub struct LogStream {
    inner: ReadLineStream<UnixStream>,
}

impl LogStream {
    /// Poll for the next log event from `ad`.
    ///
    /// Returns `Some(Err())` if the log event is malformed.
    pub async fn next(&mut self) -> Option<Result<LogEvent>> {
        self.inner
            .next()
            .await
            .map(|line| LogEvent::from_str(&line))
    }
}

impl SessionMeta {
    /// Create a new [Client] for this session.
    pub async fn async_client_for_session(&self) -> Result<Client> {
        Ok(Client {
            inner: UnixClient::new_unix(&self.socket_name, "/").await?,
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
    /// Write the provided data to the buffer.
    pub async fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(self.client.write(&self.path, 0, buf).await?)
    }

    /// Mark the buffer as being clean
    pub async fn mark_clean(&mut self) -> Result<()> {
        self.client.write("ctl", 0, "mark-clean".as_bytes()).await?;

        Ok(())
    }
}
