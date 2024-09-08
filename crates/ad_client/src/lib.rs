//! A simple 9p based client for interacting with ad
use ninep::client;
use std::{fmt, io};

/// A simple 9p client for ad
#[derive(Debug)]
pub struct Client {
    inner: client::Client,
}

impl Client {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            inner: client::Client::new_unix("ad", "")?,
        })
    }

    /// Get the currently active buffer id.
    pub fn current_buffer(&mut self) -> io::Result<String> {
        self.inner.read_str("buffers/current")
    }

    fn _ctl(&mut self, command: &str, args: impl fmt::Display) -> io::Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub fn echo(&mut self, msg: impl fmt::Display) -> io::Result<()> {
        self._ctl("echo", msg)
    }

    /// Open the requested file.
    pub fn open(&mut self, path: impl Into<String>) -> io::Result<()> {
        self._ctl("open", path.into())
    }

    /// Reload the currently active buffer.
    pub fn reload_current_buffer(&mut self) -> io::Result<()> {
        self._ctl("reload", "")
    }
}
