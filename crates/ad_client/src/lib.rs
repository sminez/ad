//! A simple 9p based client for interacting with ad
use ninep::client::UnixClient;
use std::io;

/// A simple 9p client for ad
#[derive(Debug)]
pub struct Client {
    inner: UnixClient,
}

impl Client {
    pub fn new() -> io::Result<Self> {
        Ok(Self {
            inner: UnixClient::new_unix("ad", "")?,
        })
    }

    /// Get the currently active buffer id.
    pub fn current_buffer(&mut self) -> io::Result<String> {
        self.inner.read_str("buffers/current")
    }

    fn _read_buffer_file(&mut self, buffer: &str, file: &str) -> io::Result<String> {
        self.inner.read_str(format!("buffers/{buffer}/{file}"))
    }

    pub fn read_dot(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "dot")
    }

    pub fn read_body(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "body")
    }

    pub fn read_addr(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "addr")
    }

    pub fn read_filename(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "filename")
    }

    pub fn read_xaddr(&mut self, buffer: &str) -> io::Result<String> {
        self._read_buffer_file(buffer, "xaddr")
    }

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

    pub fn write_dot(&mut self, buffer: &str, offset: u64, content: &[u8]) -> io::Result<usize> {
        self._write_buffer_file(buffer, "dot", offset, content)
    }

    pub fn write_body(&mut self, buffer: &str, offset: u64, content: &[u8]) -> io::Result<usize> {
        self._write_buffer_file(buffer, "body", offset, content)
    }

    pub fn write_addr(&mut self, buffer: &str, offset: u64, content: &[u8]) -> io::Result<usize> {
        self._write_buffer_file(buffer, "addr", offset, content)
    }

    pub fn write_xaddr(&mut self, buffer: &str, offset: u64, content: &[u8]) -> io::Result<usize> {
        self._write_buffer_file(buffer, "xaddr", offset, content)
    }

    pub fn write_xdot(&mut self, buffer: &str, offset: u64, content: &[u8]) -> io::Result<usize> {
        self._write_buffer_file(buffer, "xdot", offset, content)
    }

    fn _ctl(&mut self, command: &str, args: &str) -> io::Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub fn echo(&mut self, msg: impl AsRef<str>) -> io::Result<()> {
        self._ctl("echo", msg.as_ref())
    }

    /// Open the requested file.
    pub fn open(&mut self, path: impl AsRef<str>) -> io::Result<()> {
        self._ctl("open", path.as_ref())
    }

    /// Reload the currently active buffer.
    pub fn reload_current_buffer(&mut self) -> io::Result<()> {
        self._ctl("reload", "")
    }
}
