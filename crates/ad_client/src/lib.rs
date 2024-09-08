//! A simple 9p based client for interacting with ad
use ninep::client;
use std::io;

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

    /// Get the currently active buffer id
    pub fn current_buffer(&mut self) -> io::Result<String> {
        self.inner.read_str("buffers/current")
    }
}
