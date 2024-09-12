//! Handling of our internal logs so they can be viewed in the editor itself
use std::{
    io::{self, Cursor, Read, Write},
    sync::{Arc, Mutex, MutexGuard},
};
use tracing_subscriber::fmt::MakeWriter;

/// A central log writer wrapping an in memory buffer so we can display logs within the editor
/// itself.
#[derive(Debug, Default, Clone)]
pub struct LogBuffer {
    inner: Arc<Mutex<Cursor<Vec<u8>>>>,
}

impl LogBuffer {
    /// Return the full content of the log
    pub fn content(&self) -> String {
        let mut guard = self.inner.lock().expect("lock poisoned");
        let pos = guard.position();
        guard.set_position(0);
        let mut s = String::new();
        _ = guard.read_to_string(&mut s);
        guard.set_position(pos);

        s
    }

    /// Clear the contents of the current log
    pub fn clear(&self) {
        let mut guard = self.inner.lock().expect("lock poisoned");
        *guard = Default::default();
    }
}

/// A handle implementing [Write] that can be used by tracing for writing our logs
#[derive(Debug)]
pub struct LogWriter<'a>(MutexGuard<'a, Cursor<Vec<u8>>>);

impl<'a> Write for LogWriter<'a> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }

    #[inline]
    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        self.0.write_vectored(bufs)
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.0.write_all(buf)
    }

    #[inline]
    fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> io::Result<()> {
        self.0.write_fmt(fmt)
    }
}

impl<'a> MakeWriter<'a> for LogBuffer {
    type Writer = LogWriter<'a>;

    fn make_writer(&'a self) -> Self::Writer {
        LogWriter(self.inner.lock().expect("lock poisoned"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reading_log_content_works() {
        let logs = LogBuffer::default();
        _ = logs.make_writer().write(b"hello, world!\n").unwrap();

        let content = logs.content();
        assert_eq!(content, "hello, world!\n");

        _ = logs.make_writer().write(b"a second line\n").unwrap();

        let content = logs.content();
        assert_eq!(content, "hello, world!\na second line\n");
    }

    #[test]
    fn clearing_works() {
        let logs = LogBuffer::default();
        _ = logs.make_writer().write(b"hello, world!\n").unwrap();

        let content = logs.content();
        assert_eq!(content, "hello, world!\n");

        logs.clear();

        let content = logs.content();
        assert!(content.is_empty());
    }
}
