//! A synchronous client implementation.
use crate::{BufferMeta, LogEvent, MiniBufferSelection, SessionMeta, parse_bufid};
use ninep::sync::client::{Error, ReadLineIter, Result, UnixClient};
use std::{
    env,
    io::{self, Write},
    os::unix::net::UnixStream,
    path::Path,
    str::FromStr,
};

mod event;

pub use event::EventFilter;

/// A simple synchronous 9p client for ad
#[derive(Debug, Clone)]
pub struct Client {
    inner: UnixClient,
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
        })
    }

    /// Create a new client connected to the socket found at `path`.
    pub fn new_with_explicit_path(path: impl AsRef<Path>) -> Result<Self> {
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return Err(io::Error::other("USER env var not set").into()),
        };

        Ok(Self {
            inner: UnixClient::new_unix_with_explicit_path(uname, path, "/")?,
        })
    }

    pub(crate) fn event_lines(&mut self, buffer_id: usize) -> Result<ReadLineIter<UnixStream>> {
        self.inner.iter_lines(format!("buffers/{buffer_id}/event"))
    }

    pub(crate) fn write_event(&mut self, buffer_id: usize, event_line: &str) -> Result<()> {
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
    pub fn current_buffer(&mut self) -> Result<usize> {
        let id = parse_bufid(&self.inner.read_str("buffers/current")?)?;

        Ok(id)
    }

    /// Get the list of currently open buffers
    pub fn open_buffers(&mut self) -> Result<Vec<BufferMeta>> {
        let buffers = self
            .inner
            .read_str("buffers/index")?
            .lines()
            .map(|line| {
                let mut it = line.split_whitespace();
                let id = parse_bufid(it.next().unwrap_or_default())?;
                let filename = it.next().map(String::from).unwrap_or_default();

                Ok(BufferMeta { id, filename })
            })
            .collect::<Result<Vec<_>>>()?;

        Ok(buffers)
    }

    fn _read_buffer_file(&mut self, buffer_id: usize, file: &str) -> Result<String> {
        self.inner.read_str(format!("buffers/{buffer_id}/{file}"))
    }

    /// Read the contents of the dot of the given buffer
    pub fn read_dot(&mut self, buffer_id: usize) -> Result<String> {
        self._read_buffer_file(buffer_id, "dot")
    }

    /// Read the body of the given buffer.
    pub fn read_body(&mut self, buffer_id: usize) -> Result<String> {
        self._read_buffer_file(buffer_id, "body")
    }

    /// Read the current dot address of the given buffer.
    pub fn read_addr(&mut self, buffer_id: usize) -> Result<String> {
        self._read_buffer_file(buffer_id, "addr")
    }

    /// Read the filename of the given buffer
    pub fn read_filename(&mut self, buffer_id: usize) -> Result<String> {
        self._read_buffer_file(buffer_id, "filename")
    }

    /// Read the x-address of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xaddr(&mut self, buffer_id: usize) -> Result<String> {
        self._read_buffer_file(buffer_id, "xaddr")
    }

    /// Read the x-dot of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xdot(&mut self, buffer_id: usize) -> Result<String> {
        self._read_buffer_file(buffer_id, "xdot")
    }

    fn _write_buffer_file(
        &mut self,
        buffer_id: usize,
        file: &str,
        offset: u64,
        content: &[u8],
    ) -> Result<usize> {
        self.inner
            .write(format!("buffers/{buffer_id}/{file}"), offset, content)
    }

    /// Replace the dot of the given buffer with the provided string.
    pub fn write_dot(&mut self, buffer_id: usize, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "dot", 0, content.as_bytes())
    }

    /// Set the addr of the given buffer.
    pub fn write_addr(&mut self, buffer_id: usize, addr: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "addr", 0, addr.as_bytes())
    }

    /// Replace the xdot of the given buffer with the provided string.
    pub fn write_xdot(&mut self, buffer_id: usize, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "xdot", 0, content.as_bytes())
    }

    /// Set the xaddr of the given buffer.
    pub fn write_xaddr(&mut self, buffer_id: usize, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "xaddr", 0, content.as_bytes())
    }

    /// Append the provided string to the given buffer.
    pub fn append_to_body(&mut self, buffer_id: usize, content: &str) -> Result<usize> {
        self._write_buffer_file(buffer_id, "body", 0, content.as_bytes())
    }

    /// Clear the contents of the given buffer
    pub fn clear(&mut self, buffer_id: usize) -> Result<()> {
        self.write_xaddr(buffer_id, ",")?;
        self.write_xdot(buffer_id, "")?;

        Ok(())
    }

    /// Focus the given buffer
    pub fn focus_buffer(&mut self, buffer_id: usize) -> Result<()> {
        self.inner
            .write_str("buffers/current", 0, &buffer_id.to_string())?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the beginning of the file
    pub fn cur_to_bof(&mut self, buffer_id: usize) -> Result<()> {
        self.write_addr(buffer_id, "0")?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the end of the file
    pub fn cur_to_eof(&mut self, buffer_id: usize) -> Result<()> {
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

    fn _id_for_path(&mut self, path: &str) -> Result<usize> {
        for BufferMeta { id, filename } in self.open_buffers()?.into_iter() {
            if filename.ends_with(path) {
                return Ok(id);
            }
        }

        Err(Error::Rerror {
            ename: "unable to determine new buffer ID".into(),
        })
    }

    /// Open the requested file, returning its ID.
    pub fn open(&mut self, path: impl AsRef<str>) -> Result<usize> {
        let path = path.as_ref();
        self.ctl("open", path)?;

        self._id_for_path(path)
    }

    /// Open the requested file in a new window, returning its ID.
    pub fn open_in_new_window(&mut self, path: impl AsRef<str>) -> Result<usize> {
        let path = path.as_ref();
        self.ctl("open-in-new-window", path)?;

        self._id_for_path(path)
    }

    /// Open a new virtual file showing the given content, returning its ID.
    pub fn open_virtual(
        &mut self,
        name: impl AsRef<str>,
        content: impl AsRef<str>,
    ) -> Result<usize> {
        let name = name.as_ref();
        let content = content.as_ref();

        self.inner.write(
            "ctl",
            0,
            format!("open-virtual {name} {content}").as_bytes(),
        )?;

        self._id_for_path(name)
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
    pub fn run_event_filter<F>(&mut self, buffer_id: usize, filter: F) -> Result<()>
    where
        F: EventFilter,
    {
        event::run_filter(buffer_id, filter, self)
    }

    /// Create a [Write] impl that can be used to continuously write to the given path
    pub fn body_writer(&self, buffer_id: usize) -> Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{buffer_id}/body"),
            client: self.inner.clone(),
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
        let mut s = self.inner.read_str("minibuffer")?;
        if s.ends_with('\n') {
            s.pop();
        }

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
        let mut s = self.inner.read_str("minibuffer")?;
        if s.ends_with('\n') {
            s.pop();
        }

        if s.is_empty() { Ok(None) } else { Ok(Some(s)) }
    }
}

impl SessionMeta {
    /// Create a new [Client] for this session.
    pub fn client_for_session(&self) -> Result<Client> {
        Ok(Client {
            inner: UnixClient::new_unix(&self.socket_name, "/")?,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        EventOutcome,
        test_util::{TestEditor, mbs_cancelled, mbs_line, mbs_user},
    };
    use ad_editor::{editor::Action, input::Event, key::Input};
    use ad_event::Source;
    use simple_test_case::test_case;
    use std::{
        sync::{Arc, Mutex},
        thread::{sleep, spawn},
        time::Duration,
    };

    fn prepare(files: &[(&str, &str)]) -> (Client, TestEditor) {
        let ted = TestEditor::prepare(files);
        let client = Client::new_with_explicit_path(ted.socket_path()).unwrap();

        (client, ted)
    }

    // We provide additional helper methods around some of the common `ctl` commands, but so long
    // as one interaction with `ctl` works, we don't need to exhaustively test all of the ad
    // command functionality.

    #[test]
    fn ctl_works() {
        let (mut client, _ted) = prepare(&[("foo", "foo content")]);

        let fname = client.read_filename(1).unwrap();
        assert!(fname.ends_with("foo"), "{fname:?}");

        client.ctl("rename-buffer", "bar").unwrap();
        let fname = client.read_filename(1).unwrap();
        assert!(fname.ends_with("bar"), "{fname:?}");
    }

    #[test]
    fn manipulating_current_buffer_works() {
        let (mut client, _ted) = prepare(&[("foo", "foo content"), ("bar", "bar content")]);
        assert_eq!(
            client.current_buffer().unwrap(),
            2,
            "initial current buffer"
        );

        client.focus_buffer(1).unwrap();
        sleep(Duration::from_millis(5));

        assert_eq!(
            client.current_buffer().unwrap(),
            1,
            "current buffer after focus"
        );
    }

    #[test]
    fn manipulating_body_file_works() {
        let (mut client, _ted) = prepare(&[("foo", "foo content")]);

        let s = client.read_body(1).unwrap();
        assert_eq!(s, "foo content", "initial content");

        client.append_to_body(1, " new").unwrap();
        let s = client.read_body(1).unwrap();
        assert_eq!(s, "foo content new", "after append");

        client.clear(1).unwrap();
        let s = client.read_body(1).unwrap();
        assert_eq!(s, "", "after clear");
    }

    #[test]
    fn manipulating_addr_and_dot_works() {
        let (mut client, _ted) = prepare(&[("test", "This is a test")]);

        assert_eq!(client.read_addr(1).unwrap(), "1:1", "initial");
        assert_eq!(client.read_dot(1).unwrap(), "T", "initial");

        client.write_addr(1, "1:1,1:4").unwrap();
        assert_eq!(client.read_addr(1).unwrap(), "1:1,1:4", "write_addr");
        assert_eq!(client.read_dot(1).unwrap(), "This", "write_addr");

        client.write_dot(1, "THIS").unwrap();
        assert_eq!(client.read_addr(1).unwrap(), "1:5", "write_dot");

        client.write_addr(1, "1:1,1:4").unwrap();
        assert_eq!(client.read_dot(1).unwrap(), "THIS", "write_dot");
    }

    #[test]
    fn manipulating_xaddr_and_xdot_works() {
        let (mut client, _ted) = prepare(&[("test", "This is a test")]);

        assert_eq!(client.read_xaddr(1).unwrap(), "1:1", "initial");
        assert_eq!(client.read_xdot(1).unwrap(), "T", "initial");

        client.write_xaddr(1, "1:1,1:4").unwrap();
        assert_eq!(client.read_xaddr(1).unwrap(), "1:1,1:4", "write_xaddr");
        assert_eq!(
            client.read_addr(1).unwrap(),
            "1:1",
            "addr should be unchanged"
        );
        assert_eq!(client.read_xdot(1).unwrap(), "This", "write_xaddr");

        client.write_xdot(1, "THIS").unwrap();
        assert_eq!(client.read_xaddr(1).unwrap(), "1:5", "write_xdot");

        client.write_xaddr(1, "1:1,1:4").unwrap();
        assert_eq!(client.read_xdot(1).unwrap(), "THIS", "write_xdot");
    }

    #[test_case(&[Input::Char('a'), Input::Return], mbs_line(0, "alpha"); "type a")]
    #[test_case(&[Input::Char('b'), Input::Return], mbs_line(1, "bravo"); "type b")]
    #[test_case(&[Input::Char('x'), Input::Return], mbs_user("x"); "type x")]
    #[test_case(&[Input::Esc], mbs_cancelled(); "cancelled")]
    #[test]
    fn minibuffer_select_works(inputs: &[Input], expected: MiniBufferSelection) {
        let (mut client, ted) = prepare(&[]);
        let handle = spawn(move || client.minibuffer_select("> ", ["alpha", "bravo"]));
        sleep(Duration::from_millis(10)); // wait for the minibuffer to open

        for input in inputs.iter() {
            ted.tx.send(Event::Input(*input)).unwrap();
        }

        let res = handle.join().unwrap();

        assert_eq!(res.unwrap(), expected);
    }

    #[test_case(&[Input::Char('a'), Input::Return], Some("a"); "user input")]
    #[test_case(&[Input::Esc], None; "cancelled")]
    #[test]
    fn minibuffer_prompt_works(inputs: &[Input], expected: Option<&str>) {
        let (mut client, ted) = prepare(&[]);
        let handle = spawn(move || client.minibuffer_prompt("> "));
        sleep(Duration::from_millis(10)); // wait for the minibuffer to open

        for input in inputs.iter() {
            ted.tx.send(Event::Input(*input)).unwrap();
        }

        let res = handle.join().unwrap();

        assert_eq!(res.unwrap().as_deref(), expected);
    }

    #[derive(Default)]
    struct TestFilter {
        inner: Arc<Mutex<Vec<&'static str>>>,
    }

    impl EventFilter for TestFilter {
        fn handle_load(
            &mut self,
            _src: Source,
            _from: usize,
            _to: usize,
            _txt: &str,
            _client: &mut Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("load");

            Ok(EventOutcome::Exit)
        }

        fn handle_execute(
            &mut self,
            _src: Source,
            _from: usize,
            _to: usize,
            _txt: &str,
            _client: &mut Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("execute");

            Ok(EventOutcome::Exit)
        }

        fn handle_insert(
            &mut self,
            _src: Source,
            _from: usize,
            _to: usize,
            _txt: &str,
            _client: &mut Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("insert");

            Ok(EventOutcome::Exit)
        }

        fn handle_delete(
            &mut self,
            _src: Source,
            _from: usize,
            _to: usize,
            _client: &mut Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("delete");

            Ok(EventOutcome::Exit)
        }
    }

    #[test_case(Action::LoadDot { new_window: false }, "load"; "load")]
    #[test_case(Action::ExecuteDot , "execute"; "execute")]
    #[test_case(Action::InsertChar { c: 'a' } , "insert"; "insert")]
    #[test_case(Action::Delete, "delete"; "delete")]
    #[test]
    fn run_event_filter_works(action: Action, expected: &str) {
        let (mut client, ted) = prepare(&[("foo", "foo content")]);

        let filter = TestFilter::default();
        let calls = Arc::clone(&filter.inner);

        let handle = spawn(move || client.run_event_filter(1, filter));
        sleep(Duration::from_millis(10)); // wait for the filter to attach

        _ = ted.tx.send(Event::Action(action));

        let res = handle.join().unwrap();
        assert!(res.is_ok(), "{res:?}");

        let recorded = calls.lock().unwrap();
        assert_eq!(*recorded, vec![expected]);
    }

    #[test]
    fn open_returns_correct_id() {
        let (mut client, ted) = prepare(&[]);
        let path = ted.write_file("test", "test content");

        let id = client.open(path).unwrap();
        assert_eq!(id, 1);

        let body = client.read_body(1).unwrap();
        assert_eq!(body, "test content");
    }

    #[test]
    fn open_in_new_window_returns_correct_id() {
        let (mut client, ted) = prepare(&[]);
        let path = ted.write_file("test", "test content");

        let id = client.open_in_new_window(path).unwrap();
        assert_eq!(id, 1);

        let body = client.read_body(1).unwrap();
        assert_eq!(body, "test content");
    }

    #[test]
    fn open_virtual_returns_correct_id() {
        let (mut client, _ted) = prepare(&[]);

        let id = client.open_virtual("+test", "test content").unwrap();
        assert_eq!(id, 1);

        let body = client.read_body(1).unwrap();
        assert_eq!(body, "test content");
    }
}
