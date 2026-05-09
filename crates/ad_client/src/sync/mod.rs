//! A synchronous client implementation.
use crate::{BufferMeta, LogEvent, MiniBufferSelection, SessionMeta, parse_bufid};
use ninep::sync::client::{Client as NinepClient, Error, ReadLineIter, Result};
use std::{
    env,
    io::{self, Write},
    ops::Deref,
    path::Path,
    str::FromStr,
    thread::sleep,
    time::Duration,
};

mod event;

pub use event::EventFilter;

/// A simple synchronous 9p client for ad
#[derive(Debug, Clone)]
pub struct Client {
    inner: NinepClient,
}

impl Client {
    /// Create a new client connected to `ad` over it's 9p unix socket
    pub fn new() -> Result<Self> {
        let ns = match env::var("AD_PID") {
            Ok(pid) => format!("ad-{pid}"),
            Err(_) => "ad".to_string(),
        };

        Ok(Self {
            inner: NinepClient::new_unix(&ns, "")?,
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
            inner: NinepClient::new_unix(&ns, "")?,
        })
    }

    /// Create a new client connected to the socket found at `path`.
    pub fn new_with_explicit_path(path: impl AsRef<Path>) -> Result<Self> {
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return Err(io::Error::other("USER env var not set").into()),
        };

        Ok(Self {
            inner: NinepClient::new_unix_with_explicit_path(uname, path, "")?,
        })
    }

    /// Iterate over the log events emitted by ad
    pub fn log_events(&self) -> Result<impl Iterator<Item = Result<LogEvent>> + use<>> {
        Ok(self
            .inner
            .iter_lines("log")?
            .map(|line| LogEvent::from_str(&line)))
    }

    /// Get the currently active buffer id.
    pub fn current_buffer(&self) -> Result<usize> {
        let id = parse_bufid(&self.inner.read_str("buffers/current")?)?;

        Ok(id)
    }

    /// Get the list of currently open buffers
    pub fn open_buffers(&self) -> Result<Vec<BufferMeta>> {
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

    /// Focus the given buffer
    pub fn focus_buffer(&self, bufid: usize) -> Result<()> {
        self.inner
            .write_str("buffers/current", 0, &bufid.to_string())?;

        Ok(())
    }

    /// Read the contents of the scratch buffer
    pub fn read_scratch(&self) -> Result<String> {
        self.inner.read_str("scratch")
    }

    /// Append to the scratch buffer
    pub fn append_scratch(&self, content: &str) -> Result<()> {
        self.inner.write_str("scratch", 0, content)?;

        Ok(())
    }

    /// Send a control message to ad.
    pub fn ctl(&self, command: &str, args: &str) -> Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub fn echo(&self, msg: impl AsRef<str>) -> Result<()> {
        self.ctl("echo", msg.as_ref())
    }

    fn _id_for_path(&self, path: &str) -> Result<usize> {
        sleep(Duration::from_millis(5));

        for BufferMeta { id, filename } in self.open_buffers()?.into_iter() {
            if filename.ends_with(path) {
                return Ok(id);
            }
        }

        Err(Error::Rerror {
            ename: "unable to determine new buffer ID".into(),
        })
    }

    /// Build a client for interacting with an open ad buffer
    pub fn for_buffer(&self, bufid: usize) -> BufferClient {
        BufferClient {
            bufid,
            client: self.clone(),
        }
    }

    /// Open the requested file, returning its ID.
    pub fn open(&self, path: impl AsRef<str>) -> Result<BufferClient> {
        let path = path.as_ref();
        self.ctl("open", path)?;

        Ok(BufferClient {
            bufid: self._id_for_path(path)?,
            client: self.clone(),
        })
    }

    /// Open the requested file in a new window, returning its ID.
    pub fn open_in_new_window(&self, path: impl AsRef<str>) -> Result<BufferClient> {
        let path = path.as_ref();
        self.ctl("open-in-new-window", path)?;

        Ok(BufferClient {
            bufid: self._id_for_path(path)?,
            client: self.clone(),
        })
    }

    /// Open a new virtual file showing the given content, returning its ID.
    pub fn open_virtual(
        &self,
        name: impl AsRef<str>,
        content: impl AsRef<str>,
    ) -> Result<BufferClient> {
        let name = name.as_ref();
        let content = content.as_ref();
        self.ctl("open-virual", &format!("{name} {content}"))?;

        Ok(BufferClient {
            bufid: self._id_for_path(name)?,
            client: self.clone(),
        })
    }

    /// Open a new virtual file showing the given content in a new window, returning its ID.
    pub fn open_virtual_in_new_window(
        &self,
        name: impl AsRef<str>,
        content: impl AsRef<str>,
    ) -> Result<BufferClient> {
        let name = name.as_ref();
        let content = content.as_ref();
        self.ctl("open-virual-in-new-window", &format!("{name} {content}"))?;

        Ok(BufferClient {
            bufid: self._id_for_path(name)?,
            client: self.clone(),
        })
    }

    /// Reload the currently active buffer.
    pub fn reload_current_buffer(&self) -> Result<()> {
        self.ctl("reload", "")
    }

    /// Mark the currently active buffer as being clean.
    pub fn mark_clean(&self) -> Result<()> {
        self.ctl("mark-clean", "")
    }

    /// Run the provided ad Edit script against the current buffer
    pub fn run_edit_script(&self, script: impl AsRef<str>) -> Result<()> {
        self.ctl("Edit", script.as_ref())
    }

    /// Open the minibuffer with the provided `prompt` showing `lines`.
    ///
    /// If the user makes a selection (either from the provided lines or
    pub fn minibuffer_select<I, S>(&self, prompt: &str, lines: I) -> Result<MiniBufferSelection>
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
    pub fn minibuffer_prompt(&self, prompt: &str) -> Result<Option<String>> {
        self.inner.write_str("minibuffer", 0, "")?;
        self.ctl("minibuffer-prompt", prompt)?;
        let mut s = self.inner.read_str("minibuffer")?;
        if s.ends_with('\n') {
            s.pop();
        }

        if s.is_empty() { Ok(None) } else { Ok(Some(s)) }
    }
}

/// A [Client] scoped for interactions with a particular ad buffer.
#[derive(Debug, Clone)]
pub struct BufferClient {
    bufid: usize,
    client: Client,
}

impl Deref for BufferClient {
    type Target = Client;

    fn deref(&self) -> &Self::Target {
        &self.client
    }
}

impl BufferClient {
    /// The ID of the buffer this client is for.
    pub fn id(&self) -> usize {
        self.bufid
    }

    pub(crate) fn event_lines(&self) -> Result<ReadLineIter> {
        self.client
            .inner
            .iter_lines(format!("buffers/{}/event", self.bufid))
    }

    pub(crate) fn write_event(&self, event_line: &str) -> Result<()> {
        self.client
            .inner
            .write_str(format!("buffers/{}/event", self.bufid), 0, event_line)?;

        Ok(())
    }

    fn _read_buffer_file(&self, file: &str) -> Result<String> {
        self.client
            .inner
            .read_str(format!("buffers/{}/{file}", self.bufid))
    }

    fn _write_buffer_file(&self, file: &str, offset: u64, content: &[u8]) -> Result<usize> {
        self.client
            .inner
            .write(format!("buffers/{}/{file}", self.bufid), offset, content)
    }

    /// Read the contents of the dot of the given buffer
    pub fn read_dot(&self) -> Result<String> {
        self._read_buffer_file("dot")
    }

    /// Read the body of the given buffer.
    pub fn read_body(&self) -> Result<String> {
        self._read_buffer_file("body")
    }

    /// Read the current dot address of the given buffer.
    pub fn read_addr(&self) -> Result<String> {
        self._read_buffer_file("addr")
    }

    /// Read the filename of the given buffer
    pub fn read_filename(&self) -> Result<String> {
        self._read_buffer_file("filename")
    }

    /// Read the x-address of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xaddr(&self) -> Result<String> {
        self._read_buffer_file("xaddr")
    }

    /// Read the x-dot of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub fn read_xdot(&self) -> Result<String> {
        self._read_buffer_file("xdot")
    }

    /// Replace the dot of the given buffer with the provided string.
    pub fn write_dot(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("dot", 0, content.as_bytes())
    }

    /// Set the addr of the given buffer.
    pub fn write_addr(&self, addr: &str) -> Result<usize> {
        self._write_buffer_file("addr", 0, addr.as_bytes())
    }

    /// Replace the xdot of the given buffer with the provided string.
    pub fn write_xdot(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("xdot", 0, content.as_bytes())
    }

    /// Set the xaddr of the given buffer.
    pub fn write_xaddr(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("xaddr", 0, content.as_bytes())
    }

    /// Append the provided string to the given buffer.
    pub fn append_to_body(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("body", 0, content.as_bytes())
    }

    /// Clear the contents of the given buffer
    pub fn clear(&self) -> Result<()> {
        self.write_xaddr(",")?;
        self.write_xdot("")?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the beginning of the file
    pub fn cur_to_bof(&self) -> Result<()> {
        self.write_addr("0")?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the end of the file
    pub fn cur_to_eof(&self) -> Result<()> {
        self.write_addr("$")?;

        Ok(())
    }

    /// Run a provided [EventFilter] until it exits or errors
    pub fn run_event_filter<F>(&self, filter: F) -> Result<()>
    where
        F: EventFilter,
    {
        event::run_filter(filter, self)
    }

    /// Create a [Write] impl that can be used to continuously write to the given path
    pub fn body_writer(&self) -> Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{}/body", self.bufid),
            client: self.client.inner.clone(),
        })
    }
}

impl SessionMeta {}

/// A writer for appending to the body of a buffer
#[derive(Debug)]
pub struct BodyWriter {
    path: String,
    client: NinepClient,
}

impl BodyWriter {
    /// Mark the buffer as being clean
    pub fn mark_clean(&self) -> Result<()> {
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
        EventData, EventOutcome,
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
        let (client, _ted) = prepare(&[("foo", "foo content")]);
        let client = client.for_buffer(1);

        let fname = client.read_filename().unwrap();
        assert!(fname.ends_with("foo"), "{fname:?}");

        client.ctl("rename-buffer", "bar").unwrap();
        let fname = client.read_filename().unwrap();
        assert!(fname.ends_with("bar"), "{fname:?}");
    }

    #[test]
    fn manipulating_current_buffer_works() {
        let (client, _ted) = prepare(&[("foo", "foo content"), ("bar", "bar content")]);
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
        let (client, _ted) = prepare(&[("foo", "foo content")]);
        let client = client.for_buffer(1);

        let s = client.read_body().unwrap();
        assert_eq!(s, "foo content", "initial content");

        client.append_to_body(" new").unwrap();
        let s = client.read_body().unwrap();
        assert_eq!(s, "foo content new", "after append");

        client.clear().unwrap();
        let s = client.read_body().unwrap();
        assert_eq!(s, "", "after clear");
    }

    #[test]
    fn manipulating_addr_and_dot_works() {
        let (client, _ted) = prepare(&[("test", "This is a test")]);
        let client = client.for_buffer(1);

        assert_eq!(client.read_addr().unwrap(), "1:1", "initial");
        assert_eq!(client.read_dot().unwrap(), "T", "initial");

        client.write_addr("1:1,1:4").unwrap();
        assert_eq!(client.read_addr().unwrap(), "1:1,1:4", "write_addr");
        assert_eq!(client.read_dot().unwrap(), "This", "write_addr");

        client.write_dot("THIS").unwrap();
        assert_eq!(client.read_addr().unwrap(), "1:5", "write_dot");

        client.write_addr("1:1,1:4").unwrap();
        assert_eq!(client.read_dot().unwrap(), "THIS", "write_dot");
    }

    #[test]
    fn manipulating_xaddr_and_xdot_works() {
        let (client, _ted) = prepare(&[("test", "This is a test")]);
        let client = client.for_buffer(1);

        assert_eq!(client.read_xaddr().unwrap(), "1:1", "initial");
        assert_eq!(client.read_xdot().unwrap(), "T", "initial");

        client.write_xaddr("1:1,1:4").unwrap();
        assert_eq!(client.read_xaddr().unwrap(), "1:1,1:4", "write_xaddr");
        assert_eq!(
            client.read_addr().unwrap(),
            "1:1",
            "addr should be unchanged"
        );
        assert_eq!(client.read_xdot().unwrap(), "This", "write_xaddr");

        client.write_xdot("THIS").unwrap();
        assert_eq!(client.read_xaddr().unwrap(), "1:5", "write_xdot");

        client.write_xaddr("1:1,1:4").unwrap();
        assert_eq!(client.read_xdot().unwrap(), "THIS", "write_xdot");
    }

    #[test_case(&[Input::Char('a'), Input::Return], mbs_line(0, "alpha"); "type a")]
    #[test_case(&[Input::Char('b'), Input::Return], mbs_line(1, "bravo"); "type b")]
    #[test_case(&[Input::Char('x'), Input::Return], mbs_user("x"); "type x")]
    #[test_case(&[Input::Esc], mbs_cancelled(); "cancelled")]
    #[test]
    fn minibuffer_select_works(inputs: &[Input], expected: MiniBufferSelection) {
        let (client, ted) = prepare(&[]);
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
        let (client, ted) = prepare(&[]);
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
        fn on_load(
            &mut self,
            _data: EventData<'_>,
            _client: &BufferClient,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("load");

            Ok(EventOutcome::Exit)
        }

        fn on_execute(
            &mut self,
            _evt: EventData<'_>,
            _arg: Option<EventData<'_>>,
            _client: &BufferClient,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("execute");

            Ok(EventOutcome::Exit)
        }

        fn on_insert(
            &mut self,
            _data: EventData<'_>,
            _client: &BufferClient,
        ) -> Result<EventOutcome> {
            self.inner.lock().unwrap().push("insert");

            Ok(EventOutcome::Exit)
        }

        fn on_delete(
            &mut self,
            _data: EventData<'_>,
            _client: &BufferClient,
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
        let (client, ted) = prepare(&[("foo", "foo content")]);
        let client = client.for_buffer(1);

        let filter = TestFilter::default();
        let calls = Arc::clone(&filter.inner);

        let handle = spawn(move || client.run_event_filter(filter));
        sleep(Duration::from_millis(10)); // wait for the filter to attach

        _ = ted.tx.send(Event::Action(action));

        let res = handle.join().unwrap();
        assert!(res.is_ok(), "{res:?}");

        let recorded = calls.lock().unwrap();
        assert_eq!(*recorded, vec![expected]);
    }

    #[test]
    fn open_returns_correct_id() {
        let (client, ted) = prepare(&[]);
        let path = ted.write_file("test", "test content");

        let bclient = client.open(path).unwrap();
        assert_eq!(bclient.bufid, 1);

        let body = bclient.read_body().unwrap();
        assert_eq!(body, "test content");
    }

    #[test]
    fn open_in_new_window_returns_correct_id() {
        let (client, ted) = prepare(&[]);
        let path = ted.write_file("test", "test content");

        let bclient = client.open_in_new_window(path).unwrap();
        assert_eq!(bclient.bufid, 1);

        let body = bclient.read_body().unwrap();
        assert_eq!(body, "test content");
    }

    #[test]
    fn open_virtual_returns_correct_id() {
        let (client, _ted) = prepare(&[]);

        let bclient = client.open_virtual("+test", "test content").unwrap();
        assert_eq!(bclient.bufid, 1);

        let body = bclient.read_body().unwrap();
        assert_eq!(body, "test content");
    }

    #[test]
    fn event_data_try_full_text_works() {
        let (client, _ted) = prepare(&[("foo", "foo content")]);
        let client = client.for_buffer(1);

        let evt = EventData {
            source: Source::Fsys,
            ch_from: 0,
            ch_to: 2,
            txt: "foo",
            truncated: false,
            from_scratch: false,
        };

        let s = evt.try_full_text(&client).unwrap();
        assert_eq!(s, "foo");
    }
}
