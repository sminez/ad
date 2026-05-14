//! An asynchronous client implementation.
use crate::{BufferMeta, LogEvent, MiniBufferSelection, SessionMeta, parse_bufid};
use ninep::tokio::client::{Client as NinepClient, Error, ReadLineStream, Result};
use std::{env, io, ops::Deref, path::Path, str::FromStr, time::Duration};
use tokio::time::sleep;

mod event;

pub use event::AsyncEventFilter;

/// A simple synchronous 9p client for ad
#[derive(Debug, Clone)]
pub struct Client {
    inner: NinepClient,
}

impl Client {
    /// Create a new client connected to `ad` over it's 9p unix socket
    pub async fn new() -> Result<Self> {
        let ns = match env::var("AD_PID") {
            Ok(pid) => format!("ad-{pid}"),
            Err(_) => "ad".to_string(),
        };

        Ok(Self {
            inner: NinepClient::new_unix(&ns, "").await?,
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
            inner: NinepClient::new_unix(&ns, "").await?,
        })
    }

    /// Create a new client connected to the socket found at `path`.
    pub async fn new_with_explicit_path(path: impl AsRef<Path>) -> Result<Self> {
        let uname = match env::var("USER") {
            Ok(s) => s,
            Err(_) => return Err(io::Error::other("USER env var not set").into()),
        };

        Ok(Self {
            inner: NinepClient::new_unix_with_explicit_path(uname, path, "").await?,
        })
    }

    /// Iterate over the log events emitted by ad
    pub async fn log_events(&self) -> Result<LogStream> {
        Ok(LogStream {
            inner: self.inner.stream_lines("log").await?,
        })
    }

    /// Get the currently active buffer id.
    pub async fn current_buffer(&self) -> Result<usize> {
        let id = parse_bufid(&self.inner.read_str("buffers/current").await?)?;

        Ok(id)
    }

    /// Get the list of currently open buffers
    pub async fn open_buffers(&self) -> Result<Vec<BufferMeta>> {
        let buffers = self
            .inner
            .read_str("buffers/index")
            .await?
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
    pub async fn focus_buffer(&self, buffer_id: usize) -> Result<()> {
        self.inner
            .write_str("buffers/current", 0, &buffer_id.to_string())
            .await?;

        Ok(())
    }

    /// Read the contents of the scratch buffer
    pub async fn read_scratch(&self) -> Result<String> {
        self.inner.read_str("scratch").await
    }

    /// Read `count` bytes from the contents of the scratch buffer from a particular offset.
    pub async fn read_scratch_from(&self, byte_offset: u64, count: u32) -> Result<Vec<u8>> {
        self.inner.read_from("scratch", byte_offset, count).await
    }

    /// Append to the scratch buffer
    pub async fn append_scratch(&self, content: &str) -> Result<()> {
        self.inner.write_str("scratch", 0, content).await?;

        Ok(())
    }

    /// Send a control message to ad.
    pub async fn ctl(&self, command: &str, args: &str) -> Result<()> {
        self.inner
            .write("ctl", 0, format!("{command} {args}").as_bytes())
            .await?;

        Ok(())
    }

    /// Echo a string message in the status line.
    pub async fn echo(&self, msg: impl AsRef<str>) -> Result<()> {
        self.ctl("echo", msg.as_ref()).await
    }

    async fn _id_for_path(&self, path: &str) -> Result<usize> {
        sleep(Duration::from_millis(5)).await;
        for BufferMeta { id, filename } in self.open_buffers().await?.into_iter() {
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
    pub async fn open(&self, path: impl AsRef<str>) -> Result<BufferClient> {
        let path = path.as_ref();
        self.ctl("open", path).await?;

        Ok(BufferClient {
            bufid: self._id_for_path(path).await?,
            client: self.clone(),
        })
    }

    /// Open the requested file in a new window, returning its ID.
    pub async fn open_in_new_window(&self, path: impl AsRef<str>) -> Result<BufferClient> {
        let path = path.as_ref();
        self.ctl("open-in-new-window", path).await?;

        Ok(BufferClient {
            bufid: self._id_for_path(path).await?,
            client: self.clone(),
        })
    }

    /// Open a new virtual file showing the given content, returning its ID.
    pub async fn open_virtual(
        &self,
        name: impl AsRef<str>,
        content: impl AsRef<str>,
    ) -> Result<BufferClient> {
        let name = name.as_ref();
        let content = content.as_ref();
        self.ctl("open-virtual", &format!("{name} {content}"))
            .await?;

        Ok(BufferClient {
            bufid: self._id_for_path(name).await?,
            client: self.clone(),
        })
    }

    /// Open a new virtual file showing the given content in a new window, returning its ID.
    pub async fn open_virtual_in_new_window(
        &self,
        name: impl AsRef<str>,
        content: impl AsRef<str>,
    ) -> Result<BufferClient> {
        let name = name.as_ref();
        let content = content.as_ref();
        self.ctl("open-virtual-in-new-window", &format!("{name} {content}"))
            .await?;

        Ok(BufferClient {
            bufid: self._id_for_path(name).await?,
            client: self.clone(),
        })
    }

    /// Reload the currently active buffer.
    pub async fn reload_current_buffer(&self) -> Result<()> {
        self.ctl("reload", "").await
    }

    /// Mark the currently active buffer as being clean.
    pub async fn mark_clean(&self) -> Result<()> {
        self.ctl("mark-clean", "").await
    }

    /// Run the provided ad Edit script against the current buffer
    pub async fn run_edit_script(&self, script: impl AsRef<str>) -> Result<()> {
        self.ctl("Edit", script.as_ref()).await
    }

    /// Create a [BodyWriter] impl that can be used to continuously write to the given path
    pub async fn body_writer(&self, buffer_id: usize) -> Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{buffer_id}/body"),
            client: self.inner.clone(),
        })
    }

    /// Open the minibuffer with the provided `prompt` showing `lines`.
    ///
    /// If the user makes a selection (either from the provided lines or
    pub async fn minibuffer_select<I, S>(
        &self,
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
        let mut s = self.inner.read_str("minibuffer").await?;
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
    pub async fn minibuffer_prompt(&self, prompt: &str) -> Result<Option<String>> {
        self.inner.write_str("minibuffer", 0, "").await?;
        self.ctl("minibuffer-prompt", prompt).await?;
        let mut s = self.inner.read_str("minibuffer").await?;
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

    pub(crate) async fn event_lines(&self) -> Result<ReadLineStream> {
        self.client
            .inner
            .stream_lines(format!("buffers/{}/event", self.bufid))
            .await
    }

    pub(crate) async fn write_event(&self, event_line: &str) -> Result<()> {
        self.client
            .inner
            .write_str(format!("buffers/{}/event", self.bufid), 0, event_line)
            .await?;

        Ok(())
    }

    async fn _read_buffer_file(&self, file: &str) -> Result<String> {
        self.client
            .inner
            .read_str(format!("buffers/{}/{file}", self.bufid))
            .await
    }

    async fn _write_buffer_file(&self, file: &str, offset: u64, content: &[u8]) -> Result<usize> {
        self.client
            .inner
            .write(format!("buffers/{}/{file}", self.bufid), offset, content)
            .await
    }

    /// Read the contents of the dot of the given buffer
    pub async fn read_dot(&self) -> Result<String> {
        self._read_buffer_file("dot").await
    }

    /// Read the body of the given buffer.
    pub async fn read_body(&self) -> Result<String> {
        self._read_buffer_file("body").await
    }

    /// Read `count` bytes from the contents of the buffer body from a particular offset.
    pub async fn read_body_from(&self, byte_offset: u64, count: u32) -> Result<Vec<u8>> {
        self.inner
            .read_from(format!("buffers/{}/body", self.bufid), byte_offset, count)
            .await
    }

    /// Read the current dot address of the given buffer.
    pub async fn read_addr(&self) -> Result<String> {
        self._read_buffer_file("addr").await
    }

    /// Read the filename of the given buffer
    pub async fn read_filename(&self) -> Result<String> {
        self._read_buffer_file("filename").await
    }

    /// Read the x-address of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub async fn read_xaddr(&self) -> Result<String> {
        self._read_buffer_file("xaddr").await
    }

    /// Read the x-dot of the given buffer.
    ///
    /// This is only used by the filesystem interface of `ad` and will not affect the current
    /// editor state.
    pub async fn read_xdot(&self) -> Result<String> {
        self._read_buffer_file("xdot").await
    }

    /// Replace the dot of the given buffer with the provided string.
    pub async fn write_dot(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("dot", 0, content.as_bytes()).await
    }

    /// Set the addr of the given buffer.
    pub async fn write_addr(&self, addr: &str) -> Result<usize> {
        self._write_buffer_file("addr", 0, addr.as_bytes()).await
    }

    /// Replace the xdot of the given buffer with the provided string.
    pub async fn write_xdot(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("xdot", 0, content.as_bytes()).await
    }

    /// Set the xaddr of the given buffer.
    pub async fn write_xaddr(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("xaddr", 0, content.as_bytes())
            .await
    }

    /// Append the provided string to the given buffer.
    pub async fn append_to_body(&self, content: &str) -> Result<usize> {
        self._write_buffer_file("body", 0, content.as_bytes()).await
    }

    /// Clear the contents of the given buffer
    pub async fn clear(&self) -> Result<()> {
        self.write_xaddr(",").await?;
        self.write_xdot("").await?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the beginning of the file
    pub async fn cur_to_bof(&self) -> Result<()> {
        self.write_addr("0").await?;

        Ok(())
    }

    /// Set the cursor position for the given buffer to the end of the file
    pub async fn cur_to_eof(&self) -> Result<()> {
        self.write_addr("$").await?;

        Ok(())
    }

    /// Run a provided [AsyncEventFilter] until it exits or errors
    pub async fn run_event_filter<F>(&self, filter: F) -> Result<()>
    where
        F: AsyncEventFilter,
    {
        event::run_filter(filter, self).await
    }

    /// Create a [BodyWriter] that can be used to continuously write to the given path
    pub async fn body_writer(&self) -> Result<BodyWriter> {
        Ok(BodyWriter {
            path: format!("buffers/{}/body", self.bufid),
            client: self.client.inner.clone(),
        })
    }
}

impl SessionMeta {
    /// Create a new [Client] for this session.
    pub async fn async_client_for_session(&self) -> Result<Client> {
        Ok(Client {
            inner: NinepClient::new_unix(&self.socket_name, "/").await?,
        })
    }
}

/// An asynchronous stream of [LogEvent]s from an `ad` instance.
#[derive(Debug)]
pub struct LogStream {
    inner: ReadLineStream,
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

/// A writer for appending to the body of a buffer
#[derive(Debug)]
pub struct BodyWriter {
    path: String,
    client: NinepClient,
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
    use std::{sync::Arc, time::Duration};
    use tokio::{spawn, sync::Mutex, time::sleep};

    async fn prepare(files: &[(&str, &str)]) -> (Client, TestEditor) {
        let ted = TestEditor::prepare(files);
        let client = Client::new_with_explicit_path(ted.socket_path())
            .await
            .unwrap();

        (client, ted)
    }

    // We provide additional helper methods around some of the common `ctl` commands, but so long
    // as one interaction with `ctl` works, we don't need to exhaustively test all of the ad
    // command functionality.

    #[tokio::test]
    async fn ctl_works() {
        let (client, _ted) = prepare(&[("foo", "foo content")]).await;
        let client = client.for_buffer(1);

        let fname = client.read_filename().await.unwrap();
        assert!(fname.ends_with("foo"), "{fname:?}");

        client.ctl("rename-buffer", "bar").await.unwrap();
        let fname = client.read_filename().await.unwrap();
        assert!(fname.ends_with("bar"), "{fname:?}");
    }

    #[tokio::test]
    async fn manipulating_current_buffer_works() {
        let (client, _ted) = prepare(&[("foo", "foo content"), ("bar", "bar content")]).await;
        assert_eq!(
            client.current_buffer().await.unwrap(),
            2,
            "initial current buffer"
        );

        client.focus_buffer(1).await.unwrap();
        sleep(Duration::from_millis(5)).await;

        assert_eq!(
            client.current_buffer().await.unwrap(),
            1,
            "current buffer after focus"
        );
    }

    #[tokio::test]
    async fn manipulating_body_file_works() {
        let (client, _ted) = prepare(&[("foo", "foo content")]).await;
        let client = client.for_buffer(1);

        let s = client.read_body().await.unwrap();
        assert_eq!(s, "foo content", "initial content");

        client.append_to_body(" new").await.unwrap();
        let s = client.read_body().await.unwrap();
        assert_eq!(s, "foo content new", "after append");

        client.clear().await.unwrap();
        let s = client.read_body().await.unwrap();
        assert_eq!(s, "", "after clear");
    }

    #[tokio::test]
    async fn manipulating_addr_and_dot_works() {
        let (client, _ted) = prepare(&[("test", "This is a test")]).await;
        let client = client.for_buffer(1);

        assert_eq!(client.read_addr().await.unwrap(), "1:1", "initial");
        assert_eq!(client.read_dot().await.unwrap(), "T", "initial");

        client.write_addr("1:1,1:4").await.unwrap();
        assert_eq!(client.read_addr().await.unwrap(), "1:1,1:4", "write_addr");
        assert_eq!(client.read_dot().await.unwrap(), "This", "write_addr");

        client.write_dot("THIS").await.unwrap();
        assert_eq!(client.read_addr().await.unwrap(), "1:5", "write_dot");

        client.write_addr("1:1,1:4").await.unwrap();
        assert_eq!(client.read_dot().await.unwrap(), "THIS", "write_dot");
    }

    #[tokio::test]
    async fn manipulating_xaddr_and_xdot_works() {
        let (client, _ted) = prepare(&[("test", "This is a test")]).await;
        let client = client.for_buffer(1);

        assert_eq!(client.read_xaddr().await.unwrap(), "1:1", "initial");
        assert_eq!(client.read_xdot().await.unwrap(), "T", "initial");

        client.write_xaddr("1:1,1:4").await.unwrap();
        assert_eq!(client.read_xaddr().await.unwrap(), "1:1,1:4", "write_xaddr");
        assert_eq!(
            client.read_addr().await.unwrap(),
            "1:1",
            "addr should be unchanged"
        );
        assert_eq!(client.read_xdot().await.unwrap(), "This", "write_xaddr");

        client.write_xdot("THIS").await.unwrap();
        assert_eq!(client.read_xaddr().await.unwrap(), "1:5", "write_xdot");

        client.write_xaddr("1:1,1:4").await.unwrap();
        assert_eq!(client.read_xdot().await.unwrap(), "THIS", "write_xdot");
    }

    #[test_case(&[Input::Char('a'), Input::Return], mbs_line(0, "alpha"); "type a")]
    #[test_case(&[Input::Char('b'), Input::Return], mbs_line(1, "bravo"); "type b")]
    #[test_case(&[Input::Char('x'), Input::Return], mbs_user("x"); "type x")]
    #[test_case(&[Input::Esc], mbs_cancelled(); "cancelled")]
    #[tokio::test]
    async fn minibuffer_select_works(inputs: &[Input], expected: MiniBufferSelection) {
        let (client, ted) = prepare(&[]).await;
        let handle = spawn(async move { client.minibuffer_select("> ", ["alpha", "bravo"]).await });
        sleep(Duration::from_millis(10)).await; // wait for the minibuffer to open

        for input in inputs.iter() {
            ted.tx.send(Event::Input(*input)).unwrap();
        }

        let res = handle.await.unwrap();

        assert_eq!(res.unwrap(), expected);
    }

    #[test_case(&[Input::Char('a'), Input::Return], Some("a"); "user input")]
    #[test_case(&[Input::Esc], None; "cancelled")]
    #[tokio::test]
    async fn minibuffer_prompt_works(inputs: &[Input], expected: Option<&str>) {
        let (client, ted) = prepare(&[]).await;
        let handle = spawn(async move { client.minibuffer_prompt("> ").await });
        sleep(Duration::from_millis(10)).await; // wait for the minibuffer to open

        for input in inputs.iter() {
            ted.tx.send(Event::Input(*input)).unwrap();
        }

        let res = handle.await.unwrap();

        assert_eq!(res.unwrap().as_deref(), expected);
    }

    #[derive(Default)]
    struct TestFilter {
        inner: Arc<Mutex<Vec<&'static str>>>,
    }

    impl AsyncEventFilter for TestFilter {
        async fn on_load(
            &mut self,
            _data: EventData<'_>,
            _client: &Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().await.push("load");

            Ok(EventOutcome::Exit)
        }

        async fn on_execute(
            &mut self,
            _data: EventData<'_>,
            _arg: Option<EventData<'_>>,
            _client: &Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().await.push("execute");

            Ok(EventOutcome::Exit)
        }

        async fn on_insert(
            &mut self,
            _data: EventData<'_>,
            _client: &Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().await.push("insert");

            Ok(EventOutcome::Exit)
        }

        async fn on_delete(
            &mut self,
            _data: EventData<'_>,
            _client: &Client,
        ) -> Result<EventOutcome> {
            self.inner.lock().await.push("delete");

            Ok(EventOutcome::Exit)
        }
    }

    #[test_case(Action::LoadDot { new_window: false }, "load"; "load")]
    #[test_case(Action::ExecuteDot , "execute"; "execute")]
    #[test_case(Action::InsertChar { c: 'a' } , "insert"; "insert")]
    #[test_case(Action::Delete, "delete"; "delete")]
    #[tokio::test]
    async fn run_event_filter_works(action: Action, expected: &str) {
        let (client, ted) = prepare(&[("foo", "foo content")]).await;

        let filter = TestFilter::default();
        let calls = Arc::clone(&filter.inner);

        let handle = spawn(async move { client.for_buffer(1).run_event_filter(filter).await });
        sleep(Duration::from_millis(10)).await; // wait for the filter to attach

        _ = ted.tx.send(Event::Action(action));

        let res = handle.await.unwrap();
        assert!(res.is_ok(), "{res:?}");

        let recorded = calls.lock().await;
        assert_eq!(*recorded, vec![expected]);
    }

    #[tokio::test]
    async fn run_event_filter_doesnt_block_calls_from_clones() {
        let (client, ted) = prepare(&[("foo", "foo content")]).await;

        let filter = TestFilter::default();
        let filter_client = client.for_buffer(1);
        let _handle = spawn(async move { filter_client.run_event_filter(filter).await });
        sleep(Duration::from_millis(10)).await; // wait for the filter to attach

        let current_id = client.current_buffer().await.unwrap();
        assert_eq!(current_id, 1);

        _ = ted.tx.send(Event::Action(Action::InsertChar { c: 'a' }));

        let body = client.for_buffer(1).read_body().await.unwrap();
        assert_eq!(body, "afoo content");
    }

    #[tokio::test]
    async fn open_returns_correct_id() {
        let (client, ted) = prepare(&[]).await;
        let path = ted.write_file("test", "test content");

        let client = client.open(path).await.unwrap();
        assert_eq!(client.bufid, 1);

        let body = client.read_body().await.unwrap();
        assert_eq!(body, "test content");
    }

    #[tokio::test]
    async fn open_in_new_window_returns_correct_id() {
        let (client, ted) = prepare(&[]).await;
        let path = ted.write_file("test", "test content");

        let client = client.open_in_new_window(path).await.unwrap();
        assert_eq!(client.bufid, 1);

        let body = client.read_body().await.unwrap();
        assert_eq!(body, "test content");
    }

    #[tokio::test]
    async fn open_virtual_returns_correct_id() {
        let (client, _ted) = prepare(&[]).await;

        let client = client.open_virtual("+test", "test content").await.unwrap();
        assert_eq!(client.bufid, 1);

        let body = client.read_body().await.unwrap();
        assert_eq!(body, "test content");
    }

    #[tokio::test]
    async fn event_data_try_full_text_works() {
        let (client, _ted) = prepare(&[("foo", "foo content")]).await;
        let client = client.for_buffer(1);

        let evt = EventData {
            source: Source::Fsys,
            byte_from: 0,
            byte_to: 2,
            ch_from: 0,
            ch_to: 2,
            txt: "foo",
            truncated: false,
            from_scratch: false,
        };

        let s = evt.try_full_text_async(&client).await.unwrap();
        assert_eq!(s, "foo");
    }
}
