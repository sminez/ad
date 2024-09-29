//! Editor actions in response to user input
use crate::{
    buffer::BufferKind,
    config::Config,
    config_handle, die,
    dot::{Cur, Dot, TextObject},
    editor::{Editor, MiniBufferSelection},
    exec::{Addr, Address, Program},
    fsys::LogEvent,
    key::Input,
    mode::Mode,
    plumb::{MatchOutcome, PlumbingMessage},
    replace_config, update_config,
    util::{
        pipe_through_command, read_clipboard, run_command, run_command_blocking, set_clipboard,
    },
};
use std::{
    env, fs,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::mpsc::Sender,
};
use tracing::{debug, error, info, trace, warn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Actions {
    Single(Action),
    Multi(Vec<Action>),
}

/// How the current viewport should be set in relation to dot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ViewPort {
    /// Dot at the bottom of the viewport
    Bottom,
    /// Dot in the center of the viewport
    Center,
    /// Dot at the top of the viewport
    Top,
}

/// Supported actions for interacting with the editor state
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Action {
    AppendToOutputBuffer { bufid: usize, content: String },
    ChangeDirectory { path: Option<String> },
    CommandMode,
    Delete,
    DeleteBuffer { force: bool },
    DotCollapseFirst,
    DotCollapseLast,
    DotExtendBackward(TextObject, usize),
    DotExtendForward(TextObject, usize),
    DotFlip,
    DotSet(TextObject, usize),
    EditCommand { cmd: String },
    ExecuteDot,
    Exit { force: bool },
    FindFile,
    FindRepoFile,
    FocusBuffer { id: usize },
    InsertChar { c: char },
    InsertString { s: String },
    JumpListForward,
    JumpListBack,
    LoadDot,
    MarkClean { bufid: usize },
    NewEditLogTransaction,
    NextBuffer,
    OpenFile { path: String },
    Paste,
    PreviousBuffer,
    RawInput { i: Input },
    Redo,
    ReloadActiveBuffer,
    ReloadBuffer { id: usize },
    ReloadConfig,
    RunMode,
    SamMode,
    SaveBuffer { force: bool },
    SaveBufferAs { path: String, force: bool },
    SearchInCurrentBuffer,
    SelectBuffer,
    SetViewPort(ViewPort),
    SetMode { m: &'static str },
    SetStatusMessage { message: String },
    ShellPipe { cmd: String },
    ShellReplace { cmd: String },
    ShellRun { cmd: String },
    ShellSend { cmd: String },
    Undo,
    UpdateConfig { input: String },
    ViewLogs,
    Yank,

    DebugBufferContents,
    DebugEditLog,
}

impl Editor {
    pub(crate) fn change_directory(&mut self, opt_path: Option<String>) {
        let p = match opt_path {
            Some(p) => p,
            None => match env::var("HOME") {
                Ok(p) => p,
                Err(e) => {
                    let msg = format!("Unable to determine home directory: {e}");
                    self.set_status_message(&msg);
                    warn!("{msg}");
                    return;
                }
            },
        };

        let new_cwd = match fs::canonicalize(p) {
            Ok(cwd) => cwd,
            Err(e) => {
                self.set_status_message(&format!("Invalid path: {e}"));
                return;
            }
        };

        if let Err(e) = env::set_current_dir(&new_cwd) {
            let msg = format!("Unable to set working directory: {e}");
            self.set_status_message(&msg);
            error!("{msg}");
            return;
        };

        debug!(new_cwd=%new_cwd.as_os_str().to_string_lossy(), "setting working directory");
        self.cwd = new_cwd;
        self.set_status_message(&self.cwd.display().to_string());
    }

    /// Open a file within the editor using a path that is relative to the current working
    /// directory
    pub fn open_file_relative_to_cwd(&mut self, path: &str) {
        self.open_file(self.cwd.join(path));
    }

    /// Open a file within the editor
    pub fn open_file<P: AsRef<Path>>(&mut self, path: P) {
        let path = path.as_ref();
        debug!(?path, "opening file");
        let was_empty_scratch = self.buffers.is_empty_scratch();
        let current_id = self.active_buffer_id();

        match self.buffers.open_or_focus(path) {
            Err(e) => self.set_status_message(&format!("Error opening file: {e}")),

            Ok(Some(new_id)) => {
                if was_empty_scratch {
                    _ = self.tx_fsys.send(LogEvent::Close(current_id));
                }
                _ = self.tx_fsys.send(LogEvent::Open(new_id));
                _ = self.tx_fsys.send(LogEvent::Focus(new_id));
            }

            Ok(None) => {
                match self.buffers.active().state_changed_on_disk() {
                    Ok(true) => {
                        let res = self.minibuffer_prompt("File changed on disk, reload? [y/n]: ");
                        if let Some("y" | "Y" | "yes") = res.as_deref() {
                            let msg = self.buffers.active_mut().reload_from_disk();
                            self.set_status_message(&msg);
                        }
                    }
                    Ok(false) => (),
                    Err(e) => self.set_status_message(&e),
                }
                let id = self.active_buffer_id();
                if id != current_id {
                    _ = self.tx_fsys.send(LogEvent::Focus(id));
                }
            }
        };
    }

    fn find_file_under_dir(&mut self, d: &Path) {
        let cmd = config_handle!().find_command.clone();

        let selection = match cmd.split_once(' ') {
            Some((cmd, args)) => {
                self.minibuffer_select_from_command_output("> ", cmd, args.split_whitespace(), d)
            }
            None => self.minibuffer_select_from_command_output(
                "> ",
                &cmd,
                std::iter::empty::<&str>(),
                d,
            ),
        };

        if let MiniBufferSelection::Line { line, .. } = selection {
            self.open_file_relative_to_cwd(&format!("{}/{}", d.display(), line.trim()));
        }
    }

    /// This shells out to the fd command line program
    pub(crate) fn find_file(&mut self) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd).to_owned();
        self.find_file_under_dir(&d);
    }

    /// This shells out to the git and fd command line programs
    pub(crate) fn find_repo_file(&mut self) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd).to_owned();
        let s = match run_command_blocking(
            "git",
            ["rev-parse", "--show-toplevel"],
            &d,
            self.active_buffer_id(),
        ) {
            Ok(s) => s,
            Err(e) => {
                self.set_status_message(&format!("unable to find git root: {e}"));
                return;
            }
        };

        let root = Path::new(s.trim());
        self.find_file_under_dir(root);
    }

    pub(crate) fn delete_buffer(&mut self, id: usize, force: bool) {
        match self.buffers.with_id(id) {
            Some(b) if b.dirty && !force => self.set_status_message("No write since last change"),
            None => warn!("attempt to close unknown buffer, id={id}"),
            _ => {
                let is_last_buffer = self.buffers.len() == 1;
                _ = self.tx_fsys.send(LogEvent::Close(id));
                self.clear_input_filter(id);
                self.buffers.close_buffer(id);
                self.running = !is_last_buffer;
            }
        }
    }

    pub(crate) fn mark_clean(&mut self, bufid: usize) {
        if let Some(b) = self.buffers.with_id_mut(bufid) {
            b.dirty = false;
        }
    }

    pub(super) fn save_current_buffer(&mut self, fname: Option<String>, force: bool) {
        trace!("attempting to save current buffer");
        let p = match self.get_buffer_save_path(fname) {
            Some(p) => p,
            None => return,
        };

        let msg = self.buffers.active_mut().save_to_disk_at(p, force);
        self.set_status_message(&msg);
        let id = self.active_buffer_id();
        _ = self.tx_fsys.send(LogEvent::Save(id));
    }

    fn get_buffer_save_path(&mut self, fname: Option<String>) -> Option<PathBuf> {
        use BufferKind as Bk;

        let desired_path = match (fname, &self.buffers.active().kind) {
            // File has a known name which is either where we loaded it from or a
            // path that has been set and verified from the Some(s) case that follows
            (None, Bk::File(ref p)) => return Some(p.clone()),
            // Renaming an existing file or attempting to save a new file created in
            // the editor: both need verifying
            (Some(s), Bk::File(_) | Bk::Unnamed) => PathBuf::from(s),
            // Attempting to save without a name so we prompt for one and verify it
            (None, Bk::Unnamed) => match self.minibuffer_prompt("Save As: ") {
                Some(s) => s.into(),
                None => return None,
            },
            // virtual and minibuffer buffers don't support saving and have no save path
            (_, Bk::Directory(_) | Bk::Virtual(_) | Bk::Output(_) | Bk::MiniBuffer) => return None,
        };

        match desired_path.try_exists() {
            Ok(false) => (),
            Ok(true) => {
                if !self.minibuffer_confirm("File already exists") {
                    return None;
                }
            }
            Err(e) => {
                self.set_status_message(&format!("Unable to check path: {e}"));
                return None;
            }
        }

        self.buffers.active_mut().kind = BufferKind::File(desired_path.clone());

        Some(desired_path)
    }

    pub(super) fn reload_buffer(&mut self, id: usize) {
        let msg = match self.buffers.with_id_mut(id) {
            Some(b) => b.reload_from_disk(),
            // Silently ignoring attempts to reload unknown buffers
            None => return,
        };

        self.set_status_message(&msg);
    }

    pub(super) fn reload_config(&mut self) {
        info!("reloading config");
        let msg = match Config::try_load() {
            Ok(config) => {
                replace_config(config);
                "config reloaded".to_string()
            }
            Err(s) => s,
        };
        info!("{msg}");

        self.set_status_message(&msg);
    }

    pub(super) fn reload_active_buffer(&mut self) {
        let msg = self.buffers.active_mut().reload_from_disk();
        self.set_status_message(&msg);
    }

    pub(super) fn update_config(&mut self, input: &str) {
        info!(%input, "updating config");
        if let Err(msg) = update_config(input) {
            self.set_status_message(&msg);
        }
    }

    pub(super) fn set_mode(&mut self, name: &str) {
        if let Some((i, _)) = self.modes.iter().enumerate().find(|(_, m)| m.name == name) {
            self.modes.swap(0, i);
            // FIXME: This should be a message to the UI rather than the editor itself writing to
            // stdout
            let cur_shape = self.modes[0].cur_shape.to_string();
            if let Err(e) = self.stdout.write_all(cur_shape.as_bytes()) {
                // In this situation we're probably not going to be able to do all that much
                // but we might as well try
                die!("Unable to write to stdout: {e}");
            };
        }
    }

    pub(super) fn exit(&mut self, force: bool) {
        let dirty_buffers = self.buffers.dirty_buffers();
        if !dirty_buffers.is_empty() && !force {
            self.set_status_message("No write since last change. Use ':q!' to force exit");
            self.minibuffer_select_from("No write since last change> ", dirty_buffers);
            return;
        }

        self.running = false;
    }

    pub(super) fn set_clipboard(&mut self, s: String) {
        trace!("setting clipboard content");
        match set_clipboard(&s) {
            Ok(_) => self.set_status_message("Yanked selection to system clipboard"),
            Err(e) => self.set_status_message(&format!("Error setting system clipboard: {e}")),
        }
    }

    pub(super) fn paste_from_clipboard(&mut self) {
        trace!("pasting from clipboard");
        match read_clipboard() {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error reading system clipboard: {e}")),
        }
    }

    pub(super) fn search_in_current_buffer(&mut self) {
        let numbered_lines = self
            .buffers
            .active()
            .string_lines()
            .into_iter()
            .enumerate()
            .map(|(i, line)| format!("{:>4} | {}", i + 1, line))
            .collect();

        let selection = self.minibuffer_select_from("> ", numbered_lines);
        if let MiniBufferSelection::Line { cy, .. } = selection {
            self.buffers.active_mut().dot = Dot::Cur {
                c: Cur::from_yx(cy, 0, self.buffers.active()),
            };
            self.handle_action(Action::DotSet(TextObject::Line, 1));
            self.handle_action(Action::SetViewPort(ViewPort::Center));
        }
    }

    pub(super) fn fsys_minibuffer(
        &mut self,
        prompt: Option<String>,
        lines: String,
        tx: Sender<String>,
    ) {
        let lines: Vec<String> = lines.split('\n').map(|s| s.to_string()).collect();
        let prompt: &str = prompt.as_deref().unwrap_or("> ");

        let selection = self.minibuffer_select_from(prompt, lines);
        let s = match selection {
            MiniBufferSelection::Line { line, .. } => line,
            MiniBufferSelection::UserInput { input } => input,
            MiniBufferSelection::Cancelled => String::new(),
        };

        _ = tx.send(s);
    }

    pub(super) fn select_buffer(&mut self) {
        let selection = self.minibuffer_select_from("> ", self.buffers.as_buf_list());
        if let MiniBufferSelection::Line { line, .. } = selection {
            // unwrap is fine here because we know the format of the buf list we are supplying
            if let Ok(id) = line.split_once(' ').unwrap().0.parse::<usize>() {
                self.focus_buffer(id);
            }
        }
    }

    pub(super) fn focus_buffer(&mut self, id: usize) {
        self.buffers.focus_id(id);
        _ = self.tx_fsys.send(LogEvent::Focus(id));
    }

    pub(super) fn debug_buffer_contents(&mut self) {
        self.minibuffer_select_from(
            "<RAW BUFFER> ",
            self.buffers
                .active()
                .string_lines()
                .into_iter()
                .map(|l| format!("{:?}", l))
                .collect(),
        );
    }

    pub(super) fn view_logs(&mut self) {
        self.open_virtual("+logs", self.log_buffer.content())
    }

    pub(super) fn debug_edit_log(&mut self) {
        self.minibuffer_select_from("<EDIT LOG> ", self.buffers.active().debug_edit_log());
    }

    /// Default semantics for attempting to load the current dot:
    ///   - an event filter is in place -> pass to the event filter
    ///   - a plumbing rule matches the load -> run the plumbing rule
    ///   - a relative path from the directory of the containing file -> open in ad
    ///   - an absolute path -> open in ad
    ///     - if either have a valid addr following a colon then set dot to that addr
    ///   - search within the current buffer for the next occurance of dot and select it
    ///
    /// Loading and executing of dot is part of what makes ad an unsual editor. The semantics are
    /// lifted almost directly from acme on plan9 and the curious user is encouraged to read the
    /// materials available at http://acme.cat-v.org/ to learn more about what is possible with
    /// such a system.
    pub(super) fn default_load_dot(&mut self) {
        let b = self.buffers.active_mut();
        b.expand_cur_dot();
        if b.notify_load() {
            return; // input filter in place
        }

        let s = b.dot.content(b);
        let id = b.id;

        let m = PlumbingMessage {
            src: Some("ad".to_string()),
            dst: None,
            wdir: b.dir().map(|p| p.display().to_string()),
            attrs: Default::default(),
            data: s.clone(),
        };

        match self.plumbing_rules.plumb(m) {
            Some(MatchOutcome::Message(m)) => self.handle_plumbing_message(m),

            Some(MatchOutcome::Run(cmd)) => {
                let mut command = Command::new("sh");
                command
                    .args(["-c", cmd.as_str()])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null());
                if let Err(e) = command.spawn() {
                    self.set_status_message(&format!("error spawning process: {e}"));
                };
            }

            None => self.load_explicit_string(id, s),
        }
    }

    /// Handling of plumbing messages that are sent to ad supports several attributes
    /// which can be set in order to configure the behaviour:
    ///   - by default the data will be treated as a filepath and opened
    ///   - if the attr "addr" is set it will be parsed as an Addr and applied
    ///   - if the attr "action" is set to "showdata" then a new buffer is created to hold the data
    ///     - if the attr "filename" is set as well then it will be used as the name for the buffer
    ///     - otherwise the filename will be "+plumbing-message"
    fn handle_plumbing_message(&mut self, m: PlumbingMessage) {
        let PlumbingMessage { attrs, data, .. } = m;
        match attrs.get("action") {
            Some(s) if s == "showdata" => {
                let filename = attrs
                    .get("filename")
                    .cloned()
                    .unwrap_or_else(|| "+plumbing-message".to_string());
                self.open_virtual(filename, data);
            }
            _ => {
                self.open_file(data);
                if let Some(s) = attrs.get("addr") {
                    match Addr::parse(&mut s.chars().peekable()) {
                        Ok(mut addr) => {
                            let b = self.buffers.active_mut();
                            b.map_addr(&mut addr);
                        }
                        Err(e) => self.set_status_message(&format!("malformed addr: {e:?}")),
                    }
                }
            }
        }
    }

    pub(super) fn load_explicit_string(&mut self, bufid: usize, s: String) {
        let b = match self.buffers.with_id_mut(bufid) {
            Some(b) => b,
            None => return,
        };

        let (maybe_path, maybe_addr) = match s.find(':') {
            Some(idx) => {
                let (s, addr) = s.split_at(idx);
                let (_, addr) = addr.split_at(1);
                match Addr::parse(&mut addr.chars().peekable()) {
                    Ok(expr) => (s, Some(expr)),
                    Err(_) => (s, None),
                }
            }
            None => (s.as_str(), None),
        };

        let mut path = Path::new(&maybe_path).to_path_buf();
        let mut is_file = path.is_absolute() && path.exists();

        if let (false, Some(dir)) = (is_file, b.dir()) {
            let full_path = dir.join(&path);
            if full_path.exists() {
                path = full_path;
                is_file = true;
            }
        }

        if is_file {
            self.open_file(path);
            if let Some(mut addr) = maybe_addr {
                let b = self.buffers.active_mut();
                b.dot = b.map_addr(&mut addr);
            }
        } else {
            b.find_forward(&s);
        }
    }

    /// Default semantics for attempting to execute the current dot:
    ///   - an event filter is in place -> pass to the event filter
    ///   - a valid ad command -> execute the command
    ///   - attempt to run as a shell command with args
    ///
    /// Loading and executing of dot is part of what makes ad an unsual editor. The semantics are
    /// lifted almost directly from acme on plan9 and the curious user is encouraged to read the
    /// materials available at http://acme.cat-v.org/ to learn more about what is possible with
    /// such a system.
    pub(super) fn default_execute_dot(&mut self) {
        let b = self.buffers.active_mut();
        b.expand_cur_dot();
        if b.notify_execute() {
            return; // input filter in place
        }

        let cmd = b.dot.content(b);

        match self.parse_command(cmd.trim_end()) {
            Some(actions) => self.handle_actions(actions),
            None => self.run_shell_cmd(&cmd),
        }
    }

    pub(super) fn execute_explicit_string(&mut self, bufid: usize, s: String) {
        let current_id = self.active_buffer_id();
        self.buffers.focus_id_silent(bufid);

        match self.parse_command(s.trim_end()) {
            Some(actions) => self.handle_actions(actions),
            None => self.run_shell_cmd(&s),
        }

        self.buffers.focus_id_silent(current_id);
    }

    pub(super) fn execute_command(&mut self, cmd: &str) {
        debug!(%cmd, "executing command");
        if let Some(actions) = self.parse_command(cmd.trim_end()) {
            self.handle_actions(actions);
        }
    }

    pub(super) fn execute_edit_command(&mut self, cmd: &str) {
        debug!(%cmd, "executing edit command");
        let mut prog = match Program::try_parse(cmd) {
            Ok(prog) => prog,
            Err(error) => {
                warn!(?error, "invalid edit command");
                self.set_status_message(&format!("Invalid edit command: {error:?}"));
                return;
            }
        };

        let mut buf = Vec::new();
        let fname = self.buffers.active().full_name().to_string();
        match prog.execute(self.buffers.active_mut(), &fname, &mut buf) {
            Ok(new_dot) => self.buffers.active_mut().dot = new_dot,
            Err(e) => self.set_status_message(&format!("Error running edit command: {e:?}")),
        }

        // FIXME: this is just using a selection mini-buffer for now to test things out. Ideally
        // this should be a scratchpad that we can dismiss and bring back but that will require
        // support in the main Buffers struct and a new way of creating a MiniBuffer.
        if !buf.is_empty() {
            self.minibuffer_select_from(
                "%>",
                String::from_utf8(buf)
                    .unwrap()
                    .lines()
                    .map(|l| l.to_string())
                    .collect(),
            );
        }
    }

    pub(super) fn command_mode(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("COMMAND"));

        if let Some(input) = self.minibuffer_prompt(":") {
            self.execute_command(&input);
        }

        self.modes.remove(0);
    }

    pub(super) fn run_mode(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("RUN"));

        if let Some(input) = self.minibuffer_prompt("!") {
            self.run_shell_cmd(&input);
        }

        self.modes.remove(0);
    }

    pub(super) fn sam_mode(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("EDIT"));

        if let Some(input) = self.minibuffer_prompt("Edit> ") {
            self.execute_edit_command(&input);
        };

        self.modes.remove(0);
    }

    pub(super) fn pipe_dot_through_shell_cmd(&mut self, raw_cmd_str: &str) {
        let (s, d) = {
            let b = self.buffers.active();
            (b.dot_contents(), b.dir().unwrap_or(&self.cwd))
        };

        let id = self.active_buffer_id();
        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => pipe_through_command(cmd, rest.split_whitespace(), &s, d, id),
            None => pipe_through_command(raw_cmd_str, std::iter::empty::<&str>(), &s, d, id),
        };

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    pub(super) fn replace_dot_with_shell_cmd(&mut self, raw_cmd_str: &str) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd);
        let id = self.active_buffer_id();
        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => run_command_blocking(cmd, rest.split_whitespace(), d, id),
            None => run_command_blocking(raw_cmd_str, std::iter::empty::<&str>(), d, id),
        };

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    pub(super) fn run_shell_cmd(&mut self, raw_cmd_str: &str) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd);
        let id = self.active_buffer_id();
        match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => {
                run_command(cmd, rest.split_whitespace(), d, id, self.tx_events.clone())
            }
            None => run_command(
                raw_cmd_str,
                std::iter::empty::<&str>(),
                d,
                id,
                self.tx_events.clone(),
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{editor::EditorMode, LogBuffer, PlumbingRules};
    use simple_test_case::test_case;

    macro_rules! assert_recv {
        ($brx:expr, $msg:ident, $expected:expr) => {
            match $brx.try_recv() {
                Ok(LogEvent::$msg(id)) if id == $expected => (),
                Ok(msg) => panic!(
                    "expected {}({}) but got {msg:?}",
                    stringify!($msg),
                    $expected
                ),
                Err(e) => panic!(
                    "err={e}
recv {}({})",
                    stringify!($msg),
                    $expected
                ),
            }
        };
    }

    #[test]
    fn opening_a_file_sends_the_correct_fsys_messages() {
        let mut ed = Editor::new(
            Config::default(),
            PlumbingRules::default(),
            EditorMode::Headless,
            LogBuffer::default(),
        );
        let brx = ed.rx_fsys.take().expect("to have fsys channels");

        ed.open_file("foo");

        // The first open should also close our scratch buffer
        assert_recv!(brx, Close, 0);
        assert_recv!(brx, Open, 1);
        assert_recv!(brx, Focus, 1);

        // Opening a second file should only notify for that file
        ed.open_file("bar");
        assert_recv!(brx, Open, 2);
        assert_recv!(brx, Focus, 2);

        // Opening the first file again should just notify for the current file
        ed.open_file("foo");
        assert_recv!(brx, Focus, 1);
    }

    #[test_case(&[], &[0]; "empty scratch")]
    #[test_case(&["foo"], &[1]; "one file")]
    #[test_case(&["foo", "bar"], &[1, 2]; "two files")]
    #[test]
    fn ensure_correct_fsys_state_works(files: &[&str], expected_ids: &[usize]) {
        let mut ed = Editor::new(
            Config::default(),
            PlumbingRules::default(),
            EditorMode::Headless,
            LogBuffer::default(),
        );
        let brx = ed.rx_fsys.take().expect("to have fsys channels");

        for file in files {
            ed.open_file(file);
        }

        ed.ensure_correct_fsys_state();

        if !files.is_empty() {
            assert_recv!(brx, Close, 0);
        }

        for &expected in expected_ids {
            assert_recv!(brx, Open, expected);
            assert_recv!(brx, Focus, expected);
        }
    }
}
