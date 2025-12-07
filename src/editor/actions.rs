//! Editor actions in response to user input
use crate::{
    buffer::BufferKind,
    config::Config,
    config_handle,
    dot::{Cur, Dot, Range, TextObject},
    editor::{Editor, MbSelector, MiniBufferSelection},
    exec::{Addr, Address, EditorRunner, Program},
    fsys::LogEvent,
    key::{Arrow, Input},
    lsp::Coords,
    mode::Mode,
    plumb::{MatchOutcome, PlumbingMessage},
    system::System,
    ui::{StateChange, UserInterface},
    util::gen_help_docs,
};
use ad_event::Source;
use std::{
    env, fs,
    path::{Path, PathBuf},
    process::{Command, Stdio},
    sync::mpsc::Sender,
};
use tracing::{debug, error, info, trace, warn};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Actions {
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
pub enum Action {
    Noop,

    AppendToOutputBuffer { bufid: usize, content: String },
    BalanceActiveColumn,
    BalanceAll,
    BalanceColumns,
    BalanceWindows,
    ChangeDirectory { path: Option<String> },
    CleanupChild { id: u32 },
    ClearScratch,
    CommandMode,
    Delete,
    DeleteBuffer { force: bool },
    DeleteColumn { force: bool },
    DeleteWindow { force: bool },
    DotCollapseFirst,
    DotCollapseLast,
    DotExtendBackward(TextObject, usize),
    DotExtendForward(TextObject, usize),
    DotFlip,
    DotSet(TextObject, usize),
    DotSetFromCoords { coords: Coords },
    DragWindow { direction: Arrow },
    EditCommand { cmd: String },
    EnsureFileIsOpen { path: String },
    ExecuteDot,
    ExecuteString { s: String },
    Exit { force: bool },
    ExpandDot,
    FindFile { new_window: bool },
    FindRepoFile { new_window: bool },
    FocusBuffer { id: usize },
    InsertChar { c: char },
    InsertString { s: String },
    JumpListForward,
    JumpListBack,
    KillRunningChild,
    LoadDot { new_window: bool },
    LspCompletion,
    LspFormat,
    LspGotoDeclaration,
    LspGotoDefinition,
    LspGotoTypeDefinition,
    LspHover,
    LspReferences,
    LspRename,
    LspRenamePrepare,
    LspShowCapabilities,
    LspShowDiagnostics,
    LspStart,
    LspStop,
    MarkClean { bufid: usize },
    MbSelect(MbSelector),
    NewEditLogTransaction,
    NewColumn,
    NewWindow,
    NextBuffer,
    NextColumn,
    NextWindowInColumn,
    OpenFile { path: String },
    OpenFileInNewWindow { path: String },
    OpenTransientScratch { name: String, txt: String },
    OpenVirtualFile { name: String, txt: String },
    Paste,
    Plumb { txt: String, new_window: bool },
    PreviousBuffer,
    PreviousColumn,
    PreviousWindowInColumn,
    RawInput { i: Input },
    Redo,
    ReloadActiveBuffer,
    ReloadBuffer { id: usize },
    ReloadConfig,
    RenameActiveBuffer { name: String },
    ResizeActiveColumn { delta: i16 },
    ResizeActiveWindow { delta: i16 },
    RunMode,
    SamMode,
    SaveBuffer { force: bool },
    SaveBufferAll { force: bool },
    SaveBufferAs { path: String, force: bool },
    SearchInCurrentBuffer,
    SendKeys { ks: Vec<Input> },
    SelectBuffer,
    SetViewPort(ViewPort),
    SetMode { m: &'static str },
    SetStatusMessage { message: String },
    ShellPipe { cmd: String },
    ShellReplace { cmd: String },
    ShellRun { cmd: String },
    ShellSend { cmd: String },
    ShowHelp,
    ToggleScratch,
    TsShowTree,
    Undo,
    ViewLogs,
    XDotSetFromCoords { coords: Coords },
    XInsertString { s: String },
    Yank,

    DebugBufferContents,
    DebugEditLog,
}

impl<S> Editor<S>
where
    S: System,
{
    pub(crate) fn change_directory(&mut self, opt_path: Option<String>) {
        let p = match opt_path {
            Some(p) => p,
            None => match env::var("HOME") {
                Ok(p) => p,
                Err(e) => {
                    let msg = format!("Unable to determine home directory: {e}");
                    warn!("{msg}");
                    self.set_status_message(msg);
                    return;
                }
            },
        };

        let new_cwd = match fs::canonicalize(p) {
            Ok(cwd) => cwd,
            Err(e) => {
                self.set_status_message(format!("Invalid path: {e}"));
                return;
            }
        };

        if let Err(e) = env::set_current_dir(&new_cwd) {
            let msg = format!("Unable to set working directory: {e}");
            error!("{msg}");
            self.set_status_message(msg);
            return;
        };

        debug!(new_cwd=%new_cwd.as_os_str().to_string_lossy(), "setting working directory");
        self.cwd = new_cwd;
        self.set_status_message(self.cwd.display().to_string());
    }

    /// Open a file within the editor using a path that is relative to the effective
    /// directory
    pub fn open_file_relative_to_effective_directory(&mut self, path: &str, new_window: bool) {
        self.open_file(self.effective_directory().join(path), new_window);
    }

    /// Open a file within the editor using a path that is relative to the current working
    /// directory
    pub fn open_file_relative_to_cwd(&mut self, path: impl AsRef<Path>, new_window: bool) {
        self.open_file(self.cwd.join(path), new_window);
    }

    /// Open a new virtual buffer within the editor.
    pub fn open_virtual(
        &mut self,
        name: impl Into<String>,
        content: impl Into<String>,
        new_window: bool,
    ) {
        self.layout.open_virtual(name, content, new_window)
    }

    /// Open a file within the editor
    pub fn open_file<P: AsRef<Path>>(&mut self, path: P, new_window: bool) {
        let path = path.as_ref();
        debug!(?path, "opening file");
        let was_empty_scratch = self.layout.is_empty_squirrel();
        let current_id = self.active_buffer_id();

        match self.layout.open_or_focus(path, new_window) {
            Err(e) => self.set_status_message(format!("Error opening file: {e}")),

            Ok(Some(new_id)) => {
                if was_empty_scratch {
                    _ = self.tx_fsys.send(LogEvent::Close(current_id));
                }
                _ = self.tx_fsys.send(LogEvent::Open(new_id));
                _ = self.tx_fsys.send(LogEvent::Focus(new_id));
            }

            Ok(None) => {
                match self
                    .layout
                    .active_buffer_ignoring_scratch()
                    .state_changed_on_disk()
                {
                    Ok(true) => {
                        let res = self.minibuffer_prompt("File changed on disk, reload? [y/n]: ");
                        if let Some("y" | "Y" | "yes") = res.as_deref() {
                            let b = self.layout.active_buffer_mut_ignoring_scratch();
                            let msg = b.reload_from_disk();
                            self.lsp_manager.document_changed(b);
                            self.set_status_message(&msg);
                        }
                    }
                    Ok(false) => (),
                    Err(e) => self.set_status_message(e),
                }
                let id = self.active_buffer_id();
                if id != current_id {
                    _ = self.tx_fsys.send(LogEvent::Focus(id));
                }
            }
        };
    }

    fn find_file_under_dir(&mut self, d: &Path, new_window: bool) {
        let cmd = config_handle!(self).find_command.clone();
        let selection = self.minibuffer_select_from_command_output("> ", &cmd, d);

        if let MiniBufferSelection::Line { line, .. } = selection {
            self.open_file(d.join(line.trim()), new_window);
        }
    }

    /// This shells out to the fd command line program
    pub(crate) fn find_file(&mut self, new_window: bool) {
        let d = self.effective_directory().to_owned();
        self.find_file_under_dir(&d, new_window);
    }

    /// This shells out to the git and fd command line programs
    pub(crate) fn find_repo_file(&mut self, new_window: bool) {
        let d = self
            .layout
            .active_buffer_ignoring_scratch()
            .dir()
            .unwrap_or(&self.cwd)
            .to_owned();
        let s = match self.system.run_command_blocking(
            "git rev-parse --show-toplevel",
            &d,
            self.active_buffer_id(),
        ) {
            Ok(s) => s,
            Err(e) => {
                self.set_status_message(format!("unable to find git root: {e}"));
                return;
            }
        };

        let root = Path::new(s.trim());
        self.find_file_under_dir(root, new_window);
    }

    pub(crate) fn delete_buffer(&mut self, id: usize, force: bool) {
        match self.layout.buffer_with_id(id) {
            Some(b) if b.dirty && !force => self.set_status_message("No write since last change"),
            None => warn!("attempt to close unknown buffer, id={id}"),
            _ => {
                _ = self.tx_fsys.send(LogEvent::Close(id));
                self.layout.clear_input_filter(id);
                let was_last_buffer = self.layout.close_buffer(id);
                self.running = !was_last_buffer;
            }
        }
    }

    pub(crate) fn delete_active_window(&mut self, force: bool) {
        let is_last_window = self.layout.close_active_window();
        if is_last_window {
            self.exit(force);
        }
    }

    pub(crate) fn delete_active_column(&mut self, force: bool) {
        let is_last_column = self.layout.close_active_column();
        if is_last_column {
            self.exit(force);
        }
    }

    pub(crate) fn mark_clean(&mut self, bufid: usize) {
        if let Some(b) = self.layout.buffer_with_id_mut(bufid) {
            b.dirty = false;
        }
    }

    pub(super) fn save_current_buffer(&mut self, fname: Option<String>, force: bool) {
        trace!("attempting to save current buffer");
        let p = match self.get_buffer_save_path(fname) {
            Some(p) => p,
            None => return,
        };

        let b = self.layout.active_buffer_mut_ignoring_scratch();
        match b.save_to_disk_at(p, force) {
            Ok(msg) => {
                self.lsp_manager.document_changed(b);
                self.lsp_manager.document_saved(b);
                self.set_status_message(msg);
                let id = self.active_buffer_id();
                _ = self.tx_fsys.send(LogEvent::Save(id));
            }

            Err(msg) => self.set_status_message(msg),
        }
    }

    pub(super) fn save_all_buffers(&mut self, force: bool) {
        trace!("attempting to save all open buffers");
        let ids: Vec<usize> = self
            .layout
            .buffers()
            .iter()
            .flat_map(|b| if b.dirty { Some(b.id) } else { None })
            .collect();
        let mut n_saved = 0;
        let mut n_errors = 0;

        for &id in ids.iter() {
            let b = self.layout.buffer_with_id_mut(id).unwrap();
            let p = match &b.kind {
                BufferKind::File(p) if b.dirty => p.clone(),
                _ => continue,
            };

            match b.save_to_disk_at(p, force) {
                Ok(_) => {
                    self.lsp_manager.document_changed(b);
                    self.lsp_manager.document_saved(b);
                    n_saved += 1;
                    _ = self.tx_fsys.send(LogEvent::Save(id));
                }

                Err(msg) => {
                    error!("id={id} {msg}");
                    n_errors += 1;
                    continue;
                }
            }
        }

        let error_msg = if n_errors > 0 {
            format!(", {n_errors} failed to save: see logs for details")
        } else {
            String::new()
        };

        self.set_status_message(format!("{n_saved} buffers saved{error_msg}"));
    }

    fn get_buffer_save_path(&mut self, fname: Option<String>) -> Option<PathBuf> {
        use BufferKind as Bk;

        let desired_path = match (fname, &self.layout.active_buffer_ignoring_scratch().kind) {
            // File has a known name which is either where we loaded it from or a
            // path that has been set and verified from the Some(s) case that follows
            (None, Bk::File(p)) => return Some(p.clone()),
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
                self.set_status_message(format!("Unable to check path: {e}"));
                return None;
            }
        }

        self.layout.active_buffer_mut_ignoring_scratch().kind =
            BufferKind::File(desired_path.clone());

        Some(desired_path)
    }

    pub(super) fn reload_buffer(&mut self, id: usize) {
        let msg = match self.layout.buffer_with_id_mut(id) {
            Some(b) => b.reload_from_disk(),
            // Silently ignoring attempts to reload unknown buffers
            None => return,
        };

        self.set_status_message(msg);
    }

    pub(super) fn reload_config(&mut self) {
        info!("reloading config");
        let msg = match Config::try_load() {
            Ok(config) => {
                *self.config.write().unwrap() = config;
                "config reloaded".to_string()
            }
            Err(s) => s,
        };
        info!("{msg}");

        self.set_status_message(msg);
        self.ui.state_change(StateChange::ConfigUpdated);
    }

    pub(super) fn reload_active_buffer(&mut self) {
        let msg = self
            .layout
            .active_buffer_mut_ignoring_scratch()
            .reload_from_disk();

        self.set_status_message(msg);
    }

    pub(super) fn set_mode(&mut self, name: &str) {
        if let Some((i, _)) = self.modes.iter().enumerate().find(|(_, m)| m.name == name) {
            self.modes.swap(0, i);
            self.ui.set_cursor_shape(self.current_cursor_shape());
        }
    }

    pub(super) fn exit(&mut self, force: bool) {
        let dirty_buffers = self.layout.dirty_buffers();
        if !dirty_buffers.is_empty() && !force {
            self.set_status_message("No write since last change. Use ':q!' to force exit");
            self.minibuffer_select_from("No write since last change> ", dirty_buffers);
            return;
        }

        self.running = false;
    }

    pub(super) fn set_clipboard(&mut self, s: String) {
        trace!("setting clipboard content");
        match self.system.set_clipboard(&s) {
            Ok(_) => self.set_status_message("Yanked selection to clipboard"),
            Err(e) => self.set_status_message(format!("Error setting clipboard: {e}")),
        }
    }

    pub(super) fn paste_from_clipboard(&mut self, source: Source) {
        trace!("pasting from clipboard");
        match self.system.read_clipboard() {
            Ok(s) => self.handle_action(Action::InsertString { s }, source),
            Err(e) => self.set_status_message(format!("Error reading clipboard: {e}")),
        }
    }

    pub(super) fn search_in_current_buffer(&mut self) {
        let numbered_lines = self
            .layout
            .active_buffer_ignoring_scratch()
            .string_lines()
            .into_iter()
            .enumerate()
            .map(|(i, line)| format!("{:>4} | {}", i + 1, line))
            .collect();

        let selection = self.minibuffer_select_from("> ", numbered_lines);
        if let MiniBufferSelection::Line { cy, .. } = selection {
            self.layout.active_buffer_mut_ignoring_scratch().dot = Dot::Cur {
                c: Cur::from_yx(cy, 0, self.layout.active_buffer_ignoring_scratch()),
            };
            self.handle_action(Action::DotSet(TextObject::Line, 1), Source::Fsys);
            self.handle_action(Action::SetViewPort(ViewPort::Center), Source::Fsys);
        }
    }

    pub(super) fn fsys_minibuffer(
        &mut self,
        prompt: Option<String>,
        raw_lines: String,
        tx: Sender<String>,
    ) {
        // Depending on how the user has provided input for us to work with we may have ended up
        // with an empty input or entirely whitespace. In both cases we want to avoid presenting
        // blank minibuffer lines to the user as they just result in visual noise.
        let lines = if raw_lines.is_empty() || raw_lines.chars().all(|c| c.is_whitespace()) {
            Vec::new()
        } else {
            raw_lines.split('\n').map(|s| s.to_string()).collect()
        };

        let prompt: &str = prompt.as_deref().unwrap_or("> ");
        let selection = self.minibuffer_select_from(prompt, lines);
        let s = match selection {
            MiniBufferSelection::Line { line, .. } => line,
            MiniBufferSelection::UserInput { input } => input,
            MiniBufferSelection::Cancelled => String::new(),
        };

        _ = tx.send(s);
    }

    /// Use the minibuffer to select an open buffer and focus it in the active window
    pub(super) fn select_buffer(&mut self) {
        let selection = self.minibuffer_select_from("> ", self.layout.as_buffer_list());
        if let MiniBufferSelection::Line { line, .. } = selection {
            // unwrap is fine here because we know the format of the buf list we are supplying
            if let Ok(id) = line.split_once(' ').unwrap().0.parse::<usize>() {
                self.focus_buffer(id, true);
            }
        }
    }

    pub(super) fn focus_buffer(&mut self, id: usize, force_active: bool) {
        self.layout.focus_id(id, force_active);
        _ = self.tx_fsys.send(LogEvent::Focus(id));
    }

    pub(super) fn debug_buffer_contents(&mut self) {
        self.minibuffer_select_from(
            "<RAW BUFFER> ",
            self.layout
                .active_buffer_ignoring_scratch()
                .string_lines()
                .into_iter()
                .map(|l| format!("{:?}", l))
                .collect(),
        );
    }

    pub(super) fn view_logs(&mut self) {
        self.layout
            .open_virtual("+logs", self.log_buffer.content(), false)
    }

    pub(super) fn show_active_ts_tree(&mut self) {
        match self
            .layout
            .active_buffer_ignoring_scratch()
            .pretty_print_ts_tree()
        {
            Some(s) => self.layout.open_virtual("+ts-tree", s, false),
            None => self.set_status_message("no tree-sitter tree for current buffer"),
        }
    }

    pub(super) fn show_help(&mut self) {
        self.layout.open_virtual("+help", gen_help_docs(), false)
    }

    pub(super) fn debug_edit_log(&mut self) {
        self.minibuffer_select_from("<EDIT LOG> ", self.layout.active_buffer().debug_edit_log());
    }

    pub(super) fn expand_current_dot(&mut self) {
        self.layout.active_buffer_mut().expand_cur_dot();
    }

    /// Default semantics for attempting to load the current dot:
    ///   - an event filter is in place -> pass to the event filter
    ///   - a plumbing rule matches the load -> run the plumbing rule
    ///   - a relative path from the directory of the containing file -> open in ad
    ///   - an absolute path -> open in ad
    ///     - if either have a valid addr following a colon then set dot to that addr
    ///   - search within the current buffer for the next occurrence of dot and select it
    ///
    /// Loading and executing of dot is part of what makes ad an unusual editor. The semantics are
    /// lifted almost directly from acme on plan9 and the curious user is encouraged to read the
    /// materials available at http://acme.cat-v.org/ to learn more about what is possible with
    /// such a system.
    pub(super) fn default_load_dot(&mut self, source: Source, load_in_new_window: bool) {
        // Grabbing the ID in this way allows us to treat loads in the scratch buffer as being from
        // the active buffer.
        let id = self.layout.active_buffer_ignoring_scratch().id;
        let b = self.layout.active_buffer_mut();
        b.expand_cur_dot();
        if b.notify_load(source) {
            return; // input filter in place
        }

        let s = b.dot.content(b);
        if s.is_empty() {
            return;
        }

        self.load_string_in_buffer(id, s, load_in_new_window);
    }

    pub(super) fn plumb(&mut self, txt: String, load_in_new_window: bool) {
        let id = self.layout.active_buffer_ignoring_scratch().id;
        self.load_string_in_buffer(id, txt, load_in_new_window);
    }

    pub(super) fn load_string_in_buffer(&mut self, id: usize, s: String, load_in_new_window: bool) {
        let b = match self.layout.buffer_with_id_mut(id) {
            Some(b) => b,
            None => return,
        };

        let wdir = b
            .dir()
            .map(|p| p.display().to_string())
            .or_else(|| Some(self.cwd.display().to_string()));

        let m = PlumbingMessage {
            src: Some("ad".to_string()),
            dst: None,
            wdir,
            cur: 0,
            attrs: Default::default(),
            data: s.clone(),
        };

        match self.plumbing_rules.plumb(m) {
            Some(MatchOutcome::Message(m)) => self.handle_plumbing_message(m, load_in_new_window),

            Some(MatchOutcome::Run(cmd)) => {
                let mut command = Command::new("sh");
                command
                    .args(["-c", cmd.as_str()])
                    .stdout(Stdio::null())
                    .stderr(Stdio::null());
                if let Err(e) = command.spawn() {
                    self.set_status_message(format!("error spawning process: {e}"));
                };
            }

            None => self.load_explicit_string(id, &s, load_in_new_window),
        }
    }

    /// Handling of plumbing messages that are sent to ad supports several attributes
    /// which can be set in order to configure the behaviour:
    ///   - by default the data will be treated as a filepath and opened
    ///   - if the attr "addr" is set it will be parsed as an Addr and applied
    ///   - if the attr "action" is set to "showdata" then a new buffer is created to hold the data
    ///     - if the attr "filename" is set as well then it will be used as the name for the buffer
    ///     - otherwise the filename will be "+plumbing-message"
    fn handle_plumbing_message(&mut self, m: PlumbingMessage, load_in_new_window: bool) {
        let PlumbingMessage { attrs, data, .. } = m;
        match attrs.get("action") {
            Some(s) if s == "showdata" => {
                let filename = attrs
                    .get("filename")
                    .cloned()
                    .unwrap_or_else(|| "+plumbing-message".to_string());
                self.layout.open_virtual(filename, data, load_in_new_window);
            }

            _ => {
                self.open_file(data, load_in_new_window);
                if let Some(s) = attrs.get("addr") {
                    match Addr::parse(s) {
                        Ok(addr) => {
                            let b = self.layout.active_buffer_mut();
                            b.dot = b.map_addr(&addr);
                        }
                        Err(e) => self.set_status_message(format!("malformed addr: {e:?}")),
                    }
                }
            }
        }
    }

    pub(super) fn load_explicit_string(&mut self, bufid: usize, s: &str, load_in_new_window: bool) {
        if s.is_empty() {
            return;
        }

        let b = match self.layout.buffer_with_id_mut(bufid) {
            Some(b) => b,
            None => return,
        };

        let (maybe_path, maybe_addr) = match s.find(':') {
            Some(idx) => {
                let (s, addr) = s.split_at(idx);
                let (_, addr) = addr.split_at(1);
                match Addr::parse(addr) {
                    Ok(expr) => (s, Some(expr)),
                    Err(_) => (s, None),
                }
            }
            None => (s, None),
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
            self.open_file(path, load_in_new_window);
            if let Some(addr) = maybe_addr {
                let b = self.layout.active_buffer_mut();
                b.dot = b.map_addr(&addr);
                self.layout.clamp_scroll();
                self.handle_action(Action::SetViewPort(ViewPort::Center), Source::Fsys);
            }
        } else {
            b.find_forward(s);
            self.handle_action(Action::SetViewPort(ViewPort::Center), Source::Fsys);
        }
    }

    /// Default semantics for attempting to execute the current dot:
    ///   - an event filter is in place -> pass to the event filter
    ///   - a valid ad command -> execute the command
    ///   - attempt to run as a shell command with args
    ///
    /// Loading and executing of dot is part of what makes ad an unusual editor. The semantics are
    /// lifted almost directly from acme on plan9 and the curious user is encouraged to read the
    /// materials available at http://acme.cat-v.org/ to learn more about what is possible with
    /// such a system.
    pub(super) fn default_execute_dot(&mut self, arg: Option<(Range, String)>, source: Source) {
        let b = self.layout.active_buffer_mut();
        b.expand_cur_dot();
        if b.notify_execute(source, arg.clone()) {
            return; // input filter in place
        }

        let mut cmd = b.dot.content(b).trim().to_string();
        if cmd.is_empty() {
            return;
        }

        if let Some((_, arg)) = arg {
            cmd.push(' ');
            cmd.push_str(&arg);
        }

        match self.parse_command(&cmd) {
            Some(actions) => self.handle_actions(actions, source),
            None => self.run_shell_cmd(&cmd),
        }
    }

    /// Silently focus `bufid` (no jumplist record) and execute the given string. If executing the
    /// string doesn't change the focused buffer we then reset back to the buffer that was active.
    pub(super) fn execute_explicit_string(&mut self, bufid: usize, s: &str, source: Source) {
        let current_id = self.active_buffer_id();
        self.layout.focus_id_silent(bufid);

        match self.parse_command(s.trim()) {
            Some(actions) => self.handle_actions(actions, source),
            None => self.run_shell_cmd(s.trim()),
        }

        if self.active_buffer_id() == bufid {
            self.layout.focus_id_silent(current_id);
        }
    }

    pub(super) fn execute_command(&mut self, cmd: &str) {
        debug!(%cmd, "executing command");
        if let Some(actions) = self.parse_command(cmd.trim_end()) {
            self.handle_actions(actions, Source::Fsys);
        }
    }

    pub(super) fn execute_edit_command(&mut self, cmd: &str) {
        debug!(%cmd, "executing edit command");
        let prog = match Program::try_parse(cmd) {
            Ok(prog) => prog,
            Err(error) => {
                warn!(?error, "invalid edit command");
                self.set_status_message(format!("Invalid edit command: {error:?}"));
                return;
            }
        };

        let mut buf = Vec::new();
        let b = self.layout.active_buffer_mut_ignoring_scratch();
        let fname = b.full_name().to_string();

        let mut runner = EditorRunner {
            system: &mut self.system,
            dir: b.dir().unwrap_or(&self.cwd).to_path_buf(),
            bufid: b.id,
        };

        match prog.execute(b, &mut runner, &fname, &mut buf) {
            Ok(new_dot) => {
                self.layout.record_jump_position();
                self.layout.active_buffer_mut_ignoring_scratch().dot = new_dot;
            }

            Err(e) => self.set_status_message(format!("Error running edit command: {e:?}")),
        }

        if !buf.is_empty() {
            let s = match String::from_utf8(buf) {
                Ok(s) => s,
                Err(e) => {
                    error!(%e, "edit command produced invalid utf8 output");
                    return;
                }
            };
            let id = self.active_buffer_id();
            self.layout.write_output_for_buffer(id, s, &self.cwd);
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
            self.set_status_message(format!("running {input:?}..."));
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

    pub(super) fn prepare_lsp_rename(&mut self) {
        self.set_status_message("preparing LSP rename...");
        self.lsp_manager
            .prepare_rename(self.layout.active_buffer_ignoring_scratch());
    }

    pub(super) fn lsp_rename(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("LSP-RENAME"));

        if let Some(input) = self.minibuffer_prompt("LSP Rename> ") {
            let b = self.layout.active_buffer_ignoring_scratch();
            self.lsp_manager.rename(b, input);
        };

        self.modes.remove(0);
    }

    pub(super) fn pipe_dot_through_shell_cmd(&mut self, raw_cmd_str: &str) {
        let (s, d) = {
            let b = self.layout.active_buffer_ignoring_scratch();
            (b.dot_contents(), b.dir().unwrap_or(&self.cwd))
        };

        let id = self.active_buffer_id();
        let res = self.system.pipe_through_command(raw_cmd_str, &s, d, id);

        match res {
            Ok(s) => self.forward_action_to_active_buffer_ignoring_scratch(
                Action::InsertString { s },
                Source::Fsys,
            ),
            Err(e) => self.set_status_message(format!("Error running external command: {e}")),
        }
    }

    pub(super) fn replace_dot_with_shell_cmd(&mut self, raw_cmd_str: &str) {
        let b = self.layout.active_buffer_ignoring_scratch();
        let d = b.dir().unwrap_or(&self.cwd);
        let id = b.id;
        let res = self.system.run_command_blocking(raw_cmd_str, d, id);

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }, Source::Fsys),
            Err(e) => self.set_status_message(format!("Error running external command: {e}")),
        }
    }

    pub(super) fn run_shell_cmd(&mut self, raw_cmd_str: &str) {
        let b = self.layout.active_buffer_ignoring_scratch();
        let d = b.dir().unwrap_or(&self.cwd);
        let id = b.id;
        let res = self
            .system
            .run_command(raw_cmd_str, d, id, self.tx_events.clone());

        if let Err(e) = res {
            self.set_status_message(format!("Error running external command: {e}"));
        }
    }

    pub(super) fn kill_running_child(&mut self) {
        let known = self.system.running_children();
        if let MiniBufferSelection::Line { cy, .. } = self.minibuffer_select_from("kill", known) {
            self.system.kill_child(cy);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LogBuffer, PlumbingRules, editor::EditorMode};
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

        ed.open_file("foo", false);

        // The first open should also close our scratch buffer
        assert_recv!(brx, Close, 0);
        assert_recv!(brx, Open, 1);
        assert_recv!(brx, Focus, 1);

        // Opening a second file should only notify for that file
        ed.open_file("bar", false);
        assert_recv!(brx, Open, 2);
        assert_recv!(brx, Focus, 2);

        // Opening the first file again should just notify for the current file
        ed.open_file("foo", false);
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
            ed.open_file(file, false);
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

    #[test_case("next-column", 2, 1; "move focus to foo")]
    #[test_case("next-column", 1, 2; "move focus to bar executed in foo")]
    #[test_case("echo hello", 2, 2; "no change of focus")]
    #[test]
    fn execute_explicit_string_handles_focus_correctly(cmd: &str, bufid: usize, active: usize) {
        let mut ed = Editor::new(
            Config::default(),
            PlumbingRules::default(),
            EditorMode::Headless,
            LogBuffer::default(),
        );
        ed.update_window_size(400, 800);

        ed.open_file("foo", false);
        assert_eq!(ed.active_buffer_id(), 1);

        ed.layout.new_column();
        ed.open_file("bar", false);
        assert_eq!(ed.active_buffer_id(), 2);

        ed.execute_explicit_string(bufid, cmd, Source::Keyboard);
        assert_eq!(ed.active_buffer_id(), active);
    }
}
