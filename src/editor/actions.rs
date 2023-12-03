//! Editor actions in response to user input
use crate::{
    buffer::{BufferKind, MiniBuffer, MiniBufferSelection},
    config::Config,
    die,
    dot::{Cur, Dot, TextObject},
    editor::Editor,
    exec::Program,
    fsys::BufId,
    key::Key,
    mode::Mode,
    replace_config, update_config,
    util::{pipe_through_command, read_clipboard, run_command, set_clipboard, spawn_command},
};
use std::{env, fs, io::Write, path::{PathBuf, Path}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Actions {
    Single(Action),
    Multi(Vec<Action>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ViewPort {
    Bottom,
    Center,
    Top,
}

/// Supported actions for interacting with the editor state
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Change,
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
    LoadDot,
    NewEditLogTransaction,
    NextBuffer,
    OpenFile { path: String },
    Paste,
    PreviousBuffer,
    RawKey { k: Key },
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
    SetConfigProp { input: String },
    SetViewPort(ViewPort),
    SetMode { m: &'static str },
    ShellPipe { cmd: String },
    ShellReplace { cmd: String },
    ShellRun { cmd: String },
    ShellSend { cmd: String },
    Undo,
    Yank,

    DebugBufferContents,
    DebugEditLog,
}

impl Editor {
    pub fn change_directory(&mut self, opt_path: Option<String>) {
        let p = match opt_path {
            Some(p) => p,
            None => match env::var("HOME") {
                Ok(p) => p,
                Err(e) => {
                    self.set_status_message(&format!("Unable to determine home directory: {e}"));
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
            self.set_status_message(&format!("Unable to set working directory: {e}"));
            return;
        };

        self.cwd = new_cwd;
        self.set_status_message(&self.cwd.display().to_string());
    }

    pub fn open_file(&mut self, path: &str) {
        match self.buffers.open_or_focus(self.cwd.join(path)) {
            Err(e) => self.set_status_message(&format!("Error opening file: {e}")),

            Ok(Some(new_id)) => {
                self.btx.send(BufId::Add(new_id)).unwrap();
                self.btx.send(BufId::Current(new_id)).unwrap();
            }

            Ok(None) => {
                match self.buffers.active().state_changed_on_disk() {
                    Ok(true) => {
                        let res = MiniBuffer::prompt("File changed on disk, reload? [y/n]: ", self);
                        if let Some("y" | "Y" | "yes") = res.as_deref() {
                            let msg = self.buffers.active_mut().reload_from_disk();
                            self.set_status_message(&msg);
                        }
                    }
                    Ok(false) => (),
                    Err(e) => self.set_status_message(&e),
                }
                let id = self.buffers.active().id;
                self.btx.send(BufId::Current(id)).unwrap();
            }
        };
    }

    /// This shells out to the fd command line program
    pub fn find_file(&mut self) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd).to_owned();
        let selection = MiniBuffer::select_from_command_output(">", "fd", ["-t", "f"], &d, self);
        if let MiniBufferSelection::Line { line, .. } = selection {
            self.open_file(&format!("{}/{}", d.display(), line.trim()));
        }
    }

    /// This shells out to the git and fd command line programs
    pub fn find_repo_file(&mut self) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd).to_owned();
        let s = match run_command("git", ["rev-parse", "--show-toplevel"], &d) {
            Ok(s) => s,
            Err(e) => {
                self.set_status_message(&format!("unable to find git root: {e}"));
                return;
            }
        };

        let root = Path::new(s.trim());
        let selection = MiniBuffer::select_from_command_output(">", "fd", ["-t", "f"], root, self);
        if let MiniBufferSelection::Line { line, .. } = selection {
            self.open_file(&format!("{}/{}", root.display(), line.trim()));
        }
    }

    pub fn delete_current_buffer(&mut self, force: bool) {
        let is_last_buffer = self.buffers.len() == 1;

        if self.buffers.active().dirty && !force {
            self.set_status_message("No write since last change");
        } else {
            let id = self.buffers.active().id;
            self.btx.send(BufId::Remove(id)).unwrap();

            self.buffers.close_active();
            if is_last_buffer {
                self.running = false;
            }
        }
    }

    pub(super) fn save_current_buffer(&mut self, fname: Option<String>, force: bool) {
        let p = match self.get_buffer_save_path(fname) {
            Some(p) => p,
            None => return,
        };

        let msg = self.buffers.active_mut().save_to_disk_at(p, force);
        self.set_status_message(&msg);
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
            (None, Bk::Unnamed) => match MiniBuffer::prompt("Save As: ", self) {
                Some(s) => s.into(),
                None => return None,
            },
            // virtual and minibuffer buffers don't support saving and have no save path
            (_, Bk::Virtual(_) | Bk::MiniBuffer) => return None,
        };

        match desired_path.try_exists() {
            Ok(false) => (),
            Ok(true) => {
                if !MiniBuffer::confirm("File already exists", self) {
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
        let msg = match Config::try_load() {
            Ok(config) => {
                replace_config(config);
                "config reloaded".to_string()
            }
            Err(s) => s,
        };

        self.set_status_message(&msg);
    }

    pub(super) fn reload_active_buffer(&mut self) {
        let msg = self.buffers.active_mut().reload_from_disk();
        self.set_status_message(&msg);
    }

    pub(super) fn set_config_prop(&mut self, input: &str) {
        if let Err(msg) = update_config(input) {
            self.set_status_message(&msg);
        }
    }

    pub(super) fn set_mode(&mut self, name: &str) {
        if let Some((i, _)) = self.modes.iter().enumerate().find(|(_, m)| m.name == name) {
            self.modes.swap(0, i);
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
            MiniBuffer::select_from("No write since last change> ", dirty_buffers, self);
            return;
        }

        self.running = false;
    }

    pub(super) fn set_clipboard(&mut self, s: String) {
        match set_clipboard(&s) {
            Ok(_) => self.set_status_message("Yanked selection to system clipboard"),
            Err(e) => self.set_status_message(&format!("Error setting system clipboard: {e}")),
        }
    }

    pub(super) fn paste_from_clipboard(&mut self) {
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

        let selection = MiniBuffer::select_from("> ", numbered_lines, self);
        if let MiniBufferSelection::Line { cy, .. } = selection {
            self.buffers.active_mut().dot = Dot::Cur {
                c: Cur::from_yx(cy, 0, self.buffers.active()),
            };
            self.handle_action(Action::DotSet(TextObject::Line, 1));
        }
    }

    pub(super) fn select_buffer(&mut self) {
        let selection = MiniBuffer::select_from("> ", self.buffers.as_buf_list(), self);
        if let MiniBufferSelection::Line { line, .. } = selection {
            // unwrap is fine here because we know the format of the buf list we are supplying
            if let Ok(id) = line.split_once(' ').unwrap().0.parse::<usize>() {
                self.focus_buffer(id);
            }
        }
    }

    pub(super) fn focus_buffer(&mut self, id: usize) {
        self.buffers.focus_id(id);
        self.btx.send(BufId::Current(id)).unwrap();
    }

    pub(super) fn debug_buffer_contents(&mut self) {
        MiniBuffer::select_from(
            "<RAW BUFFER> ",
            self.buffers
                .active()
                .string_lines()
                .into_iter()
                .map(|l| format!("{:?}", l))
                .collect(),
            self,
        );
    }

    pub(super) fn debug_edit_log(&mut self) {
        MiniBuffer::select_from("<EDIT LOG> ", self.buffers.active().debug_edit_log(), self);
    }

    pub(super) fn execute_command(&mut self, cmd: &str) {
        if let Some(actions) = self.parse_command(cmd.trim_end()) {
            self.handle_actions(actions);
        }
    }

    pub(super) fn execute_edit_command(&mut self, cmd: &str) {
        let mut prog = match Program::try_parse(cmd) {
            Ok(prog) => prog,
            Err(e) => {
                self.set_status_message(&format!("Invalid edit command: {e:?}"));
                self.modes.remove(0);
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
            MiniBuffer::select_from(
                "%>",
                String::from_utf8(buf)
                    .unwrap()
                    .lines()
                    .map(|l| l.to_string())
                    .collect(),
                self,
            );
        }
    }

    pub(super) fn command_mode(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("COMMAND"));

        if let Some(input) = MiniBuffer::prompt(":", self) {
            self.execute_command(&input);
        }

        self.modes.remove(0);
    }

    pub(super) fn run_mode(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("RUN"));

        if let Some(input) = MiniBuffer::prompt("!", self) {
            self.run_shell_cmd(&input);
        }

        self.modes.remove(0);
    }

    pub(super) fn sam_mode(&mut self) {
        self.modes.insert(0, Mode::ephemeral_mode("EDIT"));

        if let Some(input) = MiniBuffer::prompt("Edit> ", self) {
            self.execute_edit_command(&input);
        };

        self.modes.remove(0);
    }

    pub(super) fn pipe_dot_through_shell_cmd(&mut self, raw_cmd_str: &str) {
        let (s, d) = {
            let b = self.buffers.active();
            (b.dot_contents(), b.dir().unwrap_or(&self.cwd))
        };

        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => pipe_through_command(cmd, rest.split_whitespace(), &s, d),
            None => pipe_through_command(raw_cmd_str, std::iter::empty::<&str>(), &s, d),
        };

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    pub(super) fn replace_dot_with_shell_cmd(&mut self, raw_cmd_str: &str) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd);
        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => run_command(cmd, rest.split_whitespace(), d),
            None => run_command(raw_cmd_str, std::iter::empty::<&str>(), d),
        };

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    pub(super) fn run_shell_cmd(&mut self, raw_cmd_str: &str) {
        let d = self.buffers.active().dir().unwrap_or(&self.cwd);
        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => spawn_command(cmd, rest.split_whitespace(), d),
            None => spawn_command(raw_cmd_str, std::iter::empty::<&str>(), d),
        };

        match res {
            Ok(_) => (),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    // TODO: sending to the shell and just running a command needs the read-only minibuffer
}
