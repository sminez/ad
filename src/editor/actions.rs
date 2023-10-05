//! Editor actions in response to user input
use crate::{
    buffer::{BufferKind, Cur, Dot, MiniBuffer, MiniBufferSelection, TextObject},
    die,
    editor::Editor,
    key::Key,
    mode::Mode,
    util::{pipe_through_command, read_clipboard, run_command, set_clipboard},
};
use std::{env, fs, io::Write, path::PathBuf};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Actions {
    Single(Action),
    Multi(Vec<Action>),
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
    Exit { force: bool },
    InsertChar { c: char },
    InsertString { s: String },
    NextBuffer,
    OpenFile { path: String },
    Paste,
    PreviousBuffer,
    RawKey { k: Key },
    Redo,
    SaveBuffer,
    SaveBufferAs { path: String },
    SearchInCurrentBuffer,
    SelectBuffer,
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
        if let Err(e) = self.buffers.open_or_focus(path) {
            self.set_status_message(&format!("Error opening file: {e}"));
        };
    }

    pub fn delete_current_buffer(&mut self, force: bool) {
        let is_last_buffer = self.buffers.len() == 1;

        if self.buffers.active().dirty && !force {
            self.set_status_message("No write since last change");
        } else {
            self.buffers.close_active();
            if is_last_buffer {
                self.running = false;
            }
        }
    }

    pub(super) fn save_current_buffer(&mut self, fname: Option<String>) {
        use BufferKind as Bk;

        let p = match (fname, &self.buffers.active().kind) {
            (Some(s), Bk::File(_) | Bk::Unnamed) => PathBuf::from(s),
            (_, Bk::File(ref p)) => p.clone(),
            (_, Bk::Unnamed) => match MiniBuffer::prompt("Save As: ", self) {
                Some(s) => {
                    let p: PathBuf = s.into();
                    self.buffers.active_mut().kind = BufferKind::File(p.clone());
                    p
                }
                None => return,
            },
            (_, Bk::Virtual(_) | Bk::MiniBuffer) => return,
        };

        let b = self.buffers.active_mut();
        let contents = b.contents();
        let n_lines = b.len_lines();
        let display_path = match p.canonicalize() {
            Ok(cp) => cp.display().to_string(),
            Err(_) => p.display().to_string(),
        };
        let n_bytes = contents.len();

        let msg = match fs::write(p, contents) {
            Ok(_) => {
                b.dirty = false;
                format!("\"{display_path}\" {n_lines}L {n_bytes}B written")
            }
            Err(e) => format!("Unable to save buffer: {e}"),
        };

        self.set_status_message(&msg);
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
        if self.buffers.active().dirty && !force {
            let dirty_buffers = self.buffers.dirty_buffers().join(" ");
            // TODO: probably want this to be a "cancel only" mini-buffer w multiple lines?
            self.set_status_message(&format!("No write since last change: {dirty_buffers}"));
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
        let selection = MiniBuffer::select_from("> ", self.buffers.active().string_lines(), self);
        if let MiniBufferSelection::Line { cy, .. } = selection {
            self.buffers.active_mut().dot = Dot::Cur {
                c: Cur { y: cy, x: 0 },
            };
        }
    }

    pub(super) fn select_buffer(&mut self) {
        let selection = MiniBuffer::select_from("> ", self.buffers.as_buf_list(), self);
        if let MiniBufferSelection::Line { line, .. } = selection {
            // unwrap is fine here because we know the format of the buf list we are supplying
            if let Ok(id) = line.split_once(' ').unwrap().0.parse::<usize>() {
                self.buffers.focus_id(id);
            }
        }
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

    pub(super) fn command_mode(&mut self) {
        self.modes.insert(0, Mode::command_mode());

        if let Some(input) = MiniBuffer::prompt(":", self) {
            if let Some(actions) = self.parse_command(&input) {
                self.handle_actions(actions);
            }
        }

        self.modes.remove(0);
    }

    pub(super) fn pipe_dot_through_shell_cmd(&mut self, raw_cmd_str: &str) {
        let s = self.buffers.active().dot_contents();
        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => pipe_through_command(cmd, rest.split_whitespace(), &s),
            None => pipe_through_command(raw_cmd_str, std::iter::empty::<&str>(), &s),
        };

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    pub(super) fn replace_dot_with_shell_cmd(&mut self, raw_cmd_str: &str) {
        let res = match raw_cmd_str.split_once(' ') {
            Some((cmd, rest)) => run_command(cmd, rest.split_whitespace()),
            None => run_command(raw_cmd_str, std::iter::empty::<&str>()),
        };

        match res {
            Ok(s) => self.handle_action(Action::InsertString { s }),
            Err(e) => self.set_status_message(&format!("Error running external command: {e}")),
        }
    }

    // TODO: sending to the shell and just running a command needs the read-only minibuffer
}
