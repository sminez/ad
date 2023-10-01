//! Editor actions in response to user input
use crate::{
    buffer::{BufferKind, MiniBuffer, MiniBufferSelection},
    die,
    editor::Editor,
    key::{Arrow, Key},
    mode::Mode,
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
    ChangeDirectory { path: Option<String> },
    CommandMode,
    DeleteBuffer { force: bool },
    DeleteChar,
    Exit { force: bool },
    InsertChar { c: char },
    InsertLine,
    Move { d: Arrow, n: usize },
    NextBuffer,
    OpenFile { path: String },
    PreviousBuffer,
    RawKey { k: Key },
    SaveBuffer,
    SaveBufferAs { path: String },
    SearchInCurrentBuffer,
    SelectBuffer,
    SetMode { m: &'static str },
    // Yank,
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
        let n_bytes = contents.as_bytes().len();

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

    pub(super) fn search_in_current_buffer(&mut self) {
        let selection = MiniBuffer::select_from("> ", self.buffers.active().lines.clone(), self);
        if let MiniBufferSelection::Line { cy, .. } = selection {
            self.buffers.active_mut().cy = cy;
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

    pub(super) fn command_mode(&mut self) {
        self.modes.insert(0, Mode::command_mode());

        if let Some(input) = MiniBuffer::prompt(":", self) {
            if let Some(actions) = self.parse_command(&input) {
                self.handle_actions(actions);
            }
        }

        self.modes.remove(0);
    }
}
