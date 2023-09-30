//! Editor actions in response to user input
use crate::{
    buffer::{Buffer, BufferKind, MiniBuffer, MiniBufferSelection},
    editor::Editor,
    key::{Arrow, Key},
    mode::Mode,
    term::clear_screen,
};
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

/// Supported actions for interacting with the editor state
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    CommandMode,
    DeleteChar,
    Exit,
    ForceExit,
    InsertChar(char),
    InsertLine,
    Move(Arrow, usize),
    RawKey(Key),
    SaveBuffer,
    SaveBufferAs(String),
    SearchInCurrentBuffer,
    SetMode(&'static str),
    // Yank,
    // NewBuffer,
    // CloseBuffer,
    // NextBuffer,
    // PreviousBuffer,
}

impl Editor {
    // TODO: check if the file is already open in our internal state
    pub fn open_file(&mut self, path: &str) -> io::Result<()> {
        match Buffer::new_from_file(path) {
            Ok(b) => self.buffers.insert(b),
            Err(e) => self.set_status_message(&format!("Error opening file: {e}")),
        };

        Ok(())
    }

    pub(super) fn save_current_buffer(&mut self, fname: Option<String>) -> io::Result<()> {
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
                None => return Ok(()),
            },
            (_, Bk::Virtual(_) | Bk::MiniBuffer) => return Ok(()),
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

        Ok(())
    }

    pub(super) fn set_mode(&mut self, name: &str) -> io::Result<()> {
        if let Some((i, _)) = self.modes.iter().enumerate().find(|(_, m)| m.name == name) {
            self.modes.swap(0, i);
            let cur_shape = self.modes[0].cur_shape.to_string();
            self.stdout.write_all(cur_shape.as_bytes())?;
        }

        Ok(())
    }

    pub(super) fn exit(&mut self, force: bool) -> io::Result<()> {
        if self.buffers.active().dirty && !force {
            let dirty_buffers = self.buffers.dirty_buffers().join(" ");
            // TODO: probably want this to be a "cancel only" mini-buffer w multiple lines?
            self.set_status_message(&format!("No write since last change: {dirty_buffers}"));
            self.refresh_screen();

            match self.read_key() {
                Key::Ctrl('q') => (),
                k => return self.handle_keypress(k),
            }
        }

        clear_screen(&mut self.stdout);
        self.running = false;

        Ok(())
    }

    pub(super) fn search_in_current_buffer(&mut self) {
        let selection = MiniBuffer::select_from("> ", self.buffers.active().lines.clone(), self);
        if let MiniBufferSelection::Line { cy, .. } = selection {
            self.buffers.active_mut().cy = cy;
        }
    }

    pub(super) fn command_mode(&mut self) -> io::Result<()> {
        self.modes.insert(0, Mode::command_mode());

        if let Some(input) = MiniBuffer::prompt(":", self) {
            if let Some(actions) = self.parse_command(&input) {
                self.handle_actions(actions)?;
            }
        }

        self.modes.remove(0);

        Ok(())
    }
}
