//! Editor actions in response to user input
use crate::{
    buffer::{Buffer, BufferKind},
    editor::Editor,
    key::Key,
    term::clear_screen,
};
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

impl Editor {
    // TODO: check if the file is already open
    pub fn open_file(&mut self, path: &str) -> io::Result<()> {
        match Buffer::new_from_file(path) {
            Ok(b) => self.buffers.insert(b),
            Err(e) => self.set_status_message(&format!("Error opening file: {e}")),
        };

        Ok(())
    }

    pub(super) fn save_current_buffer(&mut self) -> io::Result<()> {
        let p = match self.buffers.active().kind {
            BufferKind::File(ref p) => p.clone(),
            BufferKind::Unnamed => match self.prompt("Save As: ") {
                Some(s) => {
                    let p: PathBuf = s.into();
                    self.buffers.active_mut().kind = BufferKind::File(p.clone());
                    p
                }
                None => return Ok(()),
            },
            BufferKind::Virtual(_) => {
                self.set_status_message("Error: virtual buffer");
                return Ok(());
            }
        };

        let b = self.buffers.active_mut();
        let contents = b.contents();
        let n_lines = b.len();
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

    pub(super) fn exit(&mut self) -> io::Result<()> {
        if self.buffers.active().dirty {
            self.set_status_message("Current buffer is dirty: press C-q again to force quit");
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
}
