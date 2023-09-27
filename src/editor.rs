use crate::{
    buffer::Buffer,
    die,
    key::Key,
    term::{
        clear_screen, enable_raw_mode, get_termios, get_termsize, set_termios, CUR_CLEAR_RIGHT,
        CUR_HIDE, CUR_SHOW, CUR_TO_START,
    },
    VERSION,
};
use libc::termios as Termios;
use std::{
    cmp::{max, min},
    io::{self, Read, Stdin, Stdout, Write},
};

pub struct Editor {
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    stdin: Stdin,
    original_termios: Termios,
    pub running: bool,
    buffers: Vec<Buffer>,
}

impl Drop for Editor {
    fn drop(&mut self) {
        set_termios(self.original_termios)
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self::new()
    }
}

impl Editor {
    pub fn new() -> Self {
        let (screen_rows, screen_cols) = get_termsize();
        let original_termios = get_termios();

        enable_raw_mode(original_termios);

        Self {
            screen_rows,
            screen_cols,
            stdout: io::stdout(),
            stdin: io::stdin(),
            original_termios,
            running: true,
            buffers: Vec::new(),
        }
    }

    // TODO:
    //   - display an error rather than erroring
    //   - check if the file is already open
    pub fn open_file(&mut self, path: &str) -> io::Result<()> {
        self.buffers.insert(0, Buffer::new_from_file(path)?);

        Ok(())
    }

    fn current_buffer(&self) -> Option<&Buffer> {
        if self.buffers.is_empty() {
            None
        } else {
            Some(&self.buffers[0])
        }
    }

    fn current_buffer_len(&self) -> usize {
        if self.buffers.is_empty() {
            0
        } else {
            self.buffers[0].len()
        }
    }

    fn row_off(&self) -> usize {
        if self.buffers.is_empty() {
            0
        } else {
            self.buffers[0].row_off
        }
    }

    fn col_off(&self) -> usize {
        if self.buffers.is_empty() {
            0
        } else {
            self.buffers[0].col_off
        }
    }

    pub fn refresh_screen(&mut self) -> io::Result<()> {
        if !self.buffers.is_empty() {
            self.buffers[0].clamp_scroll(self.screen_rows, self.screen_cols);
        }

        let mut buf = format!("{CUR_HIDE}{CUR_TO_START}");
        self.render_rows(&mut buf);

        let (cy, rx) = match self.current_buffer() {
            Some(b) => (b.cy, b.rx),
            None => (0, 0),
        };

        buf.push_str(&format!(
            "\x1b[{};{}H{CUR_SHOW}",
            cy - self.row_off() + 1,
            rx - self.col_off() + 1
        ));

        self.stdout.write_all(buf.as_bytes())?;
        self.stdout.flush()
    }

    fn render_rows(&self, buf: &mut String) {
        for y in 0..self.screen_rows {
            let file_row = y + self.row_off();

            if file_row >= self.current_buffer_len() {
                if self.buffers.is_empty() && y == self.screen_rows / 3 {
                    let mut banner = format!("ad editor :: version {VERSION}");
                    banner.truncate(self.screen_cols);
                    let mut padding = (self.screen_cols - banner.len()) / 2;
                    if padding > 0 {
                        buf.push('~');
                        padding -= 1;
                    }
                    buf.push_str(&" ".repeat(padding));
                    buf.push_str(&banner);
                } else {
                    buf.push('~');
                }
            } else {
                let col_off = self.col_off();
                // file_row < self.current_buffer_len() so there is an active buffer
                let rline = &self.buffers[0].render_lines[file_row];
                let mut len = max(0, rline.len() - col_off);
                len = min(self.screen_cols, len);
                buf.push_str(&rline[col_off..min(self.screen_cols, len)]);
            }

            buf.push_str(CUR_CLEAR_RIGHT);
            if y < self.screen_rows - 1 {
                buf.push_str("\r\n");
            }
        }
    }

    #[inline]
    fn read_char(&mut self) -> char {
        let mut buf: [u8; 1] = [0; 1];
        loop {
            match self.stdin.read_exact(&mut buf) {
                Ok(_) => break,
                Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => continue,
                Err(e) => die(format!("read: {e}")),
            }
        }

        buf[0] as char
    }

    #[inline]
    fn try_read_char(&mut self) -> Option<char> {
        let mut buf: [u8; 1] = [0; 1];
        let res = self.stdin.read_exact(&mut buf);
        if res.is_ok() {
            Some(buf[0] as char)
        } else {
            None
        }
    }

    pub fn read_key(&mut self) -> Key {
        let c = self.read_char();

        if let Some(key) = Key::try_from_char(c) {
            return key;
        }

        let c2 = match self.try_read_char() {
            Some(c2) => c2,
            None => return Key::Char(c),
        };
        let c3 = match self.try_read_char() {
            Some(c3) => c3,
            None => return Key::Char(c),
        };

        if let Some(key) = Key::try_from_seq2(c2, c3) {
            return key;
        }

        if c2 == '[' && c3.is_ascii_digit() {
            if let Some('~') = self.try_read_char() {
                if let Some(key) = Key::try_from_bracket_tilde(c3) {
                    return key;
                }
            }
        }

        Key::Char(c)
    }

    pub fn handle_keypress(&mut self, k: Key) -> io::Result<()> {
        match k {
            Key::Arrow(_) | Key::Home | Key::End | Key::PageUp | Key::PageDown => {
                if !self.buffers.is_empty() {
                    self.buffers[0].handle_keypress(k, self.screen_rows)?;
                }
            }

            Key::Ctrl('q') => {
                clear_screen(&mut self.stdout)?;
                self.running = false;
            }
            _ => (),
        }

        Ok(())
    }
}
