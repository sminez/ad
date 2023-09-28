use crate::{
    buffer::Buffer,
    die,
    key::Key,
    term::{
        clear_screen, enable_raw_mode, get_termios, get_termsize, set_termios, CUR_CLEAR_RIGHT,
        CUR_HIDE, CUR_SHOW, CUR_TO_START, RESTORE_VIDEO, REVERSE_VIDEO,
    },
    STATUS_TIMEOUT, UNNAMED_BUFFER, VERSION,
};
use libc::termios as Termios;
use std::{
    cmp::{max, min},
    io::{self, Read, Stdin, Stdout, Write},
    time::Instant,
};

pub struct Editor {
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    stdin: Stdin,
    original_termios: Termios,
    running: bool,
    status_message: String,
    status_time: Instant,
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
            screen_rows: screen_rows - 2, // stats+msg bars
            screen_cols,
            stdout: io::stdout(),
            stdin: io::stdin(),
            original_termios,
            running: true,
            buffers: Vec::new(),
            status_message: String::new(),
            status_time: Instant::now(),
        }
    }

    // TODO:
    //   - display an error rather than erroring
    //   - check if the file is already open
    pub fn open_file(&mut self, path: &str) -> io::Result<()> {
        self.buffers.insert(0, Buffer::new_from_file(path)?);

        Ok(())
    }

    #[inline]
    pub fn running(&self) -> bool {
        self.running
    }

    pub fn set_status_message(&mut self, msg: &str) {
        self.status_message.clear();
        self.status_message.push_str(msg);
        self.status_time = Instant::now();
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
        self.render_status_bar(&mut buf);
        self.render_message_bar(&mut buf);

        let (cy, rx) = self
            .buffers
            .get(0)
            .map(|b| (b.cy, b.rx))
            .unwrap_or_default();

        buf.push_str(&format!(
            "\x1b[{};{}H{CUR_SHOW}",
            cy - self.row_off() + 1,
            rx - self.col_off() + 1
        ));

        self.stdout.write_all(buf.as_bytes())?;
        self.stdout.flush()
    }

    fn render_rows(&self, buf: &mut String) {
        let buffer_len = self.buffers.get(0).map(|b| b.len()).unwrap_or_default();

        for y in 0..self.screen_rows {
            let file_row = y + self.row_off();

            if file_row >= buffer_len {
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
                let rline = &self.buffers[0].lines[file_row].render;
                let mut len = max(0, rline.len() - col_off);
                len = min(self.screen_cols, len);
                buf.push_str(&rline[col_off..min(self.screen_cols, len)]);
            }

            buf.push_str(&format!("{CUR_CLEAR_RIGHT}\r\n"));
        }
    }

    fn render_status_bar(&self, buf: &mut String) {
        let (name, n_lines, cy, rx, dirty) = if self.buffers.is_empty() {
            (UNNAMED_BUFFER, 1, 1, 1, false)
        } else {
            let b = &self.buffers[0];
            let name = b.display_name().unwrap_or(UNNAMED_BUFFER);

            (name, b.len(), b.cy + 1, b.rx + 1, b.dirty)
        };

        let lstatus = format!(
            "{name} - {n_lines} lines {}",
            if dirty { "[+]" } else { "" }
        );
        let rstatus = format!("{cy}:{rx}");
        let width = self.screen_cols - lstatus.len();
        buf.push_str(&format!(
            "{REVERSE_VIDEO}{lstatus}{rstatus:>width$}{RESTORE_VIDEO}\r\n"
        ));
    }

    fn render_message_bar(&self, buf: &mut String) {
        buf.push_str(CUR_CLEAR_RIGHT);

        let mut msg = self.status_message.clone();
        msg.truncate(self.screen_cols);

        let delta = (Instant::now() - self.status_time).as_secs();
        if !msg.is_empty() && delta < STATUS_TIMEOUT {
            buf.push_str(&msg);
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
            None => return Key::Esc,
        };
        let c3 = match self.try_read_char() {
            Some(c3) => c3,
            None => return Key::Esc,
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

        Key::Esc
    }

    pub fn handle_keypress(&mut self, k: Key) -> io::Result<()> {
        match k {
            Key::Ctrl('q') => {
                clear_screen(&mut self.stdout)?;
                self.running = false;
            }

            k => {
                if !self.buffers.is_empty() {
                    self.buffers[0].handle_keypress(k, self.screen_rows)?;
                }
            }
        }

        Ok(())
    }
}
