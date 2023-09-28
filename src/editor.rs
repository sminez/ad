use crate::{
    buffer::{Buffer, BufferKind, Buffers},
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
    fs,
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
    buffers: Buffers,
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
            status_message: String::new(),
            status_time: Instant::now(),
            buffers: Buffers::new(),
        }
    }

    // TODO:
    //   - display an error rather than erroring
    //   - check if the file is already open
    pub fn open_file(&mut self, path: &str) -> io::Result<()> {
        self.buffers.insert(Buffer::new_from_file(path)?);

        Ok(())
    }

    fn save_current_buffer(&mut self) -> io::Result<()> {
        let b = self.buffers.active_mut();
        let p = match b.kind {
            BufferKind::File(ref p) => p,
            _ => {
                self.set_status_message("Error: no file name");
                return Ok(()); // TODO: prompt for name if possible
            }
        };

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

    #[inline]
    pub fn running(&self) -> bool {
        self.running
    }

    pub fn set_status_message(&mut self, msg: &str) {
        self.status_message.clear();
        self.status_message.push_str(msg);
        self.status_time = Instant::now();
    }

    pub fn refresh_screen(&mut self) -> io::Result<()> {
        self.buffers
            .active_mut()
            .clamp_scroll(self.screen_rows, self.screen_cols);

        let mut buf = format!("{CUR_HIDE}{CUR_TO_START}");
        self.render_rows(&mut buf);
        self.render_status_bar(&mut buf);
        self.render_message_bar(&mut buf);

        let active = self.buffers.active();

        buf.push_str(&format!(
            "\x1b[{};{}H{CUR_SHOW}",
            active.cy - active.row_off + 1,
            active.rx - active.col_off + 1
        ));

        self.stdout.write_all(buf.as_bytes())?;
        self.stdout.flush()
    }

    fn render_rows(&self, buf: &mut String) {
        let Buffer {
            lines,
            row_off,
            col_off,
            ..
        } = self.buffers.active();

        for y in 0..self.screen_rows {
            let file_row = y + row_off;

            if file_row >= lines.len() {
                if self.buffers.is_empty_scratch() && y == self.screen_rows / 3 {
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
                let rline = &lines[file_row].render;
                let mut len = max(0, rline.len() - col_off);
                len = min(self.screen_cols, len);
                buf.push_str(&rline[*col_off..min(self.screen_cols, len)]);
            }

            buf.push_str(&format!("{CUR_CLEAR_RIGHT}\r\n"));
        }
    }

    fn render_status_bar(&self, buf: &mut String) {
        let b = self.buffers.active();
        let name = b.display_name().unwrap_or(UNNAMED_BUFFER);
        let (name, n_lines, cy, rx, dirty) = (name, b.len(), b.cy + 1, b.rx + 1, b.dirty);

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
                if self.buffers.active().dirty {
                    self.set_status_message(
                        "Current buffer is dirty: press C-q again to force quit",
                    );
                    self.refresh_screen()?;

                    match self.read_key() {
                        Key::Ctrl('q') => (),
                        k => return self.handle_keypress(k),
                    }
                }

                clear_screen(&mut self.stdout)?;
                self.running = false;
            }

            Key::Ctrl('s') => self.save_current_buffer()?,

            k => self
                .buffers
                .active_mut()
                .handle_keypress(k, self.screen_rows)?,
        }

        Ok(())
    }
}
