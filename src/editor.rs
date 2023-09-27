use crate::{
    buffer::Buffer,
    die,
    key::{Arrow, Key},
    term::{
        clear_screen, enable_raw_mode, get_termsize, set_termios, CUR_CLEAR_RIGHT, CUR_HIDE,
        CUR_SHOW, CUR_TO_START,
    },
    VERSION,
};
use libc::STDOUT_FILENO;
use std::{
    cmp::min,
    io::{self, Read, Stdin, Stdout, Write},
};
use termios::Termios;

pub struct Editor {
    cx: usize,
    cy: usize,
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
        let original_termios = match Termios::from_fd(STDOUT_FILENO) {
            Ok(t) => t,
            Err(e) => die(format!("unable to init termios: {e}")),
        };

        enable_raw_mode(original_termios);

        Self {
            cx: 0,
            cy: 0,
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

    fn current_buffer_len(&self) -> usize {
        if self.buffers.is_empty() {
            0
        } else {
            self.buffers[0].len()
        }
    }

    pub fn refresh_screen(&mut self) -> io::Result<()> {
        let mut buf = format!("{CUR_HIDE}{CUR_TO_START}");
        self.render_rows(&mut buf);
        buf.push_str(&format!("\x1b[{};{}H{CUR_SHOW}", self.cy + 1, self.cx + 1));

        self.stdout.write_all(buf.as_bytes())?;
        self.stdout.flush()
    }

    fn render_rows(&self, buf: &mut String) {
        for y in 0..self.screen_rows {
            if y >= self.current_buffer_len() {
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
                let line = &self.buffers[0].lines[y];
                buf.push_str(&line[0..min(self.screen_cols, line.len())]);
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

    // The written char will be garbage if this function returns false
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
            Key::Arrow(arr) => self.move_cursor(arr),
            Key::Home => self.cx = 0,
            Key::End => self.cx = self.screen_cols - 1,
            Key::PageUp | Key::PageDown => {
                for _ in 0..self.screen_rows {
                    self.move_cursor(if k == Key::PageUp {
                        Arrow::Up
                    } else {
                        Arrow::Down
                    });
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

    fn move_cursor(&mut self, arr: Arrow) {
        match arr {
            Arrow::Up => {
                if self.cy != 0 {
                    self.cy -= 1;
                }
            }
            Arrow::Down => {
                if self.cy != self.screen_rows - 1 {
                    self.cy += 1;
                }
            }
            Arrow::Left => {
                if self.cx != 0 {
                    self.cx -= 1;
                }
            }
            Arrow::Right => {
                if self.cx != self.screen_cols - 1 {
                    self.cx += 1;
                }
            }
        }
    }
}
