use libc::{ioctl, STDOUT_FILENO, TIOCGWINSZ};
use std::{
    fmt,
    io::{self, Read, Stdin, Stdout, Write},
    os::fd::AsRawFd,
};
use termios::{
    tcsetattr, Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, ISIG, ISTRIP, IXON, OPOST,
    TCSAFLUSH, VMIN, VTIME,
};

const VERSION: &str = "1.0.0";

// ANSI escape codes:
//   https://vt100.net/docs/vt100-ug/chapter3.html
const CLEAR_SCREEN: &str = "\x1b[2J";
const CUR_TO_START: &str = "\x1b[H";
const CUR_HIDE: &str = "\x1b[?25l";
const CUR_SHOW: &str = "\x1b[?25h";
const CUR_CLEAR_RIGHT: &str = "\x1b[K";

// const fn ctrl_key(c: char) -> char {
//     (c as u8 & 0x1f) as char
// }

fn get_termsize() -> (usize, usize) {
    #[repr(C)]
    struct Termsize {
        r: u16,
        c: u16,
        x: u16,
        y: u16,
    }

    let mut ts = Termsize {
        r: 0,
        c: 0,
        x: 0,
        y: 0,
    };
    unsafe {
        ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut ts as *mut _);
    }

    (ts.r as usize, ts.c as usize)
}

fn clear_screen(stdout: &mut Stdout) -> io::Result<()> {
    stdout.write_all(format!("{CLEAR_SCREEN}{CUR_TO_START}").as_bytes())?;
    stdout.flush()
}

fn die<D: fmt::Display>(msg: D) -> ! {
    let _ = clear_screen(&mut io::stdout());
    panic!("{}", msg);
}

#[derive(Debug, PartialEq, Eq)]
enum Key {
    Char(char),
    Ctrl(char),
    Tab,
    Return,
    Backspace,
    Arrow(Arrow),
    Del,
    Home,
    End,
    PageUp,
    PageDown,
}

#[derive(Debug, PartialEq, Eq)]
enum Arrow {
    Up,
    Down,
    Left,
    Right,
}

impl Key {
    fn try_from_char(c: char) -> Option<Self> {
        match c {
            '\x1b' => None,
            '\n' | '\r' => Some(Key::Return),
            '\x7f' => Some(Key::Backspace),
            '\t' => Some(Key::Tab),
            c @ '\x01'..='\x1A' => Some(Key::Ctrl((c as u8 - 0x1 + b'a') as char)),
            c @ '\x1C'..='\x1F' => Some(Key::Ctrl((c as u8 - 0x1C + b'4') as char)),
            _ => Some(Key::Char(c)),
        }
    }

    fn try_from_seq2(c1: char, c2: char) -> Option<Self> {
        match (c1, c2) {
            ('O', 'H') => Some(Key::Home),
            ('O', 'F') => Some(Key::End),

            ('[', 'A') => Some(Key::Arrow(Arrow::Up)),
            ('[', 'B') => Some(Key::Arrow(Arrow::Down)),
            ('[', 'C') => Some(Key::Arrow(Arrow::Right)),
            ('[', 'D') => Some(Key::Arrow(Arrow::Left)),
            _ => None,
        }
    }

    fn try_from_bracket_tilde(c: char) -> Option<Self> {
        match c {
            '1' | '7' => Some(Key::Home),
            '4' | '8' => Some(Key::End),
            '3' => Some(Key::Del),
            '5' => Some(Key::PageUp),
            '6' => Some(Key::PageDown),
            _ => None,
        }
    }
}

struct Editor {
    cx: usize,
    cy: usize,
    screen_rows: usize,
    screen_cols: usize,
    stdout: Stdout,
    stdin: Stdin,
    original_termios: Termios,
    running: bool,
}

impl Drop for Editor {
    fn drop(&mut self) {
        self.disable_raw_mode()
    }
}

impl Editor {
    fn new() -> Self {
        let (screen_rows, screen_cols) = get_termsize();
        let original_termios = match Termios::from_fd(STDOUT_FILENO) {
            Ok(t) => t,
            Err(e) => die(format!("unable to init termios: {e}")),
        };

        let mut e = Self {
            cx: 0,
            cy: 0,
            screen_rows,
            screen_cols,
            stdout: io::stdout(),
            stdin: io::stdin(),
            original_termios,
            running: true,
        };

        e.enable_raw_mode();

        e
    }

    fn enable_raw_mode(&mut self) {
        let mut t = self.original_termios;

        t.c_iflag &= !(BRKINT | ICRNL | ISTRIP | IXON);
        t.c_oflag &= !OPOST;
        t.c_cflag |= CS8;
        t.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
        t.c_cc[VMIN] = 0;
        t.c_cc[VTIME] = 1;

        if let Err(e) = tcsetattr(self.stdout.as_raw_fd(), TCSAFLUSH, &t) {
            die(format!("tcsetattr while enabling raw mode: {e}"));
        }
    }

    fn disable_raw_mode(&self) {
        if let Err(e) = tcsetattr(self.stdout.as_raw_fd(), TCSAFLUSH, &self.original_termios) {
            die(format!("tcsetattr while disabling raw mode: {e}"));
        }
    }

    fn refresh_screen(&mut self) -> io::Result<()> {
        let mut buf = format!("{CUR_HIDE}{CUR_TO_START}");
        self.render_rows(&mut buf);
        buf.push_str(&format!("\x1b[{};{}H{CUR_SHOW}", self.cy + 1, self.cx + 1));

        self.stdout.write_all(buf.as_bytes())?;
        self.stdout.flush()
    }

    fn render_rows(&self, buf: &mut String) {
        for y in 0..self.screen_rows {
            if y == self.screen_rows / 3 {
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

    fn read_key(&mut self) -> Key {
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

    fn handle_keypress(&mut self, k: Key) -> io::Result<()> {
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

fn main() -> io::Result<()> {
    let mut e = Editor::new();

    while e.running {
        e.refresh_screen().unwrap();
        let k = e.read_key();
        e.handle_keypress(k)?;
    }

    Ok(())
}
