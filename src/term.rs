use crate::die;
use libc::{
    ioctl, tcgetattr, tcsetattr, termios as Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN,
    ISIG, ISTRIP, IXON, OPOST, STDOUT_FILENO, TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME,
};
use std::{
    fmt,
    io::{Stdout, Write},
    mem,
};

// ANSI escape codes:
//   https://vt100.net/docs/vt100-ug/chapter3.html
pub const CLEAR_SCREEN: &str = "\x1b[2J";

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Color {
    r: u8,
    b: u8,
    g: u8,
}

impl From<&str> for Color {
    fn from(s: &str) -> Self {
        let [_, r, g, b] = match u32::from_str_radix(s.strip_prefix('#').unwrap_or(s), 16) {
            Ok(hex) => hex.to_be_bytes(),
            Err(e) => die!("invalid color ('{s}'): {e}"),
        };

        Self { r, g, b }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Style {
    Fg(Color),
    Bg(Color),
    Bold,
    Italic,
    Underline,
    Reverse,
    Reset,
}

impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Style::*;

        match self {
            Fg(Color { r, b, g }) => write!(f, "\x1b[38;2;{r};{g};{b}m"),
            Bg(Color { r, b, g }) => write!(f, "\x1b[48;2;{r};{g};{b}m"),
            Bold => write!(f, "\x1b[1m"),
            Italic => write!(f, "\x1b[3m"),
            Underline => write!(f, "\x1b[4m"),
            Reverse => write!(f, "\x1b[7m"),
            Reset => write!(f, "\x1b[m"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cur {
    To(usize, usize),
    ToStart,
    Hide,
    Show,
    ClearRight,
}

impl fmt::Display for Cur {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Cur::*;

        match self {
            To(x, y) => write!(f, "\x1b[{y};{x}H"),
            ToStart => write!(f, "\x1b[H"),
            Hide => write!(f, "\x1b[?25l"),
            Show => write!(f, "\x1b[?25h"),
            ClearRight => write!(f, "\x1b[K"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CurShape {
    Block,
    Bar,
    Underline,
    BlinkingBlock,
    BlinkingBar,
    BlinkingUnderline,
}

impl fmt::Display for CurShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use CurShape::*;

        match self {
            BlinkingBlock => write!(f, "\x1b[\x31 q"),
            Block => write!(f, "\x1b[\x32 q"),
            BlinkingUnderline => write!(f, "\x1b[\x33 q"),
            Underline => write!(f, "\x1b[\x34 q"),
            BlinkingBar => write!(f, "\x1b[\x35 q"),
            Bar => write!(f, "\x1b[\x36 q"),
        }
    }
}

/// Request the current terminal size from the kernel using ioctl
pub(crate) fn get_termsize() -> (usize, usize) {
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

pub(crate) fn clear_screen(stdout: &mut Stdout) {
    if let Err(e) = stdout.write_all(format!("{CLEAR_SCREEN}{}", Cur::ToStart).as_bytes()) {
        panic!("unable to clear screen: {e}");
    }
    if let Err(e) = stdout.flush() {
        panic!("unable to clear screen: {e}");
    }
}

pub(crate) fn enable_raw_mode(mut t: Termios) {
    t.c_iflag &= !(BRKINT | ICRNL | ISTRIP | IXON);
    t.c_oflag &= !OPOST;
    t.c_cflag |= CS8;
    t.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    t.c_cc[VMIN] = 0;
    t.c_cc[VTIME] = 1;

    set_termios(t);
}

pub(crate) fn set_termios(t: Termios) {
    if unsafe { tcsetattr(STDOUT_FILENO, TCSAFLUSH, &t) } == -1 {
        die!("tcsetattr");
    }
}

pub(crate) fn get_termios() -> Termios {
    unsafe {
        let mut t: Termios = mem::zeroed();
        if tcgetattr(STDOUT_FILENO, &mut t as *mut _) == -1 {
            die!("tcgetattr");
        }

        t
    }
}
