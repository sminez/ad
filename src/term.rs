//! Terminal TUI support.
use crate::die;
use libc::{
    BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, ISIG, ISTRIP, IXON, OPOST, SA_SIGINFO, SIGWINCH,
    STDOUT_FILENO, TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME, c_int, c_void, ioctl, sigaction,
    sighandler_t, siginfo_t, tcgetattr, tcsetattr, termios as Termios,
};
use serde::Deserialize;
use std::{
    fmt,
    io::{self, Write},
    mem, ptr,
    sync::atomic::{AtomicBool, Ordering},
};

// ANSI escape codes:
//   https://vt100.net/docs/vt100-ug/chapter3.html
const CLEAR_SCREEN: &str = "\x1b[2J";
const ENABLE_MOUSE_SUPPORT: &str = "\x1b[?1000h\x1b[?1002h\x1b[?1015h\x1b[?1006h";
const DISABLE_MOUSE_SUPPORT: &str = "\x1b[?1006l\x1b[?1015l\x1b[?1002l\x1b[?1000l";
const ENABLE_ALTERNATE_SCREEN: &str = "\x1b[?1049h";
const DISABLE_ALTERNATE_SCREEN: &str = "\x1b[?1049l";
const ENABLE_BRACKETED_PASTE: &str = "\x1b[?2004h";
const DISABLE_BRACKETED_PASTE: &str = "\x1b[?2004l";
pub const RESET_STYLE: &str = "\x1b[m";

/// Used for storing and checking whether or not we've received a signal that our window
/// size has changed.
static WIN_SIZE_CHANGED: AtomicBool = AtomicBool::new(false);

extern "C" fn handle_win_size_change(_: c_int, _: *mut siginfo_t, _: *mut c_void) {
    WIN_SIZE_CHANGED.store(true, Ordering::Relaxed)
}

#[inline]
pub(crate) fn win_size_changed() -> bool {
    WIN_SIZE_CHANGED.swap(false, Ordering::Relaxed)
}

/// # Safety
/// must only be called once
pub unsafe fn register_signal_handler() {
    let mut maybe_sa = mem::MaybeUninit::<sigaction>::uninit();
    // SAFETY: we are meeting the C API requirements around usage of null pointers
    unsafe {
        if libc::sigemptyset(&mut (*maybe_sa.as_mut_ptr()).sa_mask) == -1 {
            die!(
                "Unable to register signal handler: {}",
                io::Error::last_os_error()
            )
        }

        let mut sa_ptr = *maybe_sa.as_mut_ptr();
        sa_ptr.sa_sigaction = handle_win_size_change as *const () as sighandler_t;
        sa_ptr.sa_flags = SA_SIGINFO;

        if libc::sigaction(SIGWINCH, &sa_ptr as *const _, ptr::null_mut()) == -1 {
            die!(
                "Unable to register signal handler: {}",
                io::Error::last_os_error()
            )
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl Color {
    pub fn as_rgb_hex_string(&self) -> String {
        let rgb: u32 = ((self.r as u32) << 16) + ((self.g as u32) << 8) + self.b as u32;
        format!("#{:0>6X}", rgb)
    }
}

impl From<Color> for String {
    fn from(value: Color) -> Self {
        value.as_rgb_hex_string()
    }
}

impl TryFrom<&str> for Color {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, String> {
        let [_, r, g, b] = match u32::from_str_radix(s.strip_prefix('#').unwrap_or(s), 16) {
            Ok(hex) => hex.to_be_bytes(),
            Err(e) => return Err(format!("invalid color ('{s}'): {e}")),
        };

        Ok(Self { r, g, b })
    }
}

impl TryFrom<String> for Color {
    type Error = String;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::try_from(value.as_str())
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Styles {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub bold: bool,
    pub italic: bool,
    pub underline: bool,
}

impl fmt::Display for Styles {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(fg) = self.fg {
            write!(f, "{}", Style::Fg(fg))?;
        }
        if let Some(bg) = self.bg {
            write!(f, "{}", Style::Bg(bg))?;
        }
        if self.bold {
            write!(f, "{}", Style::Bold)?;
        }
        if self.italic {
            write!(f, "{}", Style::Italic)?;
        }
        if self.underline {
            write!(f, "{}", Style::Underline)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Style {
    Fg(Color),
    Bg(Color),
    Bold,
    NoBold,
    Italic,
    NoItalic,
    Underline,
    NoUnderline,
    Reverse,
    NoReverse,
    Reset,
}

// https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#8-16-colors
impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Style::*;

        match self {
            Fg(Color { r, b, g }) => write!(f, "\x1b[38;2;{r};{g};{b}m"),
            Bg(Color { r, b, g }) => write!(f, "\x1b[48;2;{r};{g};{b}m"),
            Bold => write!(f, "\x1b[1m"),
            NoBold => write!(f, "\x1b[22m"),
            Italic => write!(f, "\x1b[3m"),
            NoItalic => write!(f, "\x1b[23m"),
            Underline => write!(f, "\x1b[4m"),
            NoUnderline => write!(f, "\x1b[24m"),
            Reverse => write!(f, "\x1b[7m"),
            NoReverse => write!(f, "\x1b[27m"),
            Reset => write!(f, "\x1b[m"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Cursor {
    To(usize, usize),
    ToStart,
    Hide,
    Show,
    ClearRight,
}

impl fmt::Display for Cursor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Cursor::*;

        match self {
            To(x, y) => write!(f, "\x1b[{y};{x}H"),
            ToStart => write!(f, "\x1b[H"),
            Hide => write!(f, "\x1b[?25l"),
            Show => write!(f, "\x1b[?25h"),
            ClearRight => write!(f, "\x1b[K"),
        }
    }
}

#[allow(dead_code)]
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

    // SAFETY: ts is a valid termsize struct to pass as a pointer here
    unsafe { ioctl(STDOUT_FILENO, TIOCGWINSZ, &mut ts as *mut _) };

    (ts.r as usize, ts.c as usize)
}

#[inline]
fn write_control_seq(seq: &str, desc: &str, stdout: &mut impl Write) {
    if let Err(e) = stdout.write_all(seq.as_bytes()) {
        panic!("unable to {desc}: {e}");
    }
    if let Err(e) = stdout.flush() {
        panic!("unable to {desc}: {e}");
    }
}

pub(crate) fn clear_screen(stdout: &mut impl Write) {
    write_control_seq(
        &format!("{CLEAR_SCREEN}{}", Cursor::ToStart),
        "clear screen",
        stdout,
    )
}

pub(crate) fn enable_mouse_support(stdout: &mut impl Write) {
    write_control_seq(ENABLE_MOUSE_SUPPORT, "enable mouse support", stdout)
}

pub(crate) fn disable_mouse_support(stdout: &mut impl Write) {
    write_control_seq(DISABLE_MOUSE_SUPPORT, "disable mouse support", stdout)
}

pub(crate) fn enable_alternate_screen(stdout: &mut impl Write) {
    write_control_seq(ENABLE_ALTERNATE_SCREEN, "enable alternate screen", stdout)
}

pub(crate) fn disable_alternate_screen(stdout: &mut impl Write) {
    write_control_seq(DISABLE_ALTERNATE_SCREEN, "disable alternate screen", stdout)
}

pub(crate) fn enable_bracketed_paste(stdout: &mut impl Write) {
    write_control_seq(ENABLE_BRACKETED_PASTE, "enable bracketed paste", stdout)
}

pub(crate) fn disable_bracketed_paste(stdout: &mut impl Write) {
    write_control_seq(DISABLE_BRACKETED_PASTE, "disable bracketed paste", stdout)
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
    // SAFETY: t is a valid termios struct to use as a pointer here
    if unsafe { tcsetattr(STDOUT_FILENO, TCSAFLUSH, &t) } == -1 {
        die!("tcsetattr");
    }
}

pub(crate) fn get_termios() -> Termios {
    // SAFETY: passing a null pointer here is valid
    unsafe {
        let mut t: Termios = mem::zeroed();
        if tcgetattr(STDOUT_FILENO, &mut t as *mut _) == -1 {
            die!("tcgetattr");
        }

        t
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn color_roundtrip() {
        let s = "#FF9E3B";
        let c: Color = s.try_into().unwrap();

        assert_eq!(c.as_rgb_hex_string(), s);
    }
}
