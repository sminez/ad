//! Terminal TUI support.
use crate::{
    die,
    ui::style::{Color, CurShape, Styles},
};
use libc::{
    BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, ISIG, ISTRIP, IXON, OPOST, SA_SIGINFO, SIGWINCH,
    STDOUT_FILENO, TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME, c_int, c_void, ioctl, sigaction,
    sighandler_t, siginfo_t, tcgetattr, tcsetattr, termios as Termios,
};
use std::{
    borrow::Cow,
    fmt,
    io::{self, Write},
    mem, ptr,
    sync::{
        OnceLock,
        atomic::{AtomicBool, Ordering},
    },
};

pub(crate) static ORIGINAL_TERMIOS: OnceLock<Termios> = OnceLock::new();

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

/// Restore the terminal state to what we had originally before starting our UI.
pub(crate) fn restore_terminal_state(so: &mut impl Write) {
    disable_alternate_screen(so);
    disable_mouse_support(so);
    disable_bracketed_paste(so);
    let t = match ORIGINAL_TERMIOS.get() {
        Some(t) => t,
        None => return,
    };
    set_termios(*t);
}

impl Styles {
    pub fn as_ansi(&self) -> String {
        [
            self.fg.as_ref().map(|fg| AnsiStyle::Fg(*fg).as_ansi()),
            self.bg.as_ref().map(|bg| AnsiStyle::Bg(*bg).as_ansi()),
            self.bold.then(|| AnsiStyle::Bold.as_ansi()),
            self.italic.then(|| AnsiStyle::Italic.as_ansi()),
            self.underline.then(|| AnsiStyle::Underline.as_ansi()),
        ]
        .iter()
        .flatten()
        .map(|cow| cow.as_ref())
        .collect()
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnsiStyle {
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

impl AnsiStyle {
    // https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797#8-16-colors
    pub fn as_ansi(&self) -> Cow<'static, str> {
        use AnsiStyle::*;

        match self {
            Fg(Color { r, b, g }) => Cow::Owned(format!("\x1b[38;2;{r};{g};{b}m")),
            Bg(Color { r, b, g }) => Cow::Owned(format!("\x1b[48;2;{r};{g};{b}m")),
            Bold => Cow::Borrowed("\x1b[1m"),
            NoBold => Cow::Borrowed("\x1b[22m"),
            Italic => Cow::Borrowed("\x1b[3m"),
            NoItalic => Cow::Borrowed("\x1b[23m"),
            Underline => Cow::Borrowed("\x1b[4m"),
            NoUnderline => Cow::Borrowed("\x1b[24m"),
            Reverse => Cow::Borrowed("\x1b[7m"),
            NoReverse => Cow::Borrowed("\x1b[27m"),
            Reset => Cow::Borrowed("\x1b[m"),
        }
    }
}

impl fmt::Display for AnsiStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_ansi())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CursorAction {
    To(usize, usize),
    ToStart,
    Hide,
    Show,
    ClearRight,
}

impl CursorAction {
    pub fn as_ansi(&self) -> Cow<'static, str> {
        match self {
            CursorAction::To(x, y) => Cow::Owned(format!("\x1b[{y};{x}H")),
            CursorAction::ToStart => Cow::Borrowed("\x1b[H"),
            CursorAction::Hide => Cow::Borrowed("\x1b[?25l"),
            CursorAction::Show => Cow::Borrowed("\x1b[?25h"),
            CursorAction::ClearRight => Cow::Borrowed("\x1b[K"),
        }
    }
}

impl fmt::Display for CursorAction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_ansi())
    }
}

impl CurShape {
    pub const fn as_ansi(&self) -> &'static str {
        match self {
            CurShape::BlinkingBlock => "\x1b[\x31 q",
            CurShape::Block => "\x1b[\x32 q",
            CurShape::BlinkingUnderline => "\x1b[\x33 q",
            CurShape::Underline => "\x1b[\x34 q",
            CurShape::BlinkingBar => "\x1b[\x35 q",
            CurShape::Bar => "\x1b[\x36 q",
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
        &format!("{CLEAR_SCREEN}{}", CursorAction::ToStart),
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
