use libc::{ioctl, STDOUT_FILENO, TIOCGWINSZ};
use std::{
    fmt,
    io::{self, Stdout, Write},
};
use termios::{
    tcsetattr, Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, ISIG, ISTRIP, IXON, OPOST,
    TCSAFLUSH, VMIN, VTIME,
};

// ANSI escape codes:
//   https://vt100.net/docs/vt100-ug/chapter3.html
pub const CLEAR_SCREEN: &str = "\x1b[2J";
pub const CUR_TO_START: &str = "\x1b[H";
pub const CUR_HIDE: &str = "\x1b[?25l";
pub const CUR_SHOW: &str = "\x1b[?25h";
pub const CUR_CLEAR_RIGHT: &str = "\x1b[K";

pub(crate) fn die<D: fmt::Display>(msg: D) -> ! {
    let _ = clear_screen(&mut io::stdout());
    panic!("{}", msg);
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

pub(crate) fn clear_screen(stdout: &mut Stdout) -> io::Result<()> {
    stdout.write_all(format!("{CLEAR_SCREEN}{CUR_TO_START}").as_bytes())?;
    stdout.flush()
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
    if let Err(e) = tcsetattr(STDOUT_FILENO, TCSAFLUSH, &t) {
        die(format!("tcsetattr: {e}"));
    }
}
