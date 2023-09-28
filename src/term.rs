use libc::{
    ioctl, tcgetattr, tcsetattr, termios as Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN,
    ISIG, ISTRIP, IXON, OPOST, STDOUT_FILENO, TCSAFLUSH, TIOCGWINSZ, VMIN, VTIME,
};
use std::{
    io::{Stdout, Write},
    mem,
};

// ANSI escape codes:
//   https://vt100.net/docs/vt100-ug/chapter3.html
pub const CLEAR_SCREEN: &str = "\x1b[2J";
pub const CUR_TO_START: &str = "\x1b[H";
pub const CUR_HIDE: &str = "\x1b[?25l";
pub const CUR_SHOW: &str = "\x1b[?25h";
pub const CUR_CLEAR_RIGHT: &str = "\x1b[K";
pub const REVERSE_VIDEO: &str = "\x1b[7m";
pub const RESTORE_VIDEO: &str = "\x1b[m";

/// Helper for panicing the program but first ensuring that the screen is cleared
#[macro_export]
macro_rules! die {
    ($template:expr $(, $arg:expr)*) => {{
        $crate::term::clear_screen(&mut ::std::io::stdout());
        panic!($template $(, $arg)*)
    }};

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
    if let Err(e) = stdout.write_all(format!("{CLEAR_SCREEN}{CUR_TO_START}").as_bytes()) {
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
