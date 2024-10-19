//! A terminal UI for ad
use crate::{
    die,
    editor::{Editor, MiniBufferState},
    input::Event,
    key::{Input, MouseEvent},
    restore_terminal_state,
    system::System,
    term::{
        clear_screen, enable_alternate_screen, enable_mouse_support, enable_raw_mode, get_termios,
        get_termsize, register_signal_handler, win_size_changed, CurShape,
    },
    ui::Ui,
    ORIGINAL_TERMIOS,
};
use std::{
    io::{stdin, stdout, Read, Stdin, Stdout, Write},
    panic,
    sync::mpsc::Sender,
    thread::{spawn, JoinHandle},
};

#[derive(Debug)]
pub struct Tui {
    stdout: Stdout,
}

impl Tui {
    pub fn new() -> Self {
        Self { stdout: stdout() }
    }
}

impl Ui for Tui {
    fn init<S>(&mut self, ed: &mut Editor<S>, tx: Sender<Event>)
    where
        S: System,
    {
        let original_termios = get_termios();
        enable_raw_mode(original_termios);
        _ = ORIGINAL_TERMIOS.set(original_termios);

        panic::set_hook(Box::new(|panic_info| {
            let mut stdout = stdout();
            restore_terminal_state(&mut stdout);
            _ = stdout.flush();

            // Restoring the terminal state to move us off of the alternate screen
            // can race with our attempt to print the panic info so given that we
            // are already in a fatal situation, sleeping briefly to ensure that
            // the cause of the panic is visible before we exit isn't _too_ bad.
            std::thread::sleep(std::time::Duration::from_millis(300));
            eprintln!("Fatal error:\n{panic_info}");
            _ = std::fs::write("/tmp/ad.panic", format!("{panic_info}"));
        }));

        enable_mouse_support(&mut self.stdout);
        enable_alternate_screen(&mut self.stdout);

        // SAFETY: we only register our signal handler once
        unsafe { register_signal_handler() };

        let (screen_rows, screen_cols) = get_termsize();
        ed.update_window_size(screen_rows, screen_cols);

        self.set_cursor_shape(ed.current_cursor_shape());

        spawn_input_thread(tx);
    }

    fn shutdown(&mut self) {
        clear_screen(&mut self.stdout);
    }

    fn refresh<S>(&mut self, ed: &mut Editor<S>, mb: Option<MiniBufferState<'_>>)
    where
        S: System,
    {
        ed.refresh_screen_w_minibuffer(mb)
    }

    fn set_cursor_shape(&mut self, cur_shape: CurShape) {
        if let Err(e) = self.stdout.write_all(cur_shape.to_string().as_bytes()) {
            // In this situation we're probably not going to be able to do all that much
            // but we might as well try
            die!("Unable to write to stdout: {e}");
        };
    }
}

/// Spawn a thread to read from stdin and process user input to send Events to
/// the main editor event loop.
fn spawn_input_thread(tx: Sender<Event>) -> JoinHandle<()> {
    let mut stdin = stdin();

    spawn(move || loop {
        if let Some(key) = try_read_input(&mut stdin) {
            _ = tx.send(Event::Input(key));
        } else if win_size_changed() {
            let (rows, cols) = get_termsize();
            _ = tx.send(Event::WinsizeChanged { rows, cols });
        }
    })
}

fn try_read_char(stdin: &mut Stdin) -> Option<char> {
    let mut buf: [u8; 1] = [0; 1];
    if stdin.read_exact(&mut buf).is_ok() {
        Some(buf[0] as char)
    } else {
        None
    }
}

fn try_read_input(stdin: &mut Stdin) -> Option<Input> {
    let c = try_read_char(stdin)?;

    // Normal key press
    match Input::from_char(c) {
        Input::Esc => (),
        key => return Some(key),
    }

    let c2 = match try_read_char(stdin) {
        Some(c2) => c2,
        None => return Some(Input::Esc),
    };
    let c3 = match try_read_char(stdin) {
        Some(c3) => c3,
        None => return Some(Input::try_from_seq2(c, c2).unwrap_or(Input::Esc)),
    };

    if let Some(key) = Input::try_from_seq2(c2, c3) {
        return Some(key);
    }

    if c2 == '[' && c3.is_ascii_digit() {
        if let Some('~') = try_read_char(stdin) {
            if let Some(key) = Input::try_from_bracket_tilde(c3) {
                return Some(key);
            }
        }
    }

    // xterm mouse encoding: "^[< Cb;Cx;Cy(;) (M or m) "
    if c2 == '[' && c3 == '<' {
        let mut buf = Vec::new();
        let m;

        loop {
            match try_read_char(stdin) {
                Some(c @ 'm' | c @ 'M') => {
                    m = c;
                    break;
                }
                Some(c) => buf.push(c as u8),
                None => return None,
            };
        }
        let s = String::from_utf8(buf).unwrap();
        let nums: Vec<usize> = s.split(';').map(|s| s.parse::<usize>().unwrap()).collect();
        let (b, x, y) = (nums[0], nums[1], nums[2]);

        return MouseEvent::try_from_raw(b, x, y, m).map(Input::Mouse);
    }

    Some(Input::Esc)
}
