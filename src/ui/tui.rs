//! A terminal UI for ad
use crate::{
    buffer::{Buffer, Buffers},
    config::ColorScheme,
    config_handle, die,
    dot::{LineRange, Range},
    editor::{Click, MiniBufferState},
    ftype::lex::{Token, TokenType, Tokens},
    input::Event,
    key::{Input, MouseButton, MouseEvent},
    restore_terminal_state,
    term::{
        clear_screen, enable_alternate_screen, enable_mouse_support, enable_raw_mode, get_termios,
        get_termsize, register_signal_handler, win_size_changed, CurShape,
    },
    term::{Cursor, Style},
    ui::{
        windows::{Column, View, Window},
        StateChange, UserInterface, Windows,
    },
    ORIGINAL_TERMIOS, VERSION,
};
use std::{
    cmp::min,
    io::{stdin, stdout, Read, Stdin, Stdout, Write},
    panic,
    sync::mpsc::Sender,
    thread::{spawn, JoinHandle},
    time::Instant,
};

const VLINE: char = '│';
const HLINE: char = '-';

#[derive(Debug)]
pub struct Tui {
    stdout: Stdout,
    screen_rows: usize,
    screen_cols: usize,
    status_message: String,
    last_status: Instant,
}

impl Default for Tui {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Tui {
    fn drop(&mut self) {
        restore_terminal_state(&mut self.stdout);
    }
}

impl Tui {
    pub fn new() -> Self {
        Self {
            stdout: stdout(),
            screen_rows: 0,
            screen_cols: 0,
            status_message: String::new(),
            last_status: Instant::now(),
        }
    }

    /// Render the visible lines of b for the available View.
    fn render_window(
        &self,
        b: &Buffer,
        view: &View,
        n_rows: usize,
        load_exec_range: Option<(bool, Range)>,
        cs: &ColorScheme,
    ) -> Vec<String> {
        let mut lines = Vec::with_capacity(n_rows);
        let (w_lnum, w_sgncol) = b.sign_col_dims();

        for y in 0..n_rows {
            let file_row = y + view.row_off;

            if file_row >= b.len_lines() {
                lines.push(format!(
                    "{}{}~ {VLINE:>width$}{}", // pad whitespace to view width
                    Style::Fg(cs.signcol_fg),
                    Style::Bg(cs.bg),
                    Style::Fg(cs.fg),
                    width = w_lnum
                ));
            } else {
                // +2 for the leading space and vline chars
                let padding = w_lnum + 2;
                lines.push(format!(
                    "{}{} {:>width$}{VLINE}{}{}",
                    Style::Fg(cs.signcol_fg),
                    Style::Bg(cs.bg),
                    file_row + 1,
                    Style::Fg(cs.fg),
                    styled_rline_unchecked(
                        b,
                        view,
                        file_row,
                        padding,
                        self.screen_cols,
                        load_exec_range,
                        cs
                    ),
                    width = w_lnum
                ));
            }
        }

        lines
    }

    /// Render UI lines for each window in the column, including horizontal separators to
    /// mark the boundaries between windows.
    fn render_column(
        &self,
        _col: &Column,
        _buffers: &Buffers,
        _load_exec_range: Option<(bool, Range)>,
        _screen_rows: usize,
        _cs: &ColorScheme,
    ) -> Vec<String> {
        // - Cut off the _bottom_ of the column based on available screen rows if they are less than
        //   expected thanks to the presence of a minibuffer.
        // - For each window in the column, produce rows for the available viewport
        // - insert '-' * col.n_cols to split each window
        todo!()
    }

    fn render_banner(&self, screen_rows: usize, cs: &ColorScheme) -> Vec<String> {
        let mut lines = Vec::with_capacity(screen_rows);
        let (w_lnum, w_sgncol) = (1, 3);
        let y_banner = self.screen_rows / 3;

        let banner_line = |mut banner: String| {
            let mut buf = String::new();
            banner.truncate(self.screen_cols - w_sgncol);
            let padding = (self.screen_cols - w_sgncol - banner.len()) / 2;
            buf.push_str(&" ".repeat(padding));
            buf.push_str(&banner);
            buf.push_str(&format!("{}\r\n", Cursor::ClearRight));

            buf
        };

        for y in 0..screen_rows {
            let mut line = format!(
                "{}{}~ {VLINE:>width$}{}",
                Style::Fg(cs.signcol_fg),
                Style::Bg(cs.bg),
                Style::Fg(cs.fg),
                width = w_lnum
            );

            if y == y_banner && y < screen_rows {
                line.push_str(&banner_line(format!("ad editor :: version {VERSION}")));
            } else if y == y_banner + 1 && y + 1 < screen_rows {
                line.push_str(&banner_line("type :help to view help".to_string()));
            }
            line.push_str(&format!("{}\r\n", Cursor::ClearRight));
            lines.push(line);
        }

        lines
    }

    fn render_windows(
        &self,
        buffers: &Buffers,
        windows: &Windows,
        load_exec_range: Option<(bool, Range)>,
        screen_rows: usize,
        cs: &ColorScheme,
    ) -> Vec<String> {
        if buffers.is_empty_scratch() {
            return self.render_banner(screen_rows, cs);
        }

        let mut rendered_cols: Vec<Vec<String>> = windows
            .cols
            .iter()
            .map(|col| self.render_column(col, buffers, load_exec_range, screen_rows, cs))
            .collect();

        let mut lines = Vec::with_capacity(screen_rows);
        for _ in 0..screen_rows {
            let mut line_fragments = Vec::with_capacity(rendered_cols.len() + 1);
            for i in 0..rendered_cols.len() {
                line_fragments.push(rendered_cols[i].remove(0));
            }
            let mut buf = line_fragments.join(&format!(
                "{}{}{VLINE}",
                Style::Fg(cs.signcol_fg),
                Style::Bg(cs.bg)
            ));
            buf.push_str(&format!("{}\r\n", Cursor::ClearRight));
            lines.push(buf);
        }

        lines
    }

    fn render_status_bar(&self, cs: &ColorScheme, mode_name: &str, b: &Buffer) -> String {
        let lstatus = format!(
            "{} {} - {} lines {}",
            mode_name,
            b.display_name(),
            b.len_lines(),
            if b.dirty { "[+]" } else { "" }
        );
        let rstatus = b.dot.addr(b);
        let width = self.screen_cols - lstatus.len();

        format!(
            "{}{}{lstatus}{rstatus:>width$}{}\r\n",
            Style::Bg(cs.bar_bg),
            Style::Fg(cs.fg),
            Style::Reset
        )
    }

    // current prompt and pending chars
    fn render_message_bar(
        &self,
        cs: &ColorScheme,
        pending_keys: &[Input],
        status_timeout: u64,
    ) -> String {
        let mut buf = String::new();
        buf.push_str(&Cursor::ClearRight.to_string());

        let mut msg = self.status_message.clone();
        msg.truncate(self.screen_cols.saturating_sub(10));

        let pending = render_pending(pending_keys);
        let delta = (Instant::now() - self.last_status).as_secs();

        if !msg.is_empty() && delta < status_timeout {
            let width = self.screen_cols - msg.len() - 10;
            buf.push_str(&format!(
                "{}{}{msg}{pending:>width$}",
                Style::Fg(cs.fg),
                Style::Bg(cs.bg)
            ));
        } else {
            let width = self.screen_cols - 10;
            buf.push_str(&format!(
                "{}{}{pending:>width$}          ",
                Style::Fg(cs.fg),
                Style::Bg(cs.bg)
            ));
        }

        buf
    }

    fn render_minibuffer_state(&self, mb: &MiniBufferState<'_>, cs: &ColorScheme) -> Vec<String> {
        let mut lines = Vec::new();

        if let Some(b) = mb.b {
            let width = self.screen_cols;

            for i in mb.top..=mb.bottom {
                let (rline, _) =
                    raw_rline_unchecked(b, &View::new(0), i, 0, self.screen_cols, None);
                let len = min(self.screen_cols, rline.len());
                if i == mb.selected_line_idx {
                    lines.push(format!(
                        "{}{}{:<width$}{}\r\n",
                        Style::Fg(cs.fg),
                        Style::Bg(cs.minibuffer_hl),
                        &rline[0..len],
                        Style::Reset,
                    ));
                } else {
                    lines.push(format!(
                        "{}{}{}{}\r\n",
                        Style::Fg(cs.fg),
                        Style::Bg(cs.bg),
                        &rline[0..len],
                        Cursor::ClearRight
                    ));
                }
            }
        }

        lines.push(format!(
            "{}{}{}{}{}",
            mb.prompt,
            mb.input,
            Style::Fg(cs.fg),
            Style::Bg(cs.bg),
            Cursor::ClearRight
        ));

        lines
    }
}

impl UserInterface for Tui {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
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
        self.screen_rows = screen_rows;
        self.screen_cols = screen_cols;

        spawn_input_thread(tx);

        (screen_rows, screen_cols)
    }

    fn shutdown(&mut self) {
        clear_screen(&mut self.stdout);
    }

    // TODO: handle the other state changes efficiently
    fn state_change(&mut self, change: StateChange) {
        let StateChange::StatusMessage { msg } = change;
        self.status_message = msg;
        self.last_status = Instant::now();
    }

    fn refresh(
        &mut self,
        mode_name: &str,
        buffers: &Buffers,
        windows: &Windows,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    ) {
        self.screen_rows = windows.screen_rows;
        self.screen_cols = windows.screen_cols;
        let w_minibuffer = mb.is_some();
        let mb = mb.unwrap_or_default();
        let active_buffer = buffers.active();
        let mb_lines = mb.b.map(|b| b.len_lines()).unwrap_or_default();
        let mb_offset = if mb_lines > 0 { 1 } else { 0 };

        // This is the screen size that we have to work with for the buffer content we currently want to
        // display. If the minibuffer is active then it take priority over anything else and we always
        // show the status bar as the final two lines of the UI.
        let effective_screen_rows = self.screen_rows - (mb.bottom - mb.top) - mb_offset;

        let (cs, status_timeout) = {
            let conf = config_handle!();
            (conf.colorscheme, conf.status_timeout)
        };

        let load_exec_range = match held_click {
            Some(click) if click.btn == MouseButton::Right || click.btn == MouseButton::Middle => {
                Some((click.btn == MouseButton::Right, click.selection))
            }
            _ => None,
        };

        // We need space for each visible line plus the two commands to hide/show the cursor
        let mut lines = Vec::with_capacity(self.screen_rows + 2);
        lines.push(format!("{}{}", Cursor::Hide, Cursor::ToStart));

        lines.append(&mut self.render_windows(
            buffers,
            windows,
            load_exec_range,
            effective_screen_rows,
            &cs,
        ));
        lines.push(self.render_status_bar(&cs, mode_name, active_buffer));

        if w_minibuffer {
            lines.append(&mut self.render_minibuffer_state(&mb, &cs));
        } else {
            lines.push(self.render_message_bar(&cs, pending_keys, status_timeout));
        }

        // Position the cursor
        let (x, y) = if w_minibuffer {
            (mb.cx, self.screen_rows + mb.n_visible_lines + 1)
        } else {
            windows.focused_view().ui_xy(active_buffer)
        };
        lines.push(format!("{}{}", Cursor::To(x + 1, y + 1), Cursor::Show));

        if let Err(e) = self.stdout.write_all(lines.join("").as_bytes()) {
            die!("Unable to refresh screen: {e}");
        }

        if let Err(e) = self.stdout.flush() {
            die!("Unable to refresh screen: {e}");
        }
    }

    fn set_cursor_shape(&mut self, cur_shape: CurShape) {
        if let Err(e) = self.stdout.write_all(cur_shape.to_string().as_bytes()) {
            // In this situation we're probably not going to be able to do all that much
            // but we might as well try
            die!("Unable to write to stdout: {e}");
        };
    }
}

fn render_pending(keys: &[Input]) -> String {
    let mut s = String::new();
    for k in keys {
        match k {
            Input::Char(c) if c.is_ascii_whitespace() => s.push_str(&format!("<{:x}>", *c as u8)),
            Input::Char(c) => s.push(*c),
            Input::Ctrl(c) => {
                s.push('^');
                s.push(*c);
            }
            Input::Alt(c) => {
                s.push('^');
                s.push('[');
                s.push(*c);
            }
            Input::CtrlAlt(c) => {
                s.push('^');
                s.push('[');
                s.push('^');
                s.push(*c);
            }

            _ => (),
        }
    }

    if s.len() > 10 {
        s = s.split_off(s.len() - 10);
    }

    s
}

/// The render representation of a given line, truncated to fit within the
/// available screen space.
/// This includes tab expansion but not any styling that might be applied,
/// trailing \r\n or screen clearing escape codes.
/// If a dot range is provided then the character offsets used will be adjusted
/// to account for expanded tab characters, returning None if self.col_off would
/// mean that the requested range is not currently visible.
fn raw_rline_unchecked(
    b: &Buffer,
    view: &View,
    y: usize,
    lpad: usize,
    screen_cols: usize,
    dot_range: Option<(usize, usize)>,
) -> (String, Option<(usize, usize)>) {
    let max_chars = screen_cols - lpad;
    let tabstop = config_handle!().tabstop;
    let mut rline = Vec::with_capacity(max_chars);
    // Iterating over characters not bytes as we need to account for multi-byte utf8
    let line = b.txt.line(y);
    let mut it = line.chars().skip(view.col_off);

    let mut update_dot = dot_range.is_some();
    let (mut start, mut end) = dot_range.unwrap_or_default();

    if update_dot && view.col_off > end {
        update_dot = false; // we're past the requested range
    } else {
        start = start.saturating_sub(view.col_off);
        end = end.saturating_sub(view.col_off);
    }

    while rline.len() <= max_chars {
        match it.next() {
            Some('\n') | None => break,
            Some('\t') => {
                if rline.len() < start {
                    start += tabstop - 1;
                }
                if rline.len() < end {
                    end = end.saturating_add(tabstop - 1);
                }
                rline.append(&mut [' '].repeat(tabstop));
            }
            Some(c) => rline.push(c),
        }
    }

    rline.truncate(max_chars); // noop if max_chars > rline.len()
    let n_chars = rline.len();
    // we pad to max_chars so that columns render correctly next to one another
    let padding = max_chars - n_chars;
    rline.append(&mut [' '].repeat(padding));
    let s = rline.into_iter().collect();

    if update_dot {
        start = min(start, n_chars);
        end = min(end, n_chars);
        (s, Some((start, end)))
    } else {
        (s, None)
    }
}

/// The render representation of a given line, truncated to fit within the
/// available screen space.
/// This includes tab expansion and any styling that might be applied but not
/// trailing \r\n or screen clearing escape codes.
fn styled_rline_unchecked(
    b: &Buffer,
    view: &View,
    y: usize,
    lpad: usize,
    screen_cols: usize,
    load_exec_range: Option<(bool, Range)>,
    cs: &ColorScheme,
) -> String {
    let map_line_range = |lr| match lr {
        // LineRange is an inclusive range so we need to insert after `end` if its
        // not the end of the line
        LineRange::Partial { start, end, .. } => (start, end + 1),
        LineRange::FromStart { end, .. } => (0, end + 1),
        LineRange::ToEnd { start, .. } => (start, usize::MAX),
        LineRange::Full { .. } => (0, usize::MAX),
    };

    let dot_range = b.dot.line_range(y, b).map(map_line_range);
    let (rline, dot_range) = raw_rline_unchecked(b, view, y, lpad, screen_cols, dot_range);

    let raw_tks = match &b.tokenizer {
        Some(t) => t.tokenize(&rline),
        None => Tokens::Single(Token {
            ty: TokenType::Default,
            s: &rline,
        }),
    };

    let mut tks = match dot_range {
        Some((start, end)) => raw_tks.with_highlighted_dot(start, end, TokenType::Dot),
        None => match raw_tks {
            Tokens::Single(tk) => vec![tk],
            Tokens::Multi(tks) => tks,
        },
    };

    match load_exec_range {
        Some((is_load, rng)) if !b.dot.contains_range(&rng) => {
            if let Some(lr) = rng.line_range(y, b).map(map_line_range) {
                if let (_, Some((start, end))) =
                    raw_rline_unchecked(b, view, y, lpad, screen_cols, Some(lr))
                {
                    let ty = if is_load {
                        TokenType::Load
                    } else {
                        TokenType::Execute
                    };
                    tks = Tokens::Multi(tks).with_highlighted_dot(start, end, ty);
                }
            }
        }

        _ => (),
    }

    let mut buf = String::new();
    for tk in tks.into_iter() {
        buf.push_str(&tk.render(cs));
    }

    buf.push_str(&Style::Bg(cs.bg).to_string());

    buf
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

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test_case("simple line", None, 0, "simple line       ", None; "simple line no dot")]
    #[test_case("simple line", Some((1, 5)), 0, "simple line       ", Some((1, 5)); "simple line partial")]
    #[test_case("simple line", Some((0, usize::MAX)), 0, "simple line       ", Some((0, 11)); "simple line full")]
    #[test_case("simple line", Some((0, 2)), 4, "le line           ", None; "scrolled past dot")]
    #[test_case("simple line", Some((0, 9)), 4, "le line           ", Some((0, 5)); "scrolled updating dot")]
    #[test_case("\twith tabs", Some((3, usize::MAX)), 0, "    with tabs     ", Some((6, 13)); "with tabs")]
    #[test_case("\twith tabs", Some((0, usize::MAX)), 0, "    with tabs     ", Some((0, 13)); "with tabs full")]
    #[test_case("\t\twith tabs", Some((4, usize::MAX)), 0, "        with tabs ", Some((10, 17)); "with multiple tabs")]
    #[test]
    fn raw_line_unchecked_updates_dot_correctly(
        line: &str,
        dot_range: Option<(usize, usize)>,
        col_off: usize,
        expected_line: &str,
        expected_dot_range: Option<(usize, usize)>,
    ) {
        let b = Buffer::new_unnamed(0, line);
        let mut view = View::new(0);
        view.col_off = col_off;

        let (line, dot_range) = raw_rline_unchecked(&b, &view, 0, 0, 18, dot_range);

        assert_eq!(dot_range, expected_dot_range);
        assert_eq!(line, expected_line);
    }
}
