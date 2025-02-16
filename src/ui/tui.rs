//! A terminal UI for ad
use crate::{
    buffer::{Buffer, Chars, GapBuffer},
    config::ColorScheme,
    config_handle, die,
    dot::Range,
    editor::{Click, MiniBufferState},
    input::Event,
    key::{Input, MouseButton, MouseEvent},
    restore_terminal_state,
    term::{
        clear_screen, enable_alternate_screen, enable_mouse_support, enable_raw_mode, get_termios,
        get_termsize, register_signal_handler, win_size_changed, CurShape, Cursor, Style, Styles,
        RESET_STYLE,
    },
    ts::{LineIter, RangeToken},
    ui::{
        layout::{Column, Window},
        Layout, StateChange, UserInterface,
    },
    ziplist, ORIGINAL_TERMIOS, VERSION,
};
use std::{
    cell::RefCell,
    char,
    cmp::{min, Ordering},
    collections::HashMap,
    io::{stdin, stdout, Read, Stdout, Write},
    iter::{repeat_n, Peekable},
    panic,
    rc::Rc,
    sync::mpsc::Sender,
    thread::{spawn, JoinHandle},
    time::Instant,
};
use unicode_width::UnicodeWidthChar;

// const HLINE: &str = "—"; // em dash
const HLINE: &str = "-";
const VLINE: &str = "│";
const TSTR: &str = "├";
const XSTR: &str = "┼";

fn box_draw_str(s: &str, cs: &ColorScheme) -> String {
    format!("{}{}{s}", Style::Fg(cs.minibuffer_hl), Style::Bg(cs.bg))
}

#[derive(Debug)]
pub struct Tui {
    stdout: Stdout,
    screen_rows: usize,
    screen_cols: usize,
    status_message: String,
    last_status: Instant,
    // Box elements for rendering window borders
    vstr: String,
    xstr: String,
    tstr: String,
    hvh: String,
    vh: String,
    // Cache of the ANSI escape code strings required for each fully qualified tree-sitter
    // highlighting tag. See render_line for details on how the cache is used.
    style_cache: Rc<RefCell<HashMap<String, String>>>,
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
        let mut tui = Self {
            stdout: stdout(),
            screen_rows: 0,
            screen_cols: 0,
            status_message: String::new(),
            last_status: Instant::now(),
            vstr: String::new(),
            tstr: String::new(),
            xstr: String::new(),
            hvh: String::new(),
            vh: String::new(),
            style_cache: Default::default(),
        };
        tui.update_cached_elements();

        tui
    }

    fn update_cached_elements(&mut self) {
        let cs = &config_handle!().colorscheme;
        let vstr = box_draw_str(VLINE, cs);
        let hstr = box_draw_str(HLINE, cs);
        self.tstr = box_draw_str(TSTR, cs);
        self.xstr = box_draw_str(XSTR, cs);
        self.hvh = format!("{hstr}{vstr}{hstr}");
        self.vh = format!("{vstr}{hstr}");
        self.vstr = vstr;
        self.style_cache.borrow_mut().clear();
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
                "{}{}{msg}{pending:>width$}          ",
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

    fn render_minibuffer_state(
        &self,
        mb: &MiniBufferState<'_>,
        tabstop: usize,
        cs: &ColorScheme,
    ) -> Vec<String> {
        let mut lines = Vec::new();

        if let Some(b) = mb.b {
            for i in mb.top..=mb.bottom {
                let slice = b.line(i).unwrap();
                let bg = if i == mb.selected_line_idx {
                    cs.minibuffer_hl
                } else {
                    cs.bg
                };

                let mut cols = 0;
                let mut chars = slice.chars().peekable();
                let mut rline = Styles {
                    fg: Some(cs.fg),
                    bg: Some(bg),
                    ..Default::default()
                }
                .to_string();

                render_chars(
                    &mut chars,
                    None,
                    self.screen_cols,
                    tabstop,
                    &mut cols,
                    &mut rline,
                );

                if i == mb.selected_line_idx && cols < self.screen_cols {
                    rline.push_str(&Style::Bg(cs.minibuffer_hl).to_string());
                }

                let len = min(self.screen_cols, rline.len());
                let width = self.screen_cols;
                lines.push(format!(
                    "{:<width$}{}\r\n",
                    &rline[0..len],
                    Cursor::ClearRight
                ));
            }
        }

        lines.push(format!(
            "{}{}{}{}{}",
            Style::Fg(cs.fg),
            Style::Bg(cs.bg),
            mb.prompt,
            mb.input,
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

    fn state_change(&mut self, change: StateChange) {
        match change {
            StateChange::ConfigUpdated => self.update_cached_elements(),
            StateChange::StatusMessage { msg } => {
                self.status_message = msg;
                self.last_status = Instant::now();
            }
        }
    }

    fn refresh(
        &mut self,
        mode_name: &str,
        layout: &Layout,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    ) {
        self.screen_rows = layout.screen_rows;
        self.screen_cols = layout.screen_cols;
        let w_minibuffer = mb.is_some();
        let mb = mb.unwrap_or_default();
        let active_buffer = layout.active_buffer();
        let mb_lines = mb.b.map(|b| b.len_lines()).unwrap_or_default();
        let mb_offset = if mb_lines > 0 { 1 } else { 0 };

        // This is the screen size that we have to work with for the buffer content we currently want to
        // display. If the minibuffer is active then it take priority over anything else and we always
        // show the status bar as the final two lines of the UI.
        let effective_screen_rows = self.screen_rows - (mb.bottom - mb.top) - mb_offset;

        let conf = config_handle!();
        let (cs, status_timeout, tabstop) = (&conf.colorscheme, conf.status_timeout, conf.tabstop);

        let load_exec_range = match held_click {
            Some(click) if click.btn == MouseButton::Right || click.btn == MouseButton::Middle => {
                Some((click.btn == MouseButton::Right, click.selection))
            }
            _ => None,
        };

        // We need space for each visible line plus the two commands to hide/show the cursor
        let mut lines = Vec::with_capacity(self.screen_rows + 2);
        lines.push(format!("{}{}", Cursor::Hide, Cursor::ToStart));

        if layout.is_empty_scratch() {
            lines.append(&mut self.render_banner(effective_screen_rows, cs));
        } else {
            lines.extend(WinsIter::new(
                layout,
                load_exec_range,
                effective_screen_rows,
                tabstop,
                self,
                cs,
            ));
        }

        lines.push(self.render_status_bar(cs, mode_name, active_buffer));

        if w_minibuffer {
            lines.append(&mut self.render_minibuffer_state(&mb, tabstop, cs));
        } else {
            lines.push(self.render_message_bar(cs, pending_keys, status_timeout));
        }

        // Position the cursor
        let (x, y) = if w_minibuffer {
            (mb.cx, self.screen_rows + mb.n_visible_lines + 1)
        } else {
            layout.ui_xy(active_buffer)
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

struct WinsIter<'a> {
    col_iters: Vec<ColIter<'a>>,
    buf: Vec<String>,
    vstr: &'a str,
    xstr: &'a str,
    tstr: &'a str,
    hvh: &'a str,
    vh: &'a str,
}

impl<'a> WinsIter<'a> {
    fn new(
        layout: &'a Layout,
        load_exec_range: Option<(bool, Range)>,
        screen_rows: usize,
        tabstop: usize,
        tui: &'a Tui,
        cs: &'a ColorScheme,
    ) -> Self {
        let col_iters: Vec<_> = layout
            .cols
            .iter()
            .map(|(is_focus, col)| {
                let rng = if is_focus { load_exec_range } else { None };
                ColIter::new(
                    col,
                    layout,
                    rng,
                    screen_rows,
                    tabstop,
                    cs,
                    tui.style_cache.clone(),
                )
            })
            .collect();
        let buf = Vec::with_capacity(col_iters.len());

        Self {
            col_iters,
            buf,
            vstr: &tui.vstr,
            xstr: &tui.xstr,
            tstr: &tui.tstr,
            hvh: &tui.hvh,
            vh: &tui.vh,
        }
    }
}

impl Iterator for WinsIter<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        self.buf.clear();

        for it in self.col_iters.iter_mut() {
            self.buf.push(it.next()?);
        }

        let mut buf = self.buf.join(self.vstr);
        buf = buf.replace(self.hvh, self.xstr).replace(self.vh, self.tstr);
        buf.push_str(&format!("{}\r\n", Cursor::ClearRight));

        Some(buf)
    }
}

struct ColIter<'a> {
    inner: ziplist::Iter<'a, Window>,
    current: Option<WinIter<'a>>,
    layout: &'a Layout,
    cs: &'a ColorScheme,
    style_cache: Rc<RefCell<HashMap<String, String>>>,
    load_exec_range: Option<(bool, Range)>,
    screen_rows: usize,
    tabstop: usize,
    n_cols: usize,
    yielded: usize,
}

impl<'a> ColIter<'a> {
    fn new(
        col: &'a Column,
        layout: &'a Layout,
        load_exec_range: Option<(bool, Range)>,
        screen_rows: usize,
        tabstop: usize,
        cs: &'a ColorScheme,
        style_cache: Rc<RefCell<HashMap<String, String>>>,
    ) -> Self {
        ColIter {
            inner: col.wins.iter(),
            current: None,
            layout,
            cs,
            style_cache,
            load_exec_range,
            screen_rows,
            tabstop,
            n_cols: col.n_cols,
            yielded: 0,
        }
    }
}

impl<'a> ColIter<'a> {
    fn next_win_iter(&mut self) -> Option<WinIter<'a>> {
        let (is_focus, w) = self.inner.next()?;
        let b = self
            .layout
            .buffer_with_id(w.view.bufid)
            .expect("valid buffer id");

        let (w_lnum, _) = b.sign_col_dims();
        let rng = if is_focus { self.load_exec_range } else { None };
        let it = b.iter_tokenized_lines_from(w.view.row_off, rng);

        Some(WinIter {
            y: 0,
            w_lnum,
            n_cols: self.n_cols,
            tabstop: self.tabstop,
            it,
            gb: &b.txt,
            w,
            cs: self.cs,
            style_cache: self.style_cache.clone(),
        })
    }
}

impl Iterator for ColIter<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.is_none() {
            self.current = Some(self.next_win_iter()?);
        }

        let next_line = self.current.as_mut()?.next();
        if self.yielded == self.screen_rows {
            return None;
        }
        self.yielded += 1;

        match next_line {
            Some(line) => Some(line),
            None => {
                self.current = None;
                Some(box_draw_str(&HLINE.repeat(self.n_cols), self.cs))
            }
        }
    }
}

struct WinIter<'a> {
    y: usize,
    w_lnum: usize,
    n_cols: usize,
    tabstop: usize,
    it: LineIter<'a>,
    gb: &'a GapBuffer,
    w: &'a Window,
    cs: &'a ColorScheme,
    style_cache: Rc<RefCell<HashMap<String, String>>>,
}

impl Iterator for WinIter<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if self.y >= self.w.n_rows {
            return None;
        }
        let file_row = self.y + self.w.view.row_off;
        self.y += 1;

        let next = self.it.next();

        let line = match next {
            None => {
                let mut buf = format!(
                    "{}{}~ {VLINE:>width$}{}",
                    Style::Fg(self.cs.signcol_fg),
                    Style::Bg(self.cs.bg),
                    Style::Fg(self.cs.fg),
                    width = self.w_lnum
                );
                let padding = self.n_cols - self.w_lnum - 2;
                buf.push_str(&" ".repeat(padding));

                buf
            }

            Some(it) => {
                // +2 for the leading space and vline chars
                let padding = self.w_lnum + 2;

                format!(
                    "{}{} {:>width$}{VLINE}{}",
                    Style::Fg(self.cs.signcol_fg),
                    Style::Bg(self.cs.bg),
                    file_row + 1,
                    render_line(
                        self.gb,
                        it,
                        self.w.view.col_off,
                        self.n_cols - padding,
                        self.tabstop,
                        self.cs,
                        &mut self.style_cache
                    ),
                    width = self.w_lnum
                )
            }
        };

        Some(line)
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

#[inline]
fn skip_token_chars(
    chars: &mut Peekable<Chars<'_>>,
    tabstop: usize,
    to_skip: &mut usize,
) -> Option<usize> {
    for ch in chars.by_ref() {
        let w = if ch == '\t' {
            tabstop
        } else {
            UnicodeWidthChar::width(ch).unwrap_or(1)
        };

        match (*to_skip).cmp(&w) {
            Ordering::Less => {
                let spaces = Some(w - *to_skip);
                *to_skip = 0;
                return spaces;
            }

            Ordering::Equal => {
                *to_skip = 0;
                break;
            }

            Ordering::Greater => *to_skip -= w,
        }
    }

    None
}

fn render_chars(
    chars: &mut Peekable<Chars<'_>>,
    spaces: Option<usize>,
    max_cols: usize,
    tabstop: usize,
    cols: &mut usize,
    buf: &mut String,
) {
    if let Some(n) = spaces {
        buf.extend(repeat_n(' ', n));
        *cols = n;
    }

    for ch in chars {
        if ch == '\n' {
            break;
        }
        let w = if ch == '\t' {
            tabstop
        } else {
            UnicodeWidthChar::width(ch).unwrap_or(1)
        };

        if *cols + w <= max_cols {
            if ch == '\t' {
                // Tab is just a control character that moves the cursor rather than
                // replacing the previous buffer content so we need to explicitly
                // insert spaces instead.
                buf.extend(repeat_n(' ', tabstop));
            } else {
                buf.push(ch);
            }
            *cols += w;
        } else {
            break;
        }
    }

    buf.push_str(RESET_STYLE);
}

fn render_line<'a>(
    gb: &'a GapBuffer,
    it: impl Iterator<Item = RangeToken<'a>>,
    col_off: usize,
    max_cols: usize,
    tabstop: usize,
    cs: &ColorScheme,
    style_cache: &mut Rc<RefCell<HashMap<String, String>>>,
) -> String {
    let mut buf = String::new();
    let mut to_skip = col_off;
    let mut cols = 0;

    for tk in it {
        let slice = tk.as_slice(gb);
        let mut chars = slice.chars().peekable();
        let spaces = if to_skip > 0 {
            let spaces = skip_token_chars(&mut chars, tabstop, &mut to_skip);
            if to_skip > 0 || (chars.peek().is_none() && spaces.is_none()) {
                continue;
            }
            spaces
        } else {
            None
        };

        // In the common case we have the styles for each tag already cached, so we take the hit on
        // allocating the cache key and looking it up a second time when we insert into the cache.
        // This allows us to avoid having to allocate the key on every lookup in order to make use
        // of the entry API.
        //
        // The cache styles are also stored against the original tag rather so they can be looked
        // up directly each time they are used, rather than need to to traverse the fallback path
        // as done in ColorScheme::styles_for.
        //
        // We always assume that it safe to borrow the style_cache mutably at this point as we are
        // only expecting this function to be called as part of a render pass where the clones of
        // the style_cache Rc are held in different iterators that are processed sequentially in a
        // single thread.
        let mut guard = style_cache.borrow_mut();
        let style_str = match guard.get(tk.tag) {
            Some(s) => s,
            None => {
                let s = cs.styles_for(tk.tag).to_string();
                guard.insert(tk.tag.to_string(), s);
                guard.get(tk.tag).unwrap()
            }
        };

        buf.push_str(style_str);
        render_chars(&mut chars, spaces, max_cols, tabstop, &mut cols, &mut buf);

        if cols == max_cols {
            break;
        }
    }

    if cols < max_cols {
        buf.push_str(&Style::Bg(cs.bg).to_string());
        buf.extend(repeat_n(' ', max_cols - cols));
    }

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

fn try_read_char(stdin: &mut impl Read) -> Option<char> {
    let mut buf: [u8; 4] = [0; 4];

    for i in 0..4 {
        if stdin.read_exact(&mut buf[i..i + 1]).is_err() {
            return if i == 0 {
                None
            } else {
                Some(char::REPLACEMENT_CHARACTER)
            };
        }

        match std::str::from_utf8(&buf[0..i + 1]) {
            Ok(s) => return s.chars().next(),
            Err(e) if e.error_len().is_some() => return Some(char::REPLACEMENT_CHARACTER),
            Err(_) => (),
        }
    }

    // utf8 requires at most 4 bytes so at this point we have invalid data
    Some(char::REPLACEMENT_CHARACTER)
}

fn try_read_input(stdin: &mut impl Read) -> Option<Input> {
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
    use crate::ts::{ByteRange, TK_DEFAULT};
    use simple_test_case::test_case;
    use std::{char::REPLACEMENT_CHARACTER, io};

    #[test_case("a".as_bytes(), &['a']; "single ascii character")]
    #[test_case(&[240, 159, 146, 150], &['💖']; "single utf8 character")]
    #[test_case(&[165, 159, 146, 150], &[REPLACEMENT_CHARACTER; 4]; "invalid utf8 with non-ascii prefix")]
    #[test_case(&[65, 159, 146, 150], &['A', REPLACEMENT_CHARACTER, REPLACEMENT_CHARACTER, REPLACEMENT_CHARACTER]; "invalid utf8 with ascii prefix")]
    #[test]
    fn try_read_char_works(bytes: &[u8], expected: &[char]) {
        let mut r = io::Cursor::new(bytes);
        let mut chars = Vec::new();

        while let Some(ch) = try_read_char(&mut r) {
            chars.push(ch);
        }

        assert_eq!(&chars, expected);
    }

    fn rt(tag: &'static str, from: usize, to: usize) -> RangeToken<'static> {
        RangeToken {
            tag,
            r: ByteRange { from, to },
        }
    }

    // The !| characters here are the dummy style strings in the style_cache
    // The $# characters are replaced with RESET_STYLE and bg color respectively
    #[test_case(0, 14, "foo\tbar baz", "!foo$|  $!bar$| $!baz$#  "; "full line padded to max cols")]
    #[test_case(0, 12, "foo\tbar baz", "!foo$|  $!bar$| $!baz$"; "full line")]
    #[test_case(1, 11, "foo\tbar baz", "!oo$|  $!bar$| $!baz$"; "skipping first character")]
    #[test_case(3, 9, "foo\tbar baz", "|  $!bar$| $!baz$"; "skipping first token")]
    #[test_case(4, 8, "foo\tbar baz", "| $!bar$| $!baz$"; "skipping part way through a tab")]
    #[test_case(0, 10, "世\t界 foo", "!世$|  $!界$| $!foo$"; "unicode full line")]
    #[test_case(0, 12, "世\t界 foo", "!世$|  $!界$| $!foo$#  "; "unicode full line padded to max cols")]
    // In the case where we skip part way through a unicode character we still apply the tag
    // styling to the spaces we insert to pad to the correct offset rather than replacing it
    // with default styling instead
    #[test_case(1, 9, "世\t界 foo", "! $|  $!界$| $!foo$"; "unicode skipping first column of multibyte char")]
    #[test]
    fn render_line_correctly_skips_tokens(
        col_off: usize,
        max_cols: usize,
        s: &str,
        expected_template: &str,
    ) {
        let gb = GapBuffer::from(s);
        let range_tokens = vec![
            rt("a", 0, 3),
            rt(TK_DEFAULT, 3, 4),
            rt("a", 4, 7),
            rt(TK_DEFAULT, 7, 8),
            rt("a", 8, 11),
        ];

        let cs = ColorScheme::default();
        let style_cache: HashMap<String, String> = [
            ("a".to_owned(), "!".to_owned()),
            (TK_DEFAULT.to_owned(), "|".to_owned()),
        ]
        .into_iter()
        .collect();

        let s = render_line(
            &gb,
            range_tokens.into_iter(),
            col_off,
            max_cols,
            2,
            &cs,
            &mut Rc::new(RefCell::new(style_cache)),
        );

        let expected = expected_template
            .replace("$", RESET_STYLE)
            .replace("#", &Style::Bg(cs.bg).to_string());

        assert_eq!(s, expected);
    }
}
