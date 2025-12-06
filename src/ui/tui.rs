//! A terminal UI for ad
use crate::{
    ORIGINAL_TERMIOS,
    buffer::{Buffer, Chars, GapBuffer},
    config::{ColorScheme, Config},
    config_handle, die,
    dot::Range,
    editor::{Click, MiniBufferState},
    input::Event,
    key::{Input, MouseButton, MouseEvent},
    restore_terminal_state,
    syntax::{LineIter, RangeToken},
    term::{
        CurShape, Cursor, RESET_STYLE, Style, Styles, clear_screen, enable_alternate_screen,
        enable_bracketed_paste, enable_mouse_support, enable_raw_mode, get_termios, get_termsize,
        register_signal_handler, win_size_changed,
    },
    ui::{
        Layout, StateChange, UserInterface,
        layout::{Column, Scratch, Window},
    },
    ziplist,
};
use std::{
    char,
    cmp::Ordering,
    collections::HashMap,
    fmt::Write as _,
    io::{self, BufWriter, Read, StdoutLock, Write, stdin, stdout},
    iter::{Peekable, repeat_n},
    panic,
    sync::{Arc, RwLock, mpsc::Sender},
    thread::{JoinHandle, spawn},
    time::Instant,
};
use tracing::debug;
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

// If the screen dimensions drop below these values then we disable rendering
const MIN_COLS: usize = 20;
const MIN_ROWS: usize = 5;

const H_STR: &str = "─";
const V_STR: &str = "│";
const TR_STR: &str = "├";
const TL_STR: &str = "┤";
const X_STR: &str = "┼";

pub type Tui = GenericTui<StdoutLock<'static>>;

#[derive(Debug)]
pub struct GenericTui<W: Write> {
    stdout: BufWriter<W>,
    config: Arc<RwLock<Config>>,
    status_message: String,
    last_status: Instant,
    mb_last_frame: bool,
    frame: Frame,
}

impl Default for Tui {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl<W: Write> Drop for GenericTui<W> {
    fn drop(&mut self) {
        restore_terminal_state(&mut self.stdout);
    }
}

impl Tui {
    pub fn new(config: Arc<RwLock<Config>>) -> Self {
        Self::new_with_stdout_handle(config, stdout().lock())
    }
}

impl<W: Write> GenericTui<W> {
    pub fn new_with_stdout_handle(config: Arc<RwLock<Config>>, stdout: W) -> Self {
        Self {
            stdout: BufWriter::new(stdout),
            config,
            status_message: String::new(),
            last_status: Instant::now(),
            mb_last_frame: false,
            frame: Frame::new(),
        }
    }

    pub fn set_size(&mut self, rows: usize, cols: usize) {
        self.frame.screen_rows = rows;
        self.frame.screen_cols = cols;
    }

    fn render(
        &mut self,
        mode_name: &str,
        layout: &Layout,
        n_running: usize,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    ) {
        let conf = config_handle!(self);
        let (cs, status_timeout, tabstop, max_mb_lines) = (
            &conf.colorscheme,
            conf.status_timeout,
            conf.tabstop,
            conf.minibuffer_lines,
        );

        // If we have a minibuffer open then that takes priority over an open scratch buffer
        let w_minibuffer = mb.is_some();
        let mb = mb.unwrap_or_default();
        let active_buffer = layout.active_buffer();

        let mb_has_lines = mb.b.map(|b| !b.is_empty()).unwrap_or_default();
        let offset = if mb_has_lines {
            mb.bottom - mb.top + 1
        } else if !w_minibuffer && layout.scratch.is_visible {
            max_mb_lines
        } else {
            0
        };

        // This is the screen size that we have to work with for the buffer content we currently want to
        // display. If the minibuffer is active then it take priority over anything else and we always
        // show the status bar as the final two lines of the UI.
        let effective_screen_rows = self.frame.screen_rows.saturating_sub(offset);

        let load_exec_range = match held_click {
            Some(Click::Text { btn, selection, .. })
                if *btn == MouseButton::Right || *btn == MouseButton::Middle =>
            {
                Some((*btn == MouseButton::Right, *selection))
            }
            _ => None,
        };

        let (load_exec_range, scratch_load_exec_range) = if layout.scratch.is_focused {
            (None, load_exec_range)
        } else {
            (load_exec_range, None)
        };

        self.frame
            .render_windows(layout, load_exec_range, effective_screen_rows, tabstop, cs);
        self.frame
            .render_status_bar(cs, mode_name, n_running, active_buffer);

        self.frame.show_mb = w_minibuffer || layout.scratch.is_visible;
        self.frame.show_msg_bar = !w_minibuffer;

        if w_minibuffer {
            self.frame.render_minibuffer_state(&mb, tabstop, cs);
        } else if layout.scratch.is_visible {
            self.frame
                .render_scratch(&layout.scratch, scratch_load_exec_range, tabstop, cs);
        };

        if self.frame.show_msg_bar {
            self.frame.render_message_bar(
                cs,
                pending_keys,
                status_timeout,
                self.status_message.clone(),
                self.last_status,
            );
        };

        let (cur_x, cur_y) = if w_minibuffer {
            (mb.cx, self.frame.screen_rows + mb.n_visible_lines + 1)
        } else {
            layout.ui_xy()
        };

        self.frame.cur_x = cur_x;
        self.frame.cur_y = cur_y;
    }
}

impl<W: Write> UserInterface for GenericTui<W> {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
        let original_termios = get_termios();
        enable_raw_mode(original_termios);
        _ = ORIGINAL_TERMIOS.set(original_termios);

        panic::set_hook(Box::new(|panic_info| {
            let mut stdout = stdout();
            restore_terminal_state(&mut stdout);
            _ = stdout.flush();

            // Force capturing a backtrace for easier debugging
            let bt = std::backtrace::Backtrace::force_capture();

            // Restoring the terminal state to move us off of the alternate screen
            // can race with our attempt to print the panic info so given that we
            // are already in a fatal situation, sleeping briefly to ensure that
            // the cause of the panic is visible before we exit isn't _too_ bad.
            std::thread::sleep(std::time::Duration::from_millis(300));
            eprintln!("Fatal error:\n{panic_info}\n{bt}");
            _ = std::fs::write("/tmp/ad.panic", format!("{panic_info}\n{bt}"));
        }));

        enable_mouse_support(&mut self.stdout);
        enable_alternate_screen(&mut self.stdout);
        enable_bracketed_paste(&mut self.stdout);

        // SAFETY: we only register our signal handler once
        unsafe { register_signal_handler() };

        let (screen_rows, screen_cols) = get_termsize();
        self.frame.screen_rows = screen_rows;
        self.frame.screen_cols = screen_cols;

        spawn_input_thread(tx);

        (screen_rows, screen_cols)
    }

    fn shutdown(&mut self) {
        clear_screen(&mut self.stdout);
    }

    fn state_change(&mut self, change: StateChange) {
        match change {
            StateChange::ConfigUpdated => self.frame.style_cache.clear(),
            StateChange::StatusMessage { msg } => {
                self.status_message = msg;
                self.last_status = Instant::now();
            }
        }
    }

    fn refresh(
        &mut self,
        mode_name: &str,
        layout: &mut Layout,
        n_running: usize,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    ) {
        self.frame.screen_rows = layout.screen_rows;
        self.frame.screen_cols = layout.screen_cols;
        self.frame.show_msg_bar = mb.is_none();
        let mb_this_frame = mb.is_some();

        if self.frame.screen_cols < MIN_COLS || self.frame.screen_rows < MIN_ROWS {
            return;
        }

        // If the UI changed or we have an active minibuffer then we need to rerender the UI.
        // We also need to re-render on the frame after a minibuffer is closed in order to
        // get rid of it, as none of the other buffers in the layout will be marked as changed
        // since the last render.
        let need_render = layout.changed_since_last_render()
            || mb_this_frame
            || self.mb_last_frame | held_click.is_some();

        if need_render {
            layout.update_visible_ts_state();
            self.render(mode_name, layout, n_running, pending_keys, held_click, mb);
            if let Err(e) = self.frame.write(&mut self.stdout) {
                die!("Unable to refresh screen: {e}");
            }
        } else if self.frame.show_msg_bar {
            // match self.render in not showing the message bar if the minibuffer is open
            let conf = config_handle!(self);
            let (cs, status_timeout) = (&conf.colorscheme, conf.status_timeout);
            self.frame.render_message_bar(
                cs,
                pending_keys,
                status_timeout,
                self.status_message.clone(),
                self.last_status,
            );
            if let Err(e) = self.frame.write_msg_bar(&mut self.stdout) {
                die!("Unable to refresh screen: {e}");
            }
        }

        if let Err(e) = self.stdout.flush() {
            die!("Unable to refresh screen: {e}");
        }

        self.mb_last_frame = mb_this_frame;
    }

    fn set_cursor_shape(&mut self, cur_shape: CurShape) {
        if let Err(e) = self.stdout.write_all(cur_shape.to_string().as_bytes()) {
            // In this situation we're probably not going to be able to do all that much
            // but we might as well try
            die!("Unable to write to stdout: {e}");
        };
    }
}

#[derive(Debug, Default)]
pub struct Frame {
    win_lines: String,
    status_bar: String,
    mb_lines: String,
    show_mb: bool,
    msg_bar: String,
    show_msg_bar: bool,
    screen_rows: usize,
    screen_cols: usize,
    cur_x: usize,
    cur_y: usize,
    // Cache of the ANSI escape code strings required for each fully qualified tree-sitter
    // highlighting tag. See render_line for details on how the cache is used.
    style_cache: HashMap<String, String>,
}

impl Frame {
    fn new() -> Self {
        let win_lines_cap = 128 * 1024;
        let bar_cap = 8 * 1024;

        Self {
            win_lines: String::with_capacity(win_lines_cap),
            mb_lines: String::with_capacity(bar_cap),
            status_bar: String::with_capacity(bar_cap),
            msg_bar: String::with_capacity(bar_cap),
            ..Default::default()
        }
    }

    fn write(&self, w: &mut impl Write) -> io::Result<()> {
        write!(w, "{}{}", Cursor::Hide, Cursor::ToStart)?;
        w.write_all(self.win_lines.as_bytes())?;
        w.write_all(self.status_bar.as_bytes())?;
        if self.show_mb {
            w.write_all(self.mb_lines.as_bytes())?;
        }
        if self.show_msg_bar {
            w.write_all(self.msg_bar.as_bytes())?;
        }

        write!(
            w,
            "{}{}",
            Cursor::To(self.cur_x + 1, self.cur_y + 1),
            Cursor::Show
        )
    }

    fn write_msg_bar(&self, w: &mut impl Write) -> io::Result<()> {
        write!(w, "{}{}", Cursor::Hide, Cursor::To(1, self.screen_rows + 2))?;
        w.write_all(self.msg_bar.as_bytes())?;
        write!(
            w,
            "{}{}",
            Cursor::To(self.cur_x + 1, self.cur_y + 1),
            Cursor::Show
        )
    }

    fn render_windows(
        &mut self,
        layout: &Layout,
        load_exec_range: Option<(bool, Range)>,
        screen_rows: usize,
        tabstop: usize,
        cs: &ColorScheme,
    ) {
        self.win_lines.clear();

        let mut col_renderers: Vec<_> = layout
            .cols
            .iter()
            .map(|(is_focus, col)| {
                let rng = if is_focus { load_exec_range } else { None };
                ColRenderer::new(col, layout, rng, screen_rows, tabstop, cs)
            })
            .collect();

        let n_cols = col_renderers.len();
        'outer: loop {
            let mut remaining;
            let mut prev_col = None;

            for (i, cr) in col_renderers.iter_mut().enumerate() {
                (remaining, prev_col) =
                    cr.render_next_line(&mut self.win_lines, prev_col, &mut self.style_cache);
                if i == n_cols - 1 && !remaining {
                    _ = write!(&mut self.win_lines, "{}\r\n", Cursor::ClearRight);
                    break 'outer;
                }
            }

            _ = write!(&mut self.win_lines, "{}\r\n", Cursor::ClearRight);
        }
    }

    fn render_status_bar(
        &mut self,
        cs: &ColorScheme,
        mode_name: &str,
        n_running: usize,
        b: &Buffer,
    ) {
        self.status_bar.clear();

        let lstatus = format!(
            "{} {} - {} lines {}{}",
            mode_name,
            b.display_name(),
            b.len_lines(),
            if b.dirty { "[+]" } else { "" },
            if !b.has_trailing_newline() {
                "[noeol]"
            } else {
                ""
            }
        );
        let rstatus = format!(
            "{}{}",
            if n_running == 0 {
                String::new()
            } else {
                format!("[{n_running} running] ")
            },
            b.dot.addr(b)
        );
        let width = self
            .screen_cols
            .saturating_sub(UnicodeWidthStr::width(lstatus.as_str()));

        _ = write!(
            &mut self.status_bar,
            "{}{}{lstatus}{rstatus:>width$}{}\r\n",
            Style::Bg(cs.bar_bg),
            Style::Fg(cs.fg),
            Style::Reset
        );
    }

    // current prompt and pending chars
    fn render_message_bar(
        &mut self,
        cs: &ColorScheme,
        pending_keys: &[Input],
        status_timeout: u64,
        mut msg: String,
        last_status: Instant,
    ) {
        self.msg_bar.clear();
        self.msg_bar.push_str(&Cursor::ClearRight.to_string());
        msg.truncate(self.screen_cols.saturating_sub(10));

        let pending = render_pending(pending_keys);
        let delta = (Instant::now() - last_status).as_secs();

        if !msg.is_empty() && delta < status_timeout {
            let width = self
                .screen_cols
                .saturating_sub(msg.len())
                .saturating_sub(10);
            _ = write!(
                &mut self.msg_bar,
                "{}{}{msg}{pending:>width$}          ",
                Style::Fg(cs.fg),
                Style::Bg(cs.bg)
            );
        } else {
            let width = self.screen_cols.saturating_sub(10);
            _ = write!(
                &mut self.msg_bar,
                "{}{}{pending:>width$}          ",
                Style::Fg(cs.fg),
                Style::Bg(cs.bg)
            );
        }
    }

    fn render_minibuffer_state(
        &mut self,
        mb: &MiniBufferState<'_>,
        tabstop: usize,
        cs: &ColorScheme,
    ) {
        self.mb_lines.clear();

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
                _ = write!(
                    &mut self.mb_lines,
                    "{}",
                    Styles {
                        fg: Some(cs.fg),
                        bg: Some(bg),
                        ..Default::default()
                    }
                );

                render_chars(
                    &mut chars,
                    None,
                    self.screen_cols,
                    tabstop,
                    &mut cols,
                    &mut self.mb_lines,
                );

                if cols < self.screen_cols {
                    self.mb_lines.push_str(&Style::Bg(bg).to_string());
                }

                let width = self.screen_cols;
                _ = write!(&mut self.mb_lines, "{:>width$}\r\n", Cursor::ClearRight);
            }
        }

        _ = write!(
            &mut self.mb_lines,
            "{}{}{}{}{}",
            Style::Fg(cs.fg),
            Style::Bg(cs.bg),
            mb.prompt,
            mb.input,
            Cursor::ClearRight
        );
    }

    fn render_scratch(
        &mut self,
        scratch: &Scratch,
        load_exec_range: Option<(bool, Range)>,
        tabstop: usize,
        cs: &ColorScheme,
    ) {
        self.mb_lines.clear();
        let b = scratch.b.buffer();
        let (w_lnum, _) = b.sign_col_dims();
        let rng = if scratch.is_focused {
            load_exec_range
        } else {
            None
        };

        let mut wr = WinRenderer {
            y: 0,
            w_lnum,
            n_cols: self.screen_cols,
            tabstop,
            it: b.iter_tokenized_lines_from(scratch.w.view.row_off, rng),
            gb: &b.txt,
            w: &scratch.w,
            cs,
        };

        while wr.render_next_line(&mut self.mb_lines, None, &mut self.style_cache) {}
    }
}

#[derive(Clone, Copy)]
enum PrevCol {
    Buffer,
    Hline,
}

struct ColRenderer<'a> {
    inner: ziplist::Iter<'a, Window>,
    current: Option<WinRenderer<'a>>,
    layout: &'a Layout,
    cs: &'a ColorScheme,
    load_exec_range: Option<(bool, Range)>,
    screen_rows: usize,
    tabstop: usize,
    n_cols: usize,
    row: usize,
}

impl<'a> ColRenderer<'a> {
    fn new(
        col: &'a Column,
        layout: &'a Layout,
        load_exec_range: Option<(bool, Range)>,
        screen_rows: usize,
        tabstop: usize,
        cs: &'a ColorScheme,
    ) -> Self {
        ColRenderer {
            inner: col.wins.iter(),
            current: None,
            layout,
            cs,
            load_exec_range,
            screen_rows,
            tabstop,
            n_cols: col.n_cols,
            row: 0,
        }
    }

    fn next_window(&mut self) -> Option<WinRenderer<'a>> {
        let (is_focus, w) = self.inner.next()?;
        let b = self
            .layout
            .buffer_with_id(w.view.bufid)
            .expect("valid buffer id");

        let (w_lnum, _) = b.sign_col_dims();
        let rng = if is_focus { self.load_exec_range } else { None };
        let it = b.iter_tokenized_lines_from(w.view.row_off, rng);

        Some(WinRenderer {
            y: 0,
            w_lnum,
            n_cols: self.n_cols,
            tabstop: self.tabstop,
            it,
            gb: &b.txt,
            w,
            cs: self.cs,
        })
    }

    /// Render the next available line into the provided buffer.
    ///
    /// Returns false if there are no more lines to render.
    fn render_next_line(
        &mut self,
        buf: &mut String,
        prev_col: Option<PrevCol>,
        style_cache: &mut HashMap<String, String>,
    ) -> (bool, Option<PrevCol>) {
        if self.current.is_none() {
            self.current = match self.next_window() {
                Some(w) => Some(w),
                None => return (false, None),
            }
        }

        let lines_remaining =
            self.current
                .as_mut()
                .unwrap()
                .render_next_line(buf, prev_col, style_cache);
        self.row += 1;

        let this_col = if lines_remaining {
            Some(PrevCol::Buffer)
        } else {
            self.current = None;
            let left_edge = match prev_col {
                Some(PrevCol::Buffer) => TR_STR,
                Some(PrevCol::Hline) => X_STR,
                None => "",
            };

            _ = write!(
                buf,
                "{}{}{left_edge}{}",
                Style::Fg(self.cs.minibuffer_hl),
                Style::Bg(self.cs.bg),
                H_STR.repeat(self.n_cols)
            );
            Some(PrevCol::Hline)
        };

        (self.row < self.screen_rows, this_col)
    }
}

struct WinRenderer<'a> {
    y: usize,
    w_lnum: usize,
    n_cols: usize,
    tabstop: usize,
    it: LineIter<'a>,
    gb: &'a GapBuffer,
    w: &'a Window,
    cs: &'a ColorScheme,
}

impl<'a> WinRenderer<'a> {
    fn render_next_line(
        &mut self,
        buf: &mut String,
        prev_col: Option<PrevCol>,
        style_cache: &mut HashMap<String, String>,
    ) -> bool {
        if self.y >= self.w.n_rows {
            return false;
        }

        let file_row = self.y + self.w.view.row_off;
        self.y += 1;

        if let Some(pc) = prev_col {
            let left_edge = match pc {
                PrevCol::Buffer => V_STR,
                PrevCol::Hline => TL_STR,
            };

            _ = write!(
                buf,
                "{}{}{left_edge}",
                Style::Fg(self.cs.minibuffer_hl),
                Style::Bg(self.cs.bg)
            );
        }

        match self.it.next() {
            None => {
                _ = write!(
                    buf,
                    "{}{}~ {V_STR:>width$}{}",
                    Style::Fg(self.cs.signcol_fg),
                    Style::Bg(self.cs.bg),
                    Style::Fg(self.cs.fg),
                    width = self.w_lnum
                );
                let padding = self.n_cols.saturating_sub(self.w_lnum).saturating_sub(2);
                buf.push_str(&" ".repeat(padding));
            }

            Some(it) => {
                // +2 for the leading space and vline chars
                let padding = self.w_lnum + 2;

                _ = write!(
                    buf,
                    "{}{} {:>width$}{V_STR}",
                    Style::Fg(self.cs.signcol_fg),
                    Style::Bg(self.cs.bg),
                    file_row + 1,
                    width = self.w_lnum
                );

                render_line(
                    self.gb,
                    it,
                    self.w.view.col_off,
                    self.n_cols.saturating_sub(padding),
                    self.tabstop,
                    self.cs,
                    style_cache,
                    buf,
                );
            }
        };

        true
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

        let (w, ch) = if ch == '\t' {
            (tabstop, ch)
        } else {
            match UnicodeWidthChar::width(ch) {
                Some(0) | None => (1, char::REPLACEMENT_CHARACTER),
                Some(n) => (n, ch),
            }
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

#[allow(clippy::too_many_arguments)]
fn render_line<'a>(
    gb: &'a GapBuffer,
    it: impl Iterator<Item = RangeToken<'a>>,
    col_off: usize,
    max_cols: usize,
    tabstop: usize,
    cs: &ColorScheme,
    style_cache: &mut HashMap<String, String>,
    buf: &mut String,
) {
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
        let style_str = match style_cache.get(tk.tag) {
            Some(s) => s,
            None => {
                let s = cs.styles_for(tk.tag).to_string();
                style_cache.insert(tk.tag.to_string(), s);
                style_cache.get(tk.tag).unwrap()
            }
        };

        buf.push_str(style_str);
        render_chars(&mut chars, spaces, max_cols, tabstop, &mut cols, buf);

        if cols == max_cols {
            break;
        }
    }

    if cols < max_cols {
        buf.push_str(&Style::Bg(cs.bg).to_string());
        buf.extend(repeat_n(' ', max_cols - cols));
    }
}

#[derive(Debug)]
enum RawInput {
    Input(Input),
    // Control sequences
    // https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Bracketed-Paste-Mode
    // https://invisible-island.net/xterm/xterm-paste64.html
    BPasteStart,
}

impl From<Input> for RawInput {
    fn from(value: Input) -> Self {
        Self::Input(value)
    }
}

/// Spawn a thread to read from stdin and process user input to send Events to
/// the main editor event loop.
fn spawn_input_thread(tx: Sender<Event>) -> JoinHandle<()> {
    spawn(move || {
        let mut stdin = stdin().lock();

        loop {
            match try_read_input(&mut stdin) {
                Some(RawInput::Input(i)) => {
                    _ = tx.send(Event::Input(i));
                    continue;
                }

                Some(RawInput::BPasteStart) => {
                    let mut s = String::new();
                    let mut buf = Vec::with_capacity(6);

                    while let Some(c) = try_read_char(&mut stdin) {
                        match (c, buf.as_slice()) {
                            ('\x1b', [])
                            | ('[', ['\x1b'])
                            | ('2', ['\x1b', '['])
                            | ('0', ['\x1b', '[', '2'])
                            | ('1', ['\x1b', '[', '2', '0']) => buf.push(c),

                            ('~', ['\x1b', '[', '2', '0', '1']) => {
                                _ = tx.send(Event::BracketedPaste(s));
                                break;
                            }

                            (c, _) => {
                                s.extend(buf.drain(..));
                                s.push(c);
                            }
                        }
                    }
                }

                None => (),
            }

            // Always check for the window size changing after processing multiple inputs or if we
            // failed to read anything.
            if win_size_changed() {
                let (rows, cols) = get_termsize();
                _ = tx.send(Event::WinsizeChanged { rows, cols });
            }
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

fn try_read_input(stdin: &mut impl Read) -> Option<RawInput> {
    let c = try_read_char(stdin)?;

    // Normal key press
    match Input::from_char(c) {
        Input::Esc => (),
        i => return Some(i.into()),
    }

    let c2 = match try_read_char(stdin) {
        Some(c2) => c2,
        None => return Some(Input::Esc.into()),
    };
    let c3 = match try_read_char(stdin) {
        Some(c3) => c3,
        None => return Some(Input::try_from_seq2(c, c2).unwrap_or(Input::Esc).into()),
    };

    if let Some(i) = Input::try_from_seq2(c2, c3) {
        return Some(i.into());
    }

    // https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands
    if c2 == '[' && c3.is_ascii_digit() {
        let mut digits = vec![c3];
        loop {
            match try_read_char(stdin)? {
                c if c.is_ascii_digit() => digits.push(c),
                '~' => break,
                c => {
                    debug!("unknown CSIC sequence: ^[[{}{c}", String::from_iter(digits));
                    return None;
                }
            }
        }

        // https://en.wikipedia.org/wiki/ANSI_escape_code#Control_Sequence_Introducer_commands
        return match digits.as_slice() {
            ['1' | '7'] => Some(Input::Home.into()),
            ['4' | '8'] => Some(Input::End.into()),
            ['3'] => Some(Input::Del.into()),
            ['5'] => Some(Input::PageUp.into()),
            ['6'] => Some(Input::PageDown.into()),
            ['2', '0', '0'] => Some(RawInput::BPasteStart),
            // 201 == bracketed paste end
            _ => None,
        };
    }

    // xterm SGR (1006) mouse encoding: "^[< Cb;Cx;Cy(;) (M or m) "
    //   https://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h3-Extended-coordinates
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

        return MouseEvent::try_from_raw(b, x, y, m).map(|i| RawInput::Input(Input::Mouse(i)));
    }

    Some(Input::Esc.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{ByteRange, TK_DEFAULT};
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

    // https://en.wikipedia.org/wiki/Bidirectional_text#Table_of_possible_BiDi_character_types
    // https://i18n-puzzles.com/puzzle/18/
    #[test]
    fn render_chars_correctly_handles_bidi_markers() {
        let line = GapBuffer::from("⁧foo⁦bar⁩baz⁩");
        let expected = format!("�foo�bar�baz�{RESET_STYLE}");

        let max_cols = line.chars().count();
        let mut chars = line.chars().peekable();
        let mut buf = String::with_capacity(line.len());
        let mut cols = 0;

        render_chars(&mut chars, None, max_cols, 4, &mut cols, &mut buf);

        assert_eq!(buf, expected);
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
        let mut style_cache: HashMap<String, String> = [
            ("a".to_owned(), "!".to_owned()),
            (TK_DEFAULT.to_owned(), "|".to_owned()),
        ]
        .into_iter()
        .collect();

        let mut s = String::new();
        render_line(
            &gb,
            range_tokens.into_iter(),
            col_off,
            max_cols,
            2,
            &cs,
            &mut style_cache,
            &mut s,
        );

        let expected = expected_template
            .replace("$", RESET_STYLE)
            .replace("#", &Style::Bg(cs.bg).to_string());

        assert_eq!(s, expected);
    }

    // Reproduction and regression test for https://github.com/sminez/ad/issues/137
    #[test]
    fn minibuffer_lines_with_multibyte_chars_dont_panic() {
        let s = "  56 | Fastställa att under samtliga öppetdagar den här veckan så finns det alltid minst en";
        let b = Buffer::new_virtual(0, "test", s, Default::default());

        let mb = MiniBufferState {
            cx: 0,
            n_visible_lines: 10,
            selected_line_idx: 0,
            prompt: "> ",
            input: Default::default(),
            b: Some(&b),
            top: 0,
            bottom: 0,
        };

        let mut frame = Frame::new();
        frame.screen_cols = 91;

        // In 137 this was panicking due to indexing into the rendered line using self.screen_cols
        // as a raw byte offset. The fix is simply not to truncate in that way as render_chars is
        // already ensuring that the buffer it is building up is staying within the available
        // screen space.
        frame.render_minibuffer_state(&mb, 4, &Default::default());
    }
}
