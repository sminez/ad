//! Rendering the user interface
use crate::{
    buffer::{Buffer, Line, MiniBufferState},
    die,
    editor::Editor,
    key::Key,
    term::{Cur, Style},
    STATUS_TIMEOUT, VERSION,
};
use std::{
    cmp::{max, min},
    io::Write,
    time::Instant,
};

// UI colors (should be pulled from config eventually)
const BAR_BG: &str = "#4E415C";
const SGNCOL_FG: &str = "#544863";
const VLINE: char = '│';

impl Editor {
    pub fn refresh_screen(&mut self) {
        self.refresh_screen_w_cursor(None);
    }

    pub(crate) fn refresh_screen_w_cursor(&mut self, cur: Option<(usize, usize)>) {
        self.buffers
            .active_mut()
            .clamp_scroll(self.screen_rows, self.screen_cols);

        let mut buf = format!("{}{}", Cur::Hide, Cur::ToStart);
        let w_sgncol = self.render_rows(&mut buf, self.screen_rows);
        self.render_status_bar(&mut buf);
        self.render_message_bar(&mut buf);

        let active = self.buffers.active();
        let (x, y) = cur.unwrap_or((
            active.rx - active.col_off + w_sgncol,
            active.cy - active.row_off,
        ));
        buf.push_str(&format!("{}{}", Cur::To(x + 1, y + 1), Cur::Show));

        if let Err(e) = self.stdout.write_all(buf.as_bytes()) {
            die!("Unable to refresh screen: {e}");
        }

        if let Err(e) = self.stdout.flush() {
            die!("Unable to refresh screen: {e}");
        }
    }

    pub(crate) fn refresh_screen_w_minibuffer(&mut self, mb: Option<MiniBufferState<'_>>) {
        let w_minibuffer = mb.is_some();
        let MiniBufferState {
            cx,
            cy,
            selected_line_idx,
            prompt_line,
            lines,
        } = mb.unwrap_or_default();

        let effective_screen_rows = self.screen_rows - lines.len();

        self.buffers
            .active_mut()
            .clamp_scroll(self.screen_rows, self.screen_cols);

        let mut buf = format!("{}{}", Cur::Hide, Cur::ToStart);
        let w_sgncol = self.render_rows(&mut buf, effective_screen_rows);
        self.render_status_bar(&mut buf);

        if w_minibuffer {
            self.render_minibuffer_state(&mut buf, prompt_line, lines, selected_line_idx);
        } else {
            self.render_message_bar(&mut buf);
        }

        let active = self.buffers.active();
        let (x, y) = if w_minibuffer {
            (cx, cy)
        } else {
            (
                active.rx - active.col_off + w_sgncol,
                active.cy - active.row_off,
            )
        };

        buf.push_str(&format!("{}{}", Cur::To(x + 1, y + 1), Cur::Show));

        if let Err(e) = self.stdout.write_all(buf.as_bytes()) {
            die!("Unable to refresh screen: {e}");
        }

        if let Err(e) = self.stdout.flush() {
            die!("Unable to refresh screen: {e}");
        }
    }

    /// Returns the width of the sign column
    fn render_rows(&self, buf: &mut String, screen_rows: usize) -> usize {
        let Buffer {
            lines,
            row_off,
            col_off,
            ..
        } = self.buffers.active();

        // Sort out dimensions of the sign/number column
        let max_linum = min(lines.len(), screen_rows + row_off);
        let w_lnum = n_digits(max_linum);
        let w_sgncol = w_lnum + 2;

        for y in 0..screen_rows {
            let file_row = y + row_off;

            if file_row >= lines.len() {
                buf.push_str(&format!(
                    "{}~ {VLINE:>width$}{}",
                    Style::Fg(SGNCOL_FG.into()),
                    Style::Reset,
                    width = w_lnum
                ));

                if self.buffers.is_empty_scratch() && y == self.screen_rows / 3 && y < screen_rows {
                    let mut banner = format!("ad editor :: version {VERSION}");
                    banner.truncate(self.screen_cols - w_sgncol);
                    let padding = (self.screen_cols - w_sgncol - banner.len()) / 2;
                    buf.push_str(&" ".repeat(padding));
                    buf.push_str(&banner);
                }
            } else {
                buf.push_str(&format!(
                    "{} {:>width$}{VLINE}{}",
                    Style::Fg(SGNCOL_FG.into()),
                    file_row + 1,
                    Style::Reset,
                    width = w_lnum
                ));

                let rline = &lines[file_row].render;
                let mut len = max(0, rline.len() - col_off);
                len = min(self.screen_cols - w_sgncol, len);
                buf.push_str(&rline[*col_off..min(self.screen_cols - w_sgncol, len)]);
            }

            buf.push_str(&format!("{}\r\n", Cur::ClearRight));
        }

        w_sgncol
    }

    fn render_status_bar(&self, buf: &mut String) {
        let b = self.buffers.active();
        let name = b.display_name();
        let (n_lines, cy, rx) = (b.len_lines(), b.cy + 1, b.rx + 1);

        let lstatus = format!(
            "{} {name} - {n_lines} lines {}",
            self.modes[0],
            if b.dirty { "[+]" } else { "" }
        );
        let rstatus = format!("{cy}:{rx}");
        let width = self.screen_cols - lstatus.len();
        buf.push_str(&format!(
            "{}{lstatus}{rstatus:>width$}{}\r\n",
            Style::Bg(BAR_BG.into()),
            Style::Reset
        ));
    }

    // current prompt and pending chars
    fn render_message_bar(&self, buf: &mut String) {
        buf.push_str(&Cur::ClearRight.to_string());

        let mut msg = self.status_message.clone();
        msg.truncate(self.screen_cols.saturating_sub(10));

        let pending = render_pending(&self.pending_keys);
        let delta = (Instant::now() - self.status_time).as_secs();

        if !msg.is_empty() && delta < STATUS_TIMEOUT {
            let width = self.screen_cols - msg.len() - 10;
            buf.push_str(&format!("{msg}{pending:>width$}"));
        } else {
            let width = self.screen_cols - 10;
            buf.push_str(&format!("{pending:>width$}"));
        }
    }

    fn render_minibuffer_state(
        &self,
        buf: &mut String,
        prompt_line: &str,
        lines: &[Line],
        selected_line_idx: usize,
    ) {
        for (i, Line { render: rline, .. }) in lines.iter().enumerate() {
            let len = min(self.screen_cols, rline.len());
            if i == selected_line_idx {
                buf.push_str(&format!(
                    "{}{}{}{}\r\n",
                    Style::Bg(SGNCOL_FG.into()),
                    &rline[0..len],
                    Style::Reset,
                    Cur::ClearRight
                ));
            } else {
                buf.push_str(&rline[0..len]);
            }
        }

        buf.push_str(&format!("{prompt_line}{}", Cur::ClearRight));
    }
}

fn render_pending(keys: &[Key]) -> String {
    let mut s = String::new();
    for k in keys {
        match k {
            Key::Char(c) if c.is_ascii_whitespace() => s.push_str(&format!("<{:x}>", *c as u8)),
            Key::Char(c) => s.push(*c),
            Key::Ctrl(c) => {
                s.push('^');
                s.push(*c);
            }
            Key::Alt(c) => {
                s.push('^');
                s.push('[');
                s.push(*c);
            }
            Key::CtrlAlt(c) => {
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

fn n_digits(mut n: usize) -> usize {
    if n == 0 {
        return 1;
    }

    let mut digits = 0;
    while n != 0 {
        digits += 1;
        n /= 10;
    }

    digits
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::Key::*;
    use simple_test_case::test_case;

    #[test_case(0, 1; "n0")]
    #[test_case(5, 1; "n5")]
    #[test_case(10, 2; "n10")]
    #[test_case(13, 2; "n13")]
    #[test_case(731, 3; "n731")]
    #[test_case(930, 3; "n930")]
    #[test]
    fn n_digits_works(n: usize, digits: usize) {
        assert_eq!(n_digits(n), digits);
    }

    #[test_case(vec![Char('a')], "a"; "single char")]
    #[test_case(vec![Ctrl('a')], "^a"; "single ctrl")]
    #[test_case(vec![Alt('a')], "^[a"; "single alt")]
    #[test_case(vec![Char(' ')], "<20>"; "space")]
    #[test_case(vec![CtrlAlt('a')], "^[^a"; "single ctrlalt")]
    #[test_case(vec![Char('a'), Char('b')], "ab"; "multi char")]
    #[test_case(vec![Ctrl('a'), Char('b')], "^ab"; "multi mixed")]
    #[test_case(
        "1234567890ABC".chars().map(Char).collect(),
        "4567890ABC";
        "truncated"
    )]
    #[test]
    fn render_pending_works(pending: Vec<Key>, expected: &str) {
        let s = render_pending(&pending);
        assert_eq!(s, expected);
    }
}
