//! Rendering the user interface
use crate::{
    buffer::{Buffer, MiniBufferState},
    config,
    config::ColorScheme,
    die,
    editor::Editor,
    key::Key,
    term::{Cursor, Style},
    VERSION,
};
use std::{cmp::min, io::Write, time::Instant};

const VLINE: char = '│';

impl Editor {
    /// Refresh the TUI state
    pub fn refresh_screen(&mut self) {
        self.refresh_screen_w_minibuffer(None);
    }

    // FIXME: This has an implicit "only one buffer is active at a time" assumption which needs to
    // be reworked to support multiple buffers open at once.
    pub(crate) fn refresh_screen_w_minibuffer(&mut self, mb: Option<MiniBufferState<'_>>) {
        let w_minibuffer = mb.is_some();
        let MiniBufferState {
            cx,
            cy,
            selected_line_idx,
            prompt_line,
            b,
            top,
            bottom,
        } = mb.unwrap_or_default();

        let mb_lines = b.map(|b| b.len_lines()).unwrap_or_default();
        let mb_offset = if mb_lines > 0 { 1 } else { 0 };
        let effective_screen_rows = self.screen_rows - (bottom - top) - mb_offset;

        self.buffers
            .active_mut()
            .clamp_scroll(self.screen_rows, self.screen_cols);

        let (cs, status_timeout) = {
            let conf = config!();
            (conf.colorscheme, conf.status_timeout)
        };

        let mut buf = format!("{}{}", Cursor::Hide, Cursor::ToStart);
        let w_sgncol = self.render_rows(&mut buf, effective_screen_rows, &cs);
        self.render_status_bar(&mut buf, &cs);

        if w_minibuffer {
            self.render_minibuffer_state(
                &mut buf,
                prompt_line,
                b,
                selected_line_idx,
                top,
                bottom,
                &cs,
            );
        } else {
            self.render_message_bar(&mut buf, &cs, status_timeout);
        }

        let active = self.buffers.active();
        let (x, y) = if w_minibuffer {
            (cx, cy)
        } else {
            let (y, _) = active.dot.active_cur().as_yx(active);
            (active.rx - active.col_off + w_sgncol, y - active.row_off)
        };

        buf.push_str(&format!("{}{}", Cursor::To(x + 1, y + 1), Cursor::Show));

        if let Err(e) = self.stdout.write_all(buf.as_bytes()) {
            die!("Unable to refresh screen: {e}");
        }

        if let Err(e) = self.stdout.flush() {
            die!("Unable to refresh screen: {e}");
        }
    }

    /// Returns the width of the sign column
    fn render_rows(&mut self, buf: &mut String, screen_rows: usize, cs: &ColorScheme) -> usize {
        let is_empty_scratch = self.buffers.is_empty_scratch();
        let b = self.buffers.active_mut();

        // Sort out dimensions of the sign/number column
        let (w_lnum, w_sgncol) = b.sign_col_dims(screen_rows);

        for y in 0..screen_rows {
            let file_row = y + b.row_off;

            if file_row >= b.len_lines() {
                buf.push_str(&format!(
                    "{}{}~ {VLINE:>width$}{}",
                    Style::Fg(cs.signcol_fg),
                    Style::Bg(cs.bg),
                    Style::Fg(cs.fg),
                    width = w_lnum
                ));

                if is_empty_scratch && y == self.screen_rows / 3 && y < screen_rows {
                    let mut banner = format!("ad editor :: version {VERSION}");
                    banner.truncate(self.screen_cols - w_sgncol);
                    let padding = (self.screen_cols - w_sgncol - banner.len()) / 2;
                    buf.push_str(&" ".repeat(padding));
                    buf.push_str(&banner);
                }
            } else {
                // +2 for the leading space and vline chars
                let padding = w_lnum + 2;
                buf.push_str(&format!(
                    "{}{} {:>width$}{VLINE}{}{}",
                    Style::Fg(cs.signcol_fg),
                    Style::Bg(cs.bg),
                    file_row + 1,
                    Style::Fg(cs.fg),
                    b.styled_rline_unchecked(file_row, padding, self.screen_cols, cs),
                    width = w_lnum
                ));
            }

            buf.push_str(&format!("{}\r\n", Cursor::ClearRight));
        }

        w_sgncol
    }

    fn render_status_bar(&self, buf: &mut String, cs: &ColorScheme) {
        let b = self.buffers.active();
        let lstatus = format!(
            "{} {} - {} lines {}",
            self.modes[0],
            b.display_name(&self.cwd),
            b.len_lines(),
            if b.dirty { "[+]" } else { "" }
        );
        let rstatus = b.dot.addr(b);
        let width = self.screen_cols - lstatus.len();

        buf.push_str(&format!(
            "{}{}{lstatus}{rstatus:>width$}{}\r\n",
            Style::Bg(cs.bar_bg),
            Style::Fg(cs.fg),
            Style::Reset
        ));
    }

    // current prompt and pending chars
    fn render_message_bar(&self, buf: &mut String, cs: &ColorScheme, status_timeout: u64) {
        buf.push_str(&Cursor::ClearRight.to_string());

        let mut msg = self.status_message.clone();
        msg.truncate(self.screen_cols.saturating_sub(10));

        let pending = render_pending(&self.pending_keys);
        let delta = (Instant::now() - self.status_time).as_secs();

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
    }

    #[allow(clippy::too_many_arguments)]
    fn render_minibuffer_state(
        &self,
        buf: &mut String,
        prompt_line: &str,
        b: Option<&Buffer>,
        selected_line_idx: usize,
        top: usize,
        bottom: usize,
        cs: &ColorScheme,
    ) {
        if let Some(b) = b {
            let width = self.screen_cols;

            for i in top..=bottom {
                let (rline, _) = b.raw_rline_unchecked(i, 0, self.screen_cols, None);
                let len = min(self.screen_cols, rline.len());
                if i == selected_line_idx {
                    buf.push_str(&format!(
                        "{}{}{:<width$}{}\r\n",
                        Style::Fg(cs.fg),
                        Style::Bg(cs.minibuffer_hl),
                        &rline[0..len],
                        Style::Reset,
                    ));
                } else {
                    buf.push_str(&format!(
                        "{}{}{}{}\r\n",
                        Style::Fg(cs.fg),
                        Style::Bg(cs.bg),
                        &rline[0..len],
                        Cursor::ClearRight
                    ));
                }
            }
        }

        buf.push_str(&format!(
            "{}{}{prompt_line}{}",
            Style::Fg(cs.fg),
            Style::Bg(cs.bg),
            Cursor::ClearRight
        ));
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::key::Key::*;
    use simple_test_case::test_case;

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
