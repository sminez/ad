//! Rendering the user interface
use crate::{
    buffer::Buffer,
    die,
    editor::Editor,
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

    pub(super) fn refresh_screen_w_cursor(&mut self, cur: Option<(usize, usize)>) {
        self.buffers
            .active_mut()
            .clamp_scroll(self.screen_rows, self.screen_cols);

        let mut buf = format!("{}{}", Cur::Hide, Cur::ToStart);
        let w_sgncol = self.render_rows(&mut buf);
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

    /// Returns the width of the sign column
    fn render_rows(&self, buf: &mut String) -> usize {
        let Buffer {
            lines,
            row_off,
            col_off,
            ..
        } = self.buffers.active();

        // Sort out dimensions of the sign/number column
        let max_linum = min(lines.len(), self.screen_rows + row_off);
        let w_lnum = n_digits(max_linum);
        let w_sgncol = w_lnum + 2;

        for y in 0..self.screen_rows {
            let file_row = y + row_off;

            if file_row >= lines.len() {
                buf.push_str(&format!(
                    "{}~ {VLINE:>width$}{}",
                    Style::Fg(SGNCOL_FG.into()),
                    Style::Reset,
                    width = w_lnum
                ));

                if self.buffers.is_empty_scratch() && y == self.screen_rows / 3 {
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
        let (name, n_lines, cy, rx, dirty) = (name, b.len(), b.cy + 1, b.rx + 1, b.dirty);

        let lstatus = format!(
            "{} {name} - {n_lines} lines {}",
            self.modes[0],
            if dirty { "[+]" } else { "" }
        );
        let rstatus = format!("{cy}:{rx}");
        let width = self.screen_cols - lstatus.len();
        buf.push_str(&format!(
            "{}{lstatus}{rstatus:>width$}{}\r\n",
            Style::Bg(BAR_BG.into()),
            Style::Reset
        ));
    }

    fn render_message_bar(&self, buf: &mut String) {
        buf.push_str(&Cur::ClearRight.to_string());

        let mut msg = self.status_message.clone();
        msg.truncate(self.screen_cols);

        let delta = (Instant::now() - self.status_time).as_secs();
        if !msg.is_empty() && delta < STATUS_TIMEOUT {
            buf.push_str(&msg);
        }
    }
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
}
