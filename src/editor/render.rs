//! Rendering the user interface
use crate::{
    buffer::Buffer,
    die,
    editor::Editor,
    term::{Cur, Style},
    STATUS_TIMEOUT, UNNAMED_BUFFER, VERSION,
};
use std::{
    cmp::{max, min},
    io::Write,
    time::Instant,
};

impl Editor {
    pub fn refresh_screen(&mut self) {
        self.refresh_screen_w_cursor(None);
    }

    pub(super) fn refresh_screen_w_cursor(&mut self, cur: Option<(usize, usize)>) {
        self.buffers
            .active_mut()
            .clamp_scroll(self.screen_rows, self.screen_cols);

        let mut buf = format!("{}{}", Cur::Hide, Cur::ToStart);
        self.render_rows(&mut buf);
        self.render_status_bar(&mut buf);
        self.render_message_bar(&mut buf);

        let active = self.buffers.active();
        let (x, y) = cur.unwrap_or((active.rx - active.col_off, active.cy - active.row_off));
        buf.push_str(&format!("{}{}", Cur::To(x + 1, y + 1), Cur::Show));

        if let Err(e) = self.stdout.write_all(buf.as_bytes()) {
            die!("Unable to refresh screen: {e}");
        }

        if let Err(e) = self.stdout.flush() {
            die!("Unable to refresh screen: {e}");
        }
    }

    fn render_rows(&self, buf: &mut String) {
        let Buffer {
            lines,
            row_off,
            col_off,
            ..
        } = self.buffers.active();

        for y in 0..self.screen_rows {
            let file_row = y + row_off;

            if file_row >= lines.len() {
                if self.buffers.is_empty_scratch() && y == self.screen_rows / 3 {
                    let mut banner = format!("ad editor :: version {VERSION}");
                    banner.truncate(self.screen_cols);
                    let mut padding = (self.screen_cols - banner.len()) / 2;
                    if padding > 0 {
                        buf.push('~');
                        padding -= 1;
                    }
                    buf.push_str(&" ".repeat(padding));
                    buf.push_str(&banner);
                } else {
                    buf.push('~');
                }
            } else {
                let rline = &lines[file_row].render;
                let mut len = max(0, rline.len() - col_off);
                len = min(self.screen_cols, len);
                buf.push_str(&rline[*col_off..min(self.screen_cols, len)]);
            }

            buf.push_str(&format!("{}\r\n", Cur::ClearRight));
        }
    }

    fn render_status_bar(&self, buf: &mut String) {
        let b = self.buffers.active();
        let name = b.display_name().unwrap_or(UNNAMED_BUFFER);
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
            Style::Bg("#4E415C".into()),
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
