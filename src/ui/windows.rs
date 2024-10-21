//! Layout of UI windows
//!
//!
//!  Windows
//! +-----------------+-----------------+
//! |                 |                 |
//! |                 |                 |
//! |                 |                 |
//! |                 |                 |
//! |    Column       ...................
//! |                 .                 .
//! |                 .    Window       .
//! |                 .                 .
//! |                 .                 .
//! +-----------------...................
//! | Status Bar                        |
//! +-----------------------------------+
use crate::{
    buffer::Buffer,
    config_handle,
    dot::{Cur, Dot},
    editor::ViewPort,
    stack,
    ui::stack::Stack,
};
use std::{cmp::min, mem::swap};
use unicode_width::UnicodeWidthChar;

// FIXME: for now there is only ever a single column with a single window

/// Windows is a screen layout of the windows available for displaying buffer
/// content to the user. The available screen space is split into a number of
/// columns each containing a vertical stack of windows.
#[derive(Debug)]
pub(crate) struct Windows {
    /// Available screen width in terms of characters
    pub(crate) screen_rows: usize,
    /// Available screen height in terms of characters
    pub(crate) screen_cols: usize,
    /// Left to right Columns of windows
    pub(super) cols: Stack<Column>,
    /// Known Buffer views that are not currently active
    pub(super) views: Vec<View>,
}

impl Windows {
    pub(crate) fn new(screen_rows: usize, screen_cols: usize, active_buffer_id: usize) -> Self {
        Self {
            screen_rows,
            screen_cols,
            cols: stack![Column::new(screen_rows, screen_cols, &[active_buffer_id])],
            views: vec![],
        }
    }

    #[inline]
    pub(crate) fn focused_view(&self) -> &View {
        &self.cols.focus.wins.focus.view
    }

    #[inline]
    pub(crate) fn focused_view_mut(&mut self) -> &mut View {
        &mut self.cols.focus.wins.focus.view
    }

    pub(crate) fn active_window_rows(&self) -> usize {
        self.cols.focus.wins.focus.n_rows
    }

    pub(crate) fn update_screen_size(&mut self, rows: usize, cols: usize) {
        self.screen_rows = rows;
        self.screen_cols = cols;
        let w_col = cols / self.cols.len();
        let slop = cols - (w_col * self.cols.len());

        for (_, col) in self.cols.iter_mut() {
            col.update_size(w_col, rows);
        }

        self.cols.focus.n_cols += slop;
    }

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn focus_buffer_in_active_window(&mut self, b: &Buffer) {
        if self.focused_view().bufid == b.id {
            return;
        }

        let mut view = match self.views.iter().position(|v| v.bufid == b.id) {
            Some(idx) => self.views.remove(idx),
            None => View::new(b.id),
        };

        swap(self.focused_view_mut(), &mut view);
        self.views.push(view);
    }

    pub(crate) fn scroll_up(&mut self, b: &mut Buffer) {
        let screen_rows = self.screen_rows;
        let view = self.focused_view_mut();
        let c = b.dot.active_cur();
        let (y, x) = c.as_yx(b);

        if view.row_off > 0 && y == view.row_off + screen_rows - 1 {
            b.dot.set_active_cur(Cur::from_yx(y - 1, x, b));
        }

        // clamp scroll is called when we render so no need to run it here as well
        view.row_off = view.row_off.saturating_sub(1);
    }

    pub(crate) fn scroll_down(&mut self, b: &mut Buffer) {
        let view = self.focused_view_mut();
        let c = b.dot.active_cur();
        let (y, x) = c.as_yx(b);

        if y == view.row_off && view.row_off < b.txt.len_lines() - 1 {
            b.dot.set_active_cur(Cur::from_yx(y + 1, x, b));
            b.dot.clamp_idx(b.txt.len_chars());
        }

        // clamp scroll is called when we render so no need to run it here as well
        view.row_off += 1;
    }

    pub(crate) fn clamp_scroll(&mut self, b: &mut Buffer) {
        let (rows, cols) = (self.screen_rows, self.screen_cols);

        if self.focused_view().bufid == b.id {
            self.focused_view_mut().clamp_scroll(b, rows, cols);
            return;
        }

        for (_, col) in self.cols.iter_mut() {
            for (_, win) in col.wins.iter_mut() {
                if win.view.bufid == b.id {
                    win.view.clamp_scroll(b, rows, cols);
                    return;
                }
            }
        }
    }

    pub(crate) fn set_viewport(&mut self, b: &mut Buffer, vp: ViewPort) {
        let (rows, cols) = (self.screen_rows, self.screen_cols);

        if self.focused_view().bufid == b.id {
            self.focused_view_mut().set_viewport(b, vp, rows, cols);
            return;
        }

        for (_, col) in self.cols.iter_mut() {
            for (_, win) in col.wins.iter_mut() {
                if win.view.bufid == b.id {
                    win.view.set_viewport(b, vp, rows, cols);
                    return;
                }
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct Column {
    /// Number of character columns wide
    pub(crate) n_cols: usize,
    /// Windows within this column
    pub(crate) wins: Stack<Window>,
}

impl Column {
    pub(crate) fn new(n_rows: usize, n_cols: usize, buf_ids: &[usize]) -> Self {
        let win_rows = n_rows / buf_ids.len();
        let mut wins =
            Stack::try_from_iter(buf_ids.iter().map(|id| Window::new(*id, win_rows))).unwrap();
        let slop = n_rows - (win_rows * buf_ids.len());
        wins.focus.n_rows += slop;

        Self { n_cols, wins }
    }

    fn update_size(&mut self, n_cols: usize, n_rows: usize) {
        self.n_cols = n_cols;
        let win_rows = n_rows / self.wins.len();

        for (_, win) in self.wins.iter_mut() {
            win.n_rows = win_rows;
        }

        let slop = n_rows - (win_rows * self.wins.len());
        self.wins.focus.n_rows += slop;
    }
}

#[derive(Debug)]
pub(crate) struct Window {
    /// Number of character rows high
    pub(crate) n_rows: usize,
    /// Buffer view details currently shown in this window
    pub(crate) view: View,
}

impl Window {
    pub(crate) fn new(n_rows: usize, bufid: usize) -> Self {
        Self {
            n_rows,
            view: View::new(bufid),
        }
    }
}

#[derive(Debug)]
pub(crate) struct View {
    pub(crate) bufid: usize,
    pub(crate) col_off: usize,
    pub(crate) row_off: usize,
    pub(crate) rx: usize,
}

impl View {
    pub(crate) fn new(bufid: usize) -> Self {
        Self {
            bufid,
            col_off: 0,
            row_off: 0,
            rx: 0,
        }
    }

    pub(crate) fn ui_xy(&self, b: &Buffer) -> (usize, usize) {
        let (_, w_sgncol) = b.sign_col_dims();
        let (y, _) = b.dot.active_cur().as_yx(b);

        (self.rx - self.col_off + w_sgncol, y - self.row_off)
    }

    pub(crate) fn rx_from_x(&self, b: &Buffer, y: usize, x: usize) -> usize {
        if y >= b.len_lines() {
            return 0;
        }

        let tabstop = config_handle!().tabstop;

        let mut rx = 0;
        for c in b.txt.line(y).chars().take(x) {
            if c == '\t' {
                rx += (tabstop - 1) - (rx % tabstop);
            }
            rx += UnicodeWidthChar::width(c).unwrap_or(1);
        }

        rx
    }

    /// Clamp the current viewport to include the [Dot].
    pub(crate) fn clamp_scroll(&mut self, b: &mut Buffer, screen_rows: usize, screen_cols: usize) {
        let (y, x) = b.dot.active_cur().as_yx(b);
        self.rx = self.rx_from_x(b, y, x);
        b.cached_rx = self.rx;

        if y < self.row_off {
            self.row_off = y;
        }

        if y >= self.row_off + screen_rows {
            self.row_off = y - screen_rows + 1;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols {
            self.col_off = self.rx - screen_cols + 1;
        }
    }

    /// Set the current [ViewPort] while accounting for screen size.
    pub(crate) fn set_viewport(
        &mut self,
        b: &mut Buffer,
        vp: ViewPort,
        screen_rows: usize,
        screen_cols: usize,
    ) {
        let (y, _) = b.dot.active_cur().as_yx(b);

        self.row_off = match vp {
            ViewPort::Top => y,
            ViewPort::Center => y.saturating_sub(screen_rows / 2),
            ViewPort::Bottom => y.saturating_sub(screen_rows),
        };

        self.clamp_scroll(b, screen_rows, screen_cols);
    }

    #[inline]
    fn set_rx_from_screen_rows(&mut self, b: &mut Buffer, x: usize) {
        self.rx = self.rx_from_screen_rows(b, x);
        b.cached_rx = self.rx;
    }

    #[inline]
    pub(crate) fn rx_from_screen_rows(&self, b: &Buffer, x: usize) -> usize {
        let (_, w_sgncol) = b.sign_col_dims();
        x.saturating_sub(1).saturating_sub(w_sgncol)
    }

    pub(crate) fn cur_from_screen_coords(&mut self, b: &mut Buffer, x: usize, y: usize) -> Cur {
        self.set_rx_from_screen_rows(b, x);
        let y = min(y + self.row_off, b.len_lines()).saturating_sub(1);
        let mut cur = Cur::from_yx(y, b.x_from_provided_rx(y, self.rx), b);

        cur.clamp_idx(b.txt.len_chars());

        cur
    }

    pub(crate) fn set_dot_from_screen_coords(&mut self, b: &mut Buffer, x: usize, y: usize) {
        b.dot = Dot::Cur {
            c: self.cur_from_screen_coords(b, x, y),
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dot::{Dot, TextObject},
        key::Arrow,
    };

    // NOTE: there was a bug around misunderstanding terminal "cells" in relation to
    //       wide unicode characters
    //       - https://github.com/crossterm-rs/crossterm/issues/458
    //       - https://github.com/unicode-rs/unicode-width
    #[test]
    fn ui_xy_correctly_handles_multibyte_characters() {
        let s = "abc 世界 🦊";
        // unicode display width for each character
        let widths = &[1, 1, 1, 1, 2, 2, 1, 2];
        let mut b = Buffer::new_virtual(0, "test", s);
        let mut view = View::new(0);
        let mut offset = 0;

        // sign column offset is 3
        for (idx, ch) in s.chars().enumerate() {
            assert_eq!(b.dot_contents(), ch.to_string());
            assert_eq!(b.dot, Dot::Cur { c: Cur { idx } });
            assert_eq!(
                view.ui_xy(&b),
                (3 + offset, 0),
                "idx={idx} content={:?}",
                b.dot_contents()
            );

            b.set_dot(TextObject::Arr(Arrow::Right), 1);
            view.clamp_scroll(&mut b, 80, 80);
            offset += widths[idx];
        }
    }
}
