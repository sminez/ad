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
    buffer::{Buffer, BufferId, Buffers},
    config_handle, die,
    dot::{Cur, Dot},
    editor::ViewPort,
    ziplist,
    ziplist::{Position, ZipList},
};
use std::{cmp::min, mem::swap};
use unicode_width::UnicodeWidthChar;

/// Windows is a screen layout of the windows available for displaying buffer
/// content to the user. The available screen space is split into a number of
/// columns each containing a vertical stack of windows.
#[derive(Debug, Clone)]
pub(crate) struct Windows {
    /// Available screen width in terms of characters
    pub(crate) screen_rows: usize,
    /// Available screen height in terms of characters
    pub(crate) screen_cols: usize,
    /// Left to right Columns of windows
    pub(super) cols: ZipList<Column>,
    /// Known Buffer views that are not currently active
    pub(super) views: Vec<View>,
}

impl Windows {
    pub(crate) fn new(screen_rows: usize, screen_cols: usize, active_buffer_id: BufferId) -> Self {
        Self {
            screen_rows,
            screen_cols,
            cols: ziplist![Column::new(screen_rows, screen_cols, &[active_buffer_id])],
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

        if self.cols.len() == 1 {
            self.cols.focus.update_size(rows, cols);
            return;
        }

        let (w_col, slop) = calculate_dims(cols, self.cols.len());
        for (i, (_, col)) in self.cols.iter_mut().enumerate() {
            let mut w = w_col;
            if i < slop {
                w += 1;
            }
            col.update_size(rows, w);
        }
    }

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn show_buffer_in_active_window(&mut self, b: &Buffer) {
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

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn show_buffer_in_new_window(&mut self, b: &Buffer) {
        let view = if self.focused_view().bufid == b.id {
            self.focused_view().clone()
        } else {
            match self.views.iter().position(|v| v.bufid == b.id) {
                Some(idx) => self.views.remove(idx),
                None => View::new(b.id),
            }
        };

        if self.cols.len() == 1 {
            let mut col = Column::new(self.screen_rows, self.screen_cols, &[b.id]);
            col.wins.last_mut().view = view;
            self.cols.insert_at(Position::Tail, col);
        } else {
            let wins = &mut self.cols.last_mut().wins;
            wins.insert_at(Position::Tail, Window { n_rows: 0, view });
            wins.focus_tail();
        }

        self.cols.focus_tail();
        self.update_screen_size(self.screen_rows, self.screen_cols);
    }

    pub(crate) fn scroll_up(&mut self, b: &mut Buffer) {
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;
        let view = self.focused_view_mut();
        let c = b.dot.active_cur();
        let (y, x) = c.as_yx(b);

        if view.row_off > 0 && y == view.row_off + rows - 1 {
            b.dot.set_active_cur(Cur::from_yx(y - 1, x, b));
        }

        view.row_off = view.row_off.saturating_sub(1);
        view.clamp_scroll(b, rows, cols);
    }

    pub(crate) fn scroll_down(&mut self, b: &mut Buffer) {
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;
        let view = self.focused_view_mut();
        let c = b.dot.active_cur();
        let (y, x) = c.as_yx(b);

        if y == view.row_off && view.row_off < b.txt.len_lines() - 1 {
            b.dot.set_active_cur(Cur::from_yx(y + 1, x, b));
            b.dot.clamp_idx(b.txt.len_chars());
            b.xdot.clamp_idx(b.txt.len_chars());
        }

        view.row_off += 1;
        view.clamp_scroll(b, rows, cols);
    }

    pub(crate) fn clamp_scroll(&mut self, buffers: &mut Buffers) {
        let b = buffers.active_mut();
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;

        self.focused_view_mut().clamp_scroll(b, rows, cols);
    }

    pub(crate) fn set_viewport(&mut self, buffers: &mut Buffers, vp: ViewPort) {
        let b = buffers.active_mut();
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;

        self.focused_view_mut().set_viewport(b, vp, rows, cols);
    }

    /// Coordinate offsets from the top left of the window layout to the top left of the active window.
    fn xy_offsets(&self) -> (usize, usize) {
        let cols_before = &self.cols.up;
        let wins_above = &self.cols.focus.wins.up;
        let x_offset = cols_before.iter().map(|c| c.n_cols).sum::<usize>() + cols_before.len();
        let y_offset = wins_above.iter().map(|w| w.n_rows).sum::<usize>() + wins_above.len();

        (x_offset, y_offset)
    }

    /// Locate the absolute cursor position based on the current window layout
    pub(crate) fn ui_xy(&self, b: &Buffer) -> (usize, usize) {
        let (x_offset, y_offset) = self.xy_offsets();
        let (x, y) = self.focused_view().ui_xy(b);

        (x + x_offset, y + y_offset)
    }

    fn buffer_for_screen_coords(&self, x: usize, y: usize) -> BufferId {
        let mut x_offset = 0;
        let mut y_offset = 0;

        for (_, col) in self.cols.iter() {
            if x > x_offset + col.n_cols {
                x_offset += col.n_cols + 1;
                continue;
            }
            for (_, win) in col.wins.iter() {
                if y > y_offset + win.n_rows {
                    y_offset += win.n_rows + 1;
                    continue;
                }
                return win.view.bufid;
            }
        }

        die!("click out of bounds (x, y)=({x}, {y})");
    }

    pub(crate) fn focus_buffer_for_screen_coords(
        &mut self,
        buffers: &mut Buffers,
        x: usize,
        y: usize,
    ) -> BufferId {
        let mut x_offset = 0;
        let mut y_offset = 0;

        self.cols.focus_head();
        for _ in 0..self.cols.len() {
            let col = &self.cols.focus;
            if x > x_offset + col.n_cols {
                x_offset += col.n_cols + 1;
                self.cols.focus_down();
                continue;
            }

            self.cols.focus.wins.focus_head();
            for _ in 0..self.cols.focus.wins.len() {
                let win = &self.cols.focus.wins.focus;
                if y > y_offset + win.n_rows {
                    y_offset += win.n_rows + 1;
                    self.cols.focus.wins.focus_down();
                    continue;
                }
                buffers.focus_id(win.view.bufid);
                return win.view.bufid;
            }
        }

        die!("click out of bounds (x, y)=({x}, {y})");
    }

    pub(crate) fn cur_from_screen_coords(
        &mut self,
        buffers: &mut Buffers,
        x: usize,
        y: usize,
        set_focus: bool,
    ) -> (BufferId, Cur) {
        let bufid = if set_focus {
            self.focus_buffer_for_screen_coords(buffers, x, y)
        } else {
            self.buffer_for_screen_coords(x, y)
        };
        let b = buffers.with_id_mut(bufid).expect("windows state is stale");

        let (x_offset, y_offset) = self.xy_offsets();
        let (_, w_sgncol) = b.sign_col_dims();
        let rx = x
            .saturating_sub(1)
            .saturating_sub(w_sgncol)
            .saturating_sub(x_offset);

        let view = self.focused_view_mut();
        view.rx = rx;
        b.cached_rx = rx;

        let y = min(y.saturating_sub(y_offset) + view.row_off, b.len_lines()).saturating_sub(1);
        let mut cur = Cur::from_yx(y, b.x_from_provided_rx(y, view.rx), b);
        cur.clamp_idx(b.txt.len_chars());

        (bufid, cur)
    }

    /// Set the active buffer and dot based on a mouse click.
    ///
    /// Returns true if the click was in the currently active buffer and false if this click has
    /// changed the active buffer.
    pub(crate) fn set_dot_from_screen_coords(
        &mut self,
        buffers: &mut Buffers,
        x: usize,
        y: usize,
    ) -> bool {
        let current_bufid = buffers.active().id;
        let (bufid, c) = self.cur_from_screen_coords(buffers, x, y, true);
        buffers.active_mut().dot = Dot::Cur { c };

        bufid == current_bufid
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Column {
    /// Number of character columns wide
    pub(crate) n_cols: usize,
    /// Windows within this column
    pub(crate) wins: ZipList<Window>,
}

impl Column {
    pub(crate) fn new(n_rows: usize, n_cols: usize, buf_ids: &[BufferId]) -> Self {
        if buf_ids.is_empty() {
            panic!("cant have an empty column");
        }
        let win_rows = n_rows / buf_ids.len();
        let mut wins =
            ZipList::try_from_iter(buf_ids.iter().map(|id| Window::new(win_rows, *id))).unwrap();

        let slop = n_rows - (win_rows * buf_ids.len()) + buf_ids.len() - 1;
        wins.focus.n_rows += slop;

        Self { n_cols, wins }
    }

    fn update_size(&mut self, n_rows: usize, n_cols: usize) {
        self.n_cols = n_cols;

        if self.wins.len() == 1 {
            self.wins.focus.n_rows = n_rows;
            return;
        }

        let (h_win, slop) = calculate_dims(n_rows, self.wins.len());
        for (i, (_, win)) in self.wins.iter_mut().enumerate() {
            let mut h = h_win;
            if i < slop {
                h += 1;
            }
            win.n_rows = h;
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Window {
    /// Number of character rows high
    pub(crate) n_rows: usize,
    /// Buffer view details currently shown in this window
    pub(crate) view: View,
}

impl Window {
    pub(crate) fn new(n_rows: usize, bufid: BufferId) -> Self {
        Self {
            n_rows,
            view: View::new(bufid),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct View {
    pub(crate) bufid: BufferId,
    pub(crate) col_off: usize,
    pub(crate) row_off: usize,
    pub(crate) rx: usize,
}

impl View {
    pub(crate) fn new(bufid: BufferId) -> Self {
        Self {
            bufid,
            col_off: 0,
            row_off: 0,
            rx: 0,
        }
    }

    /// provides an (x, y) coordinate assuming that this window is in the top left
    fn ui_xy(&self, b: &Buffer) -> (usize, usize) {
        let (_, w_sgncol) = b.sign_col_dims();
        let (y, _) = b.dot.active_cur().as_yx(b);
        let x = self.rx - self.col_off + w_sgncol;
        let y = y - self.row_off;

        (x, y)
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
}

/// Calculate the size (rows/cols) for n blocks within an available space of t
/// while accounting for "slop" that will be added to some elements to make up
/// the correct total.
///
/// This calculation is derived from:  t = n(size) + (n - 1) + slop
///
/// where the (n - 1) is spacer rows/columns between each region. The use of
/// truncating division in computing "size" gets us an approximate answer for
/// an integer value that solve the equation above without "slop", which is then
/// calculated to get the correct total
fn calculate_dims(t: usize, n: usize) -> (usize, usize) {
    let size = (t + 1) / n - 1;
    let slop = t + 1 - n * (size + 1);

    (size, slop)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        dot::{Dot, TextObject},
        key::Arrow,
    };
    use simple_test_case::test_case;

    #[test_case(&[1], 30, 40, 0; "one col one win")]
    #[test_case(&[1, 1], 30, 40, 0; "two cols one win each click in first")]
    #[test_case(&[1, 1], 60, 40, 1; "two cols one win each click in second")]
    #[test_case(&[1, 2], 60, 40, 1; "two cols second with two click in second window")]
    #[test_case(&[1, 2], 60, 60, 2; "two cols second with two click in third window")]
    #[test_case(&[1, 3], 60, 15, 1; "two cols second with three click in first window")]
    #[test_case(&[1, 3], 60, 35, 2; "two cols second with three click in second window")]
    #[test_case(&[1, 3], 60, 60, 3; "two cols second with three click in third window")]
    #[test_case(&[1, 4], 60, 70, 4; "two cols second with four click in fourth window")]
    #[test]
    fn buffer_for_screen_coords_works(col_wins: &[usize], x: usize, y: usize, expected: BufferId) {
        let mut cols = Vec::with_capacity(col_wins.len());
        let mut n = 0;

        for m in col_wins.iter() {
            let ids: Vec<usize> = (n..(n + m)).collect();
            n += m;
            cols.push(Column::new(80, 100, &ids));
        }

        let mut ws = Windows {
            screen_rows: 80,
            screen_cols: 100,
            cols: ZipList::try_from_iter(cols).unwrap(),
            views: vec![],
        };
        ws.update_screen_size(80, 100);
        println!("{ws:#?}");

        assert_eq!(
            ws.buffer_for_screen_coords(x, y),
            expected,
            "bufid without mutation"
        );
        assert_eq!(
            ws.cols.focus.wins.focus.view.bufid, 0,
            "focused id before mutation"
        );
        assert_eq!(
            ws.focus_buffer_for_screen_coords(&mut Buffers::new(), x, y),
            expected,
            "bufid with mutation"
        );
        assert_eq!(
            ws.cols.focus.wins.focus.view.bufid, expected,
            "focused id after mutation"
        );
    }

    #[test]
    fn focus_buffer_for_screen_coords_doesnt_reorder_windows() {
        let (x, y) = (60, 70);
        let expected = 4;
        let mut cols = Vec::with_capacity(2);
        let mut n = 0;

        for m in [1, 4].iter() {
            let ids: Vec<usize> = (n..(n + m)).collect();
            n += m;
            cols.push(Column::new(80, 100, &ids));
        }

        let mut ws = Windows {
            screen_rows: 80,
            screen_cols: 100,
            cols: ZipList::try_from_iter(cols).unwrap(),
            views: vec![],
        };
        ws.update_screen_size(80, 100);

        let ids = |ws: &Windows| {
            ws.cols
                .iter()
                .flat_map(|(_, c)| c.wins.iter().map(|(_, w)| w.view.bufid))
                .collect::<Vec<_>>()
        };

        assert_eq!(&ids(&ws), &[0, 1, 2, 3, 4], "before first click");

        assert_eq!(
            ws.focus_buffer_for_screen_coords(&mut Buffers::new(), x, y),
            expected,
            "bufid with mutation"
        );

        assert_eq!(&ids(&ws), &[0, 1, 2, 3, 4], "after first click");

        assert_eq!(
            ws.focus_buffer_for_screen_coords(&mut Buffers::new(), x, y),
            expected,
            "bufid with mutation"
        );

        assert_eq!(&ids(&ws), &[0, 1, 2, 3, 4], "after second click");
    }

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
