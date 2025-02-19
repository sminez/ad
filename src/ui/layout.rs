//! Layout of UI windows
use crate::{
    buffer::{Buffer, BufferId, Buffers},
    config_handle,
    dot::{Cur, Dot},
    editor::ViewPort,
    lsp::LspManagerHandle,
    ziplist,
    ziplist::{Position, ZipList},
};
use std::{cmp::min, io, mem::swap, path::Path, sync::Arc};
use tracing::debug;
use unicode_width::UnicodeWidthChar;

/// Layout is a screen layout of the windows available for displaying buffer
/// content to the user. The available screen space is split into a number of
/// columns each containing a vertical stack of windows.
#[derive(Debug)]
pub(crate) struct Layout {
    buffers: Buffers,
    /// Available screen width in terms of characters
    pub(crate) screen_rows: usize,
    /// Available screen height in terms of characters
    pub(crate) screen_cols: usize,
    /// Left to right Columns of windows
    pub(super) cols: ZipList<Column>,
    /// Known Buffer views that are not currently active
    pub(super) views: Vec<View>,
}

impl Layout {
    pub(crate) fn new(
        screen_rows: usize,
        screen_cols: usize,
        lsp_handle: Arc<LspManagerHandle>,
    ) -> Self {
        let buffers = Buffers::new(lsp_handle);
        let id = buffers.active().id;

        Self {
            buffers,
            screen_rows,
            screen_cols,
            cols: ziplist![Column::new(screen_rows, screen_cols, &[id])],
            views: vec![],
        }
    }

    pub(crate) fn buffers(&self) -> &Buffers {
        &self.buffers
    }

    pub(crate) fn ensure_file_is_open(&mut self, path: &str) {
        self.buffers.ensure_file_is_open(path)
    }

    pub(crate) fn is_empty_scratch(&self) -> bool {
        self.buffers.is_empty_scratch()
    }

    fn buffer_is_visible(&self, id: BufferId) -> bool {
        self.cols
            .iter()
            .any(|(_, c)| c.wins.iter().any(|(_, w)| w.view.bufid == id))
    }

    pub(crate) fn active_buffer(&self) -> &Buffer {
        self.buffers.active()
    }

    pub(crate) fn active_buffer_mut(&mut self) -> &mut Buffer {
        self.buffers.active_mut()
    }

    pub(crate) fn buffer_with_id(&self, id: BufferId) -> Option<&Buffer> {
        self.buffers.with_id(id)
    }

    pub(crate) fn buffer_with_id_mut(&mut self, id: BufferId) -> Option<&mut Buffer> {
        self.buffers.with_id_mut(id)
    }

    fn focus_first_window_with_buffer(&mut self, id: BufferId) {
        self.cols
            .focus_element_by_mut(|c| c.wins.focus_element_by_mut(|w| w.view.bufid == id));
    }

    pub(crate) fn open_or_focus<P: AsRef<Path>>(
        &mut self,
        path: P,
        mut new_window: bool,
    ) -> io::Result<Option<BufferId>> {
        if self.buffers.is_empty_scratch() {
            // in the case where we only have an empty scratch buffer present, we always
            // replace the current buffer with the one that is newly opened.
            new_window = false;
        }

        let opt = self.buffers.open_or_focus(path)?;
        let id = self.active_buffer().id;

        if self.buffer_is_visible(id) {
            self.focus_first_window_with_buffer(id);
        } else if new_window {
            self.show_buffer_in_new_window(id);
        } else {
            self.show_buffer_in_active_window(id);
        }

        Ok(opt)
    }

    /// Open a new virtual buffer that is not backed by a file on disk
    ///
    /// Opening the same virtual buffer a second time will replace the contents.
    pub(crate) fn open_virtual(
        &mut self,
        name: impl Into<String>,
        content: impl Into<String>,
        new_window: bool,
    ) {
        let id = self.buffers.open_virtual(name.into(), content.into());

        if self.buffer_is_visible(id) {
            self.focus_first_window_with_buffer(id);
        } else if new_window {
            self.show_buffer_in_new_window(self.active_buffer().id);
        } else {
            self.show_buffer_in_active_window(self.active_buffer().id);
        }
    }

    /// Returns true if this was the last buffer otherwise false.
    ///
    /// Closing a buffer also updates the UI:
    ///   - any cached views for the buffer are cleared
    ///   - any open windows containing the buffer are closed
    ///   - if a buffer was the only window in a given column, the column is removed
    ///   - if the active column is removed then focus moves to the next column
    ///   - if there are no other columns then the "next buffer" is placed in the
    ///     first column
    pub(crate) fn close_buffer(&mut self, id: BufferId) -> bool {
        if self.buffers.len() == 1 {
            // We could have been asked to close a non-existant buffer.
            // If this was the last buffer then Editor::delete_buffer will exit
            return self.active_buffer().id == id;
        }

        debug_assert!(self.buffers.len() > 1, "we have at least two buffers");
        self.views.retain(|v| v.bufid != id);
        self.buffers.close_buffer(id);
        let focused_id = self.active_buffer().id;
        let ix = self.views.iter().position(|v| v.bufid == id);
        let existing_view = ix.map(|ix| self.views.remove(ix));

        let only_closing_buffer = self
            .cols
            .iter()
            .flat_map(|(_, c)| c.wins.iter().map(|(_, w)| w.view.bufid))
            .all(|bufid| bufid == id);

        if only_closing_buffer {
            self.cols = ziplist![Column::new(
                self.screen_rows,
                self.screen_cols,
                &[focused_id]
            )];
            if let Some(view) = existing_view {
                self.cols.focus.wins.focus.view = view;
            }
            self.update_screen_size(self.screen_rows, self.screen_cols);
            return false;
        }

        // Remove columns where there are only views of the closing buffer
        self.cols
            .filter_unchecked(|c| c.wins.iter().any(|(_, w)| w.view.bufid != id));

        // Remove remaining windows which were showing the closing buffer
        for (_, c) in self.cols.iter_mut() {
            c.wins.filter_unchecked(|w| w.view.bufid != id)
        }

        self.update_screen_size(self.screen_rows, self.screen_cols);

        false
    }

    pub(crate) fn focus_id(&mut self, id: BufferId) {
        if let Some(id) = self.buffers.focus_id(id) {
            if self.buffer_is_visible(id) {
                self.focus_first_window_with_buffer(id);
            } else {
                self.show_buffer_in_active_window(id);
            }
        }
    }

    /// Focus the given buffer ID without touching the jump list
    pub(crate) fn focus_id_silent(&mut self, id: BufferId) {
        self.buffers.focus_id_silent(id);
    }

    pub(crate) fn focus_next_buffer(&mut self) -> BufferId {
        self.buffers.next();
        let id = self.active_buffer().id;
        self.show_buffer_in_active_window(id);

        id
    }

    pub(crate) fn focus_previous_buffer(&mut self) -> BufferId {
        self.buffers.previous();
        let id = self.active_buffer().id;
        self.show_buffer_in_active_window(id);

        id
    }

    /// Close the active window, if this was the last remaining window then
    /// Editor::delete_active_window will exit.
    pub(crate) fn close_active_window(&mut self) -> bool {
        if self.cols.len() == 1 && self.cols.focus.wins.len() == 1 {
            return true;
        }

        if self.cols.focus.wins.len() == 1 {
            self.cols.remove_focused_unchecked();
        } else {
            self.cols.focus.wins.remove_focused_unchecked();
        }

        let id = self.cols.focus.wins.focus.view.bufid;
        self.buffers.focus_id(id);
        self.update_screen_size(self.screen_rows, self.screen_cols);

        false
    }

    /// Close the active column, if this was the last remaining column then
    /// Editor::delete_active_column will exit.
    pub(crate) fn close_active_column(&mut self) -> bool {
        if self.cols.len() == 1 {
            return true;
        }

        self.cols.remove_focused_unchecked();

        let id = self.cols.focus.wins.focus.view.bufid;
        self.buffers.focus_id(id);
        self.update_screen_size(self.screen_rows, self.screen_cols);

        false
    }

    pub(crate) fn record_jump_position(&mut self) {
        self.buffers.record_jump_position();
    }

    pub(crate) fn dirty_buffers(&self) -> Vec<String> {
        self.buffers.dirty_buffers()
    }

    pub(crate) fn as_buffer_list(&self) -> Vec<String> {
        self.buffers.as_buffer_list()
    }

    pub(crate) fn jump_forward(&mut self) -> Option<BufferId> {
        let maybe_ids = self.buffers.jump_list_forward();
        if let Some((prev_id, new_id)) = maybe_ids {
            self.show_buffer_in_active_window(self.active_buffer().id);
            self.set_viewport(ViewPort::Center);
            if new_id != prev_id {
                return Some(new_id);
            }
        }

        None
    }

    pub(crate) fn jump_backward(&mut self) -> Option<BufferId> {
        let maybe_ids = self.buffers.jump_list_backward();
        if let Some((prev_id, new_id)) = maybe_ids {
            self.show_buffer_in_active_window(self.active_buffer().id);
            self.set_viewport(ViewPort::Center);
            if new_id != prev_id {
                return Some(new_id);
            }
        }

        None
    }

    pub(crate) fn write_output_for_buffer(&mut self, id: usize, s: String, cwd: &Path) {
        let id = self.buffers.write_output_for_buffer(id, s, cwd);
        if !self.buffer_is_visible(id) {
            self.show_buffer_in_new_window(id);
        } else {
            self.show_buffer_in_active_window(id);
        }
    }

    /// Move focus to the column to the right of current focus (wrapping)
    pub(crate) fn next_column(&mut self) {
        self.cols.focus_down();
        self.buffers.focus_id(self.focused_view().bufid);
    }

    /// Move focus to the column to the left of current focus (wrapping)
    pub(crate) fn prev_column(&mut self) {
        self.cols.focus_up();
        self.buffers.focus_id(self.focused_view().bufid);
    }

    /// Move focus to the window below in the current column (wrapping)
    pub(crate) fn next_window_in_column(&mut self) {
        self.cols.focus.wins.focus_down();
        self.buffers.focus_id(self.focused_view().bufid);
    }

    /// Move focus to the window above in the current column (wrapping)
    pub(crate) fn prev_window_in_column(&mut self) {
        self.cols.focus.wins.focus_up();
        self.buffers.focus_id(self.focused_view().bufid);
    }

    /// Drag the focused window up through the column containing it (wrapping)
    pub(crate) fn drag_up(&mut self) {
        self.cols.focus.wins.swap_up();
    }

    /// Drag the focused window down through the column containing it (wrapping)
    pub(crate) fn drag_down(&mut self) {
        self.cols.focus.wins.swap_down();
    }

    /// Drag the focused window to the column on the left.
    ///
    /// # Semantics
    /// - If the current columns contains multiple windows and the target exists
    ///   then the current focus is moved to the focus position of the target column
    /// - We anchor if the current column is the extreme left or right and this is
    ///   the only window, otherwise a new column is created and the window is moved
    ///   into it as the focus.
    /// - If the focused window is the only window in and extremal column and the
    ///   direction is towards other columns then the window is moved to that column
    ///   and the previous column is removed.
    pub(crate) fn drag_left(&mut self) {
        if self.cols.len() == 1 || self.cols.up.is_empty() {
            if self.cols.focus.wins.len() == 1 {
                return;
            }
            let win = self.cols.focus.wins.remove_focused_unchecked();
            let mut col = Column::new(self.screen_rows, self.screen_cols, &[win.view.bufid]);
            col.wins.focus = win;
            self.cols.insert_at(Position::Head, col);
            self.cols.focus_up();
        } else if self.cols.focus.wins.len() == 1 {
            let on_left = self.cols.up.is_empty();
            let win = self.cols.remove_focused_unchecked().wins.focus;
            if !on_left {
                self.cols.focus_up();
            }
            self.cols.focus.wins.insert(win);
        } else {
            let win = self.cols.focus.wins.remove_focused_unchecked();
            self.cols.focus_up();
            self.cols.focus.wins.insert(win);
        }
        self.update_screen_size(self.screen_rows, self.screen_cols);
    }

    /// Drag the focused window to the column on the right.
    ///
    /// See [Layout::drag_left] for semantics.
    pub(crate) fn drag_right(&mut self) {
        if self.cols.len() == 1 || self.cols.down.is_empty() {
            if self.cols.focus.wins.len() == 1 {
                return;
            }
            let win = self.cols.focus.wins.remove_focused_unchecked();
            let mut col = Column::new(self.screen_rows, self.screen_cols, &[0]);
            col.wins.focus = win;
            self.cols.insert_at(Position::Tail, col);
            self.cols.focus_down();
        } else if self.cols.focus.wins.len() == 1 {
            let win = self.cols.remove_focused_unchecked().wins.focus;
            self.cols.focus.wins.insert(win);
        } else {
            let win = self.cols.focus.wins.remove_focused_unchecked();
            self.cols.focus_down();
            self.cols.focus.wins.insert(win);
        }
        self.update_screen_size(self.screen_rows, self.screen_cols);
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

        self.clamp_scroll();
    }

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn show_buffer_in_active_window(&mut self, id: BufferId) {
        if self.focused_view().bufid == id {
            return;
        }

        let mut view = match self.views.iter().position(|v| v.bufid == id) {
            Some(idx) => self.views.remove(idx),
            None => View::new(id),
        };

        swap(self.focused_view_mut(), &mut view);
        self.views.push(view);
    }

    /// Create a new column containing a single window showing the same view found in the
    /// current active window.
    pub(crate) fn new_column(&mut self) {
        let view = self.focused_view().clone();
        let mut col = Column::new(self.screen_rows, self.screen_cols, &[view.bufid]);
        col.wins.last_mut().view = view;
        self.cols.insert_at(Position::Tail, col);
        self.cols.focus_tail();
        self.update_screen_size(self.screen_rows, self.screen_cols);
    }

    /// Create a new window at the end of the current column showing the same view
    /// found in the current active window.
    pub(crate) fn new_window(&mut self) {
        let view = self.focused_view().clone();
        let wins = &mut self.cols.focus.wins;
        wins.insert_at(Position::Tail, Window { n_rows: 0, view });
        wins.focus_tail();
        self.update_screen_size(self.screen_rows, self.screen_cols);
    }

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn show_buffer_in_new_window(&mut self, id: BufferId) {
        let view = if self.focused_view().bufid == id {
            self.focused_view().clone()
        } else {
            match self.views.iter().position(|v| v.bufid == id) {
                Some(idx) => self.views.remove(idx),
                None => View::new(id),
            }
        };

        if self.cols.len() == 1 {
            let mut col = Column::new(self.screen_rows, self.screen_cols, &[id]);
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

    pub(crate) fn scroll_up(&mut self) {
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;
        let view = self.cols.focus.focused_view_mut();
        let b = self.buffers.active_mut();
        let c = b.dot.active_cur();
        let (y, x) = c.as_yx(b);

        if view.row_off > 0 && y == view.row_off + rows - 1 {
            b.dot.set_active_cur(Cur::from_yx(y - 1, x, b));
        }

        view.row_off = view.row_off.saturating_sub(1);
        view.clamp_scroll(b, rows, cols);
    }

    pub(crate) fn scroll_down(&mut self) {
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;
        let view = self.cols.focus.focused_view_mut();
        let b = self.buffers.active_mut();
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

    pub(crate) fn clamp_scroll(&mut self) {
        let b = self.buffers.active_mut();
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;

        self.cols
            .focus
            .focused_view_mut()
            .clamp_scroll(b, rows, cols);
    }

    pub(crate) fn set_viewport(&mut self, vp: ViewPort) {
        let b = self.buffers.active_mut();
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;

        self.cols
            .focus
            .focused_view_mut()
            .set_viewport(b, vp, rows, cols);
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

        debug!("click out of bounds (x, y)=({x}, {y})");
        self.active_buffer().id
    }

    pub(crate) fn focus_buffer_for_screen_coords(&mut self, x: usize, y: usize) -> BufferId {
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
                self.buffers.focus_id(win.view.bufid);
                return win.view.bufid;
            }
        }

        debug!("click out of bounds (x, y)=({x}, {y})");
        self.active_buffer().id
    }

    pub(crate) fn cur_from_screen_coords(
        &mut self,
        x: usize,
        y: usize,
        set_focus: bool,
    ) -> (BufferId, Cur) {
        let bufid = if set_focus {
            self.focus_buffer_for_screen_coords(x, y)
        } else {
            self.buffer_for_screen_coords(x, y)
        };
        let (x_offset, y_offset) = self.xy_offsets();
        let b = self
            .buffers
            .with_id_mut(bufid)
            .expect("windows state is stale");
        let (_, w_sgncol) = b.sign_col_dims();
        let rx = x
            .saturating_sub(1)
            .saturating_sub(w_sgncol)
            .saturating_sub(x_offset);

        let view = self.cols.focus.focused_view_mut();
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
    pub(crate) fn set_dot_from_screen_coords(&mut self, x: usize, y: usize) -> bool {
        let current_bufid = self.buffers.active().id;
        let (bufid, c) = self.cur_from_screen_coords(x, y, true);
        self.buffers.active_mut().dot = Dot::Cur { c };

        bufid == current_bufid
    }

    pub(crate) fn update_visible_ts_state(&mut self) {
        let it = self.cols.iter().flat_map(|(_, c)| {
            c.wins
                .iter()
                .map(|(_, w)| (w.view.bufid, w.view.row_off, w.n_rows))
        });

        for (bufid, from, n_rows) in it {
            // SAFETY: we know this id is valid
            let b = unsafe { self.buffers.with_id_mut(bufid).unwrap_unchecked() };
            b.update_ts_state(from, n_rows);
        }
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

    /// Needed to avoid borrowing all of Layout when calling [Layout::focused_view_mut].
    #[inline]
    fn focused_view_mut(&mut self) -> &mut View {
        &mut self.wins.focus.view
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
        let (_, w_sgncol) = b.sign_col_dims();
        self.rx = self.rx_from_x(b, y, x);
        b.cached_rx = self.rx;

        if y < self.row_off {
            self.row_off = y;
        }

        if y >= self.row_off + screen_rows {
            self.row_off = y + 1 - screen_rows;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + screen_cols - w_sgncol {
            self.col_off = self.rx + w_sgncol + 1 - screen_cols;
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
    use std::sync::mpsc::channel;

    fn test_windows(col_wins: &[usize], n_rows: usize, n_cols: usize) -> Layout {
        let mut cols = Vec::with_capacity(col_wins.len());
        let mut n = 0;
        let mut all_ids = Vec::new();

        for m in col_wins.iter() {
            let ids: Vec<usize> = (n..(n + m)).collect();
            n += m;
            cols.push(Column::new(n_rows, n_cols, &ids));
            all_ids.extend(ids);
        }

        let (tx, _) = channel();
        let mut ws = Layout {
            buffers: Buffers::new_stubbed(&all_ids, tx),
            screen_rows: n_rows,
            screen_cols: n_cols,
            cols: ZipList::try_from_iter(cols).unwrap(),
            views: vec![],
        };
        ws.update_screen_size(n_rows, n_cols);

        ws
    }

    fn ordered_window_ids(ws: &Layout) -> Vec<usize> {
        ws.cols
            .iter()
            .flat_map(|(_, c)| c.wins.iter().map(|(_, w)| w.view.bufid))
            .collect::<Vec<_>>()
    }

    #[test]
    fn drag_left_works() {
        let mut ws = test_windows(&[1, 1, 2], 80, 100);
        ws.next_column();
        assert_eq!(ws.active_buffer().id, 1);
        ws.drag_left();

        assert_eq!(ws.cols.len(), 2);
        let first_col: Vec<usize> = ws
            .cols
            .head()
            .wins
            .iter()
            .map(|(_, w)| w.view.bufid)
            .collect();
        let second_col: Vec<usize> = ws
            .cols
            .last()
            .wins
            .iter()
            .map(|(_, w)| w.view.bufid)
            .collect();

        assert_eq!(&first_col, &[1, 0]);
        assert_eq!(&second_col, &[2, 3]);
    }

    #[test]
    fn drag_right_works() {
        let mut ws = test_windows(&[1, 1, 2], 80, 100);
        assert_eq!(ws.active_buffer().id, 0);
        ws.drag_right();

        assert_eq!(ws.cols.len(), 2);
        let first_col: Vec<usize> = ws
            .cols
            .head()
            .wins
            .iter()
            .map(|(_, w)| w.view.bufid)
            .collect();
        let second_col: Vec<usize> = ws
            .cols
            .last()
            .wins
            .iter()
            .map(|(_, w)| w.view.bufid)
            .collect();

        assert_eq!(&first_col, &[0, 1]);
        assert_eq!(&second_col, &[2, 3]);
    }

    #[test]
    fn next_prev_column_methods_work() {
        let mut ws = test_windows(&[1, 1, 2], 80, 100);
        assert_eq!(ws.focused_view().bufid, 0);

        // next wrapping
        ws.next_column();
        assert_eq!(ws.focused_view().bufid, 1);
        ws.next_column();
        assert_eq!(ws.focused_view().bufid, 2);
        ws.next_column();
        assert_eq!(ws.focused_view().bufid, 0);

        // prev wrapping
        ws.prev_column();
        assert_eq!(ws.focused_view().bufid, 2);
        ws.prev_column();
        assert_eq!(ws.focused_view().bufid, 1);
        ws.prev_column();
        assert_eq!(ws.focused_view().bufid, 0);
    }

    #[test]
    fn next_prev_window_methods_work() {
        let mut ws = test_windows(&[3, 1], 80, 100);
        assert_eq!(ws.focused_view().bufid, 0);

        // next wrapping
        ws.next_window_in_column();
        assert_eq!(ws.focused_view().bufid, 1);
        ws.next_window_in_column();
        assert_eq!(ws.focused_view().bufid, 2);
        ws.next_window_in_column();
        assert_eq!(ws.focused_view().bufid, 0);

        // prev wrapping
        ws.prev_window_in_column();
        assert_eq!(ws.focused_view().bufid, 2);
        ws.prev_window_in_column();
        assert_eq!(ws.focused_view().bufid, 1);
        ws.prev_window_in_column();
        assert_eq!(ws.focused_view().bufid, 0);
    }

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
        let mut ws = test_windows(col_wins, 80, 100);
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
            ws.focus_buffer_for_screen_coords(x, y),
            expected,
            "bufid with mutation"
        );
        assert_eq!(
            ws.cols.focus.wins.focus.view.bufid, expected,
            "focused id after mutation"
        );
    }

    #[test_case(0, &[1, 2, 3, 4]; "0")]
    #[test_case(1, &[0, 2, 3, 4]; "1")]
    #[test_case(2, &[0, 1, 3, 4]; "2")]
    #[test_case(3, &[0, 1, 2, 4]; "3")]
    #[test_case(4, &[0, 1, 2, 3]; "4")]
    #[test]
    fn close_buffer_works(id: usize, expected: &[usize]) {
        let mut ws = test_windows(&[1, 4], 80, 100);
        assert_eq!(&ordered_window_ids(&ws), &[0, 1, 2, 3, 4], "initial ids");

        ws.close_buffer(id);
        assert!(
            !ws.buffers.contains_bufid(id),
            "buffer id should be removed"
        );

        for bufid in expected.iter() {
            assert!(
                ws.buffers.contains_bufid(*bufid),
                "other buffers should still be there"
            );
        }

        assert_eq!(
            &ordered_window_ids(&ws),
            expected,
            "ids for each window should be correct"
        );
    }

    #[test]
    fn focus_buffer_for_screen_coords_doesnt_reorder_windows() {
        let (x, y) = (60, 70);
        let expected = 4;
        let mut ws = test_windows(&[1, 4], 80, 100);

        assert_eq!(
            &ordered_window_ids(&ws),
            &[0, 1, 2, 3, 4],
            "before first click"
        );

        assert_eq!(
            ws.focus_buffer_for_screen_coords(x, y),
            expected,
            "bufid with mutation"
        );

        assert_eq!(
            &ordered_window_ids(&ws),
            &[0, 1, 2, 3, 4],
            "after first click"
        );

        assert_eq!(
            ws.focus_buffer_for_screen_coords(x, y),
            expected,
            "bufid with mutation"
        );

        assert_eq!(
            &ordered_window_ids(&ws),
            &[0, 1, 2, 3, 4],
            "after second click"
        );
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
