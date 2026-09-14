//! Layout of UI windows
use crate::{
    buffer::{Buffer, BufferId, Buffers, SCRATCH_ID},
    config::Config,
    die,
    dot::Cur,
    editor::{ActionOutcome, UAction, ViewPort},
    key::Arrow,
    ziplist::{Position, ZipList},
    zlist,
};
use parking_lot::RwLock;
use std::{cmp::min, mem::swap, sync::Arc};
use tracing::debug;
use unicode_width::UnicodeWidthChar;

/// Layout is a screen layout of the windows available for displaying buffer
/// content to the user. The available screen space is split into a number of
/// columns each containing a vertical stack of windows.
#[derive(Debug)]
pub struct Layout {
    /// Global editor config
    config: Arc<RwLock<Config>>,
    /// An anonymous buffer that sits outside of the main buffer state and acts as though it is the
    /// active buffer for the purposes of Load/Execute.
    pub(crate) scratch: Scratch,
    /// Available screen width in terms of characters
    pub(crate) screen_rows: usize,
    /// Available screen height in terms of characters
    pub(crate) screen_cols: usize,
    /// Left to right Columns of windows
    pub(super) cols: ZipList<Column>,
    /// Known Buffer views that are not currently active
    pub(super) views: Vec<View>,
    /// Whether or not the on-screen state changed since the last render of the UI.
    /// Per-buffer state changes are tracked on each [Buffer], this flag is only for
    /// changes to the UI layout itself.
    pub(crate) changed_since_last_render: bool,
}

impl Layout {
    pub fn new(
        active_buffer_id: usize,
        screen_rows: usize,
        screen_cols: usize,
        config: Arc<RwLock<Config>>,
    ) -> Self {
        let scratch = Scratch::new(config.clone());

        Self {
            config,
            scratch,
            screen_rows,
            screen_cols,
            cols: zlist![Column::new(screen_rows, screen_cols, &[active_buffer_id])],
            views: vec![],
            changed_since_last_render: false,
        }
    }

    pub fn handle_ui_action(&mut self, uaction: UAction) -> Option<ActionOutcome> {
        use Arrow::*;
        use UAction::*;

        match uaction {
            BalanceActiveColumn => self.balance_active_column(),
            BalanceAll => self.balance_all(),
            BalanceColumns => self.balance_columns(),
            BalanceWindows => self.balance_windows(),

            DeleteColumn { force } => return Some(self.close_active_column(force)),
            DeleteWindow { force } => return Some(self.close_active_window(force)),

            DragWindow { direction: Up } => self.drag_up(),
            DragWindow { direction: Down } => self.drag_down(),
            DragWindow { direction: Left } => self.drag_left(),
            DragWindow { direction: Right } => self.drag_right(),

            NewColumn => return Some(self.new_column()),
            NewWindow => return Some(self.new_window()),
            NextColumn => return Some(self.next_column()),
            NextWindowInColumn => return Some(self.next_window_in_column()),
            PreviousColumn => return Some(self.prev_column()),
            PreviousWindowInColumn => return Some(self.prev_window_in_column()),

            ResizeActiveColumn { delta } => self.resize_active_column(delta),
            ResizeActiveWindow { delta } => self.resize_active_window(delta),

            SetViewPort(vp) => self.set_viewport(vp),
        }

        None
    }

    /// Used in the assert_invariants macro
    #[cfg(test)]
    pub(crate) fn cols(&self) -> &ZipList<Column> {
        &self.cols
    }

    /// Used in the assert_invariants macro
    #[cfg(test)]
    pub(crate) fn views(&self) -> &[View] {
        &self.views
    }

    pub(crate) fn iter_windows(&self) -> impl Iterator<Item = &Window> {
        self.cols
            .iter()
            .flat_map(|(_, c)| c.wins.iter())
            .map(|(_, w)| w)
    }

    pub(crate) fn focused_bufid(&self) -> BufferId {
        self.cols.focus.wins.focus.view.bufid
    }

    #[inline]
    pub(crate) fn focused_view(&self) -> &View {
        &self.cols.focus.wins.focus.view
    }

    #[inline]
    pub(crate) fn focused_view_mut(&mut self) -> &mut View {
        &mut self.cols.focus.wins.focus.view
    }

    pub(crate) fn active_window_rows(&self, scratch_is_focused: bool) -> usize {
        if scratch_is_focused {
            self.scratch.w.n_rows
        } else {
            self.cols.focus.wins.focus.n_rows
        }
    }

    pub(crate) fn ids(&self) -> Vec<Vec<BufferId>> {
        self.cols
            .iter()
            .map(|(_, col)| col.wins.iter().map(|(_, win)| win.view.bufid).collect())
            .collect()
    }

    /// The number of currently visible windows
    pub(crate) fn n_open_windows(&self) -> usize {
        self.cols.iter().map(|(_, c)| c.wins.len()).sum()
    }

    pub(crate) fn buffer_is_visible(&self, id: BufferId) -> bool {
        self.cols
            .iter()
            .any(|(_, c)| c.wins.iter().any(|(_, w)| w.view.bufid == id))
    }

    fn focus_first_window_with_buffer(&mut self, id: BufferId) {
        self.cols
            .focus_element_by_mut(|c| c.wins.focus_element_by_mut(|w| w.view.bufid == id));
    }

    pub(crate) fn toggle_scratch_visibility(&mut self) {
        self.scratch.is_visible = !self.scratch.is_visible;
        self.changed_since_last_render = true;
    }

    pub(crate) fn set_scratch_visible(&mut self, is_visible: bool) {
        self.scratch.is_visible = is_visible;
        self.changed_since_last_render = true;
    }

    pub(crate) fn clear_stale_views(&mut self, buffers: &Buffers) {
        self.views.retain(|v| buffers.contains_bufid(v.bufid));
    }

    pub fn open_or_focus_id(&mut self, id: BufferId, new_window: bool) {
        self.changed_since_last_render = true;

        if self.buffer_is_visible(id) {
            self.focus_first_window_with_buffer(id);
        } else if new_window {
            self.show_buffer_in_new_window(id);
        } else {
            self.show_buffer_in_active_window(id);
        }
    }

    pub(crate) fn focus_id(&mut self, id: BufferId, force_active: bool) {
        self.changed_since_last_render = true;

        if !force_active && self.buffer_is_visible(id) {
            self.focus_first_window_with_buffer(id);
        } else {
            self.show_buffer_in_active_window(id);
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
    pub(crate) fn close_buffer(&mut self, id: BufferId, focused_id: BufferId) -> bool {
        self.changed_since_last_render = true;
        debug_assert_ne!(
            id, SCRATCH_ID,
            "scratch ID should have been handled in editor method"
        );

        self.views.retain(|v| v.bufid != id);
        let ix = self.views.iter().position(|v| v.bufid == id);
        let existing_view = ix.map(|ix| self.views.remove(ix));

        let only_closing_buffer = self
            .cols
            .iter()
            .flat_map(|(_, c)| c.wins.iter().map(|(_, w)| w.view.bufid))
            .all(|bufid| bufid == id);

        if only_closing_buffer {
            self.cols = zlist![Column::new(
                self.screen_rows,
                self.screen_cols,
                &[focused_id]
            )];
            if let Some(view) = existing_view {
                self.cols.focus.wins.focus.view = view;
            }

            return false;
        }

        // Remove columns where there are only views of the closing buffer
        let cols_before = self.cols.len();
        self.cols
            .filter_unchecked(|c| c.wins.iter().any(|(_, w)| w.view.bufid != id));

        if self.cols.len() < cols_before {
            self.balance_columns();
        }

        // Remove remaining windows which were showing the closing buffer
        for (_, c) in self.cols.iter_mut() {
            let wins_before = c.wins.len();
            c.wins.filter_unchecked(|w| w.view.bufid != id);
            if c.wins.len() < wins_before {
                c.balance_windows(self.screen_rows);
            }
        }

        false
    }

    /// Focus the column at the given index without recording a jump.
    ///
    /// # Panics
    /// Panics if col_idx is out of bounds.
    pub fn focus_column_for_resize(&mut self, col_idx: usize) -> BufferId {
        assert!(col_idx < self.cols.len(), "col_idx out of bounds");
        self.changed_since_last_render = true;

        self.cols.focus_head();
        for _ in 0..col_idx {
            self.cols.focus_down();
        }

        self.focused_view().bufid
    }

    /// Focus the window at the given index within the currently focused column,
    /// without recording a jump.
    ///
    /// # Panics
    /// Panics if win_idx is out of bounds.
    pub fn focus_window_for_resize(&mut self, win_idx: usize) -> BufferId {
        let wins = &mut self.cols.focus.wins;
        assert!(win_idx < wins.len(), "win_idx out of bounds");
        self.changed_since_last_render = true;

        wins.focus_head();
        for _ in 0..win_idx {
            wins.focus_down();
        }

        self.focused_view().bufid
    }

    fn close_active_window(&mut self, force: bool) -> ActionOutcome {
        self.changed_since_last_render = true;

        if self.cols.len() == 1 && self.cols.focus.wins.len() == 1 {
            return ActionOutcome::Exit(force);
        }

        if self.cols.focus.wins.len() == 1 {
            self.cols.remove_focused_unchecked();
            self.balance_columns();
        } else {
            self.cols.focus.wins.remove_focused_unchecked();
            self.balance_active_column();
        }

        ActionOutcome::FocusChange(self.focused_bufid())
    }

    fn close_active_column(&mut self, force: bool) -> ActionOutcome {
        self.changed_since_last_render = true;
        if self.cols.len() == 1 {
            return ActionOutcome::Exit(force);
        }

        self.cols.remove_focused_unchecked();
        self.balance_columns();

        ActionOutcome::FocusChange(self.focused_bufid())
    }

    pub(crate) fn jump_forward(
        &mut self,
        maybe_ids: Option<(BufferId, BufferId)>,
    ) -> Option<BufferId> {
        if let Some((prev_id, new_id)) = maybe_ids {
            self.show_buffer_in_active_window(new_id);
            self.set_viewport(ViewPort::Center);
            if new_id != prev_id {
                return Some(new_id);
            }
        }

        None
    }

    pub(crate) fn jump_backward(
        &mut self,
        maybe_ids: Option<(BufferId, BufferId)>,
    ) -> Option<BufferId> {
        if let Some((prev_id, new_id)) = maybe_ids {
            self.show_buffer_in_active_window(new_id);
            self.set_viewport(ViewPort::Center);
            if new_id != prev_id {
                return Some(new_id);
            }
        }

        None
    }

    /// Move focus to the column to the right of current focus (wrapping)
    fn next_column(&mut self) -> ActionOutcome {
        self.changed_since_last_render = true;
        self.cols.focus_down();
        let cur = self.focused_view().cur;

        ActionOutcome::SetCursor(self.focused_bufid(), cur)
    }

    /// Move focus to the column to the left of current focus (wrapping)
    fn prev_column(&mut self) -> ActionOutcome {
        self.changed_since_last_render = true;
        self.cols.focus_up();
        let cur = self.focused_view().cur;

        ActionOutcome::SetCursor(self.focused_bufid(), cur)
    }

    /// Move focus to the window below in the current column (wrapping)
    fn next_window_in_column(&mut self) -> ActionOutcome {
        self.changed_since_last_render = true;
        self.cols.focus.wins.focus_down();
        let cur = self.focused_view().cur;

        ActionOutcome::SetCursor(self.focused_bufid(), cur)
    }

    /// Move focus to the window above in the current column (wrapping)
    pub(crate) fn prev_window_in_column(&mut self) -> ActionOutcome {
        self.changed_since_last_render = true;
        self.cols.focus.wins.focus_up();
        let cur = self.focused_view().cur;

        ActionOutcome::SetCursor(self.focused_bufid(), cur)
    }

    /// Drag the focused window up through the column containing it (wrapping)
    pub(crate) fn drag_up(&mut self) {
        self.changed_since_last_render = true;
        self.cols.focus.wins.swap_up();
    }

    /// Drag the focused window down through the column containing it (wrapping)
    pub(crate) fn drag_down(&mut self) {
        self.changed_since_last_render = true;
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
        self.changed_since_last_render = true;

        // Strictly speaking, self.cols.up.is_empty() == true implies self.cols.len() == 1 but
        // we keep the explicit check for clarity.
        if self.cols.up.is_empty() || self.cols.len() == 1 {
            // Single column or far left column

            // If we only have a single window in this column then we're done...
            if self.cols.focus.wins.len() == 1 {
                return;
            }

            // Otherwise we need to create a new column containing only this window
            let win = self.cols.focus.wins.remove_focused_unchecked();
            self.balance_active_column(); // tidy up the column we've just popped from
            let mut col = Column::new(self.screen_rows, self.screen_cols, &[win.view.bufid]);
            col.wins.focus = win;
            self.cols.insert_at(Position::Head, col);
            self.cols.focus_up();
            self.balance_columns();
        } else if self.cols.focus.wins.len() == 1 {
            // Column that is not on the far left containing only a single window

            // If this column only has a single window then remove it an place the window in
            // the column to the left
            let on_left = self.cols.up.is_empty();
            let win = self.cols.remove_focused_unchecked().wins.focus;
            self.balance_columns();
            self.balance_active_column();
            if !on_left {
                self.cols.focus_up();
            }
            self.cols.focus.wins.insert(win);
            self.balance_active_column();
        } else {
            // Column that is not on the far left containing more than one window

            let win = self.cols.focus.wins.remove_focused_unchecked();
            self.balance_active_column();
            self.cols.focus_up();
            self.cols.focus.wins.insert(win);
            self.balance_active_column();
        }
    }

    /// Drag the focused window to the column on the right.
    ///
    /// See [Layout::drag_left] for semantics.
    pub(crate) fn drag_right(&mut self) {
        self.changed_since_last_render = true;

        // Strictly speaking, self.cols.up.is_empty() == true implies self.cols.len() == 1 but
        // we keep the explicit check for clarity.
        if self.cols.len() == 1 || self.cols.down.is_empty() {
            // Single column or far right column

            // If we only have a single window in this column then we're done...
            if self.cols.focus.wins.len() == 1 {
                return;
            }

            // Otherwise we need to create a new column containing only this window
            let win = self.cols.focus.wins.remove_focused_unchecked();
            self.balance_active_column(); // tidy up the column we've just popped from

            let mut col = Column::new(self.screen_rows, self.screen_cols, &[0]);
            col.wins.focus = win;
            self.cols.insert_at(Position::Tail, col);
            self.cols.focus_down();
            self.balance_columns();
        } else if self.cols.focus.wins.len() == 1 {
            // Column that is not on the far right containing only a single window

            let win = self.cols.remove_focused_unchecked().wins.focus;
            self.cols.focus.wins.insert(win);
            self.balance_active_column();
            self.balance_columns();
        } else {
            // Column that is not on the far right containing more than one window

            let win = self.cols.focus.wins.remove_focused_unchecked();
            self.balance_active_column();
            self.cols.focus_down();
            self.cols.focus.wins.insert(win);
            self.balance_active_column();
        }
    }

    /// Adjust the size of the active [Column] by increasing or decreasing the number of
    /// character columns it takes up.
    ///
    /// The adjustment is always applied from the left of the Column unless the active column
    /// is the first in the [Layout], in which case the adjustment is made from the right.
    pub(crate) fn resize_active_column(&mut self, delta_cols: i16) {
        self.changed_since_last_render = true;
        self.cols.grow_focus(delta_cols);
    }

    /// Adjust the size of the active [Window] by increasing or decreasing the number of rows
    /// it takes up.
    ///
    /// The adjustment is always applied from the top of the window unless the active window
    /// is the first window in its [Column], in which case the adjustment is made from the
    /// bottom of the window instead.
    pub(crate) fn resize_active_window(&mut self, delta_rows: i16) {
        self.changed_since_last_render = true;
        self.cols.focus.wins.grow_focus(delta_rows);
    }

    /// Resize the active column against the column to its right.
    pub fn resize_active_column_against_next(&mut self, delta: i16) {
        self.changed_since_last_render = true;
        self.cols.grow_focus_against_next(delta);
    }

    /// Resize the active window against the window below it.
    pub fn resize_active_window_against_next(&mut self, delta: i16) {
        self.changed_since_last_render = true;
        self.cols.focus.wins.grow_focus_against_next(delta);
    }

    /// Update the current layout state to reflect a new physical screen size given in terms
    /// of the number of character rows and columns.
    ///
    /// Any existing layout customisation will be preserved as far as possible by converting
    /// dimensions to be relative to the size of the full screen.
    pub(crate) fn update_screen_size(&mut self, rows: usize, cols: usize) {
        self.changed_since_last_render = true;

        let col_ratio = (cols as f32) / (self.screen_cols as f32);
        let row_ratio = (rows as f32) / (self.screen_rows as f32);

        self.screen_rows = rows;
        self.screen_cols = cols;

        self.cols.scale_sizes(col_ratio, cols);
        for (_, c) in self.cols.iter_mut() {
            c.wins.scale_sizes(row_ratio, rows);
        }
    }

    /// Force the columns within the layout to be equally sized.
    pub(crate) fn balance_columns(&mut self) {
        self.changed_since_last_render = true;

        let (n_cols, slop) = calculate_dims(self.screen_cols, self.cols.len());
        for (i, (_, col)) in self.cols.iter_mut().enumerate() {
            col.n_cols = n_cols;
            if i < slop {
                col.n_cols += 1;
            }
        }
    }

    /// Force the windows within the active column to be balanced.
    pub(crate) fn balance_active_column(&mut self) {
        self.changed_since_last_render = true;
        self.cols.focus.balance_windows(self.screen_rows);
    }

    /// Force the all windows within the layout to be equally sized within their respective
    /// columns.
    pub(crate) fn balance_windows(&mut self) {
        self.changed_since_last_render = true;
        for (_, col) in self.cols.iter_mut() {
            col.balance_windows(self.screen_rows);
        }
    }

    /// Force all columns and windows to be equally sized.
    pub(crate) fn balance_all(&mut self) {
        self.balance_columns();
        self.balance_windows();
    }

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn show_buffer_in_active_window(&mut self, id: BufferId) {
        self.changed_since_last_render = true;

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
    pub(crate) fn new_column(&mut self) -> ActionOutcome {
        self.changed_since_last_render = true;

        let view = self.focused_view().clone();
        let mut col = Column::new(self.screen_rows, self.screen_cols, &[view.bufid]);
        col.wins.last_mut().view = view;
        self.cols.insert_at(Position::Tail, col);
        self.cols.focus_tail();
        self.balance_columns();

        ActionOutcome::ClearScratchFocus
    }

    /// Create a new window at the end of the current column showing the same view
    /// found in the current active window.
    pub(crate) fn new_window(&mut self) -> ActionOutcome {
        self.changed_since_last_render = true;

        let view = self.focused_view().clone();
        let wins = &mut self.cols.focus.wins;
        wins.insert_at(Position::Tail, Window { n_rows: 0, view });
        wins.focus_tail();
        self.balance_active_column();

        ActionOutcome::ClearScratchFocus
    }

    /// Set the currently focused window to contain the given buffer
    pub(crate) fn show_buffer_in_new_window(&mut self, id: BufferId) {
        self.changed_since_last_render = true;

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
            self.cols.focus_tail();
            self.balance_columns();
        } else {
            self.cols.focus_tail();
            let wins = &mut self.cols.focus.wins;
            wins.insert_at(Position::Tail, Window { n_rows: 0, view });
            wins.focus_tail();
            self.balance_active_column();
        }
    }

    fn set_viewport(&mut self, vp: ViewPort) {
        self.changed_since_last_render = true;
        self.cols.focus.focused_view_mut().pending_viewport = Some(vp);
    }

    /// Coordinate offsets from the top left of the window layout to the top left of the active window.
    fn xy_offsets(&self, scratch_is_focused: bool) -> (usize, usize) {
        if scratch_is_focused {
            let y_offset = self.screen_rows - self.scratch.w.n_rows + 1; // +1 for status line
            return (0, y_offset);
        }

        let cols_before = &self.cols.up;
        let wins_above = &self.cols.focus.wins.up;
        let x_offset = cols_before.iter().map(|c| c.n_cols).sum::<usize>() + cols_before.len();
        let y_offset = wins_above.iter().map(|w| w.n_rows).sum::<usize>() + wins_above.len();

        (x_offset, y_offset)
    }

    /// Whether or not the given screen row is within a visible scratch buffer
    fn row_is_scratch(&self, y: usize) -> bool {
        if !self.scratch.is_visible {
            return false;
        }

        y > (self.screen_rows - self.scratch.w.n_rows + 1)
    }

    pub fn border_at_coords(&self, x: usize, y: usize) -> Option<Border> {
        if self.row_is_scratch(y) {
            return None;
        }

        let n_cols = self.cols.len();
        let mut x_offset = 0;

        for (col_idx, (_, col)) in self.cols.iter().enumerate() {
            let border_x = x_offset + col.n_cols + 1;

            if x == border_x && col_idx < n_cols - 1 {
                return Some(Border::Vertical { col_idx });
            } else if x > border_x {
                x_offset = border_x;
                continue;
            }

            let n_wins = col.wins.len();
            let mut y_offset = 0;

            for (win_idx, (_, win)) in col.wins.iter().enumerate() {
                let border_y = y_offset + win.n_rows + 1;

                if y == border_y && win_idx < n_wins - 1 {
                    return Some(Border::Horizontal { col_idx, win_idx });
                } else if y > border_y {
                    y_offset = border_y;
                    continue;
                }

                return None; // Click was inside a window
            }

            return None;
        }

        None
    }

    /// If the given coordinates lie within the scratch buffer return None, otherwise return the ID
    /// of the buffer containing the point.
    pub(crate) fn buffer_for_screen_coords(&self, x: usize, y: usize) -> Option<BufferId> {
        let mut x_offset = 0;
        let mut y_offset = 0;

        if self.row_is_scratch(y) {
            return Some(SCRATCH_ID);
        }

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
                return Some(win.view.bufid);
            }
        }

        debug!("click out of bounds (x, y)=({x}, {y})");
        None
    }

    pub(crate) fn focus_buffer_for_screen_xy(&mut self, x: usize, y: usize) -> Option<BufferId> {
        self.changed_since_last_render = true;
        let mut x_offset = 0;
        let mut y_offset = 0;

        if self.row_is_scratch(y) {
            return Some(SCRATCH_ID);
        }

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
                return Some(win.view.bufid);
            }
        }

        debug!("click out of bounds (x, y)=({x}, {y})");
        None
    }

    // XXX: Application of layout state to Buffers

    /// Clamp the active view to the current dot of the Buffer it is displaying and ensure that all
    /// other views are within bounds for the end of the buffer.
    ///
    /// We need to do this for every visible view, not just the active one as external
    /// inputs from systems such as the 9p filesystem and LSP servers can manipulate
    /// state for non-active buffers.
    pub(crate) fn clamp_scroll(&mut self, buffers: &mut Buffers) {
        let tabstop = self.config.read().tabstop;

        // Clamp the scratch buffer if it is visible unconditionally as we can't have multiple
        // views of it.
        if self.scratch.is_visible {
            self.scratch.w.view.clamp_scroll(
                buffers.scratch_mut().buffer_mut(),
                self.scratch.w.n_rows,
                self.screen_cols,
                tabstop,
            );
        }

        // Clamp the active buffer fully to ensure that Dot is remaining within bounds
        let b = buffers.active_ignoring_scratch_mut();
        let cols = self.cols.focus.n_cols;
        let rows = self.cols.focus.wins.focus.n_rows;

        self.cols
            .focus
            .focused_view_mut()
            .clamp_scroll(b, rows, cols, tabstop);

        // For all other visible Views, ensure that row_off is clamped to the end of the buffer but
        // don't _fully_ clamp to force the current Dot to be visible. This allows us to present
        // multiple Views of the same Buffer while only scrolling one of them.
        for (col_focused, col) in self.cols.iter_mut() {
            for (win_focused, win) in col.wins.iter_mut() {
                if col_focused && win_focused {
                    continue; // handled above
                }

                let b = buffers.with_id_mut(win.view.bufid).unwrap();
                let y_max = b.txt.len_lines() - 1;
                win.view.row_off = min(win.view.row_off, y_max);
            }
        }
    }

    /// Scroll the `View` under the given cursor coordinates up or down by `scroll_rows`
    pub fn scroll_view(
        &mut self,
        x: usize,
        y: usize,
        up: bool,
        scroll_rows: usize,
        buffers: &mut Buffers,
    ) {
        self.changed_since_last_render = true;
        let tabstop = self.config.read().tabstop;
        let mut x_offset = 0;
        let mut y_offset = 0;

        if self.row_is_scratch(y) {
            let focused = buffers.scratch_is_focused();
            apply_scroll(
                buffers.scratch_mut().buffer_mut(),
                &mut self.scratch.w,
                self.screen_cols,
                tabstop,
                focused,
                up,
                scroll_rows,
            );

            return;
        }

        for (focused_col, col) in self.cols.iter_mut() {
            if x > x_offset + col.n_cols {
                x_offset += col.n_cols + 1;
                continue;
            }
            for (focused_win, win) in col.wins.iter_mut() {
                if y > y_offset + win.n_rows {
                    y_offset += win.n_rows + 1;
                    continue;
                }

                let b = buffers.with_id_mut(win.view.bufid).unwrap_or_else(|| {
                    die!("invalid buffer ID {}", win.view.bufid);
                });
                let focused = focused_col && focused_win;
                apply_scroll(b, win, col.n_cols, tabstop, focused, up, scroll_rows);

                return;
            }
        }

        // Default to scrolling the active window
        let n_cols = self.cols.focus.n_cols;
        let win = &mut self.cols.focus.wins.focus;
        let b = buffers.with_id_mut(win.view.bufid).unwrap();
        apply_scroll(b, win, n_cols, tabstop, true, up, scroll_rows);
    }

    /// Locate the absolute cursor position based on the current window layout
    pub(crate) fn ui_xy(&self, buffers: &Buffers) -> (usize, usize) {
        let (x_offset, y_offset) = self.xy_offsets(buffers.scratch_is_focused());
        let (x, y) = if buffers.scratch_is_focused() {
            self.scratch.w.view.ui_xy(buffers.scratch().buffer())
        } else {
            self.focused_view().ui_xy(buffers.active())
        };

        (x + x_offset, y + y_offset)
    }

    /// Map a given (x, y) point into a Cur for the active buffer or tag, updating the Buffer state
    /// and returning a copy of the new [Cur].
    pub(crate) fn set_cur_from_screen_xy(
        &mut self,
        x: usize,
        y: usize,
        buffers: &mut Buffers,
    ) -> Cur {
        let (x_offset, y_offset) = self.xy_offsets(buffers.scratch_is_focused());
        let (b, win) = if buffers.scratch_is_focused() {
            (buffers.scratch_mut().buffer_mut(), &mut self.scratch.w)
        } else {
            (
                buffers.active_ignoring_scratch_mut(),
                &mut self.cols.focus.wins.focus,
            )
        };

        let row_off = win.view.row_off;

        let (_, w_sgncol) = b.sign_col_dims();
        let rx = x
            .saturating_sub(1)
            .saturating_sub(x_offset)
            .saturating_sub(w_sgncol);
        let y = min(
            y.saturating_sub(y_offset).saturating_add(row_off),
            b.len_lines(),
        )
        .saturating_sub(1);

        win.view.rx = rx;
        b.cached_rx = rx;

        let mut cur = Cur::from_yx(y, b.x_from_provided_rx(y, rx), b);
        cur.clamp_idx(b.len_chars());

        cur
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
        let (win_rows, slop) = calculate_dims(n_rows, buf_ids.len());
        let mut wins = ZipList::try_from_iter(buf_ids.iter().map(|id| Window::new(win_rows, *id)))
            .expect("can't have an empty column");

        for (i, (_, w)) in wins.iter_mut().enumerate() {
            if i < slop {
                w.n_rows += 1;
            }
        }

        Self { n_cols, wins }
    }

    /// Needed to avoid borrowing all of Layout when calling [Layout::focused_view_mut].
    #[inline]
    fn focused_view_mut(&mut self) -> &mut View {
        &mut self.wins.focus.view
    }

    /// Force the windows within this column to be balanced regardless of their current sizes
    fn balance_windows(&mut self, screen_rows: usize) {
        let (n_rows, slop) = calculate_dims(screen_rows, self.wins.len());
        for (i, (_, win)) in self.wins.iter_mut().enumerate() {
            win.n_rows = n_rows;
            if i < slop {
                win.n_rows += 1;
            }
        }
    }
}

/// State for the scratch buffer
#[derive(Debug)]
pub(crate) struct Scratch {
    pub(super) w: Window,
    pub(crate) is_visible: bool,
}

impl Scratch {
    // n_rows is read from config on startup but then not modified after that
    fn new(config: Arc<RwLock<Config>>) -> Self {
        let n_rows = config.read().minibuffer_lines;

        Self {
            w: Window::new(n_rows, SCRATCH_ID),
            is_visible: false,
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
    pending_viewport: Option<ViewPort>,
    cur: Cur,
}

impl View {
    pub(crate) fn new(bufid: BufferId) -> Self {
        Self {
            bufid,
            col_off: 0,
            row_off: 0,
            rx: 0,
            cur: Cur::default(),
            pending_viewport: None,
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

    pub(crate) fn rx_from_x(&self, b: &Buffer, y: usize, x: usize, tabstop: usize) -> usize {
        if y >= b.len_lines() {
            return 0;
        }

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
    pub(crate) fn clamp_scroll(
        &mut self,
        b: &mut Buffer,
        rows: usize,
        cols: usize,
        tabstop: usize,
    ) {
        self.cur = b.dot.active_cur();
        let (y, x) = self.cur.as_yx(b);

        if let Some(vp) = self.pending_viewport.take() {
            self.row_off = match vp {
                ViewPort::Top => y,
                ViewPort::Center => y.saturating_sub(rows / 2),
                ViewPort::Bottom => y.saturating_sub(rows),
            };
        }

        let (_, w_sgncol) = b.sign_col_dims();
        self.rx = self.rx_from_x(b, y, x, tabstop);
        b.cached_rx = self.rx;

        if y < self.row_off {
            self.row_off = y;
        }

        if y >= self.row_off + rows {
            self.row_off = y + 1 - rows;
        }

        if self.rx < self.col_off {
            self.col_off = self.rx;
        }

        if self.rx >= self.col_off + cols - w_sgncol {
            self.col_off = self.rx + w_sgncol + 1 - cols;
        }
    }
}

/// Min window size is 5x5
const MIN_DIM: usize = 5;

pub trait Growable {
    fn size(&mut self) -> &mut usize;

    fn clamped_sub(&mut self, delta: usize, min_val: usize) -> usize {
        let clamped = (*self.size()).saturating_sub(delta);
        if clamped >= min_val {
            *self.size() = clamped;
            delta
        } else {
            let actual = *self.size() - min_val;
            *self.size() = min_val;
            actual
        }
    }
}

impl Growable for Column {
    fn size(&mut self) -> &mut usize {
        &mut self.n_cols
    }
}

impl Growable for Window {
    fn size(&mut self) -> &mut usize {
        &mut self.n_rows
    }
}

impl<T> ZipList<T>
where
    T: Growable,
{
    /// Attempt to adjust the size of the focused element by a given delta.
    ///
    /// Clamp to [MIN_DIM] for both the focused element and the adjacent element
    /// that is being modified along with it.
    fn grow_focus(&mut self, delta: i16) {
        if self.len() == 1 || delta == 0 {
            return; // nothing to grow
        }

        let other = if self.up.is_empty() {
            &mut self.down[0]
        } else {
            &mut self.up[0]
        };

        if delta < 0 {
            let actual = self.focus.clamped_sub((-delta) as usize, MIN_DIM);
            *other.size() += actual;
        } else {
            let actual = other.clamped_sub(delta as usize, MIN_DIM);
            *self.focus.size() += actual;
        }
    }

    /// Resize the focused element against the next element (down[0]).
    ///
    /// No-op if there's no next element to resize against.
    fn grow_focus_against_next(&mut self, delta: i16) {
        if self.down.is_empty() || delta == 0 {
            return;
        }

        let other = &mut self.down[0];
        if delta < 0 {
            let actual = self.focus.clamped_sub((-delta) as usize, MIN_DIM);
            *other.size() += actual;
        } else {
            let actual = other.clamped_sub(delta as usize, MIN_DIM);
            *self.focus.size() += actual;
        }
    }

    /// Attempt to preserve the current relative size of each element when the
    /// overall available space changes.
    fn scale_sizes(&mut self, ratio: f32, new_total: usize) {
        let mut total = 0;
        for (_, elem) in self.iter_mut() {
            let new_size = (*elem.size() as f32 * ratio) as usize;
            *elem.size() = new_size;
            total += new_size;
        }

        total += self.len() - 1;
        let slop = new_total - total;

        for (i, (_, elem)) in self.iter_mut().enumerate() {
            if i < slop {
                *elem.size() += 1;
            }
        }
    }
}

/// A border that can be dragged to resize
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Border {
    /// Vertical border after column at index `col_idx`
    Vertical { col_idx: usize },
    /// Horizontal border after window at index `win_idx` within column at index `col_idx`
    Horizontal { col_idx: usize, win_idx: usize },
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

/// When we apply scrolling to a [View] we need to keep track of a preferred cursor position so
/// that when the user bounces between windows they don't get reset to a default position based on
/// the viewport alone.
fn apply_scroll(
    b: &mut Buffer,
    win: &mut Window,
    n_cols: usize,
    tabstop: usize,
    focused: bool,
    up: bool,
    scroll_rows: usize,
) {
    let n_rows = win.n_rows;
    let view = &mut win.view;
    let mut cur = if focused {
        b.dot.active_cur()
    } else {
        view.cur
    };
    let (y, x) = cur.as_yx(b);
    let y_max = b.txt.len_lines() - 1;
    let mut need_clamp = false;

    if up && view.row_off > 0 && y == view.row_off + n_rows - 1 {
        cur = Cur::from_yx(y.saturating_sub(scroll_rows), x, b);
    } else if !up && y == view.row_off && view.row_off < y_max {
        cur = Cur::from_yx(min(y + scroll_rows, y_max), x, b);
        need_clamp = true;
    };

    if focused {
        b.dot.set_active_cur(cur);
        if need_clamp {
            b.dot.clamp_idx(b.txt.len_chars());
            b.xdot.clamp_idx(b.txt.len_chars());
        }
    } else {
        view.cur = cur;
    }

    view.row_off = if up {
        view.row_off.saturating_sub(scroll_rows)
    } else {
        min(view.row_off + scroll_rows, y_max)
    };

    if focused {
        view.clamp_scroll(b, n_rows, n_cols, tabstop);
    }
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

    impl Layout {
        pub fn column_widths(&self) -> Vec<usize> {
            self.cols.iter().map(|(_, c)| c.n_cols).collect()
        }

        pub fn window_heights(&self) -> Vec<usize> {
            self.cols.focus.wins.iter().map(|(_, w)| w.n_rows).collect()
        }

        pub fn cols_before_focus(&self) -> usize {
            self.cols.up.len()
        }
    }

    fn test_layout(col_wins: &[usize], n_rows: usize, n_cols: usize) -> (Layout, Buffers) {
        let mut cols = Vec::with_capacity(col_wins.len());
        let mut n = 0;
        let mut all_ids = Vec::new();
        let (col_size, slop) = calculate_dims(n_cols, col_wins.len());

        for (i, m) in col_wins.iter().enumerate() {
            let ids: Vec<usize> = (n..(n + m)).collect();
            n += m;
            let col_n_cols = if i < slop { col_size + 1 } else { col_size };
            cols.push(Column::new(n_rows, col_n_cols, &ids));
            all_ids.extend(ids);
        }

        let (tx, _) = channel();
        let config = Arc::new(RwLock::new(Config::default()));
        let scratch = Scratch::new(config.clone());

        let buffers = Buffers::new_stubbed(&all_ids, tx, config.clone());
        let mut l = Layout {
            config,
            scratch,
            screen_rows: n_rows,
            screen_cols: n_cols,
            cols: ZipList::try_from_iter(cols).unwrap(),
            views: vec![],
            changed_since_last_render: false,
        };
        l.update_screen_size(n_rows, n_cols);

        (l, buffers)
    }

    fn ordered_window_ids(l: &Layout) -> Vec<usize> {
        l.cols
            .iter()
            .flat_map(|(_, c)| c.wins.iter().map(|(_, w)| w.view.bufid))
            .collect::<Vec<_>>()
    }

    #[test]
    fn drag_left_works() {
        let (mut l, _) = test_layout(&[1, 1, 2], 80, 100);
        let ao = l.next_column();
        assert_eq!(ao, ActionOutcome::SetCursor(1, Cur { idx: 0 }));
        l.drag_left();

        assert_eq!(l.cols.len(), 2);
        let first_col: Vec<usize> = l
            .cols
            .head()
            .wins
            .iter()
            .map(|(_, w)| w.view.bufid)
            .collect();
        let second_col: Vec<usize> = l
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
        let (mut l, bfs) = test_layout(&[1, 1, 2], 80, 100);
        assert_eq!(bfs.active_id(), 0);
        l.drag_right();

        assert_eq!(l.cols.len(), 2);
        let first_col: Vec<usize> = l
            .cols
            .head()
            .wins
            .iter()
            .map(|(_, w)| w.view.bufid)
            .collect();
        let second_col: Vec<usize> = l
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
        let (mut l, _) = test_layout(&[1, 1, 2], 80, 100);
        assert_eq!(l.focused_view().bufid, 0);

        // next wrapping
        l.next_column();
        assert_eq!(l.focused_view().bufid, 1);
        l.next_column();
        assert_eq!(l.focused_view().bufid, 2);
        l.next_column();
        assert_eq!(l.focused_view().bufid, 0);

        // prev wrapping
        l.prev_column();
        assert_eq!(l.focused_view().bufid, 2);
        l.prev_column();
        assert_eq!(l.focused_view().bufid, 1);
        l.prev_column();
        assert_eq!(l.focused_view().bufid, 0);
    }

    #[test]
    fn next_prev_window_methods_work() {
        let (mut l, _) = test_layout(&[3, 1], 80, 100);
        assert_eq!(l.focused_view().bufid, 0);

        // next wrapping
        l.next_window_in_column();
        assert_eq!(l.focused_view().bufid, 1);
        l.next_window_in_column();
        assert_eq!(l.focused_view().bufid, 2);
        l.next_window_in_column();
        assert_eq!(l.focused_view().bufid, 0);

        // prev wrapping
        l.prev_window_in_column();
        assert_eq!(l.focused_view().bufid, 2);
        l.prev_window_in_column();
        assert_eq!(l.focused_view().bufid, 1);
        l.prev_window_in_column();
        assert_eq!(l.focused_view().bufid, 0);
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
    fn buffer_for_screen_xy_works(col_wins: &[usize], x: usize, y: usize, expected: BufferId) {
        let (mut l, _) = test_layout(col_wins, 80, 100);

        assert_eq!(
            l.buffer_for_screen_coords(x, y),
            Some(expected),
            "bufid without mutation"
        );
        assert_eq!(
            l.cols.focus.wins.focus.view.bufid, 0,
            "focused id before mutation"
        );
        assert_eq!(
            l.focus_buffer_for_screen_xy(x, y),
            Some(expected),
            "bufid with mutation"
        );
        assert_eq!(
            l.cols.focus.wins.focus.view.bufid, expected,
            "focused id after mutation"
        );
    }

    #[test_case(1, 1, "f"; "before wide char SOB")]
    #[test_case(4, 1, " "; "immediately before wide char")]
    #[test_case(5, 1, "世"; "on first wide char")]
    #[test_case(7, 1, "界"; "on second wide char")]
    #[test_case(9, 1, " "; "after second wide char")]
    #[test_case(1, 2, "🦊"; "second line first wide char")]
    #[test_case(3, 2, "⌖"; "second line multibyte single cell char")]
    #[test_case(6, 2, "a"; "second line ascii after wide and multibyte")]
    #[test]
    fn set_cur_from_screen_xy_handles_wide_utf8_chars(x: usize, y: usize, s: &str) {
        let (mut l, mut bfs) = test_layout(&[1], 80, 100);
        // This is a mix of ascii and utf-8 multi-byte characters where some (but not all) of the
        // multi-byte characters have width > 1. Our handling of the raw x position given to us
        // from terminal input needs to be based on _character width_ rather than the number of
        // bytes in the character.
        let content = "foo 世界 ⌠\n🦊⌖ bar".to_string();
        bfs.active_mut().insert_xdot(content);

        // cur_from_screen_coords has to account for the additional UI elements we have in place
        // for the sign column so this gets added on here to allow the test case parameters to
        // represent the logical position within the buffer.
        let (_, w_sgncol) = bfs.active().sign_col_dims();
        let c = l.set_cur_from_screen_xy(x + w_sgncol, y, &mut bfs);
        bfs.active_mut().dot = Dot::Cur { c };

        assert_eq!(bfs.active().dot_contents(), s, "click=({x}, {y})");
    }

    #[test_case(0, &[1, 2, 3, 4]; "0")]
    #[test_case(1, &[0, 2, 3, 4]; "1")]
    #[test_case(2, &[0, 1, 3, 4]; "2")]
    #[test_case(3, &[0, 1, 2, 4]; "3")]
    #[test_case(4, &[0, 1, 2, 3]; "4")]
    #[test]
    fn close_buffer_works(id: usize, expected: &[usize]) {
        let (mut l, mut bfs) = test_layout(&[1, 4], 80, 100);
        assert_eq!(&ordered_window_ids(&l), &[0, 1, 2, 3, 4], "initial ids");

        bfs.close_buffer(id);
        l.close_buffer(id, bfs.active_id());
        assert!(!bfs.contains_bufid(id), "buffer id should be removed");

        for bufid in expected.iter() {
            assert!(
                bfs.contains_bufid(*bufid),
                "other buffers should still be there"
            );
        }

        assert_eq!(
            &ordered_window_ids(&l),
            expected,
            "ids for each window should be correct"
        );
    }

    #[test]
    fn focus_buffer_for_screen_xy_doesnt_reorder_windows() {
        let (x, y) = (60, 70);
        let expected = Some(4);
        let (mut l, _) = test_layout(&[1, 4], 80, 100);

        assert_eq!(
            &ordered_window_ids(&l),
            &[0, 1, 2, 3, 4],
            "before first click"
        );

        assert_eq!(
            l.focus_buffer_for_screen_xy(x, y),
            expected,
            "bufid with mutation"
        );

        assert_eq!(
            &ordered_window_ids(&l),
            &[0, 1, 2, 3, 4],
            "after first click"
        );

        assert_eq!(
            l.focus_buffer_for_screen_xy(x, y),
            expected,
            "bufid with mutation"
        );

        assert_eq!(
            &ordered_window_ids(&l),
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
        let mut b = Buffer::new_virtual(0, "test", s, Default::default());
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
            view.clamp_scroll(&mut b, 80, 80, 4);
            offset += widths[idx];
        }
    }

    #[test_case(1, 0, 10, &[100]; "one col inc")]
    #[test_case(1, 0, -10, &[100]; "one col dec")]
    #[test_case(2, 0, 10, &[60, 39]; "two cols inc one")]
    #[test_case(2, 0, -10, &[40, 59]; "two cols dec one")]
    #[test_case(2, 1, 10, &[40, 59]; "two cols inc two")]
    #[test_case(2, 1, -10, &[60, 39]; "two cols dec two")]
    #[test_case(3, 1, 10, &[23, 43, 32]; "three cols inc two")]
    #[test_case(3, 1, -10, &[43, 23, 32]; "three cols dec two")]
    #[test_case(2, 0, -200, &[MIN_DIM, 100 - MIN_DIM - 1]; "two cols dec one clamping")]
    #[test_case(2, 0, 200, &[100 - MIN_DIM - 1, MIN_DIM]; "two cols inc one clamping")]
    #[test]
    fn resize_active_column_works(n_cols: usize, ix: usize, delta: i16, expected_cols: &[usize]) {
        assert_eq!(expected_cols.len(), n_cols, "malformed test case");
        let (mut l, _) = test_layout(&vec![1; n_cols], 80, 100);
        // set focus to the target column
        l.cols.focus_head();
        for _ in 0..ix {
            l.cols.focus_down();
        }

        l.resize_active_column(delta);

        for (i, (_, c)) in l.cols.iter().enumerate() {
            assert_eq!(c.n_cols, expected_cols[i], "column {i}");
        }
    }

    #[test_case(1, 0, 10, &[80]; "one win inc")]
    #[test_case(1, 0, -10, &[80]; "one win dec")]
    #[test_case(2, 0, 10, &[50, 29]; "two wins inc one")]
    #[test_case(2, 0, -10, &[30, 49]; "two wins dec one")]
    #[test_case(2, 1, 10, &[30, 49]; "two wins inc two")]
    #[test_case(2, 1, -10, &[50, 29]; "two wins dec two")]
    #[test_case(3, 1, 10, &[16, 36, 26]; "three wins inc two")]
    #[test_case(3, 1, -10, &[36, 16, 26]; "three wins dec two")]
    #[test_case(2, 0, -200, &[MIN_DIM, 80 - MIN_DIM - 1]; "two wins dec one clamping")]
    #[test_case(2, 0, 200, &[80 - MIN_DIM - 1, MIN_DIM]; "two wins inc one clamping")]
    #[test]
    fn resize_active_window_works(n_wins: usize, ix: usize, delta: i16, expected_rows: &[usize]) {
        assert_eq!(expected_rows.len(), n_wins, "malformed test case");
        let (mut l, _) = test_layout(&[n_wins], 80, 100);
        // set focus to the target window
        l.cols.focus.wins.focus_head();
        for _ in 0..ix {
            l.cols.focus.wins.focus_down();
        }

        l.resize_active_window(delta);

        for (i, (_, w)) in l.cols.focus.wins.iter().enumerate() {
            assert_eq!(w.n_rows, expected_rows[i], "window {i}");
        }
    }

    #[test_case(100, 120, (73, 46), (100, 63, 36); "increase width and height")]
    #[test_case(60, 80, (48, 31), (60, 38, 21); "decrease width and height")]
    #[test]
    fn update_screen_size_preserves_relative_sizes(
        w: usize,
        h: usize,
        expected_cols: (usize, usize),
        expected_wins: (usize, usize, usize),
    ) {
        let (mut l, _) = test_layout(&[1, 2], 80, 100);

        l.cols.focus_head();
        l.resize_active_column(10);
        l.cols.focus_down();
        l.cols.focus.wins.focus_head();
        l.resize_active_window(10); // now focused on 1st window of 2nd column

        let cols = |l: &Layout| (l.cols.up[0].n_cols, l.cols.focus.n_cols);
        let wins = |l: &Layout| {
            (
                l.cols.up[0].wins.focus.n_rows,
                l.cols.focus.wins.focus.n_rows,
                l.cols.focus.wins.down[0].n_rows,
            )
        };

        // check that the initial column and window sizes are correct
        assert_eq!(cols(&l), (60, 39), "initial column widths");
        assert_eq!(wins(&l), (80, 50, 29), "initial window heights");

        l.update_screen_size(w, h);

        assert_eq!(cols(&l), expected_cols, "updated column widths");
        assert_eq!(wins(&l), expected_wins, "updated window heights");
    }

    #[test]
    fn single_column_single_window_has_no_borders() {
        let (l, _) = test_layout(&[1], 80, 100);

        for x in 1..=100 {
            for y in 1..=80 {
                assert_eq!(
                    l.border_at_coords(x, y),
                    None,
                    "unexpected hit @ ({x}, {y})"
                );
            }
        }
    }

    #[test]
    fn single_column_multiple_windows_has_horizontal_borders() {
        let (l, _) = test_layout(&[3], 80, 100);

        assert_eq!(l.cols[0].wins[0].n_rows, 26);
        assert_eq!(l.cols[0].wins[1].n_rows, 26);
        assert_eq!(l.cols[0].wins[2].n_rows, 26);

        assert_eq!(
            l.border_at_coords(50, 27),
            Some(Border::Horizontal {
                col_idx: 0,
                win_idx: 0
            })
        );

        assert_eq!(
            l.border_at_coords(50, 54),
            Some(Border::Horizontal {
                col_idx: 0,
                win_idx: 1
            })
        );

        assert_eq!(l.border_at_coords(50, 81), None); // past last window
        assert_eq!(l.border_at_coords(50, 1), None); // inside first window
        assert_eq!(l.border_at_coords(50, 26), None); // last row of first window
        assert_eq!(l.border_at_coords(50, 28), None); // first row of second window
    }

    #[test]
    fn multiple_columns_single_window_each_has_vertical_borders() {
        let (l, _) = test_layout(&[1, 1, 1], 80, 100);

        assert_eq!(l.cols[0].n_cols, 33);
        assert_eq!(l.cols[1].n_cols, 33);
        assert_eq!(l.cols[2].n_cols, 32);

        assert_eq!(
            l.border_at_coords(34, 40),
            Some(Border::Vertical { col_idx: 0 })
        );

        assert_eq!(
            l.border_at_coords(68, 40),
            Some(Border::Vertical { col_idx: 1 })
        );

        assert_eq!(l.border_at_coords(101, 40), None); // past last column
        assert_eq!(l.border_at_coords(1, 40), None); // inside first column
        assert_eq!(l.border_at_coords(33, 40), None); // last char of first column
        assert_eq!(l.border_at_coords(35, 40), None); // first char of second column
    }

    #[test]
    fn multiple_columns_multiple_windows_has_both_border_types() {
        let (l, _) = test_layout(&[2, 2], 80, 100);

        let col0_width = l.cols[0].n_cols;
        let win0_height = l.cols[0].wins[0].n_rows;

        assert_eq!(
            l.border_at_coords(col0_width + 1, 20),
            Some(Border::Vertical { col_idx: 0 })
        );

        assert_eq!(
            l.border_at_coords(10, win0_height + 1),
            Some(Border::Horizontal {
                col_idx: 0,
                win_idx: 0
            })
        );

        let col1_x = col0_width + 1 + 10; // inside col1
        assert_eq!(
            l.border_at_coords(col1_x, win0_height + 1),
            Some(Border::Horizontal {
                col_idx: 1,
                win_idx: 0
            })
        );

        assert_eq!(l.border_at_coords(10, 10), None); // inside window
        assert_eq!(l.border_at_coords(col1_x, 10), None); // inside window in col1
    }

    #[test]
    fn border_coords_at_screen_edges() {
        let (l, _) = test_layout(&[1, 1], 80, 100);

        assert_eq!(l.border_at_coords(1, 1), None); // top-left corner, inside first window
        assert_eq!(l.border_at_coords(101, 40), None); // past right edge
        assert_eq!(l.border_at_coords(10, 81), None); // past bottom edge
    }

    #[test]
    fn focus_column_for_resize_works() {
        let (mut l, _) = test_layout(&[1, 1, 1], 80, 100);
        assert_eq!(l.cols.up.len(), 0);

        l.focus_column_for_resize(1);
        assert_eq!(l.cols.up.len(), 1);
        assert_eq!(l.cols.down.len(), 1);

        l.focus_column_for_resize(2);
        assert_eq!(l.cols.up.len(), 2);
        assert_eq!(l.cols.down.len(), 0);

        l.focus_column_for_resize(0);
        assert_eq!(l.cols.up.len(), 0);
        assert_eq!(l.cols.down.len(), 2);
    }

    #[test]
    #[should_panic(expected = "col_idx out of bounds")]
    fn focus_column_for_resize_panics_on_out_of_bounds() {
        let (mut l, _) = test_layout(&[1, 1, 1], 80, 100);
        l.focus_column_for_resize(99);
    }

    #[test]
    fn focus_window_for_resize_works() {
        let (mut l, _) = test_layout(&[3], 80, 100);
        assert_eq!(l.cols.focus.wins.up.len(), 0);

        l.focus_window_for_resize(1);
        assert_eq!(l.cols.focus.wins.up.len(), 1);
        assert_eq!(l.cols.focus.wins.down.len(), 1);

        l.focus_window_for_resize(2);
        assert_eq!(l.cols.focus.wins.up.len(), 2);
        assert_eq!(l.cols.focus.wins.down.len(), 0);

        l.focus_window_for_resize(0);
        assert_eq!(l.cols.focus.wins.up.len(), 0);
        assert_eq!(l.cols.focus.wins.down.len(), 2);
    }

    #[test]
    #[should_panic(expected = "win_idx out of bounds")]
    fn focus_window_for_resize_panics_on_out_of_bounds() {
        let (mut l, _) = test_layout(&[3], 80, 100);
        l.focus_window_for_resize(99);
    }

    #[test_case(2, 0, 10, &[60, 39]; "two cols grow first against second")]
    #[test_case(2, 0, -10, &[40, 59]; "two cols shrink first against second")]
    #[test_case(3, 1, 10, &[33, 43, 22]; "three cols grow middle against last")]
    #[test_case(3, 1, -10, &[33, 23, 42]; "three cols shrink middle against last")]
    #[test_case(2, 0, -200, &[MIN_DIM, 100 - MIN_DIM - 1]; "clamps to MIN_DIM")]
    #[test_case(2, 1, 10, &[50, 49]; "last column has no next so noop")]
    #[test]
    fn resize_column_against_next(n_cols: usize, focus_idx: usize, delta: i16, expected: &[usize]) {
        let (mut l, _) = test_layout(&vec![1; n_cols], 80, 100);
        l.focus_column_for_resize(focus_idx);
        l.resize_active_column_against_next(delta);

        for (i, (_, c)) in l.cols.iter().enumerate() {
            assert_eq!(c.n_cols, expected[i], "column {i}");
        }
    }

    #[test_case(2, 0, 10, &[50, 29]; "two wins grow first against second")]
    #[test_case(2, 0, -10, &[30, 49]; "two wins shrink first against second")]
    #[test_case(3, 1, 10, &[26, 36, 16]; "three wins grow middle against last")]
    #[test_case(3, 1, -10, &[26, 16, 36]; "three wins shrink middle against last")]
    #[test_case(2, 0, -200, &[MIN_DIM, 80 - MIN_DIM - 1]; "clamps to MIN_DIM")]
    #[test_case(2, 1, 10, &[40, 39]; "last window has no next so noop")]
    #[test]
    fn resize_window_against_next(n_wins: usize, focus_idx: usize, delta: i16, expected: &[usize]) {
        let (mut l, _) = test_layout(&[n_wins], 80, 100);
        l.focus_window_for_resize(focus_idx);
        l.resize_active_window_against_next(delta);

        for (i, (_, w)) in l.cols.focus.wins.iter().enumerate() {
            assert_eq!(w.n_rows, expected[i], "window {i}");
        }
    }

    #[test]
    fn clamp_scroll_clamps_all_visible_views() {
        let (mut l, mut bfs) = test_layout(&[2, 3], 80, 100);

        for (_, col) in l.cols.iter_mut() {
            for (_, win) in col.wins.iter_mut() {
                let b = bfs.with_id_mut(win.view.bufid).unwrap();
                b.insert_xdot("line1\nline2\nline3".to_string());
                win.view.row_off = 100;
            }
        }

        l.clamp_scroll(&mut bfs);

        for (_, col) in l.cols.iter() {
            for (_, win) in col.wins.iter() {
                assert_eq!(
                    win.view.row_off, 2,
                    "bufid {} had row_off={}",
                    win.view.bufid, win.view.row_off
                );
            }
        }
    }

    #[test]
    fn apply_scroll_for_unfocused_window_clamps_row_off() {
        let config = Arc::new(RwLock::new(Config::default()));
        let mut b = Buffer::new_unnamed(0, "line1\nline2\nline3\nline4\nline5", config);
        let y_max = b.txt.len_lines() - 1;
        assert_eq!(y_max, 4);

        let mut win = Window::new(0, 3);
        win.view.row_off = 3;

        let focused = false;
        let up = false;
        let n_cols = 80;
        let tabstop = 4;
        let scroll_rows = 5;

        apply_scroll(&mut b, &mut win, n_cols, tabstop, focused, up, scroll_rows);

        assert_eq!(win.view.row_off, y_max);
    }
}
