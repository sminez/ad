//! Wrapper logic around the behaviour of ui::Layout to ensure that buffer and UI state are kept in
//! sync during operations that involve both.
use crate::{
    Editor,
    buffer::{BufferId, Buffers, SCRATCH_ID},
    die,
    dot::{Cur, Dot},
    editor::UAction,
    fsys::LogEvent,
    system::System,
    ui::Layout,
};
use std::{io, path::Path};
use tracing::trace;

/// Similar to in ../buffer/internal.rs:/assert_line_endings/ this is used to hunt for exactly
/// _where_ state becomes invalid between the actual buffer state in Buffers and the layout
/// state referencing what should always be known IDs in Layout. This macro should be called
/// at all points where the buffers & views are created / destroyed, as well as any time that
/// views change their IDs. It should always be wrapped with #[cfg(test)] so that it doesn't
/// affect the performance of the editor when it is actually in use.
#[cfg(test)]
macro_rules! assert_invariants {
    ($self:expr) => {{ assert_invariants!($self.layout, $self.buffers) }};

    ($layout:expr, $buffers:expr) => {{
        for (i, (_, col)) in $layout.cols().iter().enumerate() {
            for (j, (_, win)) in col.wins.iter().enumerate() {
                assert!(
                    $buffers.contains_bufid(win.view.bufid),
                    "col {i} window {j} held unknown bufid ({})",
                    win.view.bufid
                );
                let b = $buffers.with_id(win.view.bufid).unwrap();
                assert!(
                    win.view.row_off < b.len_lines(),
                    "col {i} window {j} has an OOB row_off ({} vs {})",
                    win.view.row_off,
                    b.len_lines()
                );
            }
        }
        for view in $layout.views().iter() {
            assert!(
                $buffers.contains_bufid(view.bufid),
                "stored view held unknown bufid ({})",
                view.bufid
            );
            let b = $buffers.with_id(view.bufid).unwrap();
            assert!(
                view.row_off < b.len_lines(),
                "stored view for bufid {} has an OOB row_off ({} vs {})",
                view.bufid,
                view.row_off,
                b.len_lines()
            );
        }
    }};
}

impl<S> Editor<S>
where
    S: System,
{
    pub(super) fn handle_ui_action(&mut self, uaction: UAction) {
        match uaction {
            // Attempting to close the active column or window when the scratch is focused is
            // treated as closing the scratch buffer.
            UAction::DeleteColumn { .. } | UAction::DeleteWindow { .. }
                if self.buffers.scratch_is_focused() =>
            {
                self.toggle_scratch();
            }

            _ => {
                if let Some(ao) = self.layout.handle_ui_action(uaction, &mut self.buffers) {
                    self.handle_action_outcome(ao);
                }
            }
        }

        #[cfg(test)]
        assert_invariants!(self);
    }

    // XXX: buffer open / close

    pub fn open_or_focus<P: AsRef<Path>>(
        &mut self,
        path: P,
        mut new_window: bool,
    ) -> io::Result<Option<BufferId>> {
        self.buffers.set_scratch_focus(false);
        if self.buffers.is_empty_squirrel() {
            // in the case where we only have an empty squirrel buffer present, we always replace
            // the current buffer with the one that is newly opened.
            new_window = false;
        }

        let retain_empty_unnamed = new_window || self.layout.n_open_windows() > 1;
        let opt = self.buffers.open_or_focus(path, retain_empty_unnamed)?;
        self.layout
            .open_or_focus_id(self.buffers.active_id(), new_window);

        // Replacing the default empty unnamed buffer when opening a file from within the editor
        // can result in a stale view being held for the id=0 unnamed buffer.
        self.layout.clear_stale_views(&self.buffers);

        #[cfg(test)]
        assert_invariants!(self);

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
        self.buffers.set_scratch_focus(false);
        let id = self.buffers.open_virtual(name.into(), content.into());
        self.layout.open_or_focus_id(id, new_window);

        _ = self.tx_fsys.send(LogEvent::Open(id));
        _ = self.tx_fsys.send(LogEvent::Focus(id));

        #[cfg(test)]
        assert_invariants!(self);
    }

    pub(crate) fn close_buffer(&mut self, id: BufferId) -> bool {
        self.buffers.set_scratch_focus(false);

        if id == SCRATCH_ID {
            self.layout.set_scratch_visible(false);
            return false;
        }

        if self.buffers.len() == 1 {
            // We could have been asked to close a non-existent buffer.
            // If this was the last buffer then Editor::delete_buffer will exit
            return self.buffers.active_id() == id;
        }

        debug_assert!(self.buffers.len() > 1, "we have at least two buffers");
        self.buffers.close_buffer(id);
        let focused_id = self.buffers.active_id();
        let was_last_buffer = self.layout.close_buffer(id, focused_id);

        #[cfg(test)]
        assert_invariants!(self);

        was_last_buffer
    }

    // XXX: focus change

    pub(crate) fn focus_id(&mut self, id: BufferId, force_active: bool) {
        if id == SCRATCH_ID {
            self.buffers.set_scratch_focus(true);
            self.layout.set_scratch_visible(true);
            return;
        }

        self.buffers.set_scratch_focus(false);

        if let Some(id) = self.buffers.focus_id(id) {
            self.layout.focus_id(id, force_active);
        }

        #[cfg(test)]
        assert_invariants!(self);
    }

    pub(crate) fn focus_next_buffer(&mut self) {
        self.buffers.set_scratch_focus(false);
        self.buffers.next();
        let id = self.buffers.active_id();
        self.layout.show_buffer_in_active_window(id);

        #[cfg(test)]
        assert_invariants!(self);
    }

    pub(crate) fn focus_previous_buffer(&mut self) {
        self.buffers.set_scratch_focus(false);
        self.buffers.previous();
        let id = self.buffers.active_id();
        self.layout.show_buffer_in_active_window(id);

        #[cfg(test)]
        assert_invariants!(self);
    }

    /// Focus the column at the given index without recording a jump.
    ///
    /// # Panics
    /// Panics if col_idx is out of bounds.
    pub fn focus_column_for_resize(&mut self, col_idx: usize) {
        self.buffers.set_scratch_focus(false);
        let id = self.layout.focus_column_for_resize(col_idx);
        self.buffers.focus_id_silent(id);

        #[cfg(test)]
        assert_invariants!(self);
    }

    /// Focus the window at the given index within the currently focused column,
    /// without recording a jump.
    ///
    /// # Panics
    /// Panics if win_idx is out of bounds.
    pub fn focus_window_for_resize(&mut self, win_idx: usize) {
        self.buffers.set_scratch_focus(false);
        let id = self.layout.focus_window_for_resize(win_idx);
        self.buffers.focus_id_silent(id);

        #[cfg(test)]
        assert_invariants!(self);
    }

    /// Focus the column and window at the given indices without recording a jump.
    ///
    /// # Panics
    /// Panics if either index is out of bounds.
    pub fn focus_column_and_window_for_resize(&mut self, col_idx: usize, win_idx: usize) {
        self.buffers.set_scratch_focus(false);
        self.layout.focus_column_for_resize(col_idx);
        let id = self.layout.focus_window_for_resize(win_idx);
        self.buffers.focus_id_silent(id);

        #[cfg(test)]
        assert_invariants!(self);
    }

    // XXX: scratch buffer methods

    pub(crate) fn toggle_scratch(&mut self) {
        if self.layout.scratch.is_visible() {
            self.buffers.scratch_mut().clear_transient();
        }

        self.layout.toggle_scratch_visibility();
        self.buffers
            .set_scratch_focus(self.layout.scratch.is_visible());
    }

    pub(crate) fn clear_scratch(&mut self) {
        self.buffers.scratch_mut().clear();
    }

    /// Open a new transient scratch buffer.
    ///
    /// This will replace the layout position of the main scratch buffer without altering it's
    /// contents. When the transient buffer is closed, the main scratch buffer will be put back.
    pub(crate) fn open_transient_scratch(
        &mut self,
        name: impl Into<String>,
        content: impl Into<String>,
    ) {
        self.buffers.set_transient_scratch(name, content);
        self.layout.set_scratch_visible(true);
    }

    // XXX: utility methods requiring state from buffers and layout

    pub(crate) fn active_window_rows(&self) -> usize {
        self.layout
            .active_window_rows(self.buffers.scratch_is_focused())
    }

    pub(crate) fn write_output_for_buffer(&mut self, id: usize, s: String) {
        let id = self.buffers.write_output_for_buffer(id, s, &self.cwd);

        if !self.layout.buffer_is_visible(id) {
            self.layout.show_buffer_in_new_window(id);
            self.buffers.focus_id(id);
        }

        #[cfg(test)]
        assert_invariants!(self);
    }

    pub(crate) fn clamp_scroll(&mut self) {
        self.layout.clamp_scroll(&mut self.buffers);

        #[cfg(test)]
        assert_invariants!(self);
    }

    /// Update the stored window size, accounting for the status and message bars
    /// This will panic if the available screen rows are 0 or 1
    pub(crate) fn update_window_size(&mut self, screen_rows: usize, screen_cols: usize) {
        trace!("window size updated: rows={screen_rows} cols={screen_cols}");
        self.layout.update_screen_size(screen_rows - 2, screen_cols);
        self.layout.clamp_scroll(&mut self.buffers);

        #[cfg(test)]
        assert_invariants!(self);
    }

    pub(crate) fn update_visible_ts_state(&mut self) {
        let it = self
            .layout
            .iter_windows()
            .map(|w| (w.view.bufid, w.view.row_off, w.n_rows));

        for (bufid, from, n_rows) in it {
            let b = self.buffers.with_id_mut(bufid).unwrap_or_else(|| {
                die!("invalid buffer ID {bufid}");
            });

            b.update_ts_state(from, n_rows);
        }

        #[cfg(test)]
        assert_invariants!(self);
    }

    // XXX: Screen position iteractions with layout state

    pub(crate) fn focus_buffer_for_screen_coords(&mut self, x: usize, y: usize) -> BufferId {
        let bufid = self
            .layout
            .focus_buffer_for_screen_coords(x, y)
            .unwrap_or_else(|| self.buffers.active_id());

        self.buffers.set_scratch_focus(bufid == SCRATCH_ID);
        if bufid != SCRATCH_ID {
            self.buffers.focus_id(bufid);
        }

        bufid
    }

    /// Focus the buffer (or tag) containing the given screen coordinates and return the current
    /// cursor position for updating held mouse state.
    pub(crate) fn focus_cur_from_screen_coords(&mut self, x: usize, y: usize) -> (BufferId, Cur) {
        let bufid = self.focus_buffer_for_screen_coords(x, y);
        let cur = self.layout.cur_from_screen_coords(x, y, &mut self.buffers);

        #[cfg(test)]
        assert_invariants!(self);

        (bufid, cur)
    }

    /// Set the active buffer and dot based on a mouse click.
    ///
    /// Returns true if the click was in the currently active buffer and false if this click has
    /// changed the active buffer.
    pub(crate) fn set_dot_from_screen_coords(&mut self, x: usize, y: usize) -> bool {
        self.layout.changed_since_last_render = true;
        let current_bufid = self.buffers.active_id();
        let bufid = self.focus_buffer_for_screen_coords(x, y);
        let c = self.layout.cur_from_screen_coords(x, y, &mut self.buffers);
        self.buffers.active_mut().dot = Dot::Cur { c };

        #[cfg(test)]
        assert_invariants!(self);

        bufid == current_bufid
    }

    /// Scroll the `View` under the given cursor coordinates up or down by `scroll_rows`
    pub(crate) fn scroll_view(&mut self, x: usize, y: usize, up: bool, scroll_rows: usize) {
        self.layout
            .scroll_view(x, y, up, scroll_rows, &mut self.buffers);
    }
}

/// Determine the cursor position for a given set of coordinates and report whether or not
/// these coordinates are inside of the currently active buffer (or tag).
///
/// This is deliberately _not_ a method on Editor as we need to call it while already holding a
/// mutable reference to other parts of the editor state.
pub(crate) fn try_active_cur_from_screen_coords(
    layout: &mut Layout,
    buffers: &mut Buffers,
    x: usize,
    y: usize,
) -> Option<Cur> {
    let active = buffers.active_id();
    let id = layout.buffer_for_screen_coords(x, y).unwrap_or(active);

    let cur = if id == active {
        Some(layout.cur_from_screen_coords(x, y, buffers))
    } else {
        None
    };

    #[cfg(test)]
    assert_invariants!(layout, buffers);

    cur
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Config, EditorMode, LogBuffer, PlumbingRules};

    #[test]
    fn writing_to_a_non_visible_output_buffer_creates_a_window() {
        let mut ed = Editor::new(
            Config::default(),
            PlumbingRules::default(),
            EditorMode::Headless,
            LogBuffer::default(),
        );

        assert_eq!(ed.layout.n_open_windows(), 1);

        ed.write_output_for_buffer(0, "some output".into());
        assert_eq!(ed.layout.n_open_windows(), 2);
    }
}
