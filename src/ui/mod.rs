//! The ad user interface
use crate::{
    editor::{Editor, MiniBufferState},
    input::Event,
    system::System,
    term::CurShape,
};
use std::sync::mpsc::Sender;

mod tui;

pub use tui::Tui;

/// A Ui can be run alongside an Editor in order to render a user interface to ad.
pub(crate) trait Ui {
    /// Initialise the UI and start processing events.
    ///
    /// Called before entering the main editor event loop
    fn init<S>(&mut self, ed: &mut Editor<S>, tx: Sender<Event>)
    where
        S: System;

    /// Called when the editor event loop exits cleanly.
    fn shutdown(&mut self);

    /// Refresh the ui to display the current editor state.
    fn refresh<S>(&mut self, ed: &mut Editor<S>, mb: Option<MiniBufferState<'_>>)
    where
        S: System;

    /// Called when the editor mode changes and a new cursor shape is required
    fn set_cursor_shape(&mut self, cur_shape: CurShape);
}
