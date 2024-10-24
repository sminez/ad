//! The ad user interface
use crate::{
    buffer::Buffers,
    editor::{Click, EditorMode, MiniBufferState},
    input::Event,
    key::Input,
    term::CurShape,
};
use std::sync::mpsc::Sender;

mod tui;
mod windows;

pub use tui::Tui;
pub(crate) use windows::Windows;

pub(crate) trait UserInterface {
    /// Initialise the UI and start processing events.
    ///
    /// Called before entering the main editor event loop
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize);

    /// Called when the editor event loop exits cleanly.
    fn shutdown(&mut self);

    /// Update internal state based on an Editor state change without rendering.
    fn state_change(&mut self, change: StateChange);

    /// Refresh the ui to display the current editor state.
    fn refresh(
        &mut self,
        mode_name: &str,
        buffers: &Buffers,
        windows: &Windows,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    );

    /// Called when the editor mode changes and a new cursor shape is required
    fn set_cursor_shape(&mut self, cur_shape: CurShape);
}

/// Sent by the [Editor] to a [Ui] when internal state has changed in such a way that
/// a UI update _may_ be required.
///
/// In cases where the data is cheap to pass directly it is included, otherwise updates
/// to the editor state can be requested through the `provide_buf_reqs` method.
#[derive(Debug, Clone)]
pub(crate) enum StateChange {
    // /// The active buffer has been updated
    // ActiveBuffer { id: usize },
    // /// The given buffer has been closed
    // BufferClosed { id: usize },
    // /// A new buffer has been opened.
    // /// If replace_active is true then the user wants to replace the active buffer
    // /// with the id specified, if it is false then they want to open a new UI
    // /// window to contain the buffer.
    // BufferOpen { id: usize, replace_active: bool },
    // /// The given buffer has had its contents modified in some way
    // BufferModified { id: usize },
    // /// The dot for the given buffer has been updated
    // BufferDotUpdated { id: usize },
    // /// The tag for the given buffer has been updated
    // TagModified { id: usize },
    /// A new status message has been set
    StatusMessage { msg: String },
}

#[derive(Debug)]
pub(crate) enum Ui {
    Headless,
    Tui(Tui),
}

impl From<EditorMode> for Ui {
    fn from(mode: EditorMode) -> Self {
        match mode {
            EditorMode::Headless => Self::Headless,
            EditorMode::Terminal => Self::Tui(Tui::new()),
        }
    }
}

impl UserInterface for Ui {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
        match self {
            Self::Headless => (60, 80),
            Self::Tui(tui) => tui.init(tx),
        }
    }

    fn shutdown(&mut self) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => tui.shutdown(),
        }
    }

    fn state_change(&mut self, change: StateChange) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => tui.state_change(change),
        }
    }

    fn refresh(
        &mut self,
        mode_name: &str,
        buffers: &Buffers,
        windows: &Windows,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    ) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => {
                tui.refresh(mode_name, buffers, windows, pending_keys, held_click, mb)
            }
        }
    }

    fn set_cursor_shape(&mut self, cur_shape: CurShape) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => tui.set_cursor_shape(cur_shape),
        }
    }
}
