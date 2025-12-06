//! The ad user interface
use crate::{
    config::Config,
    editor::{Click, EditorMode, MiniBufferState},
    input::Event,
    key::Input,
    term::CurShape,
};
use std::{
    fmt,
    sync::{Arc, RwLock, mpsc::Sender},
};

mod layout;
mod tui;

pub use layout::{Border, Layout, SCRATCH_ID};
pub use tui::{GenericTui, Tui};

/// Something that can be used as a user interface
pub trait UserInterface {
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
        layout: &mut Layout,
        n_running: usize,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    );

    /// Called when the editor mode changes and a new cursor shape is required
    fn set_cursor_shape(&mut self, cur_shape: CurShape);
}

/// Sent by the Editor to a [UserInterface] when internal state has changed in such a way that
/// a UI update _may_ be required.
///
/// In cases where the data is cheap to pass directly it is included, otherwise updates
/// to the editor state can be requested through the `provide_buf_reqs` method.
#[derive(Debug, Clone)]
pub enum StateChange {
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
    /// User level config has been modified in some way
    ConfigUpdated,
    // /// The tag for the given buffer has been updated
    // TagModified { id: usize },
    /// A new status message has been set
    StatusMessage { msg: String },
}

#[allow(clippy::large_enum_variant)]
pub(crate) enum Ui {
    Headless,
    Tui(Tui),
    Boxed(Box<dyn UserInterface>),
}

impl fmt::Debug for Ui {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Headless => f.debug_struct("Ui::Headless").finish(),
            Self::Tui(_) => f.debug_struct("Ui::Tui").finish(),
            Self::Boxed(_) => f.debug_struct("Ui::Boxed").finish(),
        }
    }
}

impl Ui {
    pub(crate) fn new(mode: EditorMode, config: Arc<RwLock<Config>>) -> Self {
        match mode {
            EditorMode::Headless => Self::Headless,
            EditorMode::Terminal => Self::Tui(Tui::new(config)),
            EditorMode::Boxed(ui) => Self::Boxed(ui),
        }
    }
}

impl UserInterface for Ui {
    fn init(&mut self, tx: Sender<Event>) -> (usize, usize) {
        match self {
            Self::Headless => (60, 80),
            Self::Tui(tui) => tui.init(tx),
            Self::Boxed(ui) => ui.init(tx),
        }
    }

    fn shutdown(&mut self) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => tui.shutdown(),
            Self::Boxed(ui) => ui.shutdown(),
        }
    }

    fn state_change(&mut self, change: StateChange) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => tui.state_change(change),
            Self::Boxed(ui) => ui.state_change(change),
        }
    }

    fn refresh(
        &mut self,
        mode_name: &str,
        layout: &mut Layout,
        n_running: usize,
        pending_keys: &[Input],
        held_click: Option<&Click>,
        mb: Option<MiniBufferState<'_>>,
    ) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => {
                tui.refresh(mode_name, layout, n_running, pending_keys, held_click, mb)
            }
            Self::Boxed(ui) => {
                ui.refresh(mode_name, layout, n_running, pending_keys, held_click, mb)
            }
        }
    }

    fn set_cursor_shape(&mut self, cur_shape: CurShape) {
        match self {
            Self::Headless => (),
            Self::Tui(tui) => tui.set_cursor_shape(cur_shape),
            Self::Boxed(ui) => ui.set_cursor_shape(cur_shape),
        }
    }
}
