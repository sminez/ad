//! Fetching and parsing input from the user
use crate::{
    editor::{Action, Actions},
    fsys::Message,
    key::Input,
};

/// An input event that can be processed by the editor event loop
#[derive(Debug)]
pub enum Event {
    /// An [Action] for the event loop to handle
    Action(Action),
    /// Multiple [Action]s to be handled in a batch
    Actions(Actions),
    /// Raw that need to be handled as an insert without applying auto-indent
    BracketedPaste(String),
    /// An [Input] from the user
    Input(Input),
    /// A Message received from the virtual filesystem interface
    Message(Message),
    /// A status message to display in the editor UI
    StatusMessage(String),
    /// A signal that our window size has changed
    WinsizeChanged { rows: usize, cols: usize },
}

impl Event {
    pub fn action(action: impl Into<Action>) -> Self {
        Self::Action(action.into())
    }

    pub fn actions<T>(actions: Vec<T>) -> Self
    where
        T: Into<Action>,
    {
        Self::Actions(Actions::Multi(
            actions.into_iter().map(Into::into).collect(),
        ))
    }
}
