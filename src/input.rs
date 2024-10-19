//! Fetching and parsing input from the user
use crate::{editor::Action, fsys::Message, key::Input};

/// An input event that can be processed by the editor event loop
#[derive(Debug)]
pub enum Event {
    /// A [Message] received from the virtual filesystem interface
    Message(Message),
    /// An [Input] from the user
    Input(Input),
    /// An [Action] for the event loop to handle
    Action(Action),
    /// A signal that our window size has changed
    WinsizeChanged { rows: usize, cols: usize },
}
