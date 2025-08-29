//! Traits and handlers for processing LSP messages
use crate::{
    editor::Action,
    lsp::{Coords, capabilities::PositionEncoding},
};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams, TextEdit, Uri};
use std::str::FromStr;

mod notification;
mod request;
mod server_notification;
mod server_request;

pub(super) use notification::LspNotification;
pub(super) use request::LspRequest;
pub(super) use server_notification::NotificationHandler;
pub(super) use server_request::RequestHandler;

#[inline]
fn txtdoc_pos(file: &str, line: u32, character: u32) -> TextDocumentPositionParams {
    TextDocumentPositionParams {
        text_document: txt_doc_id(file),
        position: Position { line, character },
    }
}

#[inline]
pub(crate) fn txt_doc_id(path: &str) -> TextDocumentIdentifier {
    TextDocumentIdentifier { uri: uri(path) }
}

#[inline]
fn uri(path: &str) -> Uri {
    Uri::from_str(&format!("file://{path}")).unwrap()
}

#[derive(Debug)]
pub(crate) struct EditAction {
    pub(crate) coords: Coords,
    pub(crate) s: String,
    pub(crate) use_xdot: bool,
}

impl EditAction {
    pub(crate) fn into_actions(
        EditAction {
            coords,
            s,
            use_xdot,
        }: EditAction,
    ) -> [Action; 2] {
        if use_xdot {
            [
                Action::XDotSetFromCoords { coords },
                Action::XInsertString { s },
            ]
        } else {
            [
                Action::DotSetFromCoords { coords },
                Action::InsertString { s },
            ]
        }
    }

    pub(crate) fn from_text_edit(edit: TextEdit, enc: PositionEncoding) -> Self {
        Self {
            coords: Coords::new_from_range(edit.range, enc),
            s: edit.new_text,
            use_xdot: true,
        }
    }

    pub(crate) fn using_dot(mut self) -> Self {
        self.use_xdot = false;
        self
    }
}

/// From the docs on TextEdit:
///   If n TextEdits are applied to a text document all text edits describe changes to the initial
///   document version. Execution wise text edits should applied from the bottom to the top of the
///   text document. Overlapping text edits are not supported.
///
/// Also see <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textEditArray>
pub(crate) fn edit_actions_as_editor_actions(mut edit_actions: Vec<EditAction>) -> Vec<Action> {
    edit_actions.sort_by_key(|a| a.coords);
    edit_actions.reverse();

    edit_actions
        .into_iter()
        .flat_map(EditAction::into_actions)
        .collect()
}
