//! Traits and handlers for processing LSP messages
use lsp_types::{TextDocumentIdentifier, TextDocumentPositionParams, Uri};
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
        position: lsp_types::Position { line, character },
    }
}

#[inline]
fn txt_doc_id(path: &str) -> TextDocumentIdentifier {
    lsp_types::TextDocumentIdentifier { uri: uri(path) }
}

#[inline]
fn uri(path: &str) -> Uri {
    Uri::from_str(&format!("file://{path}")).unwrap()
}
