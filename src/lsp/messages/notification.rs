use crate::lsp::{
    LspManager,
    client::Status,
    messages::{txt_doc_id, uri},
    rpc::{Message, Notification},
};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    InitializedParams, TextDocumentContentChangeEvent, TextDocumentItem,
    VersionedTextDocumentIdentifier,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Exit, Initialized,
    },
};
use std::borrow::Cow;

/// Notifications sent from us to the server
pub(crate) trait LspNotification: lsp_types::notification::Notification {
    type Data;

    fn send(lsp_id: usize, data: Self::Data, man: &mut LspManager) {
        let client = match man.clients.get_mut(&lsp_id) {
            Some(client) => match client.status {
                Status::Running => client,
                Status::Initializing => {
                    man.send_status("LSP server still initializing");
                    return;
                }
            },
            None => {
                man.send_status("no attached LSP client for buffer");
                return;
            }
        };

        let params = Self::prepare(data);
        let res = client.write(Message::Notification(Notification {
            method: Cow::Borrowed(Self::METHOD),
            params: serde_json::to_value(params).unwrap(),
        }));

        if let Err(e) = res {
            man.report_error(format!(
                "unable to send {} LSP notification: {e}",
                Self::METHOD
            ));
        }
    }

    fn prepare(data: Self::Data) -> Self::Params;
}

impl LspNotification for DidOpenTextDocument {
    type Data = (String, String, String);

    fn prepare((language_id, path, text): Self::Data) -> Self::Params {
        DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri(&path),
                language_id,
                version: 1,
                text,
            },
        }
    }
}

impl LspNotification for DidChangeTextDocument {
    type Data = (String, String, i32);

    fn prepare((path, text, version): Self::Data) -> Self::Params {
        DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri: uri(&path),
                version,
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text,
            }],
        }
    }
}

impl LspNotification for DidCloseTextDocument {
    type Data = String;

    fn prepare(path: Self::Data) -> Self::Params {
        DidCloseTextDocumentParams {
            text_document: txt_doc_id(&path),
        }
    }
}

impl LspNotification for Exit {
    type Data = ();

    fn prepare(_: Self::Data) -> Self::Params {}
}

impl LspNotification for Initialized {
    type Data = ();

    fn prepare(_: Self::Data) -> Self::Params {
        InitializedParams {}
    }
}
