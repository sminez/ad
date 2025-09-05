use crate::lsp::{
    LspManager,
    client::Status,
    messages::{txt_doc_id, uri},
    rpc::{Message, Notification},
};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, InitializedParams, TextDocumentContentChangeEvent, TextDocumentItem,
    VersionedTextDocumentIdentifier,
    notification::{
        self as notif, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
        DidSaveTextDocument, Exit, Initialized,
    },
};
use std::{borrow::Cow, fmt};

/// Notifications sent from us to the server.
///
/// This trait defines how to prepare the data needed to send the notification.
pub(crate) trait LspNotification:
    notif::Notification + fmt::Debug + Send + Sync + Sized + 'static
{
    /// The data needed to prepare the notification for sending.
    type Data: Send + Sync + fmt::Debug + 'static;

    /// Build the [NotificationData] needed for processing this request.
    ///
    /// This data needs to be sent to the [LspManager] event loop in order to be processed as an
    /// actual JSON RPC request to the appropriate server.
    fn data(lsp_id: usize, data: Self::Data) -> NotificationData<Self>
    where
        Self: Sized,
    {
        NotificationData {
            lsp_id,
            data: Some(data),
        }
    }

    /// Map this trait's `Data` type into the correct LSP request `Params` for sending the
    /// notification to the server.
    fn build_params(data: Self::Data) -> Self::Params;

    /// Identify the appropriate server for this notification and send it.
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

        let params = Self::build_params(data);
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
}

#[derive(Debug)]
pub(crate) struct NotificationData<T: LspNotification> {
    lsp_id: usize,
    data: Option<T::Data>,
}

pub(crate) trait PreparedLspNotification: Send + Sync + fmt::Debug + 'static {
    fn send(&mut self, man: &mut LspManager);
}

impl<T> PreparedLspNotification for NotificationData<T>
where
    T: LspNotification,
{
    fn send(&mut self, man: &mut LspManager) {
        T::send(self.lsp_id, self.data.take().unwrap(), man)
    }
}

impl LspNotification for DidOpenTextDocument {
    type Data = (String, String, String);

    fn build_params((language_id, path, text): Self::Data) -> Self::Params {
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

    fn build_params((path, text, version): Self::Data) -> Self::Params {
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

impl LspNotification for DidSaveTextDocument {
    type Data = String;

    fn build_params(path: Self::Data) -> Self::Params {
        DidSaveTextDocumentParams {
            text_document: txt_doc_id(&path),
            text: None,
        }
    }
}

impl LspNotification for DidCloseTextDocument {
    type Data = String;

    fn build_params(path: Self::Data) -> Self::Params {
        DidCloseTextDocumentParams {
            text_document: txt_doc_id(&path),
        }
    }
}

impl LspNotification for Exit {
    type Data = ();

    fn build_params(_: Self::Data) -> Self::Params {}
}

impl LspNotification for Initialized {
    type Data = ();

    fn build_params(_: Self::Data) -> Self::Params {
        InitializedParams {}
    }
}
