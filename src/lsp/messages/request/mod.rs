use crate::{
    editor::Actions,
    lsp::{
        LspManager, Pending,
        client::Status,
        rpc::{ErrorCode, Message, Request, Response, ResponseError},
    },
};
use std::borrow::Cow;
use tracing::error;

mod completion;
mod goto;
mod hover;
mod init;
mod references;

/// Outgoing requests from us to the server that we will need to handle responses for
pub(crate) trait LspRequest: lsp_types::request::Request {
    type Pending;
    type Data;

    fn prepare(data: Self::Data) -> Self::Params;
    fn pending(p: Self::Pending) -> Pending;

    fn send(lsp_id: usize, data: Self::Data, p: Self::Pending, man: &mut LspManager) {
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
        let id = client.next_id();
        let res = client.write(Message::Request(Request {
            id: id.clone(),
            method: Cow::Borrowed(Self::METHOD),
            params: serde_json::to_value(params).unwrap(),
        }));

        if let Err(e) = res {
            man.report_error(format!("unable to send {} LSP request: {e}", Self::METHOD));
            return;
        }

        man.pending.insert((client.id, id), Self::pending(p));
    }

    fn handle(
        lsp_id: usize,
        res: Response,
        pending: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        match res {
            Response::Result { result, .. } => match serde_json::from_value(result) {
                Ok(res) => Self::handle_res(lsp_id, res, pending, man),
                Err(error) => Self::handle_err(
                    lsp_id,
                    ResponseError {
                        code: ErrorCode::Unknown,
                        message: error.to_string(),
                        data: None,
                    },
                    man,
                ),
            },

            Response::Error { error, .. } => Self::handle_err(lsp_id, error, man),
        }
    }

    fn handle_res(
        lsp_id: usize,
        res: Self::Result,
        pending: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions>;

    #[allow(unused_variables)]
    fn handle_err(lsp_id: usize, err: ResponseError, man: &mut LspManager) -> Option<Actions> {
        error!("LSP - dropping malformed response: {err:?}");
        None
    }
}
