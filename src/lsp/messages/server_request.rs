//! LSP requests sent from the server to us, the client
//!
//! <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#requestMessage>
use crate::{
    editor::Actions,
    input::Event,
    lsp::{
        LspManager,
        rpc::{Message, Request, RequestId, Response},
    },
};
use lsp_types::{WorkDoneProgressCreateParams, request::WorkDoneProgressCreate};
use tracing::{error, warn};

/// Incoming requests from the server handle and respond to
pub(crate) trait LspServerRequest: lsp_types::request::Request {
    fn handle_params(
        lsp_id: usize,
        req_id: RequestId,
        params: Self::Params,
        man: &mut LspManager,
    ) -> (Response, Option<Actions>);
}

/// Helper struct for routing server requests to their appropriate handler
pub struct RequestHandler<'a> {
    pub lsp_id: usize,
    pub r: Option<Request>,
    pub man: &'a mut LspManager,
}

impl RequestHandler<'_> {
    pub fn handle<R>(&mut self) -> &mut Self
    where
        R: LspServerRequest,
    {
        let r = match self.r.take() {
            Some(r) if r.method == R::METHOD => r,
            Some(r) => {
                self.r = Some(r);
                return self;
            }
            None => return self,
        };

        let (res, actions) = match serde_json::from_value(r.params) {
            Ok(params) => R::handle_params(self.lsp_id, r.id, params, self.man),
            Err(e) => {
                warn!("LSP - malformed server request: {e}");
                return self;
            }
        };

        match self.man.clients.get_mut(&self.lsp_id) {
            Some(client) => {
                if let Err(e) = client.write(Message::Response(res)) {
                    error!("LSP - failed to respond request: {e}");
                }
            }
            None => {
                error!("LSP - no client available for responding to request");
                return self;
            }
        }

        if let Some(actions) = actions
            && self.man.tx_events.send(Event::Actions(actions)).is_err()
        {
            error!("LSP - sender actions channel closed: exiting");
        }

        self
    }

    pub fn log_unhandled(&mut self) {
        if let Some(r) = &self.r {
            warn!("LSP - unhandled server request: {r:?}");
        }
    }
}

impl LspServerRequest for WorkDoneProgressCreate {
    fn handle_params(
        lsp_id: usize,
        req_id: RequestId,
        WorkDoneProgressCreateParams { token }: WorkDoneProgressCreateParams,
        man: &mut LspManager,
    ) -> (Response, Option<Actions>) {
        man.progress_tokens(lsp_id).insert(token, String::new());

        (Response::null_resp(req_id), None)
    }
}
