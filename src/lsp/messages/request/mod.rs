use crate::{
    editor::Actions,
    lsp::{
        LspManager,
        client::Status,
        rpc::{ErrorCode, Message, Request, Response, ResponseError},
    },
};
use lsp_types::request as req;
use std::{borrow::Cow, fmt};
use tracing::error;

mod completion;
mod format;
mod goto;
mod hover;
mod init;
mod references;
mod rename;

pub(crate) use init::OpenDocument;

/// Outgoing requests from us to the server that we will need to handle responses for.
///
/// This trait defines how to prepare the data needed to send the request and map the response
/// that comes back from the server into [Actions] for the main editor event loop to process.
pub(crate) trait LspRequest:
    req::Request + fmt::Debug + Send + Sync + Sized + 'static
{
    /// The data needed to prepare the request for sending.
    type Data: Send + Sync + fmt::Debug + 'static;
    /// The data needed for processing the response from the server.
    type Pending: Send + Sync + fmt::Debug + 'static;

    /// Build the [RequestData] needed for processing this request.
    ///
    /// This data needs to be sent to the [LspManager] event loop in order to be processed as an
    /// actual JSON RPC request to the appropriate server.
    fn data(lsp_id: usize, data: Self::Data, pending: Self::Pending) -> RequestData<Self>
    where
        Self: Sized,
    {
        RequestData {
            lsp_id,
            data: Some(data),
            pending: Some(pending),
        }
    }

    /// Map this trait's `Data` type into the correct LSP request `Params` for sending the request
    /// to the server.
    fn build_params(data: Self::Data) -> Self::Params;

    /// Identify the appropriate server for this request and send it before stashing the
    /// [PendingRequestData] we need to process the response when it comes back.
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

        let params = match serde_json::to_value(Self::build_params(data)) {
            Ok(params) => params,
            Err(e) => {
                error!(error=%e, "unable to serialize LSP request");
                man.report_error(format!("unable to send {} LSP request: {e}", Self::METHOD));
                return;
            }
        };

        let id = client.next_id();
        let res = client.write(Message::Request(Request {
            id: id.clone(),
            method: Cow::Borrowed(Self::METHOD),
            params,
        }));

        if let Err(e) = res {
            man.report_error(format!("unable to send {} LSP request: {e}", Self::METHOD));
            return;
        }

        man.pending.insert(
            (client.id, id),
            Box::new(PendingRequestData::<Self> {
                lsp_id,
                pending: Some(p),
            }),
        );
    }

    /// Handle the response returned from the server for this request using the stashed
    /// [PendingRequestData] from the original call to [send][LspRequest::send].
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

    /// Map a successful response from the server into editor [Actions] for processing in the main
    /// event loop.
    fn handle_res(
        lsp_id: usize,
        res: Self::Result,
        pending: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions>;

    /// Map an error response from the server into editor [Actions] for processin in the main event
    /// loop. By default this method will error log the raw response before dropping it and taking
    /// no further action.
    #[allow(unused_variables)]
    fn handle_err(lsp_id: usize, err: ResponseError, man: &mut LspManager) -> Option<Actions> {
        error!("LSP - dropping malformed response: {err:?}");
        None
    }
}

/// A wrapper around the data needed to make an LSP request and process the response.
/// This is always boxed and used as a [PreparedLspRequest] in order to erase the generic type and
/// allow us to pass arbitrary requests on the channels needed for communicating between the main
/// editor event loop and the [LspManager] event loop.
#[derive(Debug)]
pub(crate) struct RequestData<T: LspRequest> {
    lsp_id: usize,
    data: Option<T::Data>,
    pending: Option<T::Pending>,
}

/// Wrapper trait for [RequestData] to allow us to erase the generic type.
pub(crate) trait PreparedLspRequest: Send + Sync + fmt::Debug + 'static {
    fn send(&mut self, man: &mut LspManager);
}

impl<T> PreparedLspRequest for RequestData<T>
where
    T: LspRequest,
{
    fn send(&mut self, man: &mut LspManager) {
        T::send(
            self.lsp_id,
            self.data.take().unwrap(),
            self.pending.take().unwrap(),
            man,
        )
    }
}

/// A wrapper around the data needed to process the response for an LSP request.
/// This is always boxed and used as a [PendingLspRequest] in order to erase the generic type and
/// allow us to store the data in the [LspManager] while we wait for the response.
#[derive(Debug)]
pub(crate) struct PendingRequestData<T: LspRequest> {
    lsp_id: usize,
    pending: Option<T::Pending>,
}

/// Wrapper trait for [PendingRequestData] to allow us to erase the generic type.
pub(crate) trait PendingLspRequest: Send + Sync + fmt::Debug + 'static {
    fn handle(&mut self, res: Response, man: &mut LspManager) -> Option<Actions>;
}

impl<T> PendingLspRequest for PendingRequestData<T>
where
    T: LspRequest,
{
    fn handle(&mut self, res: Response, man: &mut LspManager) -> Option<Actions> {
        T::handle(self.lsp_id, res, self.pending.take().unwrap(), man)
    }
}
