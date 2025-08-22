//! LSP notifications sent from the server to us, the client.
//!
//! https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage
use crate::{
    editor::{Action, Actions},
    input::Event,
    lsp::{Diagnostic, LspManager, rpc::Notification},
};
use tracing::{error, warn};

/// Notifications sent from the server to us that we need to handle
pub(crate) trait LspServerNotification: lsp_types::notification::Notification {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions>;
}

/// Helper struct for routing server notifications to their appropriate handler
pub struct NotificationHandler<'a> {
    pub lsp_id: usize,
    pub n: Option<Notification>,
    pub man: &'a mut LspManager,
}

impl NotificationHandler<'_> {
    pub fn handle<N>(&mut self) -> &mut Self
    where
        N: LspServerNotification,
    {
        let n = match self.n.take() {
            Some(n) if n.method == N::METHOD => n,
            Some(n) => {
                self.n = Some(n);
                return self;
            }
            None => return self,
        };

        let actions = match serde_json::from_value(n.params) {
            Ok(params) => N::handle_params(self.lsp_id, params, self.man),
            Err(e) => {
                warn!("LSP - malformed notification: {e}");
                None
            }
        };

        if let Some(actions) = actions
            && self.man.tx_events.send(Event::Actions(actions)).is_err()
        {
            error!("LSP - sender actions channel closed: exiting");
        }

        self
    }

    pub fn log_unhandled(&mut self) {
        if let Some(n) = &self.n {
            warn!("LSP - unhandled notification: {n:?}");
        }
    }
}

impl LspServerNotification for lsp_types::notification::Progress {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions> {
        use ProgressParamsValue::*;
        use WorkDoneProgress::*;
        use lsp_types::{
            ProgressParamsValue, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd,
            WorkDoneProgressReport,
        };

        let actions = |title: &str, message: Option<String>, perc: Option<u32>| {
            let message = message.unwrap_or_default();
            let message = if let Some(perc) = perc {
                format!("{title}: {message} ({perc}/100)")
            } else {
                format!("{title}: {message}")
            };

            Some(Actions::Single(Action::SetStatusMessage { message }))
        };

        match params.value {
            WorkDone(Begin(WorkDoneProgressBegin {
                title,
                message,
                percentage,
                ..
            })) => {
                let actions = actions(&title, message, percentage);
                man.progress_tokens(lsp_id).insert(params.token, title);

                actions
            }

            WorkDone(Report(WorkDoneProgressReport {
                message,
                percentage,
                ..
            })) => {
                let title: &str = man
                    .progress_tokens(lsp_id)
                    .get(&params.token)
                    .map_or("", |s| s);
                actions(title, message, percentage)
            }

            WorkDone(End(WorkDoneProgressEnd { .. })) => {
                man.progress_tokens(lsp_id).remove(&params.token);

                // Clear the status message when progress is done
                Some(Actions::Single(Action::SetStatusMessage {
                    message: "".to_owned(),
                }))
            }
        }
    }
}

/// Currently throwing away a LOT of the information contained in the payload from the server
/// Servers are in control over the state of diagnostics so any push of diagnostic state for
/// a given file overwrites our current state
impl LspServerNotification for lsp_types::notification::PublishDiagnostics {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions> {
        use lsp_types::PublishDiagnosticsParams;

        let encoding = match man.clients.get(&lsp_id) {
            Some(c) => c.position_encoding,
            None => return None,
        };

        let PublishDiagnosticsParams {
            uri, diagnostics, ..
        } = params;

        let new_diagnostics: Vec<Diagnostic> = diagnostics
            .into_iter()
            .map(|d| Diagnostic::new(uri.clone(), d, encoding))
            .collect();

        let mut guard = man.diagnostics.write().unwrap();
        guard.insert(uri, new_diagnostics);

        None
    }
}
