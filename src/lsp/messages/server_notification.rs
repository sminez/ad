//! LSP notifications sent from the server to us, the client.
//!
//! <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#notificationMessage>
use crate::{
    buffer::Buffers,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection, ViewPort},
    input::Event,
    lsp::{
        LspManager,
        capabilities::{Coords, PositionEncoding},
        rpc::Notification,
    },
};
use lsp_types::{
    DiagnosticSeverity, Location, ProgressParamsValue, PublishDiagnosticsParams, Uri,
    WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd, WorkDoneProgressReport,
    notification::{Progress, PublishDiagnostics},
};
use tracing::{debug, error, warn};

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

impl LspServerNotification for Progress {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions> {
        use ProgressParamsValue::*;
        use WorkDoneProgress::*;

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
impl LspServerNotification for PublishDiagnostics {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions> {
        let encoding = man.clients.get(&lsp_id)?.position_encoding;

        let PublishDiagnosticsParams {
            uri, diagnostics, ..
        } = params;

        debug!(uri=%uri.to_string(), n=%diagnostics.len(), "received new diagnostics");

        let new_diagnostics: Vec<Diagnostic> = diagnostics
            .into_iter()
            .map(|d| Diagnostic::new(uri.clone(), d, encoding))
            .collect();

        let mut guard = man.diagnostics.write().unwrap();
        guard.insert(uri, new_diagnostics);

        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    path: String,
    content: String,
    coords: Coords,
}

impl Diagnostic {
    pub(crate) fn new(uri: Uri, d: lsp_types::Diagnostic, encoding: PositionEncoding) -> Self {
        let loc = Location {
            uri: uri.clone(),
            range: d.range,
        };
        let (path, coords) = Coords::new(loc, encoding);
        let fname = path
            .split("/")
            .last()
            .expect("str::split always returns at least one element");
        let source = d.source.unwrap_or_else(|| "unknown".to_string());

        let severity = match d.severity.unwrap_or(DiagnosticSeverity::ERROR) {
            DiagnosticSeverity::ERROR => "ERROR",
            DiagnosticSeverity::HINT => "HINT ",
            DiagnosticSeverity::WARNING => "WARN ",
            DiagnosticSeverity::INFORMATION => "INFO ",
            _ => "???  ",
        };

        let content = format!(
            "({severity} {source}) {fname}:{} {}",
            coords.line() + 1, // UI lines start at 1
            d.message
                .lines()
                .next()
                .expect("str::lines always returns at least one line")
        );

        Diagnostic {
            path,
            content,
            coords,
        }
    }

    pub fn as_actions(&self) -> Actions {
        Actions::Multi(vec![
            Action::OpenFile {
                path: self.path.clone(),
            },
            Action::DotSetFromCoords {
                coords: self.coords,
            },
            Action::SetViewPort(ViewPort::Center),
        ])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostics(pub(crate) Vec<Diagnostic>);

impl MbSelect for Diagnostics {
    fn clone_selector(&self) -> MbSelector {
        self.clone().into_selector()
    }

    fn prompt_and_options(&self, _: &Buffers) -> (String, Vec<String>) {
        (
            "Diagnostics> ".to_owned(),
            self.0.iter().map(|d| d.content.clone()).collect(),
        )
    }

    fn selected_actions(&self, sel: MiniBufferSelection) -> Option<Actions> {
        match sel {
            MiniBufferSelection::Line { cy, .. } => self.0.get(cy).map(|d| d.as_actions()),
            _ => None,
        }
    }
}
