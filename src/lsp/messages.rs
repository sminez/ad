//! Traits and handlers for processing LSP messages
use crate::{
    buffer::Buffers,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection, ViewPort},
    input::Event,
    lsp::{
        capabilities::{Capabilities, Coords},
        client::Status,
        rpc::{ErrorCode, Message, Notification, Request, RequestId, Response, ResponseError},
        Diagnostic, LspManager, Pending, PendingParams, PendingRequest, Pos, PositionEncoding,
        LSP_FILE,
    },
};
use lsp_types::{
    DynamicRegistrationClientCapabilities, GotoDefinitionParams, GotoDefinitionResponse, Location,
    TextDocumentIdentifier, TextDocumentPositionParams, Uri, WorkDoneProgressCreateParams,
};
use std::{borrow::Cow, process, str::FromStr};
use tracing::{error, warn};

/// Outgoing requests from us to the server that we will need to handle responses for
pub(crate) trait LspRequest: lsp_types::request::Request {
    type Pending;
    type Data;

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

    fn prepare(data: Self::Data) -> Self::Params;
    fn pending(p: Self::Pending) -> Pending;

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

fn handle_goto_response(
    lsp_id: usize,
    params: Option<GotoDefinitionResponse>,
    man: &mut LspManager,
) -> Option<Actions> {
    let enc = man.clients.get(&lsp_id)?.position_encoding;

    let (path, coords) = match params? {
        GotoDefinitionResponse::Scalar(loc) => Coords::new(loc, enc),
        GotoDefinitionResponse::Array(mut locs) => {
            if locs.is_empty() {
                return None;
            }
            Coords::new(locs.remove(0), enc)
        }
        GotoDefinitionResponse::Link(links) => {
            error!("unhandled goto definition links response: {links:?}");
            return None;
        }
    };

    Some(Actions::Multi(vec![
        Action::OpenFile { path },
        Action::DotSetFromCoords { coords },
        Action::SetViewPort(ViewPort::Center),
    ]))
}

fn pos_to_params(
    Pos {
        file,
        line,
        character,
    }: Pos,
) -> GotoDefinitionParams {
    GotoDefinitionParams {
        text_document_position_params: txtdoc_pos(&file, line, character),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    }
}

impl LspRequest for lsp_types::request::GotoDeclaration {
    type Pending = ();
    type Data = Pos;

    fn prepare(data: Self::Data) -> Self::Params {
        pos_to_params(data)
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::GotoDeclaration
    }

    fn handle_res(
        lsp_id: usize,
        params: Option<GotoDefinitionResponse>,
        _: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        handle_goto_response(lsp_id, params, man)
    }
}

impl LspRequest for lsp_types::request::GotoDefinition {
    type Pending = ();
    type Data = Pos;

    fn prepare(data: Self::Data) -> Self::Params {
        pos_to_params(data)
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::GotoDefinition
    }

    fn handle_res(
        lsp_id: usize,
        params: Option<GotoDefinitionResponse>,
        _: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        handle_goto_response(lsp_id, params, man)
    }
}

impl LspRequest for lsp_types::request::GotoTypeDefinition {
    type Pending = ();
    type Data = Pos;

    fn prepare(data: Self::Data) -> Self::Params {
        pos_to_params(data)
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::GotoTypeDefinition
    }

    fn handle_res(
        lsp_id: usize,
        params: Option<GotoDefinitionResponse>,
        _: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        handle_goto_response(lsp_id, params, man)
    }
}

impl LspRequest for lsp_types::request::HoverRequest {
    type Pending = ();
    type Data = Pos;

    fn prepare(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        lsp_types::HoverParams {
            text_document_position_params: txtdoc_pos(&file, line, character),
            work_done_progress_params: Default::default(),
        }
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::Hover
    }

    fn handle_res(
        _: usize,
        res: Self::Result,
        _: Self::Pending,
        _: &mut LspManager,
    ) -> Option<Actions> {
        use lsp_types::{HoverContents, MarkedString};

        let ms_to_string = |ms: MarkedString| match ms {
            MarkedString::String(s) => s,
            MarkedString::LanguageString(ls) => ls.value,
        };

        let txt = match res?.contents {
            HoverContents::Scalar(ms) => ms_to_string(ms),
            HoverContents::Markup(mc) => mc.value,
            HoverContents::Array(mss) => {
                let strs: Vec<_> = mss.into_iter().map(ms_to_string).collect();
                strs.join("\n")
            }
        };

        Some(Actions::Single(Action::OpenVirtualFile {
            name: LSP_FILE.to_string(),
            txt,
        }))
    }
}

impl LspRequest for lsp_types::request::Initialize {
    type Pending = (String, Vec<PendingParams>);
    type Data = (String, Option<serde_json::Value>);

    // Need a custom send impl for initialize as the default one checks that the client is running
    fn send(lsp_id: usize, data: Self::Data, p: Self::Pending, man: &mut LspManager) {
        use lsp_types::request::Request as _;

        let client = match man.clients.get_mut(&lsp_id) {
            Some(client) => client,
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

    fn prepare((root, initialization_options): Self::Data) -> Self::Params {
        use lsp_types::{
            ClientCapabilities, GeneralClientCapabilities, HoverClientCapabilities,
            InitializeParams, MarkupKind, NumberOrString, PositionEncodingKind,
            TextDocumentClientCapabilities, Uri, WindowClientCapabilities, WorkDoneProgressParams,
            WorkspaceClientCapabilities, WorkspaceFolder,
        };

        let basename = root.split("/").last().unwrap_or_default();

        #[allow(deprecated)] // root_uri, root_path
        InitializeParams {
            process_id: Some(process::id()),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: Some(NumberOrString::String("abc123".to_string())),
            },
            root_path: Some(root.to_string()),
            root_uri: Some(Uri::from_str(&format!("file://{root}")).unwrap()),
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Uri::from_str(&format!("file://{root}")).unwrap(),
                name: basename.to_string(),
            }]),
            initialization_options,
            capabilities: ClientCapabilities {
                workspace: Some(WorkspaceClientCapabilities {
                    // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.WorkspaceClientCapabilities.html
                    workspace_folders: Some(true),
                    configuration: Some(true),
                    did_change_configuration: Some(DynamicRegistrationClientCapabilities {
                        dynamic_registration: Some(false),
                    }),
                    ..Default::default()
                }),
                text_document: Some(TextDocumentClientCapabilities {
                    hover: Some(HoverClientCapabilities {
                        dynamic_registration: Some(true),
                        content_format: Some(vec![MarkupKind::PlainText]),
                    }),
                    // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.TextDocumentClientCapabilities.html
                    ..Default::default()
                }),
                // This is what we need for getting rust-analyzer (and presumably other LSPs?) to
                // report things like their current state and progress during init
                // -> results in us getting "window/workDoneProgress/create" requests
                window: Some(WindowClientCapabilities {
                    work_done_progress: Some(true),
                    ..Default::default()
                }),
                general: Some(GeneralClientCapabilities {
                    // Explicitly not supporting utf-16 for now and seeing how well that works...!
                    position_encodings: Some(vec![
                        PositionEncodingKind::UTF32,
                        PositionEncodingKind::UTF8,
                        PositionEncodingKind::UTF16,
                    ]),
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    fn pending((lang, open_bufs): Self::Pending) -> Pending {
        Pending::Initialize(lang, open_bufs)
    }

    fn handle_res(
        lsp_id: usize,
        res: Self::Result,
        (lang, open_bufs): Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        use lsp_types::notification::Initialized;

        match Capabilities::try_new(res) {
            Some(c) => {
                let client = match man.clients.get_mut(&lsp_id) {
                    Some(client) => client,
                    None => {
                        man.send_status(format!("no attached LSP client for {lang}"));
                        return None;
                    }
                };

                client.status = Status::Running;
                client.position_encoding = c.position_encoding;
                man.capabilities.write().unwrap().insert(lang, (lsp_id, c));

                Initialized::send(lsp_id, (), man);

                for pending in open_bufs {
                    man.handle_pending(PendingRequest { lsp_id, pending });
                }
            }

            // Unknown position encoding that we can't support
            None => man.stop_client(lsp_id),
        };

        None
    }
}

impl LspRequest for lsp_types::request::References {
    type Pending = ();
    type Data = Pos;

    fn prepare(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        use lsp_types::{ReferenceContext, ReferenceParams};

        ReferenceParams {
            text_document_position: txtdoc_pos(&file, line, character),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: ReferenceContext {
                include_declaration: false,
            },
        }
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::FindReferences
    }

    fn handle_res(
        lsp_id: usize,
        locs: Option<Vec<Location>>,
        _: (),
        man: &mut LspManager,
    ) -> Option<Actions> {
        let enc = match man.clients.get_mut(&lsp_id) {
            Some(client) => client.position_encoding,
            None => {
                man.send_status("no attached LSP client".to_string());
                return None;
            }
        };

        let refs: Vec<_> = locs?
            .into_iter()
            .map(|loc| Reference::from_loc(loc, enc))
            .collect();
        let mut actions: Vec<_> = refs
            .iter()
            .map(|r| Action::EnsureFileIsOpen {
                path: r.path.clone(),
            })
            .collect();
        actions.push(Action::MbSelect(References(refs).into_selector()));

        Some(Actions::Multi(actions))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Reference {
    path: String,
    coords: Coords,
    prefix: String,
}

impl Reference {
    fn from_loc(loc: Location, enc: PositionEncoding) -> Self {
        let (path, coords) = Coords::new(loc, enc);
        let prefix = format!("{}:{}", path, coords.line() + 1); // LSP ranges are 0-based

        Self {
            path,
            coords,
            prefix,
        }
    }

    fn mb_line(&self, buffers: &Buffers, width: usize) -> String {
        let try_buffer_line = |path: &str, y: usize| -> Option<String> {
            let s = buffers.with_path(path)?.line(y)?.to_string();
            Some(s.trim().to_string())
        };

        match try_buffer_line(&self.path, self.coords.line() as usize) {
            Some(line) => format!("{:<width$} | {line}", self.prefix, width = width),
            None => self.prefix.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct References(Vec<Reference>);

impl MbSelect for References {
    fn clone_selector(&self) -> MbSelector {
        self.clone().into_selector()
    }

    fn prompt_and_options(&self, buffers: &Buffers) -> (String, Vec<String>) {
        let width = self
            .0
            .iter()
            .map(|r| r.prefix.chars().count())
            .max()
            .unwrap_or_default();

        (
            "References> ".to_owned(),
            self.0.iter().map(|r| r.mb_line(buffers, width)).collect(),
        )
    }

    fn selected_actions(&self, sel: MiniBufferSelection) -> Option<Actions> {
        match sel {
            MiniBufferSelection::Line { cy, .. } => self.0.get(cy).map(|r| {
                Actions::Multi(vec![
                    Action::OpenFile {
                        path: r.path.clone(),
                    },
                    Action::DotSetFromCoords { coords: r.coords },
                    Action::SetViewPort(ViewPort::Center),
                ])
            }),

            _ => None,
        }
    }
}

impl LspRequest for lsp_types::request::Shutdown {
    type Pending = ();
    type Data = ();

    fn prepare(_: Self::Data) -> Self::Params {}
    fn pending(_: Self::Pending) -> Pending {
        Pending::GotoDefinition // dummy
    }

    fn handle_res(
        _: usize,
        _: Self::Result,
        _: Self::Pending,
        _: &mut LspManager,
    ) -> Option<Actions> {
        None
    }
}

/// Helper struct for routing server requests to their appropriate handler
pub(super) struct RequestHandler<'a> {
    pub(super) lsp_id: usize,
    pub(super) r: Option<Request>,
    pub(super) man: &'a mut LspManager,
}

impl RequestHandler<'_> {
    pub(super) fn handle<R>(&mut self) -> &mut Self
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

        if let Some(actions) = actions {
            if self.man.tx_events.send(Event::Actions(actions)).is_err() {
                error!("LSP - sender actions channel closed: exiting");
            }
        }

        self
    }

    pub(super) fn log_unhandled(&mut self) {
        if let Some(r) = &self.r {
            warn!("LSP - unhandled server request: {r:?}");
        }
    }
}

/// Incoming requests from the server handle and respone to
pub(crate) trait LspServerRequest: lsp_types::request::Request {
    fn handle_params(
        lsp_id: usize,
        req_id: RequestId,
        params: Self::Params,
        man: &mut LspManager,
    ) -> (Response, Option<Actions>);
}

impl LspServerRequest for lsp_types::request::WorkDoneProgressCreate {
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

impl LspNotification for lsp_types::notification::DidChangeTextDocument {
    type Data = (String, String, i32);

    fn prepare((path, text, version): Self::Data) -> Self::Params {
        use lsp_types::{
            DidChangeTextDocumentParams, TextDocumentContentChangeEvent,
            VersionedTextDocumentIdentifier,
        };

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

impl LspNotification for lsp_types::notification::DidCloseTextDocument {
    type Data = String;

    fn prepare(path: Self::Data) -> Self::Params {
        lsp_types::DidCloseTextDocumentParams {
            text_document: txt_doc_id(&path),
        }
    }
}

impl LspNotification for lsp_types::notification::DidOpenTextDocument {
    type Data = (String, String, String);

    fn prepare((language_id, path, text): Self::Data) -> Self::Params {
        use lsp_types::{DidOpenTextDocumentParams, TextDocumentItem};

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

impl LspNotification for lsp_types::notification::Exit {
    type Data = ();

    fn prepare(_: Self::Data) -> Self::Params {}
}

impl LspNotification for lsp_types::notification::Initialized {
    type Data = ();

    fn prepare(_: Self::Data) -> Self::Params {
        lsp_types::InitializedParams {}
    }
}

/// Helper struct for routing server notifications to their appropriate handler
pub(super) struct NotificationHandler<'a> {
    pub(super) lsp_id: usize,
    pub(super) n: Option<Notification>,
    pub(super) man: &'a mut LspManager,
}

impl NotificationHandler<'_> {
    pub(super) fn handle<N>(&mut self) -> &mut Self
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

        if let Some(actions) = actions {
            if self.man.tx_events.send(Event::Actions(actions)).is_err() {
                error!("LSP - sender actions channel closed: exiting");
            }
        }

        self
    }

    pub(super) fn log_unhandled(&mut self) {
        if let Some(n) = &self.n {
            warn!("LSP - unhandled notification: {n:?}");
        }
    }
}

/// Notifications sent from the server to us that we need to handle
pub(crate) trait LspServerNotification: lsp_types::notification::Notification {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions>;
}

impl LspServerNotification for lsp_types::notification::Progress {
    fn handle_params(lsp_id: usize, params: Self::Params, man: &mut LspManager) -> Option<Actions> {
        use lsp_types::{
            ProgressParamsValue, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressEnd,
            WorkDoneProgressReport,
        };
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

#[inline]
fn uri(path: &str) -> Uri {
    Uri::from_str(&format!("file://{path}")).unwrap()
}

#[inline]
fn txt_doc_id(path: &str) -> TextDocumentIdentifier {
    lsp_types::TextDocumentIdentifier { uri: uri(path) }
}

#[inline]
fn txtdoc_pos(file: &str, line: u32, character: u32) -> TextDocumentPositionParams {
    TextDocumentPositionParams {
        text_document: txt_doc_id(file),
        position: lsp_types::Position { line, character },
    }
}
