use crate::{
    buffer::Buffers,
    die,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection, ViewPort},
    lsp::{
        LSP_FILE, LspManager, Pending, PendingParams, PendingRequest, Pos, PositionEncoding, Req,
        capabilities::{Capabilities, Coords},
        client::Status,
        messages::{LspNotification, txtdoc_pos},
        rpc::{ErrorCode, Message, Request, Response, ResponseError},
    },
};
use lsp_types::{
    DynamicRegistrationClientCapabilities, GotoDefinitionParams, GotoDefinitionResponse, Location,
};
use std::{borrow::Cow, process, str::FromStr, sync::mpsc::Sender};
use tracing::{debug, error};

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
            ClientCapabilities, CompletionClientCapabilities, CompletionItemCapability,
            GeneralClientCapabilities, HoverClientCapabilities, InitializeParams, MarkupKind,
            NumberOrString, PositionEncodingKind, TextDocumentClientCapabilities, Uri,
            WindowClientCapabilities, WorkDoneProgressParams, WorkspaceClientCapabilities,
            WorkspaceFolder,
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
                    completion: Some(CompletionClientCapabilities {
                        dynamic_registration: Some(true),
                        completion_item: Some(CompletionItemCapability {
                            snippet_support: None,
                            commit_characters_support: None,
                            documentation_format: Some(vec![MarkupKind::PlainText]),
                            deprecated_support: None,
                            preselect_support: None,
                            tag_support: None,
                            insert_replace_support: Some(false),
                            resolve_support: Some(
                                lsp_types::CompletionItemCapabilityResolveSupport {
                                    properties: vec!["additionalTextEdits".to_string()],
                                },
                            ),
                            insert_text_mode_support: None,
                            label_details_support: None,
                        }),
                        completion_item_kind: Some(Default::default()),
                        context_support: Some(true),
                        insert_text_mode: None,
                        completion_list: None,
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

                debug!(%lsp_id, "LSP initialized");
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

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
impl LspRequest for lsp_types::request::Completion {
    type Pending = ();
    type Data = Pos;

    fn prepare(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        use lsp_types::{CompletionContext, CompletionParams, CompletionTriggerKind};

        CompletionParams {
            text_document_position: txtdoc_pos(&file, line, character),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: Some(CompletionContext {
                trigger_kind: CompletionTriggerKind::INVOKED,
                trigger_character: None,
            }),
        }
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::Completion
    }

    fn handle_res(
        lsp_id: usize,
        resp: Option<lsp_types::CompletionResponse>,
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

        let items = match resp? {
            lsp_types::CompletionResponse::List(l) => l.items,
            lsp_types::CompletionResponse::Array(items) => items,
        };

        let completions: Vec<_> = items
            .into_iter()
            .map(|item| Completion::new(item, enc, lsp_id, &man.tx_req))
            .collect();

        Some(Actions::Single(Action::MbSelect(
            Completions(completions).into_selector(),
        )))
    }
}

// We assume that each TextEdit is a single edit to the current buffer.
fn actions_from_text_edit(
    edit: &lsp_types::TextEdit,
    enc: PositionEncoding,
    use_xdot: bool,
) -> [Action; 2] {
    let coords = Coords::new_from_range(edit.range, enc);

    if use_xdot {
        [
            Action::XDotSetFromCoords { coords },
            Action::XInsertString {
                s: edit.new_text.clone(),
            },
        ]
    } else {
        [
            Action::DotSetFromCoords { coords },
            Action::InsertString {
                s: edit.new_text.clone(),
            },
        ]
    }
}

#[derive(Debug, Clone)]
struct Completion {
    comp_item: lsp_types::CompletionItem,
    edit_actions: Option<Actions>,
    resolve_args: Option<(usize, Sender<Req>)>,
    kind: String,
}

impl Completion {
    fn new(
        comp_item: lsp_types::CompletionItem,
        enc: PositionEncoding,
        lsp_id: usize,
        tx_req: &Sender<Req>,
    ) -> Self {
        let kind = comp_item.kind.map(|k| format!("{k:?}")).unwrap_or_default();
        let (resolve_args, edit_actions) = match comp_item.text_edit.as_ref() {
            Some(lsp_types::CompletionTextEdit::Edit(edit)) => {
                // Some LSP servers will defer computing data for additional edits until explicitly
                // requested by the client using completionItem/resolve. If the "data" field is
                // non-null then this is an indicator that we need to make the resolve request (the
                // data inside of the "data" field itself is only intended for use by the LSP
                // server itself in order to resolve the request so we shouldn't be doing anything
                // with it other than passing it back).
                if comp_item.data.is_some() {
                    (Some((lsp_id, tx_req.clone())), None)
                } else {
                    (
                        None,
                        Some(Actions::Multi(
                            actions_from_text_edit(edit, enc, false).to_vec(),
                        )),
                    )
                }
            }

            Some(lsp_types::CompletionTextEdit::InsertAndReplace(_)) | None => (None, None),
        };

        Self {
            comp_item,
            edit_actions,
            resolve_args,
            kind,
        }
    }

    // From the LSP spec docs on the insertText field:
    // 	 A string that should be inserted into a document when selecting
    //   this completion. When omitted the label is used as the insert text
    //   for this item.
    fn raw_insert_text(&self) -> String {
        self.comp_item
            .insert_text
            .clone()
            .unwrap_or_else(|| self.comp_item.label.clone())
    }

    fn mb_line(&self, label_width: usize, kind_width: usize) -> String {
        format!(
            "{:<label_width$} {:<kind_width$}  {}",
            self.comp_item.label,
            self.kind,
            self.comp_item.detail.as_deref().unwrap_or_default(),
            label_width = label_width,
            kind_width = kind_width
        )
    }
}

#[derive(Debug, Clone)]
pub struct Completions(Vec<Completion>);

impl MbSelect for Completions {
    fn clone_selector(&self) -> MbSelector {
        self.clone().into_selector()
    }

    /// The initial filter input we want is the "word" so far under the cursor.
    /// We move back a character before expanding in order to use the last character typed rather
    /// than the current insert position.
    ///
    /// > This is a little funky with completions at word boundaries.
    fn initial_input(&self, buffers: &Buffers) -> Option<String> {
        let b = buffers.active();
        let mut cur = b.dot.active_cur();
        cur.idx = cur.idx.saturating_sub(1);

        let input = buffers.active().word_under_dot(cur.into());

        match input.chars().next() {
            Some(ch) if ch.is_alphanumeric() => Some(input),
            _ => None,
        }
    }

    fn prompt_and_options(&self, _buffers: &Buffers) -> (String, Vec<String>) {
        let label_width = self
            .0
            .iter()
            .map(|c| c.comp_item.label.chars().count())
            .max()
            .unwrap_or_default();
        let kind_width = self
            .0
            .iter()
            .map(|c| c.kind.chars().count())
            .max()
            .unwrap_or_default();

        (
            "Completions> ".to_owned(),
            self.0
                .iter()
                .map(|c| c.mb_line(label_width, kind_width))
                .collect(),
        )
    }

    fn selected_actions(&self, sel: MiniBufferSelection) -> Option<Actions> {
        match sel {
            MiniBufferSelection::Line { cy, .. } => self.0.get(cy).and_then(|c| {
                if let Some((lsp_id, tx_req)) = c.resolve_args.clone() {
                    let pending =
                        PendingParams::ResolveCompletionItem(Box::new(c.comp_item.clone()));
                    let req = Req::Pending(PendingRequest { lsp_id, pending });
                    if let Err(e) = tx_req.send(req) {
                        die!("LSP manager died: {e}")
                    }

                    None
                } else {
                    c.edit_actions.clone().or_else(|| {
                        Some(Actions::Single(Action::InsertString {
                            s: c.raw_insert_text(),
                        }))
                    })
                }
            }),

            _ => None,
        }
    }
}

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem_resolve
//
// We need to resolve both the completion itself and any additional edits
impl LspRequest for lsp_types::request::ResolveCompletionItem {
    type Pending = ();
    type Data = Box<lsp_types::CompletionItem>;

    fn prepare(item: Self::Data) -> Self::Params {
        *item
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::ResolveCompletionItem
    }

    fn handle_res(
        lsp_id: usize,
        comp_item: lsp_types::CompletionItem,
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

        let mut edit_actions = match comp_item.text_edit.as_ref() {
            Some(lsp_types::CompletionTextEdit::Edit(edit)) => {
                actions_from_text_edit(edit, enc, false).to_vec()
            }
            Some(lsp_types::CompletionTextEdit::InsertAndReplace(_)) | None => Vec::new(),
        };

        edit_actions.extend(
            comp_item
                .additional_text_edits
                .unwrap_or_default()
                .iter()
                .flat_map(|te| actions_from_text_edit(te, enc, true)),
        );

        Some(Actions::Multi(edit_actions))
    }
}
