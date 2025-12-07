use crate::{
    VERSION,
    editor::Actions,
    lsp::{
        LspManager, PreparedMessage,
        capabilities::Capabilities,
        client::Status,
        messages::{
            LspNotification,
            request::{LspRequest, PendingRequestData},
        },
        rpc::{Message, Request},
    },
};
use lsp_types::{
    ClientCapabilities, ClientInfo, CompletionClientCapabilities, CompletionItemCapability,
    CompletionItemCapabilityResolveSupport, DiagnosticTag, DynamicRegistrationClientCapabilities,
    GeneralClientCapabilities, HoverClientCapabilities, InitializeParams, MarkupKind,
    NumberOrString, PositionEncodingKind, PublishDiagnosticsClientCapabilities,
    RenameClientCapabilities, TagSupport, TextDocumentClientCapabilities,
    TextDocumentSyncClientCapabilities, Uri, WindowClientCapabilities, WorkDoneProgressParams,
    WorkspaceClientCapabilities, WorkspaceEditClientCapabilities, WorkspaceFolder,
    WorkspaceSymbolClientCapabilities,
    notification::{DidOpenTextDocument, Initialized},
    request::{Initialize, Request as _, Shutdown},
};

use std::{borrow::Cow, process, str::FromStr};
use tracing::debug;

/// The details we need in order to send a [DidOpenTextDocument] notification once the server is
/// initialized.
#[derive(Debug)]
pub(crate) struct OpenDocument {
    pub(crate) ftype: String,
    pub(crate) path: String,
    pub(crate) content: String,
}

impl LspRequest for Initialize {
    type Data = (String, Option<serde_json::Value>);
    type Pending = (String, Vec<OpenDocument>);

    // Need a custom send impl for initialize as the default one checks that the client is running
    fn send(lsp_id: usize, data: Self::Data, p: Self::Pending, man: &mut LspManager) {
        let client = match man.clients.get_mut(&lsp_id) {
            Some(client) => client,
            None => {
                man.send_status("no attached LSP client for buffer");
                return;
            }
        };

        let params = Self::build_params(data);
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

        man.pending.insert(
            (client.id, id),
            Box::new(PendingRequestData::<Self> {
                lsp_id,
                pending: Some(p),
            }),
        );
    }

    fn build_params((root, initialization_options): Self::Data) -> Self::Params {
        let basename = root.split("/").last().unwrap_or_default();

        #[allow(deprecated)] // root_uri, root_path
        InitializeParams {
            process_id: Some(process::id()),
            client_info: Some(ClientInfo {
                name: "ad".to_string(),
                version: Some(VERSION.to_string()),
            }),
            // trace: Some(lsp_types::TraceValue::Verbose),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: Some(NumberOrString::String("init".to_string())),
            },
            root_path: Some(root.to_string()),
            root_uri: Some(Uri::from_str(&format!("file://{root}")).unwrap()),
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Uri::from_str(&format!("file://{root}")).unwrap(),
                name: basename.to_string(),
            }]),
            initialization_options,
            capabilities: ClientCapabilities {
                // https://docs.rs/lsp-types/0.97.0/lsp_types/struct.WorkspaceClientCapabilities.html
                workspace: Some(WorkspaceClientCapabilities {
                    apply_edit: Some(true),
                    workspace_folders: Some(true),
                    configuration: Some(true),
                    symbol: Some(WorkspaceSymbolClientCapabilities {
                        dynamic_registration: Some(false),
                        ..Default::default()
                    }),
                    execute_command: Some(DynamicRegistrationClientCapabilities {
                        dynamic_registration: Some(false),
                    }),
                    did_change_configuration: Some(DynamicRegistrationClientCapabilities {
                        dynamic_registration: Some(false),
                    }),
                    workspace_edit: Some(WorkspaceEditClientCapabilities {
                        document_changes: Some(false),
                        resource_operations: Some(Vec::new()),
                        failure_handling: None,
                        normalizes_line_endings: Some(true),
                        change_annotation_support: None,
                    }),
                    ..Default::default()
                }),
                text_document: Some(TextDocumentClientCapabilities {
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
                            resolve_support: Some(CompletionItemCapabilityResolveSupport {
                                properties: vec![
                                    "documentation".to_string(),
                                    "additionalTextEdits".to_string(),
                                ],
                            }),
                            insert_text_mode_support: None,
                            label_details_support: None,
                        }),
                        completion_item_kind: Some(Default::default()),
                        context_support: Some(true),
                        insert_text_mode: None,
                        completion_list: None,
                    }),
                    formatting: Some(DynamicRegistrationClientCapabilities {
                        dynamic_registration: Some(false),
                    }),
                    hover: Some(HoverClientCapabilities {
                        dynamic_registration: Some(false),
                        content_format: Some(vec![MarkupKind::PlainText]),
                    }),
                    synchronization: Some(TextDocumentSyncClientCapabilities {
                        dynamic_registration: Some(false),
                        did_save: Some(true),
                        ..Default::default()
                    }),
                    publish_diagnostics: Some(PublishDiagnosticsClientCapabilities {
                        version_support: Some(true),
                        tag_support: Some(TagSupport {
                            value_set: vec![DiagnosticTag::UNNECESSARY, DiagnosticTag::DEPRECATED],
                        }),
                        ..Default::default()
                    }),
                    rename: Some(RenameClientCapabilities {
                        dynamic_registration: Some(false),
                        prepare_support: Some(true),
                        prepare_support_default_behavior: None,
                        honors_change_annotations: Some(false),
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

    fn handle_res(
        lsp_id: usize,
        res: Self::Result,
        (ftype, open_docs): Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        match Capabilities::try_new(res) {
            Some(c) => {
                let client = man.clients.get_mut(&lsp_id)?;

                debug!(%lsp_id, "LSP initialized");
                client.status = Status::Running;
                client.position_encoding = c.position_encoding;
                man.capabilities.write().unwrap().insert(ftype, (lsp_id, c));

                Initialized::send(lsp_id, (), man);

                for doc in open_docs {
                    man.handle_prepared_message(PreparedMessage::Notification(Box::new(
                        DidOpenTextDocument::data(lsp_id, (doc.ftype, doc.path, doc.content)),
                    )));
                }
            }

            // Unknown position encoding that we can't support
            None => man.stop_client(lsp_id),
        };

        None
    }
}

impl LspRequest for Shutdown {
    type Data = ();
    type Pending = ();

    fn build_params(_: Self::Data) -> Self::Params {}

    fn handle_res(
        _: usize,
        _: Self::Result,
        _: Self::Pending,
        _: &mut LspManager,
    ) -> Option<Actions> {
        None
    }
}
