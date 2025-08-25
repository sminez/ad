use crate::{
    editor::Actions,
    lsp::{
        LspManager, Pending, PendingParams, PendingRequest,
        capabilities::Capabilities,
        client::Status,
        messages::{LspNotification, request::LspRequest},
        rpc::{Message, Request},
    },
};
use lsp_types::{
    ClientCapabilities, CompletionClientCapabilities, CompletionItemCapability,
    DynamicRegistrationClientCapabilities, GeneralClientCapabilities, HoverClientCapabilities,
    InitializeParams, MarkupKind, NumberOrString, PositionEncodingKind,
    TextDocumentClientCapabilities, Uri, WindowClientCapabilities, WorkDoneProgressParams,
    WorkspaceClientCapabilities, WorkspaceFolder,
    notification::Initialized,
    request::{Initialize, Request as _, Shutdown},
};

use std::{borrow::Cow, process, str::FromStr};
use tracing::debug;

impl LspRequest for Initialize {
    type Pending = (String, Vec<PendingParams>);
    type Data = (String, Option<serde_json::Value>);

    // Need a custom send impl for initialize as the default one checks that the client is running
    fn send(lsp_id: usize, data: Self::Data, p: Self::Pending, man: &mut LspManager) {
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

impl LspRequest for Shutdown {
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
