use crate::{
    editor::{Action, Actions},
    lsp::{
        Coords, LspManager, Pos,
        messages::{EditAction, edit_actions_as_editor_actions, request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::{
    DocumentChanges, OneOf, RenameParams, TextDocumentEdit,
    request::{PrepareRenameRequest, Rename},
};
use tracing::warn;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareRename
impl LspRequest for PrepareRenameRequest {
    type Data = Pos;
    type Pending = ();

    fn build_params(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        txtdoc_pos(&file, line, character)
    }

    fn handle_res(
        _: usize,
        res: Self::Result,
        _: Self::Pending,
        _: &mut LspManager,
    ) -> Option<Actions> {
        match res {
            Some(_) => Some(Actions::Multi(vec![
                Action::SetStatusMessage {
                    message: "triggering LSP rename".to_string(),
                },
                Action::LspRename,
            ])),

            None => Some(Actions::Single(Action::SetStatusMessage {
                message: "LSP rename not possible".into(),
            })),
        }
    }
}

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
impl LspRequest for Rename {
    type Data = (Pos, String);
    type Pending = (Pos, usize);

    fn build_params(
        (
            Pos {
                file,
                line,
                character,
            },
            new_name,
        ): Self::Data,
    ) -> Self::Params {
        RenameParams {
            text_document_position: txtdoc_pos(&file, line, character),
            work_done_progress_params: Default::default(),
            new_name,
        }
    }

    fn handle_res(
        lsp_id: usize,
        res: Self::Result,
        (pos, buffer_id): Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        let td_edits = match res?.document_changes? {
            DocumentChanges::Edits(edits) => edits,
            DocumentChanges::Operations(_) => {
                warn!(
                    "received unexpected DocumentChanges::Operations instead of edits for rename"
                );
                return None;
            }
        };

        let enc = man.clients.get(&lsp_id)?.position_encoding;
        let mut actions = Vec::new();

        for TextDocumentEdit {
            text_document,
            edits,
        } in td_edits.into_iter()
        {
            let uri = text_document.uri;
            let path = uri.to_string().strip_prefix("file://").unwrap().to_owned();

            actions.push(Action::OpenFile { path });
            actions.extend(edit_actions_as_editor_actions(
                edits
                    .into_iter()
                    .map(|edit| {
                        let edit = match edit {
                            OneOf::Left(edit) => edit,
                            OneOf::Right(annotated_edit) => annotated_edit.text_edit,
                        };

                        EditAction::from_text_edit(edit, enc)
                    })
                    .collect(),
            ));
        }

        actions.push(Action::FocusBuffer { id: buffer_id });
        actions.push(Action::DotSetFromCoords {
            coords: Coords::new_from_pos(pos, enc),
        });

        Some(Actions::Multi(actions))
    }
}
