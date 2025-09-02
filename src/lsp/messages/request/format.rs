use crate::{
    editor::Actions,
    lsp::{
        LspManager,
        messages::{EditAction, edit_actions_as_editor_actions, request::LspRequest},
    },
};
use lsp_types::{
    DocumentFormattingParams, FormattingOptions, TextDocumentIdentifier, request::Formatting,
};

impl LspRequest for Formatting {
    type Data = (TextDocumentIdentifier, u32);
    type Pending = ();

    fn build_params((text_document, tab_size): Self::Data) -> Self::Params {
        DocumentFormattingParams {
            text_document,
            work_done_progress_params: Default::default(),
            options: FormattingOptions {
                tab_size,
                insert_spaces: false,
                insert_final_newline: Some(true),
                trim_final_newlines: Some(true),
                trim_trailing_whitespace: Some(true),
                properties: Default::default(),
            },
        }
    }

    fn handle_res(
        lsp_id: usize,
        res: Self::Result,
        _: Self::Pending,
        man: &mut LspManager,
    ) -> Option<Actions> {
        let text_edits = res?;
        let enc = man.clients.get(&lsp_id)?.position_encoding;

        let actions = edit_actions_as_editor_actions(
            text_edits
                .into_iter()
                .map(|edit| EditAction::from_text_edit(edit, enc))
                .collect(),
        );

        Some(Actions::Multi(actions))
    }
}
