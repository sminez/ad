use crate::{
    editor::{Action, Actions},
    lsp::{
        LSP_FILE, LspManager, Pos,
        messages::{request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::{HoverContents, HoverParams, MarkedString, request::HoverRequest};

impl LspRequest for HoverRequest {
    type Data = Pos;
    type Pending = ();

    fn build_params(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        HoverParams {
            text_document_position_params: txtdoc_pos(&file, line, character),
            work_done_progress_params: Default::default(),
        }
    }

    fn handle_res(
        _: usize,
        res: Self::Result,
        _: Self::Pending,
        _: &mut LspManager,
    ) -> Option<Actions> {
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

        Some(Actions::Single(Action::OpenTransientScratch {
            name: LSP_FILE.to_string(),
            txt,
        }))
    }
}
