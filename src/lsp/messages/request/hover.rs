use crate::{
    editor::{Action, Actions},
    lsp::{
        LSP_FILE, LspManager, Pending, Pos,
        messages::{request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::request::HoverRequest;

impl LspRequest for HoverRequest {
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
