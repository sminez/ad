use crate::{
    editor::{Action, Actions, ViewPort},
    lsp::{
        LspManager, Pos,
        capabilities::Coords,
        messages::{request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse,
    request::{GotoDeclaration, GotoDefinition, GotoTypeDefinition},
};
use tracing::error;

// The types being used here look a little odd at first glance but the lsp_types crate reuses the
// same params and response types for each of the Goto* requests as they have the same shape.
macro_rules! impl_goto_req {
    ($req_ty:ident) => {
        impl LspRequest for $req_ty {
            type Data = Pos;
            type Pending = ();

            fn build_params(
                Pos {
                    file,
                    line,
                    character,
                }: Pos,
            ) -> Self::Params {
                GotoDefinitionParams {
                    text_document_position_params: txtdoc_pos(&file, line, character),
                    work_done_progress_params: Default::default(),
                    partial_result_params: Default::default(),
                }
            }

            fn handle_res(
                lsp_id: usize,
                params: Option<GotoDefinitionResponse>,
                _: Self::Pending,
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
        }
    };
}

impl_goto_req!(GotoDeclaration);
impl_goto_req!(GotoDefinition);
impl_goto_req!(GotoTypeDefinition);
