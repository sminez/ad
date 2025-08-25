use crate::{
    editor::{Action, Actions, ViewPort},
    lsp::{
        LspManager, Pending, Pos,
        capabilities::Coords,
        messages::{request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse,
    request::{GotoDeclaration, GotoDefinition, GotoTypeDefinition},
};
use tracing::error;

impl LspRequest for GotoDeclaration {
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

impl LspRequest for GotoDefinition {
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

impl LspRequest for GotoTypeDefinition {
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
