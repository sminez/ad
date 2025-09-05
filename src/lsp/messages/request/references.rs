use crate::{
    buffer::Buffers,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection, ViewPort},
    lsp::{
        LspManager, Pos, PositionEncoding,
        capabilities::Coords,
        messages::{request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::{
    Location, NumberOrString, ReferenceContext, ReferenceParams, WorkDoneProgressParams,
    request as req,
};

impl LspRequest for req::References {
    type Data = Pos;
    type Pending = ();

    fn build_params(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        ReferenceParams {
            text_document_position: txtdoc_pos(&file, line, character),
            work_done_progress_params: WorkDoneProgressParams {
                work_done_token: Some(NumberOrString::String("references".into())),
            },
            partial_result_params: Default::default(),
            context: ReferenceContext {
                include_declaration: false,
            },
        }
    }

    fn handle_res(
        lsp_id: usize,
        locs: Option<Vec<Location>>,
        _: (),
        man: &mut LspManager,
    ) -> Option<Actions> {
        let enc = man.clients.get_mut(&lsp_id)?.position_encoding;
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
