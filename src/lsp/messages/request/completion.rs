use crate::{
    buffer::Buffers,
    die,
    editor::{Action, Actions, MbSelect, MbSelector, MiniBufferSelection},
    lsp::{
        LspManager, Pending, PendingParams, PendingRequest, Pos, PositionEncoding, Req,
        capabilities::Coords,
        messages::{request::LspRequest, txtdoc_pos},
    },
};
use lsp_types::{
    CompletionContext, CompletionItem, CompletionParams, CompletionResponse, CompletionTextEdit,
    CompletionTriggerKind, request as req,
};
use std::sync::mpsc::Sender;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
impl LspRequest for req::Completion {
    type Pending = ();
    type Data = Pos;

    fn prepare(
        Pos {
            file,
            line,
            character,
        }: Self::Data,
    ) -> Self::Params {
        CompletionParams {
            text_document_position: txtdoc_pos(&file, line, character),
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
            context: Some(CompletionContext {
                // We only support explicit explicitly invoked completions rather than always-on
                // auto-complete firing for each character that the user types
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
        resp: Option<CompletionResponse>,
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
            CompletionResponse::List(l) => l.items,
            CompletionResponse::Array(items) => items,
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

#[derive(Debug, Clone)]
struct Completion {
    comp_item: CompletionItem,
    actions: CompletionAction,
    kind: String,
}

#[derive(Debug, Clone)]
enum CompletionAction {
    Resolve(usize, Sender<Req>),
    Actions(Actions),
}

impl Completion {
    fn new(
        comp_item: CompletionItem,
        enc: PositionEncoding,
        lsp_id: usize,
        tx_req: &Sender<Req>,
    ) -> Self {
        let kind = comp_item.kind.map(|k| format!("{k:?}")).unwrap_or_default();

        // Some LSP servers will defer computing data for additional edits until explicitly
        // requested by the client using completionItem/resolve. If the "data" field is
        // non-null then this is an indicator that we need to make the resolve request (the
        // data inside of the "data" field itself is only intended for use by the LSP
        // server itself in order to resolve the request so we shouldn't be doing anything
        // with it other than passing it back).
        let actions = if comp_item.data.is_some() {
            CompletionAction::Resolve(lsp_id, tx_req.clone())
        } else {
            CompletionAction::Actions(actions_for_resolved_completion_item(comp_item.clone(), enc))
        };

        Self {
            comp_item,
            actions,
            kind,
        }
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
        let width = |f: fn(&Completion) -> usize| self.0.iter().map(f).max().unwrap_or_default();
        let label_width = width(|c| c.comp_item.label.chars().count());
        let kind_width = width(|c| c.kind.chars().count());

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
            MiniBufferSelection::Line { cy, .. } => {
                self.0.get(cy).and_then(|c| match c.actions.clone() {
                    CompletionAction::Actions(actions) => Some(actions),

                    CompletionAction::Resolve(lsp_id, tx_req) => {
                        let pending =
                            PendingParams::ResolveCompletionItem(Box::new(c.comp_item.clone()));
                        let req = Req::Pending(PendingRequest { lsp_id, pending });
                        if let Err(e) = tx_req.send(req) {
                            die!("LSP manager died: {e}")
                        }

                        None
                    }
                })
            }

            _ => None,
        }
    }
}

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItem_resolve
//
// We need to resolve both the completion itself and any additional edits as we didn't make any
// modifications to buffer state as part of resolving the original completion item when there are
// additional actions to resolve.
impl LspRequest for req::ResolveCompletionItem {
    type Pending = ();
    type Data = Box<CompletionItem>;

    fn prepare(item: Self::Data) -> Self::Params {
        *item
    }

    fn pending(_: Self::Pending) -> Pending {
        Pending::ResolveCompletionItem
    }

    fn handle_res(
        lsp_id: usize,
        comp_item: CompletionItem,
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

        Some(actions_for_resolved_completion_item(comp_item, enc))
    }
}

/// Once a completion item is fully resolved (no `data` field or following a completionItem/resolve
/// request) we need to combine the edits both from the primary edit itself and any additional
/// edits that are given.
///
/// We assume that each TextEdit is a single edit to the current buffer. "Additional edits" need to
/// be made via the xdot as the LSP protocol specifies that they are edits that need to be made to
/// the file without affecting the editor's cursor position. Annoyingly the protocol doesn't
/// provide any mechanism for specifying the final cursor position following a multi-part edit like
/// this so we're left to figure out the correct final cursor position ourselves.
fn actions_for_resolved_completion_item(
    comp_item: CompletionItem,
    enc: PositionEncoding,
) -> Actions {
    let mut edit_actions = match comp_item.text_edit.as_ref() {
        Some(CompletionTextEdit::Edit(edit)) => {
            vec![
                Action::DotSetFromCoords {
                    coords: Coords::new_from_range(edit.range, enc),
                },
                Action::InsertString {
                    s: edit.new_text.clone(),
                },
            ]
        }

        Some(CompletionTextEdit::InsertAndReplace(_)) => Vec::new(),

        None => {
            // From the LSP spec docs on the insertText field:
            // 	 A string that should be inserted into a document when selecting
            //   this completion. When omitted the label is used as the insert text
            //   for this item.
            vec![Action::InsertString {
                s: comp_item
                    .insert_text
                    .clone()
                    .unwrap_or_else(|| comp_item.label.clone()),
            }]
        }
    };

    edit_actions.extend(
        comp_item
            .additional_text_edits
            .unwrap_or_default()
            .iter()
            .flat_map(|edit| {
                let coords = Coords::new_from_range(edit.range, enc);
                [
                    Action::XDotSetFromCoords { coords },
                    Action::XInsertString {
                        s: edit.new_text.clone(),
                    },
                ]
            }),
    );

    Actions::Multi(edit_actions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ad_event::Source;
    use simple_test_case::test_case;
    use std::sync::mpsc::channel;

    #[test_case("foo", Some("foo"); "alphanum")]
    #[test_case("foo::", None; "punctuation following alphanum")]
    #[test_case("foo::bar", Some("bar"); "alphanum following punctuation")]
    #[test]
    fn mb_completions_initial_input(s: &str, expected: Option<&str>) {
        let (tx, _rx) = channel();
        let completions = Completions(Vec::new());
        let mut buffers = Buffers::new_stubbed(&[1], tx, Default::default());
        buffers
            .active_mut()
            .handle_action(Action::InsertString { s: s.to_string() }, Source::Fsys);

        let initial_input = completions.initial_input(&buffers);

        assert_eq!(initial_input.as_deref(), expected);
    }
}
