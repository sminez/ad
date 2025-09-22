//! Traits and handlers for processing LSP messages
use crate::{
    editor::Action,
    lsp::{Coords, capabilities::PositionEncoding},
};
use lsp_types::{Position, TextDocumentIdentifier, TextDocumentPositionParams, TextEdit, Uri};
use std::str::FromStr;

mod notification;
mod request;
mod server_notification;
mod server_request;

pub(super) use notification::{LspNotification, PreparedLspNotification};
pub(super) use request::{LspRequest, OpenDocument, PendingLspRequest, PreparedLspRequest};
pub(super) use server_notification::{Diagnostic, Diagnostics, NotificationHandler};
pub(super) use server_request::RequestHandler;

#[inline]
fn txtdoc_pos(file: &str, line: u32, character: u32) -> TextDocumentPositionParams {
    TextDocumentPositionParams {
        text_document: txt_doc_id(file),
        position: Position { line, character },
    }
}

#[inline]
pub(crate) fn txt_doc_id(path: &str) -> TextDocumentIdentifier {
    TextDocumentIdentifier { uri: uri(path) }
}

#[inline]
fn uri(path: &str) -> Uri {
    Uri::from_str(&format!("file://{path}")).unwrap()
}

// From the spec:
// ```
// interface TextEdit {
//    /**
// 	   * The range of the text document to be manipulated. To insert
// 	   * text into a document create a range where start === end.
// 	   */
//     range: Range;
//
//    /**
// 	   * The string to be inserted. For delete operations use an
// 	   * empty string.
// 	   */
//     newText: string;
// }
//
// Complex text manipulations are described with an array of TextEdit’s or AnnotatedTextEdit’s,
// representing a single change to the document.
//
// All text edits ranges refer to positions in the document they are computed on. They therefore
// move a document from state S1 to S2 without describing any intermediate state. Text edits ranges
// must never overlap, that means no part of the original document must be manipulated by more than
// one edit. However, it is possible that multiple edits have the same start position: multiple
// inserts, or any number of inserts followed by a single remove or replace edit. If multiple
// inserts have the same position, the order in the array defines the order in which the inserted
// strings appear in the resulting text.
// ```
//
// And then the kicker, the docs for TextDocumentEdit (rather than TextEdit):
// ```
// A TextDocumentEdit describes all changes on a version Si and after they are applied move the
// document to version Si+1. So the creator of a TextDocumentEdit doesn’t need to sort the array of
// edits or do any kind of ordering. However the edits must be non overlapping.
// ```

#[derive(Debug)]
pub(crate) struct EditAction {
    pub(crate) coords: Coords,
    pub(crate) s: String,
    pub(crate) use_xdot: bool,
}

impl EditAction {
    pub(crate) fn into_actions(
        EditAction {
            coords,
            s,
            use_xdot,
        }: EditAction,
    ) -> [Action; 2] {
        if use_xdot {
            [
                Action::XDotSetFromCoords { coords },
                Action::XInsertString { s },
            ]
        } else {
            [
                Action::DotSetFromCoords { coords },
                Action::InsertString { s },
            ]
        }
    }

    pub(crate) fn from_text_edit(edit: TextEdit, enc: PositionEncoding) -> Self {
        Self {
            coords: Coords::new_from_range(edit.range, enc),
            s: edit.new_text,
            use_xdot: true,
        }
    }

    pub(crate) fn using_dot(mut self) -> Self {
        self.use_xdot = false;
        self
    }
}

/// From the docs on TextEdit:
///   If n TextEdits are applied to a text document all text edits describe changes to the initial
///   document version. Execution wise text edits should applied from the bottom to the top of the
///   text document. Overlapping text edits are not supported.
///
/// Also see <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textEditArray>
pub(crate) fn edit_actions_as_editor_actions(mut edit_actions: Vec<EditAction>) -> Vec<Action> {
    edit_actions.sort_by_key(|a| a.coords);
    edit_actions.reverse();

    edit_actions
        .into_iter()
        .flat_map(EditAction::into_actions)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use simple_test_case::test_case;

    // The first "blank" line here contains leading whitespace
    const TEST_BUF: &str = r#"fn test() {
    for x in 0..10 {
        println!("X is {x}");
        println!("Another line");
        


        println!("Yet another line");
    }
}"#;

    const EXPECTED: &str = r#"fn test() {
    for x in 0..10 {
        println!("X is {x}");
        println!("Another line");
        println!("Yet another line");
    }
}"#;

    const TEST_BUF_2: &str = r#"use crate::{
    dot::{
    Cur, Dot, Range, TextObject, find::find_forward_wrapping},
};

fn main() {}"#;

    const EXPECTED_2: &str = r#"use crate::dot::{Cur, Dot, Range, TextObject, find::find_forward_wrapping};

fn main() {}"#;

    #[test_case(
        TEST_BUF, EXPECTED,
        vec![TextEdit {
            range: Range {
                start: Position { line: 4, character: 0 },
                end: Position { line: 6, character: 0 },
            },
            new_text: "".to_string(),
        }];
        "blank lines being removed"
    )]
    #[test_case(
        TEST_BUF_2, EXPECTED_2,
        vec![
            TextEdit {
                range: Range {
                    start: Position { line: 0, character: 11 },
                    end: Position { line: 1, character: 4 }
                },
                new_text: "".to_string(),
            },
            TextEdit {
                range: Range {
                    start: Position { line: 1, character: 10 },
                    end: Position { line: 2, character: 4 }
                },
                new_text: "".to_string(),
            },
            TextEdit {
                range: Range {
                    start: Position { line: 2, character: 60 },
                    end: Position { line: 3, character: 0 }
                },
                new_text: "".to_string(),
            }
        ];
        "joining lines"
    )]
    #[test]
    fn format_actions_work_when_blank_lines_are_involved(
        content: &str,
        expected: &str,
        text_edits: Vec<TextEdit>,
    ) {
        use ad_event::Source;
        use lsp_types::{Range, TextEdit};

        let mut b = Buffer::new_virtual(0, "test", content, Default::default());

        let actions = edit_actions_as_editor_actions(
            text_edits
                .into_iter()
                .map(|edit| EditAction::from_text_edit(edit, PositionEncoding::Utf32))
                .collect(),
        );

        for action in actions {
            b.handle_action(action, Source::Fsys);
        }

        assert_eq!(b.str_contents(), expected);
    }
}
