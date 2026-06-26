//! vim style normal mode
use crate::{
    dot::TextObject::*,
    editor::{Actions, BAction::*, EAction::*, UAction::*, ViewPort},
    key::{Arrow::*, Input::*},
    keymap,
    mode::Mode,
    ui::style::CurShape,
};

pub(crate) fn normal_mode() -> (Mode, Vec<(String, &'static str)>) {
    let leader = Char(' ');

    let (keymap, docs) = keymap! {
        // Exiting
        "close window";
        [ leader, Char('q') ] => [ DeleteWindow { force: false } ],
        "force exit";
        [ leader, Char('Q') ] => [ Exit { force: true } ],

        // Modes
        "dynamic buffer select";
        [ leader, Char('b') ] => [ SelectBuffer ],
        "search in current buffer";
        [ Char('/') ] => [ SearchInCurrentBuffer ],
        "toggle the visibility of the scratch buffer";
        [ Alt(';') ] => [ ToggleScratch ],
        "enter COMMAND mode";
        [ Char(':') ] => [ CommandMode ],
        "enter RUN mode";
        [ Char('!') ] => [ RunMode ],
        "enter EDIT mode";
        [ Char('.') ] => [ SamMode ],
        "find file in current directory";
        [ Char('-') ] => [ FindFile { new_window: false } ],
        "find file in repo";
        [ Char('_') ] => [ FindRepoFile { new_window: false } ],
        "find file in current directory (new window)";
        [ Alt('-') ] => [ FindFile { new_window: true } ],
        "find file in repo (new window)";
        [ Alt('_') ] => [ FindRepoFile { new_window: true } ],

        // DEBUG
        "view debug buffer contents";
        [ Alt('?') ] => [ DebugBufferContents ],
        "view debug edit log";
        [ Alt('#') ] => [ DebugEditLog ],
        "view logs";
        [ Alt('=') ] => [ ViewLogs ],

        // Entering INSERT mode
        "enter INSERT mode at current position";
        [ Char('i') ] => [
            SetMode { m: "INSERT" }.into(),
            NewEditLogTransaction.for_active()
        ],
        "enter INSERT mode at start of line";
        [ Char('I') ] => [
            DotSet(LineStart, 1).for_active(),
            SetMode { m: "INSERT" }.into(),
            NewEditLogTransaction.for_active()
        ],
        "enter INSERT mode after current position";
        [ Char('a') ] => [
            DotSet(Arr(Right), 1).for_active(),
            SetMode { m: "INSERT" }.into(),
            NewEditLogTransaction.for_active()
        ],
        "enter INSERT mode at end of line";
        [ Char('A') ] => [
            DotSet(LineEnd, 1).for_active(),
            SetMode { m: "INSERT" }.into(),
            NewEditLogTransaction.for_active()
        ],
        "enter INSERT mode on new line below";
        [ Char('o') ] => [
            DotSet(LineEnd, 1).for_active(),
            SetMode { m: "INSERT" }.into(),
            NewEditLogTransaction.for_active(),
            InsertChar { c: '\n' }.for_active()
        ],
        "enter INSERT mode on new line above";
        [ Char('O') ] => [
            DotSet(LineStart, 1).for_active(),
            SetMode { m: "INSERT" }.into(),
            NewEditLogTransaction.for_active(),
            InsertChar { c: '\n' }.for_active(),
            DotSet(Arr(Up), 1).for_active()
        ],

        // Setting dot
        // >> character positions
        "move one character left";
        [ Char('h') ] => [ DotSet(Arr(Left), 1).for_active() ],
        "move one character down";
        [ Char('j') ] => [ DotSet(Arr(Down), 1).for_active() ],
        "move one character up";
        [ Char('k') ] => [ DotSet(Arr(Up), 1).for_active() ],
        "move one character right";
        [ Char('l') ] => [ DotSet(Arr(Right), 1).for_active() ],
        // >> line anchors
        "move to start of line";
        [ Ctrl('h') ] => [ DotSet(LineStart, 1).for_active() ],
        "move to end of line";
        [ Ctrl('l') ] => [ DotSet(LineEnd, 1).for_active() ],
        "move to start of line";
        [ Home ] => [ DotSet(LineStart, 1).for_active() ],
        "move to end of line";
        [ End ] => [ DotSet(LineEnd, 1).for_active() ],
        // >> objects
        "move forward word";
        [ Char('w') ] => [ DotExtendForward(Word, 1).for_active(), DotCollapseLast.for_active() ],
        "move backward word";
        [ Char('b') ] => [ DotExtendBackward(Word, 1).for_active(), DotCollapseFirst.for_active() ],
        "select current line";
        [ Char('x') ] => [ DotSet(Line, 1).for_active() ],
        "select current paragraph";
        [ Char('X') ] => [ DotSet(Paragraph, 1).for_active() ],
        "select buffer";
        [ Char('%') ] => [ DotSet(BufferStart, 1).for_active(), DotExtendForward(BufferEnd, 1).for_active() ],
        "move to end of paragraph";
        [ Char('{') ] => [ DotExtendBackward(Paragraph, 1).for_active(), DotCollapseFirst.for_active() ],
        "move to start of paragraph";
        [ Char('}') ] => [ DotExtendForward(Paragraph, 1).for_active(), DotCollapseLast.for_active() ],

        "move to start of buffer";
        [ Char('g'), Char('g') ] => [ DotSet(BufferStart, 1).for_active() ],
        "move to end of buffer";
        [ Char('g'), Char('e') ] => [ DotSet(BufferEnd, 1).for_active() ],
        "move to start of line";
        [ Char('g'), Char('h') ] => [ DotSet(LineStart, 1).for_active() ],
        "move to end of line";
        [ Char('g'), Char('l') ] => [ DotSet(LineEnd, 1).for_active() ],

        // Delimited pairs
        "select inside of parens";
        [ Alt('i'), Char('(') ] => [ DotSet(Delimited('(', ')'), 1).for_active() ],
        "select inside of parens";
        [ Alt('i'), Char(')') ] => [ DotSet(Delimited('(', ')'), 1).for_active() ],
        "select inside of brackets";
        [ Alt('i'), Char('[') ] => [ DotSet(Delimited('[', ']'), 1).for_active() ],
        "select inside of brackets";
        [ Alt('i'), Char(']') ] => [ DotSet(Delimited('[', ']'), 1).for_active() ],
        "select inside of curlies";
        [ Alt('i'), Char('{') ] => [ DotSet(Delimited('{', '}'), 1).for_active() ],
        "select inside of curlies";
        [ Alt('i'), Char('}') ] => [ DotSet(Delimited('{', '}'), 1).for_active() ],
        "select inside of angle brackets";
        [ Alt('i'), Char('<') ] => [ DotSet(Delimited('<', '>'), 1).for_active() ],
        "select inside of angle brackets";
        [ Alt('i'), Char('>') ] => [ DotSet(Delimited('<', '>'), 1).for_active() ],
        "select inside of double quotes";
        [ Alt('i'), Char('"') ] => [ DotSet(Delimited('"', '"'), 1).for_active() ],
        "select inside of single quotes";
        [ Alt('i'), Char('\'') ] => [ DotSet(Delimited('\'', '\''), 1).for_active() ],
        "select inside of forward slashes";
        [ Alt('i'), Char('/') ] => [ DotSet(Delimited('/', '/'), 1).for_active() ],

        // Extending dot
        // >> character positions
        "extend selection one character left";
        [ Char('H') ] => [ DotExtendBackward(Character, 1).for_active() ],
        "extend selection one line down";
        [ Char('J') ] => [ DotExtendForward(Line, 1).for_active() ],
        "extend selection one line up";
        [ Char('K') ] => [ DotExtendBackward(Line, 1).for_active() ],
        "extend selection one character right";
        [ Char('L') ] => [ DotExtendForward(Character, 1).for_active() ],
        // >> lines
        "extend selection to start of line";
        [ Alt('h') ] => [ DotExtendBackward(LineStart, 1).for_active() ],
        "extend selection one line down";
        [ Alt('j') ] => [ DotExtendForward(Line, 1).for_active() ],
        "extend selection one line up";
        [ Alt('k') ] => [ DotExtendBackward(Line, 1).for_active() ],
        "extend selection to end of line";
        [ Alt('l') ] => [ DotExtendForward(LineEnd, 1).for_active() ],
        // >> objects
        "extend selection forward one word";
        [ Char('W') ] => [ DotExtendForward(Word, 1).for_active() ],
        "extend selection backward one word";
        [ Char('B') ] => [ DotExtendBackward(Word, 1).for_active() ],
        "extend selection to end of paragraph";
        [ Alt('{') ] => [ DotExtendBackward(Paragraph, 1).for_active() ],
        "extend selection to start of paragraph";
        [ Alt('}') ] => [ DotExtendForward(Paragraph, 1).for_active() ],

        // Manipulate dot
        "flip active cursor";
        [ Char(';') ] => [ DotFlip.for_active() ],
        "collapse dot to start";
        [ Char(',') ] => [ DotCollapseFirst.for_active() ],
        "collapse dot to end";
        [ Alt(',') ] => [ DotCollapseLast.for_active() ],

        // Manipulating viewport
        "set viewport to top";
        [ Char('z'), Char('t') ] => [ SetViewPort(ViewPort::Top) ],
        "set viewport to center";
        [ Char('z'), Char('z') ] => [ SetViewPort(ViewPort::Center) ],
        "set viewport to bottom";
        [ Char('z'), Char('b') ] => [ SetViewPort(ViewPort::Bottom) ],

        // Window manipulation
        "focus previous window in column";
        [ Alt('w') ] => [ PreviousWindowInColumn ],
        "focus previous column";
        [ Alt('a') ] => [ PreviousColumn ],
        "focus next window in column";
        [ Alt('s') ] => [ NextWindowInColumn ],
        "focus next column";
        [ Alt('d') ] => [ NextColumn ],
        "drag window up";
        [ Alt('W') ] => [ DragWindow { direction: Up } ],
        "drag window left";
        [ Alt('A') ] => [ DragWindow { direction: Left } ],
        "drag window down";
        [ Alt('S') ] => [ DragWindow { direction: Down } ],
        "drag window right";
        [ Alt('D') ] => [ DragWindow { direction: Right } ],

        // Editing actions
        "delete current selection and enter INSERT mode";
        [ Char('c') ] => [ Delete.for_active(), SetMode { m: "INSERT" }.into() ],
        "delete current selection";
        [ Char('d') ] => [ Delete.for_active() ],
        "paste";
        [ Char('p') ] => [ NewEditLogTransaction.for_active(), Paste.into(), NewEditLogTransaction.for_active() ],
        "yank (copy)";
        [ Char('y') ] => [ Yank ],
        "undo";
        [ Char('u') ] => [ Undo.for_active() ],
        "redo";
        [ Char('U') ] => [ Redo.for_active() ],

        "move backward in the jump list";
        [ Ctrl('o') ] => [ JumpListBack ],
        "move forward in the jump list";
        [ Tab ] => [ JumpListForward ], // ctrl-i
        "move backward in the jump list";
        [ Alt('[') ] => [ JumpListBack ],
        "move forward in the jump list";
        [ Alt(']') ] => [ JumpListForward ],

        "load dot in current window";
        [ Return ] => [ LoadDot { bufid: None, new_window: false } ],
        "load dot in new window";
        [ AltReturn ] => [ LoadDot { bufid: None, new_window: true } ],
        "execute dot";
        [ Char('@') ] => [ ExecuteDot { bufid: None } ],
        "expand dot";
        [ Char('*') ] => [ ExpandDot.for_active() ],

        // LSP
        "LSP: show diagnostics";
        [ leader, Char('e') ] => [ LspShowDiagnostics ],
        "LSP: go to declaration";
        [ Char('g'), Char('D') ] => [ LspGotoDeclaration ],
        "LSP: go to definition";
        [ Char('g'), Char('d') ] => [ LspGotoDefinition ],
        "LSP: show references";
        [ Char('g'), Char('r') ] => [ LspReferences ],
        "LSP: go to type definition";
        [ Char('g'), Char('t') ] => [ LspGotoTypeDefinition ],
        "LSP: show hover";
        [ Ctrl('k') ] => [ LspHover ],
        "LSP: rename";
        [ leader, Char('l'), Char('R') ] => [ LspRenamePrepare ],
    };

    let mode = Mode {
        name: "NORMAL".to_string(),
        cur_shape: CurShape::Block,
        keymap,
        handle_expired_pending: |keys| {
            if keys.len() > 1 {
                return None;
            }
            let i = keys[0];
            match i {
                Mouse(_) | Arrow(_) | PageUp | PageDown => Some(Actions::single(RawInput { i })),
                _ => None,
            }
        },
    };

    (mode, docs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        config::{Inputs, KeyBindings},
        key::Input,
    };
    use simple_test_case::test_case;

    #[test_case("C-k", Some(Actions::single(LspHover)); "direct override single")]
    #[test_case("g d", Some(Actions::single(LspGotoDefinition)); "direct override sequence")]
    #[test_case("z x", None; "sharing prefix with defaults")]
    #[test]
    fn overrides_work(binding: &str, expected_default_actions: Option<Actions>) {
        let action = r#"{ send_keys = "A" }"#;
        let overrides: KeyBindings =
            toml::from_str(&format!("[normal]\n\"{binding}\" = {action}")).unwrap();

        let mut mode = normal_mode().0;

        // Without the overrides in place we should get the default behaviour
        let Inputs(mut keys) = Inputs::try_from(binding.to_owned()).unwrap();
        let default_actions = mode.handle_keys(&mut keys);
        assert_eq!(default_actions, expected_default_actions, "default");

        mode.keymap = mode
            .keymap
            .merge_overriding(overrides.normal.additional)
            .unwrap();

        // With the overrides we should see the send_keys action
        let Inputs(mut keys) = Inputs::try_from(binding.to_owned()).unwrap();
        let override_actions = mode.handle_keys(&mut keys);
        assert_eq!(
            override_actions,
            Some(Actions::single(SendKeys {
                ks: vec![Input::Char('A')]
            })),
            "override"
        );
    }

    #[test_case("d g"; "shadowing an existing binding")]
    #[test_case("g g g"; "shadowing an existing sequence")]
    #[test]
    fn overrides_shadowing_defaults_error(binding: &str) {
        let action = r#"{ send_keys = "A" }"#;
        let overrides: KeyBindings =
            toml::from_str(&format!("[normal]\n\"{binding}\" = {action}")).unwrap();

        let mut mode = normal_mode().0;
        assert!(mode.with_overrides(overrides.normal).is_err());
    }
}
