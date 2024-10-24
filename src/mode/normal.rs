//! vim style normal mode
use crate::{
    config_handle,
    dot::TextObject::*,
    editor::{Action::*, Actions, ViewPort},
    key::{Arrow::*, Input::*},
    keymap,
    mode::Mode,
    term::CurShape,
};

pub(crate) fn normal_mode() -> Mode {
    let leader = Char(' ');

    let mut keymap = keymap! {
        // Exiting
        [ leader, Char('q') ] => [ Exit { force: false } ],
        [ leader, Char('Q') ] => [ Exit { force: true } ],

        // Modes
        [ leader, Char('b') ] => [ SelectBuffer ],
        [ Char('/') ] => [ SearchInCurrentBuffer ],
        [ Char(':') ] => [ CommandMode ],
        [ Char('!') ] => [ RunMode ],
        [ Char('.') ] => [ SamMode ],
        [ Char('-') ] => [ FindFile { new_window: false } ],
        [ Char('_') ] => [ FindRepoFile { new_window: false } ],
        [ Alt('-') ] => [ FindFile { new_window: true } ],
        [ Alt('_') ] => [ FindRepoFile { new_window: true } ],

        // DEBUG
        [ Alt('?') ] => [ DebugBufferContents ],
        [ Alt('#') ] => [ DebugEditLog ],
        [ Alt('=') ] => [ ViewLogs ],

        // Entering INSERT mode
        [ Char('i') ] => [ SetMode { m: "INSERT" }, NewEditLogTransaction ],
        [ Char('I') ] => [ DotSet(LineStart, 1), SetMode { m: "INSERT" }, NewEditLogTransaction ],
        [ Char('a') ] => [ DotSet(Arr(Right), 1), SetMode { m: "INSERT" }, NewEditLogTransaction ],
        [ Char('A') ] => [ DotSet(LineEnd, 1), SetMode { m: "INSERT" }, NewEditLogTransaction ],
        [ Char('o') ] => [ DotSet(LineEnd, 1), SetMode { m: "INSERT" }, NewEditLogTransaction, InsertChar { c: '\n' } ],
        [ Char('O') ] => [ DotSet(LineStart, 1), SetMode { m: "INSERT" }, NewEditLogTransaction, InsertChar { c: '\n' }, DotSet(Arr(Up), 1) ],

        // Setting dot
        // >> character positions
        [ Char('h') ] => [ DotSet(Arr(Left), 1) ],
        [ Char('j') ] => [ DotSet(Arr(Down), 1) ],
        [ Char('k') ] => [ DotSet(Arr(Up), 1) ],
        [ Char('l') ] => [ DotSet(Arr(Right), 1) ],
        // >> line anchors
        [ Ctrl('h') ] => [ DotSet(LineStart, 1) ],
        [ Ctrl('l') ] => [ DotSet(LineEnd, 1) ],
        [ Home ] => [ DotSet(LineStart, 1) ],
        [ End ] => [ DotSet(LineEnd, 1) ],
        // >> objects
        [ Char('w') ] => [ DotExtendForward(Word, 1), DotCollapseLast ],
        [ Char('b') ] => [ DotExtendBackward(Word, 1), DotCollapseFirst ],
        [ Char('x') ] => [ DotSet(Line, 1) ],
        [ Char('X') ] => [ DotSet(Paragraph, 1) ],
        [ Char('%') ] => [ DotSet(BufferStart, 1), DotExtendForward(BufferEnd, 1) ],
        [ Char('{') ] => [ DotExtendBackward(Paragraph, 1), DotCollapseFirst ],
        [ Char('}') ] => [ DotExtendForward(Paragraph, 1), DotCollapseLast ],

        [ Char('g'), Char('g') ] => [ DotSet(BufferStart, 1) ],
        [ Char('g'), Char('e') ] => [ DotSet(BufferEnd, 1) ],
        [ Char('g'), Char('h') ] => [ DotSet(LineStart, 1) ],
        [ Char('g'), Char('l') ] => [ DotSet(LineEnd, 1) ],

        // Delimited pairs
        [ Alt('i'), Char('(') ] => [ DotSet(Delimited('(', ')'), 1) ],
        [ Alt('i'), Char(')') ] => [ DotSet(Delimited('(', ')'), 1) ],
        [ Alt('i'), Char('[') ] => [ DotSet(Delimited('[', ']'), 1) ],
        [ Alt('i'), Char(']') ] => [ DotSet(Delimited('[', ']'), 1) ],
        [ Alt('i'), Char('{') ] => [ DotSet(Delimited('{', '}'), 1) ],
        [ Alt('i'), Char('}') ] => [ DotSet(Delimited('{', '}'), 1) ],
        [ Alt('i'), Char('<') ] => [ DotSet(Delimited('<', '>'), 1) ],
        [ Alt('i'), Char('>') ] => [ DotSet(Delimited('<', '>'), 1) ],
        [ Alt('i'), Char('"') ] => [ DotSet(Delimited('"', '"'), 1) ],
        [ Alt('i'), Char('\'') ] => [ DotSet(Delimited('\'', '\''), 1) ],
        [ Alt('i'), Char('/') ] => [ DotSet(Delimited('/', '/'), 1) ],

        // Extending dot
        // >> character positions
        [ Char('H') ] => [ DotExtendBackward(Character, 1) ],
        [ Char('J') ] => [ DotExtendForward(Line, 1) ],
        [ Char('K') ] => [ DotExtendBackward(Line, 1) ],
        [ Char('L') ] => [ DotExtendForward(Character, 1) ],
        // >> lines
        [ Alt('h') ] => [ DotExtendBackward(LineStart, 1) ],
        [ Alt('j') ] => [ DotExtendForward(Line, 1) ],
        [ Alt('k') ] => [ DotExtendBackward(Line, 1) ],
        [ Alt('l') ] => [ DotExtendForward(LineEnd, 1) ],
        // >> objects
        [ Char('W') ] => [ DotExtendForward(Word, 1) ],
        [ Char('B') ] => [ DotExtendBackward(Word, 1) ],
        [ Alt('{') ] => [ DotExtendBackward(Paragraph, 1) ],
        [ Alt('}') ] => [ DotExtendForward(Paragraph, 1) ],

        // Manipulate dot
        [ Char(';') ] => [ DotFlip ],
        [ Char(',') ] => [ DotCollapseFirst ],
        [ Alt(',') ] => [ DotCollapseLast ],

        // Manipulating viewport
        [ Char('z'), Char('t') ] => [ SetViewPort(ViewPort::Top) ],
        [ Char('z'), Char('z') ] => [ SetViewPort(ViewPort::Center) ],
        [ Char('z'), Char('b') ] => [ SetViewPort(ViewPort::Bottom) ],

        // Window manipulation
        [ Alt('w') ] => [ PreviousWindowInColumn ],
        [ Alt('a') ] => [ PreviousColumn ],
        [ Alt('s') ] => [ NextWindowInColumn ],
        [ Alt('d') ] => [ NextColumn ],
        [ Alt('W') ] => [ DragWindow { direction: Up } ],
        [ Alt('A') ] => [ DragWindow { direction: Left } ],
        [ Alt('S') ] => [ DragWindow { direction: Down } ],
        [ Alt('D') ] => [ DragWindow { direction: Right } ],

        // Editing actions
        [ Char('c') ] => [ Delete, SetMode { m: "INSERT" } ],
        [ Char('d') ] => [ Delete ],
        [ Char('p') ] => [ NewEditLogTransaction, Paste, NewEditLogTransaction ],
        [ Char('y') ] => [ Yank ],
        [ Char('u') ] => [ Undo ],
        [ Char('U') ] => [ Redo ],

        [ Ctrl('o') ] => [ JumpListBack ],
        [ Tab ] => [ JumpListForward ], // ctrl-i
        [ Alt('[') ] => [ JumpListBack ],
        [ Alt(']') ] => [ JumpListForward ],

        [ Return ] => [ LoadDot { new_window: false } ],
        [ AltReturn ] => [ LoadDot { new_window: true } ],
        [ Char('@') ] => [ ExecuteDot ],
        [ Char('*') ] => [ ExpandDot ],

    };

    keymap.set_default(|&i| match i {
        Mouse(_) | Arrow(_) | PageUp | PageDown => Some(Actions::Single(RawInput { i })),
        _ => None,
    });

    Mode {
        name: "NORMAL".to_string(),
        cur_shape: CurShape::Block,
        keymap,
        handle_expired_pending: |keys| {
            config_handle!()
                .bindings
                .get(keys)
                .map(|prog| Actions::Single(ShellRun { cmd: prog.clone() }))
        },
    }
}
