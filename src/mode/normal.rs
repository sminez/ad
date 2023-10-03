//! vim style normal mode
use crate::{
    buffer::TextObject::*,
    editor::{Action::*, Actions},
    key::{Arrow::*, Key::*},
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
        [ Char(':') ] => [ CommandMode ],
        [ Char('/') ] => [ SearchInCurrentBuffer ],

        // Entering INSERT mode
        [ Char('i') ] => [ SetMode { m: "INSERT" } ],
        [ Char('I') ] => [ DotSet(LineStart), SetMode { m: "INSERT" } ],
        [ Char('a') ] => [ DotSet(Arr(Right)), SetMode { m: "INSERT" } ],
        [ Char('A') ] => [ DotSet(LineEnd), SetMode { m: "INSERT" } ],
        [ Char('o') ] => [ DotSet(LineEnd), InsertChar { c: '\n' }, SetMode { m: "INSERT" } ],
        [ Char('O') ] => [ DotSet(LineStart), InsertChar { c: '\n' }, DotSet(Arr(Up)), SetMode { m: "INSERT" } ],

        // Setting dot
        [ Char('h') ] => [ DotSet(Arr(Left)) ],
        [ Char('j') ] => [ DotSet(Arr(Down)) ],
        [ Char('k') ] => [ DotSet(Arr(Up)) ],
        [ Char('l') ] => [ DotSet(Arr(Right)) ],
        [ Alt('h') ] => [ DotSet(LineStart) ],
        [ Alt('l') ] => [ DotSet(LineEnd) ],
        [ Home ] => [ DotSet(LineStart) ],
        [ End ] => [ DotSet(LineEnd) ],
        [ Char('x') ] => [ DotSet(Line) ],
        [ Char('%') ] => [ DotSet(BufferStart), DotExtendForward(BufferEnd) ],
        [ Char('g'), Char('g') ] => [ DotSet(BufferStart) ],
        [ Char('g'), Char('e') ] => [ DotSet(BufferEnd) ],

        // Extending dot
        [ Char('H') ] => [ DotExtendBackward(Character) ],
        [ Char('J') ] => [ DotExtendForward(Line) ],
        [ Char('K') ] => [ DotExtendBackward(Line) ],
        [ Char('L') ] => [ DotExtendForward(Character) ],

        // Dot -> Cur
        [ Char(';') ] => [ DotFlip ],
        [ Char(',') ] => [ DotCollapseFirst ],
        [ Alt(',') ] => [ DotCollapseLast ],

        // Editing actions
        [ Char('c') ] => [ Delete, SetMode { m: "INSERT" } ],
        [ Char('d') ] => [ Delete ],
        [ Char('p') ] => [ Paste ],
        [ Char('y') ] => [ Yank ],


    };

    keymap.set_default(|&k| match k {
        Arrow(_) | PageUp | PageDown => Some(Actions::Single(RawKey { k })),
        _ => None,
    });

    Mode {
        name: "NORMAL".to_string(),
        cur_shape: CurShape::Block,
        keymap,
        handle_expired_pending: |_| None,
    }
}
