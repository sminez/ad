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
        [ Char('I') ] => [ DotSet(LineBoundary), DotCollapseFirst, SetMode { m: "INSERT" } ],
        [ Char('a') ] => [ Move { d: Right }, SetMode { m: "INSERT" } ],
        [ Char('A') ] => [ DotSet(Line), DotCollapseLast, SetMode { m: "INSERT" } ],
        [ Char('o') ] => [ DotSet(Line), DotCollapseLast, InsertChar { c: '\n' }, SetMode { m: "INSERT" } ],
        [ Char('O') ] => [ DotSet(Line), DotCollapseFirst, InsertChar { c: '\n' }, Move { d: Up }, SetMode { m: "INSERT" } ],

        // Setting dot
        [ Char('h') ] => [ Move { d: Left } ],
        [ Char('j') ] => [ Move { d: Down } ],
        [ Char('k') ] => [ Move { d: Up } ],
        [ Char('l') ] => [ Move { d: Right } ],

        [ Char(';') ] => [ DotFlip ],
        [ Char(',') ] => [ DotCollapseFirst ],
        [ Alt(',') ] => [ DotCollapseLast ],

        [ Alt('h') ] => [ DotSet(LineBoundary), DotCollapseFirst ],
        [ Alt('l') ] => [ DotSet(LineBoundary), DotCollapseLast ],
        [ Home ] => [ DotSet(LineBoundary), DotCollapseFirst ],
        [ End ] => [ DotSet(LineBoundary), DotCollapseLast ],

        [ Char('x') ] => [ DotSet(LineBoundary) ],
        [ Char('%') ] => [ DotSet(Buffer) ],

        // Extending dot
        [ Char('H') ] => [ DotExtendBackward(Character) ],
        [ Char('J') ] => [ DotExtendForward(Line) ],
        [ Char('K') ] => [ DotExtendBackward(Line) ],
        [ Char('L') ] => [ DotExtendForward(Character) ],

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
