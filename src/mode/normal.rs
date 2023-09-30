//! vim style normal mode
use crate::{
    editor::Action::*,
    key::{Arrow::*, Key::*},
    keymap,
    mode::Mode,
    term::CurShape,
};

pub(crate) fn normal_mode() -> Mode {
    let leader = Char(' ');

    let mut keymap = keymap! {
        [ leader, Char('q') ] => [ Exit ],
        [ leader, Char('Q') ] => [ ForceExit ],

        [ Ctrl('s') ] => [ SaveBuffer ],

        [ Char('/') ] => [ SearchInCurrentBuffer ],

        [ Char('i') ] => [ SetMode("INSERT") ],
        [ Char('a') ] => [ Move(Right, 1), SetMode("INSERT") ],

        [ Char('x') ] => [ Move(Right, 1), DeleteChar ],
        [ Char('o') ] => [ InsertLine, Move(Down, 1), SetMode("INSERT") ],
        [ Char('O') ] => [ Move(Up, 1), InsertLine, Move(Down, 1), SetMode("INSERT") ],

        [ Char('h') ] => [ Move(Left, 1) ],
        [ Char('j') ] => [ Move(Down, 1) ],
        [ Char('k') ] => [ Move(Up, 1) ],
        [ Char('l') ] => [ Move(Right, 1) ],

        [ Char('H') ] => [ RawKey(Home) ],
        [ Char('L') ] => [ RawKey(End) ]

    };

    keymap.set_default(|k| match k {
        Arrow(_) | PageUp | PageDown | Home | End => Some(vec![RawKey(*k)]),
        _ => None,
    });

    Mode {
        name: "NORMAL".to_string(),
        cur_shape: CurShape::Block,
        keymap,
        handle_expired_pending: |_| None,
    }
}
