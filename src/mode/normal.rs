//! vim style normal mode
use crate::{
    editor::{Action::*, Actions},
    key::{Arrow::*, Key::*},
    keymap,
    mode::Mode,
    term::CurShape,
};

pub(crate) fn normal_mode() -> Mode {
    let leader = Char(' ');

    let mut keymap = keymap! {
        [ leader, Char('q') ] => [ Exit { force: false } ],
        [ leader, Char('Q') ] => [ Exit { force: true } ],

        [ Char(':') ] => [ CommandMode ],
        [ Char('/') ] => [ SearchInCurrentBuffer ],

        [ Char('i') ] => [ SetMode { m: "INSERT" } ],
        [ Char('a') ] => [ Move { d: Right, n: 1 }, SetMode { m: "INSERT" } ],

        [ Char('x') ] => [ Move { d: Right, n: 1 }, DeleteChar ],
        [ Char('o') ] => [ InsertLine, Move { d: Down, n: 1 }, SetMode { m: "INSERT" } ],
        [ Char('O') ] => [ Move { d: Up, n: 1 }, InsertLine, Move { d: Down, n: 1 }, SetMode { m: "INSERT" } ],

        [ Char('h') ] => [ Move { d: Left, n: 1 } ],
        [ Char('j') ] => [ Move { d: Down, n: 1 } ],
        [ Char('k') ] => [ Move { d: Up, n: 1 } ],
        [ Char('l') ] => [ Move { d: Right, n: 1 } ],

        [ Char('H') ] => [ RawKey { k: Home } ],
        [ Char('L') ] => [ RawKey { k: End } ]

    };

    keymap.set_default(|&k| match k {
        Arrow(_) | PageUp | PageDown | Home | End => Some(Actions::Single(RawKey { k })),
        _ => None,
    });

    Mode {
        name: "NORMAL".to_string(),
        cur_shape: CurShape::Block,
        keymap,
        handle_expired_pending: |_| None,
    }
}
