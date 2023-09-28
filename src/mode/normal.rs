//! vim style normal mode
use crate::{
    key::{Arrow::*, Key::*},
    keymap,
    mode::{Action::*, Mode},
    term::CurShape,
};

pub(crate) fn normal_mode() -> Mode {
    let mut keymap = keymap! {
        [ Ctrl('q') ] => [ Exit ],
        [ Ctrl('s') ] => [ SaveBuffer ],
        [ Char('i') ] => [ SetMode("INSERT".to_string()) ],
        [ Char('a') ] => [ Move(Right), SetMode("INSERT".to_string()) ],

        [ Char('x') ] => [ Move(Right), DeleteChar ],

        [ Char('h') ] => [ Move(Left) ],
        [ Char('j') ] => [ Move(Down) ],
        [ Char('k') ] => [ Move(Up) ],
        [ Char('l') ] => [ Move(Right) ],

        [ Char('H') ] => [ RawKey(Home) ],
        [ Char('L') ] => [ RawKey(End) ],

        [ Arrow(Up) ] => [ Move(Up) ],
        [ Arrow(Down) ] => [ Move(Down) ],
        [ Arrow(Right) ] => [ Move(Right) ],
        [ Arrow(Left) ] => [ Move(Left) ]

    };

    keymap.set_default(|k| match k {
        PageUp | PageDown | Home | End => Some(vec![RawKey(*k)]),
        _ => None,
    });

    Mode {
        name: "NORMAL".to_string(),
        cur_shape: CurShape::Block,
        keymap,
        handle_expired_pending: |_| None,
    }
}
