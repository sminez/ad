//! vim style insert mode where most keys are directly modifying the buffer
use crate::{
    key::{Arrow::*, Key::*},
    keymap,
    mode::{Action::*, Mode},
    term::CurShape,
};

pub(crate) fn insert_mode() -> Mode {
    let mut keymap = keymap! {
        [ Esc ] => [ SetMode("NORMAL".to_string()) ],
        [ Char('f'), Char('d') ] => [ SetMode("NORMAL".to_string()) ],
        [ Backspace ] => [ DeleteChar ],
        [ Del ] => [ Move(Right), DeleteChar ],

        [ Arrow(Up) ] => [ Move(Up) ],
        [ Arrow(Down) ] => [ Move(Down) ],
        [ Arrow(Right) ] => [ Move(Right) ],
        [ Arrow(Left) ] => [ Move(Left) ]

    };

    // By default we just let the buffer try to handle this
    keymap.set_default(|k| Some(vec![RawKey(*k)]));

    Mode {
        name: "INSERT".to_string(),
        cur_shape: CurShape::Bar,
        keymap,
        handle_expired_pending: |keys| Some(keys.iter().map(|k| RawKey(*k)).collect()),
    }
}
