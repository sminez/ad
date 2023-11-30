//! vim style insert mode where most keys are directly modifying the buffer
use crate::{
    dot::TextObject::*,
    editor::{Action::*, Actions},
    key::{Arrow::*, Key::*},
    keymap,
    mode::Mode,
    term::CurShape,
};

pub(crate) fn insert_mode() -> Mode {
    let mut keymap = keymap! {
        [ Esc ] => [ SetMode { m: "NORMAL" } ],
        [ Char('f'), Char('d') ] => [ SetMode { m: "NORMAL" } ],
        [ Backspace ] => [ DotSet(Arr(Left), 1), Delete ],
        [ Del ] => [ Delete ],
        [ Home ] => [ DotSet(LineStart, 1) ],
        [ End ] => [ DotSet(LineEnd, 1) ],

        // readline style bindings
        [ Ctrl('a') ] => [ DotSet(LineStart, 1) ],
        [ Ctrl('e') ] => [ DotSet(LineEnd, 1) ],
        [ Ctrl('w') ] => [ DotSet(Arr(Left), 1), DotExtendBackward(Word, 1), Delete ],

    };

    // By default we just let the buffer try to handle this
    keymap.set_default(|&k| Some(Actions::Single(RawKey { k })));

    Mode {
        name: "INSERT".to_string(),
        cur_shape: CurShape::Bar,
        keymap,
        handle_expired_pending: |keys| {
            Some(if keys.len() == 1 {
                Actions::Single(RawKey { k: keys[0] })
            } else {
                Actions::Multi(keys.iter().map(|&k| RawKey { k }).collect())
            })
        },
    }
}
