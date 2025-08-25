//! vim style insert mode where most keys are directly modifying the buffer
use crate::{
    dot::TextObject::*,
    editor::{Action::*, Actions},
    key::{Arrow::*, Input::*},
    keymap,
    mode::Mode,
    term::CurShape,
    trie::QueryResult,
};

pub(crate) fn insert_mode() -> (Mode, Vec<(String, &'static str)>) {
    let (keymap, docs) = keymap! {
        "return to NORMAL mode";
        [ Esc ] => [ SetMode { m: "NORMAL" }, NewEditLogTransaction ],
        "toggle the visibility of the scratch buffer";
        [ Alt(';') ] => [ ToggleScratch ],

        "backspace";
        [ Backspace ] => [ DotSet(Arr(Left), 1), Delete ],
        "delete";
        [ Del ] => [ Delete ],
        "move to start of line";
        [ Home ] => [ DotSet(LineStart, 1) ],
        "move to end of line";
        [ End ] => [ DotSet(LineEnd, 1) ],

        // following vim here: alt-hjkl will move the cursor the same as normal mode hjkl
        // with the added effect of moving you to normal mode.
        "return to NORMAL mode and move one character left";
        [ Alt('h') ] => [ SetMode { m: "NORMAL" }, DotSet(Arr(Left), 1) ],
        "return to NORMAL mode and move one line down";
        [ Alt('j') ] => [ SetMode { m: "NORMAL" }, DotSet(Arr(Down), 1) ],
        "return to NORMAL mode and move one line up";
        [ Alt('k') ] => [ SetMode { m: "NORMAL" }, DotSet(Arr(Up), 1) ],
        "return to NORMAL mode and move one character right";
        [ Alt('l') ] => [ SetMode { m: "NORMAL" }, DotSet(Arr(Right), 1) ],

        // readline style bindings
        "move to start of line";
        [ Ctrl('a') ] => [ DotSet(LineStart, 1) ],
        "move to end of line";
        [ Ctrl('e') ] => [ DotSet(LineEnd, 1) ],
        "delete previous word";
        [ Ctrl('w') ] => [ DotSet(Arr(Left), 1), DotExtendBackward(Word, 1), Delete ],

        // LSP
        "LSP: request completions";
        [ Alt(' ') ] => [ LspCompletion ],
    };

    let mode = Mode {
        name: "INSERT".to_string(),
        cur_shape: CurShape::Bar,
        keymap,
        handle_expired_pending: |keys, cfg| {
            let res = cfg.keys.insert.get(keys).map(|ka| ka.as_actions());

            match res {
                QueryResult::Val(_) => res,
                QueryResult::Partial => QueryResult::Partial,
                QueryResult::Missing => QueryResult::Val(if keys.len() == 1 {
                    Actions::Single(RawInput { i: keys[0] })
                } else {
                    Actions::Multi(keys.iter().map(|&i| RawInput { i }).collect())
                }),
            }
        },
    };

    (mode, docs)
}
