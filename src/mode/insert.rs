//! vim style insert mode where most keys are directly modifying the buffer
use crate::{
    dot::TextObject::*,
    editor::{Action::*, Actions},
    key::{Arrow::*, Input::*},
    keymap,
    mode::Mode,
    term::CurShape,
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
        handle_expired_pending: |keys| {
            Some(if keys.len() == 1 {
                Actions::Single(RawInput { i: keys[0] })
            } else {
                Actions::Multi(keys.iter().map(|&i| RawInput { i }).collect())
            })
        },
    };

    (mode, docs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        config::{Inputs, KeyBindings},
        editor::Action,
        key::Input,
    };
    use simple_test_case::test_case;

    #[test_case("C-a", Some(Actions::Single(Action::DotSet(LineStart, 1))); "direct override single")]
    #[test]
    fn overrides_work(binding: &str, expected_default_actions: Option<Actions>) {
        let action = r#"{ send_keys = "A" }"#;
        let overrides: KeyBindings =
            toml::from_str(&format!("[insert]\n\"{binding}\" = {action}")).unwrap();

        let mut mode = insert_mode().0;

        // Without the overrides in place we should get the default behaviour
        let Inputs(mut keys) = Inputs::try_from(binding.to_owned()).unwrap();
        let default_actions = mode.handle_keys(&mut keys);
        assert_eq!(default_actions, expected_default_actions, "default");

        mode.keymap = mode.keymap.merge_overriding(overrides.insert).unwrap();

        // With the overrides we should see the send_keys action
        let Inputs(mut keys) = Inputs::try_from(binding.to_owned()).unwrap();
        let override_actions = mode.handle_keys(&mut keys);
        assert_eq!(
            override_actions,
            Some(Actions::Single(Action::SendKeys {
                ks: vec![Input::Char('A')]
            })),
            "override"
        );
    }

    #[test_case("C-a g"; "shadowing an existing binding")]
    #[test]
    fn overrides_shadowing_defaults_error(binding: &str) {
        let action = r#"{ send_keys = "A" }"#;
        let overrides: KeyBindings =
            toml::from_str(&format!("[normal]\n\"{binding}\" = {action}")).unwrap();

        let mut mode = insert_mode().0;
        assert!(mode.with_overrides(overrides.normal).is_err());
    }
}
