//! Modal editing support.
use crate::{
    config::KeyBindings,
    editor::Actions,
    key::Input,
    term::CurShape,
    trie::{QueryResult, Trie},
};
use std::fmt;
use tracing::error;

mod insert;
mod normal;

/// The modes available for ad
pub(crate) fn modes(custom_bindings: &KeyBindings) -> Vec<Mode> {
    let mut normal = normal::normal_mode().0;
    if let Err(e) = normal.with_overrides(custom_bindings.normal.clone()) {
        error!("unable to apply keymap overrides for NORMAL mode: {e}");
    }

    let mut insert = insert::insert_mode().0;
    if let Err(e) = insert.with_overrides(custom_bindings.insert.clone()) {
        error!("unable to apply keymap overrides for INSERT mode: {e}");
    }

    vec![normal, insert]
}

/// Docs for the different keybindings available in each mode
pub(crate) fn keybindings() -> Vec<(&'static str, Vec<(String, &'static str)>)> {
    vec![
        ("NORMAL", normal::normal_mode().1),
        ("INSERT", insert::insert_mode().1),
    ]
}

#[derive(Debug)]
pub(crate) struct Mode {
    pub(crate) name: String,
    pub(crate) cur_shape: CurShape,
    pub(crate) keymap: Trie<Input, Actions>,
    handle_expired_pending: fn(&[Input]) -> Option<Actions>,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Mode {
    pub(crate) fn ephemeral_mode(name: &str) -> Self {
        Mode {
            name: name.to_string(),
            cur_shape: CurShape::Block,
            keymap: Trie::try_from_iter(Vec::new()).unwrap(),
            handle_expired_pending: |_| None,
        }
    }

    fn with_overrides(&mut self, overrides: Trie<Input, Actions>) -> Result<(), &'static str> {
        self.keymap = self.keymap.clone().merge_overriding(overrides)?;

        Ok(())
    }

    pub fn handle_keys(&self, keys: &mut Vec<Input>) -> Option<Actions> {
        match self.keymap.get(keys) {
            QueryResult::Val(actions) => {
                keys.clear();
                Some(actions.clone())
            }
            QueryResult::Partial => None,
            QueryResult::Missing => {
                let res = (self.handle_expired_pending)(keys);
                keys.clear();
                res
            }
        }
    }
}

/// Construct a new [Trie] based keymap
#[macro_export]
macro_rules! keymap {
    ($($docs:expr; [$($k:expr),+] => [ $($v:expr),+ ]),+,) => {
        {
            let mut pairs = Vec::new();
            let mut docs = Vec::new();

            $(
                let key = vec![$($k),+];
                let value = $crate::keymap!(@action $($v),+);
                pairs.push((key, value));

                let doc_key = vec![$($k.to_string()),+].join(" ");
                docs.push((doc_key, $docs));
            )+

            ($crate::trie::Trie::try_from_iter(pairs).unwrap(), docs)
        }
    };

    (@action $v:expr) => { $crate::editor::Actions::Single($v) };
    (@action $($v:expr),+) => { $crate::editor::Actions::Multi(vec![$($v),+]) };
}

#[cfg(test)]
mod tests {
    use super::*;

    // This test will panic if any of the default keymaps end up with mappings that
    // collide internally. The Trie struct rejects overlapping or duplicate keys on
    // creation which will just panic the editor if this happens so it's worthwhile
    // making sure we've not messed anything up.
    #[test]
    fn mode_keymaps_have_no_collisions() {
        _ = modes(&KeyBindings::default());
    }
}
