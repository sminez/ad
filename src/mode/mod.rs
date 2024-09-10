use crate::{
    editor::Actions,
    key::Key,
    term::CurShape,
    trie::{QueryResult, Trie},
};
use std::fmt;

mod insert;
mod normal;

pub(crate) use normal::normal_mode;

/// The modes available for ad
pub(crate) fn modes() -> Vec<Mode> {
    vec![normal::normal_mode(), insert::insert_mode()]
}

#[derive(Debug)]
pub struct Mode {
    pub(crate) name: String,
    pub(crate) cur_shape: CurShape,
    pub(crate) keymap: Trie<Key, Actions>,
    handle_expired_pending: fn(&[Key]) -> Option<Actions>,
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
            keymap: Trie::from_pairs(vec![]),
            handle_expired_pending: |_| None,
        }
    }

    pub fn handle_keys(&self, keys: &mut Vec<Key>) -> Option<Actions> {
        match self.keymap.get(keys) {
            QueryResult::Val(outcome) => {
                keys.clear();
                Some(outcome)
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

#[macro_export]
macro_rules! keymap {
    ($([$($k:expr),+] => [ $($v:expr),+ ]),+,) => {
        {
            let mut pairs = Vec::new();

            $(
                let key = vec![$($k),+];
                let value = $crate::keymap!(@action $($v),+);
                pairs.push((key, value));
            )+

            $crate::trie::Trie::from_pairs(pairs)
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
        _ = modes();
    }
}
