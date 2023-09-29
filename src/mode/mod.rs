use crate::{
    editor::Action,
    key::Key,
    term::CurShape,
    trie::{QueryResult, Trie},
};
use std::fmt;

mod insert;
mod normal;

/// The modes available for ad
pub(crate) fn modes() -> Vec<Mode> {
    vec![normal::normal_mode(), insert::insert_mode()]
}

pub struct Mode {
    pub(crate) name: String,
    pub(crate) cur_shape: CurShape,
    keymap: Trie<Key, Vec<Action>>,
    handle_expired_pending: fn(&[Key]) -> Option<Vec<Action>>,
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Mode {
    pub fn handle_keys(&self, keys: &mut Vec<Key>) -> Option<Vec<Action>> {
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
    ($([$($k:expr),+] => [ $($v:expr),+ ]),+) => {
        {
            let mut pairs = Vec::new();

            $(
                let key = vec![$($k),+];
                let value = vec![$($v),+];
                pairs.push((key, value));
            )+

            $crate::trie::Trie::from_pairs(pairs)
        }
    };
}
