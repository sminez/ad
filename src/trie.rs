//! A trie data structure with modifications for supporting key bindings and autocompletions in a
//! composible way with the rest of the ad internal APIs.
use std::{cmp, fmt};

/// A singly initialised Trie mapping sequences to a value
///
/// NOTE: It is not permitted for values to be mapped to a key that is a prefix of another key also
/// existing in the same Try.
///
/// There are convenience methods provided for `Trie<char, V>` for when &str values are used as keys.
#[derive(Clone)]
pub struct Trie<K, V>
where
    K: Clone + PartialEq,
    V: Clone,
{
    parent_key: Option<Vec<K>>,
    roots: Vec<Node<K, V>>,
    default: Option<DefaultMapping<K, V>>,
}

impl<K, V> fmt::Debug for Trie<K, V>
where
    K: Clone + PartialEq + fmt::Debug,
    V: Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Trie")
            .field("parent_key", &self.parent_key)
            .field("roots", &self.roots)
            .field("default", &stringify!(self.default))
            .finish()
    }
}

impl<K, V> Trie<K, V>
where
    K: Clone + PartialEq,
    V: Clone,
{
    /// Will panic if there are any key collisions or if there are any sequences that nest
    /// under a prefix that is already required to hold a value
    pub fn from_pairs(pairs: Vec<(Vec<K>, V)>) -> Self {
        let mut roots = Vec::new();

        for (k, v) in pairs.into_iter() {
            insert(k, v, &mut roots)
        }

        Self {
            parent_key: None,
            roots,
            default: None,
        }
    }

    /// Set the default handler for unmatched single element keys
    pub fn set_default(&mut self, default: DefaultMapping<K, V>) {
        self.default = Some(default);
    }

    /// Query this Try for a given key or key prefix.
    ///
    /// If the key maps to a leaf then the value is returned, if it maps to a sub-trie then
    /// `Partial` is returned to denote that the given key is a parent of one or more values. If
    /// the key is not found within the `Try` then `Missing` is returned.
    pub fn get(&self, key: &[K]) -> QueryResult<V> {
        match get_node(key, &self.roots) {
            Some(Node {
                d: Data::Val(v), ..
            }) => QueryResult::Val(v.clone()),

            Some(_) => QueryResult::Partial,

            None => match self.default {
                Some(f) if key.len() == 1 => f(&key[0]).into(),
                _ => QueryResult::Missing,
            },
        }
    }

    /// Query this Try for a given key or key prefix requiring the key to match exactly.
    ///
    /// If the key maps to a leaf then the `Some(value)` is returned, otherwise `None`.
    pub fn get_exact(&self, key: &[K]) -> Option<V> {
        match get_node(key, &self.roots) {
            Some(Node {
                d: Data::Val(v), ..
            }) => Some(v.clone()),

            Some(_) => None,

            None => match self.default {
                Some(f) if key.len() == 1 => f(&key[0]),
                _ => None,
            },
        }
    }

    /// Find all candidate keys with the given key as a prefix (up to and including the given
    /// prefix itself).
    pub fn candidates(&self, key: &[K]) -> Vec<Vec<K>> {
        match get_node(key, &self.roots) {
            None => vec![],
            Some(n) => n.resolved_keys(key),
        }
    }

    /// The number of leaf nodes in this Try
    pub fn len(&self) -> usize {
        self.roots.iter().map(|r| r.len()).sum()
    }

    /// The number of leaf nodes in this Try
    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    pub fn contains_key_or_prefix(&self, key: &[K]) -> bool {
        !matches!(self.get(key), QueryResult::Missing)
    }
}

impl<V> Trie<char, V>
where
    V: Clone,
{
    pub fn from_str_keys(pairs: Vec<(&str, V)>) -> Self {
        let mut roots = Vec::new();

        for (k, v) in pairs.into_iter() {
            insert(k.chars().collect(), v, &mut roots)
        }

        Self {
            parent_key: None,
            roots,
            default: None,
        }
    }

    pub fn get_str(&self, key: &str) -> QueryResult<V> {
        // panic!("{:?}", self.true_key(&key.chars().collect::<Vec<_>>()));
        self.get(&key.chars().collect::<Vec<_>>())
    }

    pub fn get_str_exact(&self, key: &str) -> Option<V> {
        self.get_exact(&key.chars().collect::<Vec<_>>())
    }

    pub fn candidate_strings(&self, key: &str) -> Vec<String> {
        let raw = self.candidates(&key.chars().collect::<Vec<_>>());
        let mut strings: Vec<String> = raw.into_iter().map(|v| v.into_iter().collect()).collect();
        strings.sort();

        strings
    }
}

/// A default handler for mapping a single length key to an `Option<V>`.
///
/// This is used to avoid having to specify large numbers of single length keys that should all be
/// handled in a similar way.
pub type DefaultMapping<K, V> = fn(&K) -> Option<V>;

/// The result of querying a [Try] for a particular Key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryResult<V> {
    Val(V),
    Partial,
    Missing,
}

impl<V> From<Option<V>> for QueryResult<V> {
    fn from(opt: Option<V>) -> Self {
        match opt {
            Some(v) => QueryResult::Val(v),
            None => QueryResult::Missing,
        }
    }
}

impl<V> From<QueryResult<V>> for Option<V> {
    fn from(q: QueryResult<V>) -> Self {
        match q {
            QueryResult::Val(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Data<K, V>
where
    K: Clone + PartialEq,
{
    Val(V),
    Children(Vec<Node<K, V>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Node<K, V>
where
    K: Clone + PartialEq,
{
    k: K,
    d: Data<K, V>,
}

impl<K, V> Node<K, V>
where
    K: Clone + PartialEq,
{
    fn len(&self) -> usize {
        match &self.d {
            Data::Children(nodes) => nodes.iter().map(|n| n.len()).sum(),
            Data::Val(_) => 1,
        }
    }

    // Panics if this node already holds a value
    fn insert(&mut self, k: Vec<K>, v: V) {
        match &mut self.d {
            Data::Children(nodes) => insert(k, v, nodes),
            Data::Val(_) => panic!("attempt to insert into value node"),
        }
    }

    fn get_child<'s>(&'s self, k: &[K]) -> Option<&'s Node<K, V>> {
        match &self.d {
            Data::Children(nodes) => get_node(k, nodes),
            Data::Val(_) => None,
        }
    }

    fn resolved_keys(&self, prefix: &[K]) -> Vec<Vec<K>> {
        match &self.d {
            Data::Val(_) => vec![prefix.to_vec()],
            Data::Children(nodes) => nodes
                .iter()
                .flat_map(|n| {
                    let mut so_far = prefix.to_vec();
                    so_far.push(n.k.clone());
                    n.resolved_keys(&so_far)
                })
                .collect(),
        }
    }
}

fn insert<K, V>(mut key: Vec<K>, v: V, current: &mut Vec<Node<K, V>>)
where
    K: Clone + PartialEq,
{
    for n in current.iter_mut() {
        if key[0] == n.k {
            if key.len() > 1 {
                key.remove(0);
                n.insert(key, v);
                return;
            }
            panic!("duplicate entry for key")
        }
    }

    let k = key.remove(0);

    // No matching root so create a new one
    if key.is_empty() {
        current.push(Node { k, d: Data::Val(v) });
    } else {
        let mut children = vec![];
        insert(key, v, &mut children);

        let d = Data::Children(children);
        current.push(Node { k, d });
    }
}

fn get_node<'n, K, V>(key: &[K], nodes: &'n [Node<K, V>]) -> Option<&'n Node<K, V>>
where
    K: Clone + PartialEq,
{
    if key.is_empty() {
        return None;
    }

    for n in nodes.iter() {
        if key[0] == n.k {
            return if key.len() == 1 {
                Some(n)
            } else {
                n.get_child(&key[1..])
            };
        }
    }

    None
}

/// A match function to use as part of a wildcard match
pub type WildcardFn<K> = fn(&K) -> bool;

/// A wildcard node in a key sequence that can conditionally match a single key element.
#[derive(Debug)]
pub enum WildCard<K> {
    Lit(K),
    Wild(WildcardFn<K>),
}

impl<K> cmp::PartialEq<K> for WildCard<K>
where
    K: PartialEq,
{
    fn eq(&self, other: &K) -> bool {
        match self {
            WildCard::Lit(k) => k == other,
            WildCard::Wild(f) => f(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test]
    #[should_panic(expected = "duplicate entry for key")]
    fn duplicate_keys_panic() {
        Trie::from_pairs(vec![(vec![42], 1), (vec![42], 2)]);
    }

    #[test]
    #[should_panic(expected = "attempt to insert into value node")]
    fn children_under_a_value_node_panics() {
        Trie::from_pairs(vec![(vec![42], 1), (vec![42, 69], 2)]);
    }

    #[test_case(&[42], None; "partial should be None")]
    #[test_case(&[144], None; "missing should be None")]
    #[test_case(&[42, 69, 144], None; "overshoot should be None")]
    #[test_case(&[42, 69], Some(1); "exact should be Some")]
    #[test]
    fn get_exact_works(k: &[usize], expected: Option<usize>) {
        let t = Trie::from_pairs(vec![(vec![42, 69], 1)]);

        assert_eq!(t.get_exact(k), expected);
    }

    #[test_case("fo", None; "partial should be None")]
    #[test_case("bar", None; "missing should be None")]
    #[test_case("fooo", None; "overshoot should be None")]
    #[test_case("foo", Some(1); "exact should be Some")]
    #[test]
    fn get_str_exact_works(k: &str, expected: Option<usize>) {
        let t = Trie::from_str_keys(vec![("foo", 1)]);

        assert_eq!(t.get_str_exact(k), expected);
    }

    #[test_case("ba", QueryResult::Partial; "partial match")]
    #[test_case("bar", QueryResult::Val(2); "exact match")]
    #[test_case("baz", QueryResult::Val(3); "exact match with shared prefix")]
    #[test_case("barf", QueryResult::Missing; "overshot known key")]
    #[test_case("have you any wool?", QueryResult::Missing; "completely missing")]
    #[test]
    fn get_works(k: &str, expected: QueryResult<usize>) {
        let t = Trie::from_str_keys(vec![("foo", 1), ("bar", 2), ("baz", 3)]);

        assert_eq!(t.get_str(k), expected);
    }

    #[test_case("f", &["fold", "food", "fool"]; "first char")]
    #[test_case("fo", &["fold", "food", "fool"]; "shared prefix")]
    #[test_case("foo", &["food", "fool"]; "shared prefix not all match")]
    #[test_case("food", &["food"]; "exact match")]
    #[test_case("foods", &[]; "overshot")]
    #[test_case("q", &[]; "unknown first char")]
    #[test_case("quux", &[]; "unknown full key")]
    #[test_case("", &[]; "empty string")]
    #[test]
    fn candidate_strings_works(k: &str, expected: &[&str]) {
        let expected: Vec<String> = expected.iter().map(|s| s.to_string()).collect();
        let t = Trie::from_str_keys(
            ["fool", "fold", "food"]
                .into_iter()
                .enumerate()
                .map(|(i, s)| (s, i))
                .collect(),
        );

        assert_eq!(t.candidate_strings(k), expected);
    }

    fn usize_default_handler(n: &usize) -> Option<usize> {
        Some(n + 1)
    }

    #[test_case(&[42], QueryResult::Val(1); "exact single should match from the Try")]
    #[test_case(&[12, 13], QueryResult::Val(2); "exact multi should match from the Try")]
    #[test_case(&[69], QueryResult::Val(70); "missing single should be defaulted")]
    #[test_case(&[69, 420], QueryResult::Missing; "missing multi should always be missing")]
    #[test_case(&[12], QueryResult::Partial; "partial should remain partial")]
    #[test]
    fn default_handlers_work(k: &[usize], expected: QueryResult<usize>) {
        let mut t = Trie::from_pairs(vec![(vec![42], 1), (vec![12, 13], 2)]);
        t.set_default(usize_default_handler);

        assert_eq!(t.get(k), expected);

        let expected_opt: Option<usize> = expected.into();
        assert_eq!(t.get_exact(k), expected_opt);
    }

    #[test]
    fn wildcards_match_correctly() {
        fn is_valid(c: &char) -> bool {
            *c == 'i' || *c == 'a'
        }

        let w = WildCard::Wild(is_valid);

        assert!(w == 'a');
        assert!(w != 'b');
    }
}
