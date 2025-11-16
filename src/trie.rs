//! A simple trie data structure for supporting key bindings and autocompletions in a composible
//! way with the rest of the ad internal APIs.
use std::{collections::BTreeMap, fmt, ops::Range};

/// A singly initialised Trie mapping key sequences to a value.
///
/// It is not permitted for values to be mapped to a key that is a prefix of another key also
/// existing in the same Trie.
///
/// There are convenience methods provided for `Trie<char, V>` for when &str values are used as keys.
#[allow(unpredictable_function_pointer_comparisons)]
#[derive(Clone, PartialEq, Eq)]
pub struct Trie<K, V>
where
    K: Clone + PartialEq + Ord,
    V: Clone,
{
    nodes: Vec<Node<K, V>>,
    n_roots: usize,
    default: Option<DefaultMapping<K, V>>,
}

impl<K, V> fmt::Debug for Trie<K, V>
where
    K: Clone + PartialEq + Ord + fmt::Debug,
    V: Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Trie")
            .field("nodes", &self.nodes)
            .field("n_roots", &self.n_roots)
            .field("default", &"<function>")
            .finish()
    }
}

impl<K, V> Default for Trie<K, V>
where
    K: Clone + PartialEq + Ord,
    V: Clone,
{
    fn default() -> Self {
        Self {
            nodes: Vec::new(),
            n_roots: 0,
            default: None,
        }
    }
}

impl<K, V> Trie<K, V>
where
    K: Clone + PartialEq + Ord,
    V: Clone,
{
    /// Construct a new Trie from key-value pairs.
    ///
    /// Will panic if there are any key collisions or if there are any sequences that nest
    /// under a prefix that is already required to hold a value
    pub fn try_from_iter(it: impl IntoIterator<Item = (Vec<K>, V)>) -> Result<Self, &'static str> {
        let mut roots = Vec::new();
        for (key, value) in it.into_iter() {
            insert(key, value, &mut roots)?;
        }

        if roots.is_empty() {
            return Ok(Self::default());
        }

        let mut nodes = Vec::new();
        let (_, n_roots) = flatten(roots, &mut nodes);

        Ok(Trie {
            nodes,
            n_roots,
            default: None,
        })
    }

    /// Merge two Tries.
    ///
    /// If the resulting Trie would be invalid to construct directly, an error is returned.
    ///
    /// Consumes both inputs.
    pub fn merge(self, other: Self) -> Result<Self, &'static str> {
        if self.is_empty() {
            return Ok(other);
        } else if other.is_empty() {
            return Ok(self);
        }

        let mut pairs = Vec::with_capacity(self.len() + other.len());
        self.extract_pairs(&mut pairs, Vec::new(), 0..self.n_roots);
        other.extract_pairs(&mut pairs, Vec::new(), 0..other.n_roots);

        Self::try_from_iter(pairs)
    }

    /// Merge two Tries preferring keys from `other` in the case of collisions.
    ///
    /// If the resulting Trie would be invalid to construct directly, an error is returned.
    ///
    /// Consumes both inputs.
    pub fn merge_overriding(self, other: Self) -> Result<Self, &'static str> {
        if self.is_empty() {
            return Ok(other);
        } else if other.is_empty() {
            return Ok(self);
        }

        let mut pairs = Vec::with_capacity(self.len() + other.len());
        self.extract_pairs(&mut pairs, Vec::new(), 0..self.n_roots);

        let mut m = BTreeMap::from_iter(pairs.drain(..));
        other.extract_pairs(&mut pairs, Vec::new(), 0..other.n_roots);
        m.extend(pairs.drain(..));

        Self::try_from_iter(m)
    }

    fn extract_pairs(&self, pairs: &mut Vec<(Vec<K>, V)>, key: Vec<K>, indices: Range<usize>) {
        for i in indices {
            let node = &self.nodes[i];
            let mut child_key = key.clone();
            child_key.push(node.key.clone());

            match &node.data {
                Data::Leaf { value } => pairs.push((child_key, value.clone())),

                Data::Internal {
                    child_start,
                    n_children,
                } => self.extract_pairs(pairs, child_key, *child_start..*child_start + *n_children),
            }
        }
    }

    /// Set the default handler for unmatched single element keys
    pub fn set_default(&mut self, default: DefaultMapping<K, V>) {
        self.default = Some(default);
    }

    /// Query this [Trie] for a given key or key prefix
    ///
    /// If the key maps to a leaf then the value is returned, if it maps to a sub-trie then
    /// `Partial` is returned to denote that the given key is a parent of one or more values. If
    /// the key is not found within the `Try` then `Missing` is returned.
    ///
    /// If this [Trie] contains a default mapping, it will be applied to missing single element
    /// keys.
    pub fn get(&self, key: &[K]) -> QueryResult<V> {
        match self.find_node(key) {
            QueryResult::Missing if key.len() == 1 => self.default.and_then(|f| f(&key[0])).into(),
            qr => qr,
        }
    }

    /// Query this Try for a given key or key prefix requiring the key to match exactly.
    ///
    /// If the key maps to a leaf then the `Some(value)` is returned, otherwise `None`.
    ///
    /// If this [Trie] contains a default mapping, it will be applied to missing single element
    /// keys.
    pub fn get_exact(&self, key: &[K]) -> Option<V> {
        match self.find_node(key) {
            QueryResult::Val(v) => Some(v),
            QueryResult::Missing if key.len() == 1 => self.default.and_then(|f| f(&key[0])),
            _ => None,
        }
    }

    fn find_node(&self, key: &[K]) -> QueryResult<V> {
        if key.is_empty() {
            return QueryResult::Missing;
        }

        let mut indices = 0..self.n_roots;
        let mut key_index = 0;

        'outer: while key_index < key.len() {
            let target = &key[key_index];

            // Binary search within the current level (assumes sorted children)
            for i in indices {
                let node = &self.nodes[i];
                if &node.key == target {
                    key_index += 1;

                    match &node.data {
                        Data::Leaf { value } => {
                            return if key_index == key.len() {
                                QueryResult::Val(value.clone())
                            } else {
                                QueryResult::Missing
                            };
                        }

                        Data::Internal {
                            child_start,
                            n_children,
                        } => {
                            indices = *child_start..*child_start + *n_children;
                            continue 'outer;
                        }
                    }
                } else if node.key > *target {
                    // We've moved past where the node would be in sorted order
                    return QueryResult::Missing;
                }
            }

            return QueryResult::Missing;
        }

        QueryResult::Partial
    }

    /// The number of leaf values in this Trie
    pub fn len(&self) -> usize {
        self.nodes.iter().filter(|n| n.is_leaf()).count()
    }

    /// Whether this Trie is empty
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
}

// Implementation for char-based convenience methods
impl<V> Trie<char, V>
where
    V: Clone,
{
    /// Construct a new [Trie] with [char] internal keys from string keys.
    pub fn from_str_keys(pairs: Vec<(&str, V)>) -> Result<Self, &'static str> {
        let char_pairs: Vec<(Vec<char>, V)> = pairs
            .into_iter()
            .map(|(k, v)| (k.chars().collect(), v))
            .collect();

        Self::try_from_iter(char_pairs)
    }

    /// Query this [Trie] using a string key.
    ///
    /// Both full and partial matches are possible.
    pub fn get_str(&self, key: &str) -> QueryResult<V> {
        self.get(&key.chars().collect::<Vec<_>>())
    }

    /// Query this [Trie] using a string key.
    ///
    /// Only fll matches will be returned.
    pub fn get_str_exact(&self, key: &str) -> Option<V> {
        self.get_exact(&key.chars().collect::<Vec<_>>())
    }
}

/// The internal data held at each node in a Trie.
///
/// Internal nodes are "pointers" to their children while leaves hold the value associated with the
/// full key used to traverse down to them.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Data<V> {
    Internal {
        /// Index of the first child node
        child_start: usize,
        /// Number of child nodes
        n_children: usize,
    },
    Leaf {
        // Value associated with the full key-path down to this node
        value: V,
    },
}

/// A single node within a [Trie].
///
/// Contains the last element of the key that traverses down to this node alongside [Data] that
/// identifies this node as being internal or a leaf.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Node<K, V>
where
    K: Clone + PartialEq + Ord,
    V: Clone,
{
    key: K,
    data: Data<V>,
}

impl<K, V> Node<K, V>
where
    K: Clone + PartialEq + PartialOrd + Ord,
    V: Clone,
{
    fn new_internal(key: K, children_start: usize, children_count: usize) -> Self {
        Self {
            key,
            data: Data::Internal {
                child_start: children_start,
                n_children: children_count,
            },
        }
    }

    fn new_leaf(key: K, value: V) -> Self {
        Self {
            key,
            data: Data::Leaf { value },
        }
    }

    fn is_leaf(&self) -> bool {
        matches!(self.data, Data::Leaf { .. })
    }
}

#[derive(Debug)]
struct BuildNode<K, V>
where
    K: PartialEq + Ord,
{
    k: K,
    data: BuildNodeData<K, V>,
}

#[derive(Debug)]
enum BuildNodeData<K, V>
where
    K: PartialEq + Ord,
{
    Internal(Vec<BuildNode<K, V>>),
    Leaf(V),
}

fn insert<K, V>(
    mut key: Vec<K>,
    v: V,
    current: &mut Vec<BuildNode<K, V>>,
) -> Result<(), &'static str>
where
    K: PartialEq + Ord,
{
    for n in current.iter_mut() {
        if key[0] == n.k {
            if key.len() <= 1 {
                return Err("duplicate entry for key");
            }

            key.remove(0);
            return match &mut n.data {
                BuildNodeData::Internal(nodes) => insert(key, v, nodes),
                BuildNodeData::Leaf(_) => Err("attempt to insert into value node"),
            };
        }
    }

    let k = key.remove(0);

    if key.is_empty() {
        current.push(BuildNode {
            k,
            data: BuildNodeData::Leaf(v),
        });
    } else {
        let mut children = vec![];
        insert(key, v, &mut children)?;
        current.push(BuildNode {
            k,
            data: BuildNodeData::Internal(children),
        });
    }

    Ok(())
}

fn flatten<K, V>(mut roots: Vec<BuildNode<K, V>>, nodes: &mut Vec<Node<K, V>>) -> (usize, usize)
where
    K: Clone + PartialEq + Ord,
    V: Clone,
{
    roots.sort_by(|l, r| l.k.cmp(&r.k));

    let child_start = nodes.len();
    let n_children = roots.len();

    let mut child_stack = Vec::new();

    // Insert roots first, storing any child nodes that need to be inserted later.
    for BuildNode { k, data } in roots.into_iter() {
        match data {
            BuildNodeData::Internal(children) => {
                let i = nodes.len();
                nodes.push(Node::new_internal(k, 0, children.len()));
                child_stack.push((i, children));
            }

            BuildNodeData::Leaf(v) => nodes.push(Node::new_leaf(k, v)),
        }
    }

    // Insert the child nodes for each root node, updating their state now that we know the offsets
    // of their children.
    for (i, children) in child_stack.into_iter() {
        let (start, _) = flatten(children, nodes);
        match &mut nodes[i] {
            Node {
                data: Data::Internal { child_start, .. },
                ..
            } => {
                *child_start = start;
            }

            _ => unreachable!(),
        }
    }

    (child_start, n_children)
}

/// A default handler for mapping a single length key to an `Option<V>`.
///
/// This is used to avoid having to specify large numbers of single length keys that should all be
/// handled in a similar way.
pub type DefaultMapping<K, V> = fn(&K) -> Option<V>;

/// The result of querying a [Trie] for a particular Key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryResult<V> {
    /// A leaf value associated with the key used in the query
    Val(V),
    /// The key used to query is a prefix to multiple values
    Partial,
    /// The key does not exist within the [Trie]
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

impl<V> QueryResult<V> {
    pub fn map<F, U>(self, f: F) -> QueryResult<U>
    where
        F: Fn(V) -> U,
    {
        match self {
            Self::Val(v) => QueryResult::Val(f(v)),
            Self::Partial => QueryResult::Partial,
            Self::Missing => QueryResult::Missing,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    #[test]
    fn duplicate_keys_errors() {
        assert!(Trie::try_from_iter(vec![(vec![42], 1), (vec![42], 2)]).is_err());
    }

    #[test]
    fn children_under_a_value_node_errors() {
        assert!(Trie::try_from_iter(vec![(vec![42], 1), (vec![42, 69], 2)]).is_err());
    }

    #[test_case("foo", QueryResult::Val(1); "val 1")]
    #[test_case("bar", QueryResult::Val(2); "val 2")]
    #[test_case("baz", QueryResult::Val(3); "val 3")]
    #[test_case("ba", QueryResult::Partial; "partial 1")] // typos:ignore
    #[test_case("fo", QueryResult::Partial; "partial 2")] // typos:ignore
    #[test_case("barf", QueryResult::Missing; "overshoot")]
    #[test_case("have you any wool?", QueryResult::Missing; "fully missing")]
    #[test]
    fn get_works(key: &str, expected: QueryResult<usize>) {
        let t = Trie::from_str_keys(vec![("foo", 1), ("bar", 2), ("baz", 3)]).unwrap();
        assert_eq!(t.get_str(key), expected);
    }

    #[test_case(&[42], None; "partial should be None")]
    #[test_case(&[144], None; "missing should be None")]
    #[test_case(&[42, 69, 144], None; "overshoot should be None")]
    #[test_case(&[42, 69], Some(1); "exact should be Some")]
    #[test]
    fn get_exact_works(key: &[usize], expected: Option<usize>) {
        let t = Trie::try_from_iter(vec![(vec![42, 69], 1)]).unwrap();
        assert_eq!(t.get_exact(key), expected);
    }

    #[test_case("fo", None; "partial")] // typos:ignore
    #[test_case("bar", None; "missing")]
    #[test_case("fool", None; "overshoot")]
    #[test_case("foo", Some(1); "found")]
    #[test]
    fn get_str_exact_works(key: &str, expected: Option<usize>) {
        let t = Trie::from_str_keys(vec![("foo", 1)]).unwrap();
        assert_eq!(t.get_str_exact(key), expected);
    }

    #[test]
    fn merge_works() {
        let t1 = Trie::from_str_keys(vec![("foo", 1), ("bar", 2)]).unwrap();
        let t2 = Trie::from_str_keys(vec![("baz", 3), ("qux", 4)]).unwrap();

        let merged = t1.merge(t2).unwrap();

        assert_eq!(merged.get_str_exact("foo"), Some(1));
        assert_eq!(merged.get_str_exact("bar"), Some(2));
        assert_eq!(merged.get_str_exact("baz"), Some(3));
        assert_eq!(merged.get_str_exact("qux"), Some(4));
        assert_eq!(merged.len(), 4);
    }

    #[test]
    fn merge_conflicts_error() {
        let t1 = Trie::from_str_keys(vec![("foo", 1)]).unwrap();
        let t2 = Trie::from_str_keys(vec![("foo", 2)]).unwrap();

        assert!(t1.merge(t2).is_err());
    }

    #[test]
    fn merge_overriding_works() {
        let t1 = Trie::from_str_keys(vec![("foo", 1), ("bar", 2)]).unwrap();
        let t2 = Trie::from_str_keys(vec![("baz", 3), ("foo", 4)]).unwrap();

        let merged = t1.merge_overriding(t2).unwrap();

        assert_eq!(merged.get_str_exact("foo"), Some(4));
        assert_eq!(merged.get_str_exact("bar"), Some(2));
        assert_eq!(merged.get_str_exact("baz"), Some(3));
        assert_eq!(merged.len(), 3);
    }

    #[test]
    fn merge_overriding_conflicts_are_ok() {
        let t1 = Trie::from_str_keys(vec![("foo", 1)]).unwrap();
        let t2 = Trie::from_str_keys(vec![("foo", 2)]).unwrap();

        assert!(t1.merge_overriding(t2).is_ok());
    }

    fn usize_default_handler(n: &usize) -> Option<usize> {
        Some(n + 1)
    }

    #[test_case(&[42], QueryResult::Val(1); "exact single should match from the Trie")]
    #[test_case(&[12, 13], QueryResult::Val(2); "exact multi should match from the Trie")]
    #[test_case(&[69], QueryResult::Val(70); "missing single should be defaulted")]
    #[test_case(&[69, 420], QueryResult::Missing; "missing multi should remain missing")]
    #[test_case(&[12], QueryResult::Partial; "partial should remain partial")]
    #[test]
    fn get_uses_default_correctly(k: &[usize], expected: QueryResult<usize>) {
        let mut t = Trie::try_from_iter(vec![(vec![42], 1), (vec![12, 13], 2)]).unwrap();
        t.set_default(usize_default_handler);

        assert_eq!(t.get(k), expected);
    }

    #[test_case(&[42], Some(1); "exact single should match from the Trie")]
    #[test_case(&[12, 13], Some(2); "exact multi should match from the Trie")]
    #[test_case(&[69], Some(70); "missing single should be defaulted")]
    #[test_case(&[69, 420], None; "missing multi should remain None")]
    #[test_case(&[12], None; "partial should remain None")]
    #[test]
    fn get_exact_uses_default_correctly(k: &[usize], expected: Option<usize>) {
        let mut t = Trie::try_from_iter(vec![(vec![42], 1), (vec![12, 13], 2)]).unwrap();
        t.set_default(usize_default_handler);

        assert_eq!(t.get_exact(k), expected);
    }
}
