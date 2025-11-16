//! A simple trie data structure for supporting key bindings and autocompletions in a composible
//! way with the rest of the ad internal APIs.
use std::{collections::BTreeMap, fmt, ops::Range, sync::Arc};

/// A singly initialised Trie mapping key sequences to a value.
///
/// It is not permitted for values to be mapped to a key that is a prefix of another key also
/// existing in the same Trie.
///
/// There are convenience methods provided for `Trie<char, V>` for when &str values are used as keys.
#[derive(Clone, PartialEq, Eq)]
pub struct Trie<K, V>
where
    K: Clone + PartialEq + Ord,
    V: Clone,
{
    nodes: Arc<[Node<K>]>,
    values: Arc<[V]>,
    n_roots: usize,
}

impl<K, V> fmt::Debug for Trie<K, V>
where
    K: Clone + PartialEq + Ord + fmt::Debug,
    V: Clone + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Trie")
            .field("nodes", &self.nodes)
            .field("values", &self.values)
            .field("n_roots", &self.n_roots)
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
            nodes: Arc::from(Vec::new()),
            values: Arc::from(Vec::new()),
            n_roots: 0,
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
        let mut values = Vec::new();
        let (_, n_roots) = flatten(roots, &mut nodes, &mut values);

        Ok(Trie {
            nodes: Arc::from(nodes),
            values: Arc::from(values),
            n_roots,
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

            match node.data {
                Data::Leaf { i } => pairs.push((child_key, self.values[i].clone())),

                Data::Internal {
                    child_start,
                    n_children,
                } => self.extract_pairs(pairs, child_key, child_start..child_start + n_children),
            }
        }
    }

    /// Query this [Trie] for a given key or key prefix
    ///
    /// If the key maps to a leaf then the value is returned, if it maps to a sub-trie then
    /// `Partial` is returned to denote that the given key is a parent of one or more values. If
    /// the key is not found within the `Trie` then `Missing` is returned.
    pub fn get<'a>(&'a self, key: &[K]) -> QueryResult<'a, V> {
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

                    match node.data {
                        Data::Leaf { i } => {
                            return if key_index == key.len() {
                                QueryResult::Val(&self.values[i])
                            } else {
                                QueryResult::Missing
                            };
                        }

                        Data::Internal {
                            child_start,
                            n_children,
                        } => {
                            indices = child_start..child_start + n_children;
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

    /// Query this [Trie] for a given key or key prefix requiring the key to match exactly.
    ///
    /// If the key maps to a leaf then the `Some(value)` is returned, otherwise `None`.
    pub fn get_exact<'a>(&'a self, key: &[K]) -> Option<&'a V> {
        self.get(key).into()
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
    pub fn get_str<'a>(&'a self, key: &str) -> QueryResult<'a, V> {
        self.get(&key.chars().collect::<Vec<_>>())
    }

    /// Query this [Trie] using a string key.
    ///
    /// Only fll matches will be returned.
    pub fn get_str_exact<'a>(&'a self, key: &str) -> Option<&'a V> {
        self.get_exact(&key.chars().collect::<Vec<_>>())
    }
}

/// A single node within a [Trie].
///
/// Contains the last element of the key that traverses down to this node alongside [Data] that
/// identifies this node as being internal or a leaf.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Node<K>
where
    K: Clone + PartialEq + Ord,
{
    key: K,
    data: Data,
}

impl<K> Node<K>
where
    K: Clone + PartialEq + PartialOrd + Ord,
{
    fn new_internal(key: K, child_start: usize, n_children: usize) -> Self {
        Self {
            key,
            data: Data::Internal {
                child_start,
                n_children,
            },
        }
    }

    fn new_leaf(key: K, i: usize) -> Self {
        Self {
            key,
            data: Data::Leaf { i },
        }
    }

    fn is_leaf(&self) -> bool {
        matches!(self.data, Data::Leaf { .. })
    }
}

/// The internal data held at each node in a Trie.
///
/// Internal nodes are "pointers" to their children while leaves hold the value associated with the
/// full key used to traverse down to them.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Data {
    Internal {
        /// Index of the first child node
        child_start: usize,
        /// Number of child nodes
        n_children: usize,
    },
    Leaf {
        // Index of the value associated with the full key-path down to this node
        i: usize,
    },
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

fn flatten<K, V>(
    mut roots: Vec<BuildNode<K, V>>,
    nodes: &mut Vec<Node<K>>,
    values: &mut Vec<V>,
) -> (usize, usize)
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

            BuildNodeData::Leaf(v) => {
                let i = values.len();
                values.push(v);
                nodes.push(Node::new_leaf(k, i))
            }
        }
    }

    // Insert the child nodes for each root node, updating their state now that we know the offsets
    // of their children.
    for (i, children) in child_stack.into_iter() {
        let (start, _) = flatten(children, nodes, values);
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
pub enum QueryResult<'a, V> {
    /// A leaf value associated with the key used in the query
    Val(&'a V),
    /// The key used to query is a prefix to multiple values
    Partial,
    /// The key does not exist within the [Trie]
    Missing,
}

impl<'a, V> From<Option<&'a V>> for QueryResult<'a, V> {
    fn from(opt: Option<&'a V>) -> Self {
        match opt {
            Some(v) => QueryResult::Val(v),
            None => QueryResult::Missing,
        }
    }
}

impl<'a, V> From<QueryResult<'a, V>> for Option<&'a V> {
    fn from(q: QueryResult<'a, V>) -> Self {
        match q {
            QueryResult::Val(v) => Some(v),
            _ => None,
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

    #[test_case("foo", QueryResult::Val(&1); "val 1")]
    #[test_case("bar", QueryResult::Val(&2); "val 2")]
    #[test_case("baz", QueryResult::Val(&3); "val 3")]
    #[test_case("ba", QueryResult::Partial; "partial 1")] // typos:ignore
    #[test_case("fo", QueryResult::Partial; "partial 2")] // typos:ignore
    #[test_case("barf", QueryResult::Missing; "overshoot")]
    #[test_case("have you any wool?", QueryResult::Missing; "fully missing")]
    #[test]
    fn get_works(key: &str, expected: QueryResult<'_, usize>) {
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
        assert_eq!(t.get_exact(key), expected.as_ref());
    }

    #[test_case("fo", None; "partial")] // typos:ignore
    #[test_case("bar", None; "missing")]
    #[test_case("fool", None; "overshoot")]
    #[test_case("foo", Some(1); "found")]
    #[test]
    fn get_str_exact_works(key: &str, expected: Option<usize>) {
        let t = Trie::from_str_keys(vec![("foo", 1)]).unwrap();
        assert_eq!(t.get_str_exact(key), expected.as_ref());
    }

    #[test]
    fn merge_works() {
        let t1 = Trie::from_str_keys(vec![("foo", 1), ("bar", 2)]).unwrap();
        let t2 = Trie::from_str_keys(vec![("baz", 3), ("qux", 4)]).unwrap();

        let merged = t1.merge(t2).unwrap();

        assert_eq!(merged.get_str_exact("foo"), Some(&1));
        assert_eq!(merged.get_str_exact("bar"), Some(&2));
        assert_eq!(merged.get_str_exact("baz"), Some(&3));
        assert_eq!(merged.get_str_exact("qux"), Some(&4));
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

        assert_eq!(merged.get_str_exact("foo"), Some(&4));
        assert_eq!(merged.get_str_exact("bar"), Some(&2));
        assert_eq!(merged.get_str_exact("baz"), Some(&3));
        assert_eq!(merged.len(), 3);
    }

    #[test]
    fn merge_overriding_conflicts_are_ok() {
        let t1 = Trie::from_str_keys(vec![("foo", 1)]).unwrap();
        let t2 = Trie::from_str_keys(vec![("foo", 2)]).unwrap();

        assert!(t1.merge_overriding(t2).is_ok());
    }
}
