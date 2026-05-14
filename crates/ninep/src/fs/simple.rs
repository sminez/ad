//! A minimal file tree data structure that can be used to implement simple 9p filesystems
use jiff::Timestamp;

use crate::{
    Result,
    fs::{FileType, Perm, QID_ROOT, Qid, Stat, WStat},
    sansio::server::{E_CREATE_NON_DIR, E_ILLEGAL_CREATE_NAME, E_UNKNOWN_FILE},
};
use parking_lot::RwLock;
use std::{collections::BTreeMap, sync::Arc};

const E_ALREADY_EXISTS: &str = "file already exists";

/// A minimal file tree implementation that can be used to implement a simple 9p file server.
#[derive(Debug, Clone)]
pub struct FileTree<T>
where
    T: Send + Sync + 'static,
{
    nodes: Arc<RwLock<Nodes<T>>>,
}

impl<T> FileTree<T>
where
    T: Send + Sync + 'static,
{
    /// Create a new [FileTree] with a root node using the given details.
    pub fn new(owner: &str, group: &str, perms: Perm, aux: T) -> Self {
        Self {
            nodes: Arc::new(RwLock::new(Nodes::new(owner, group, perms, aux))),
        }
    }

    fn with_nodes<F, U>(&self, f: F) -> U
    where
        F: FnOnce(&Nodes<T>) -> U,
    {
        f(&self.nodes.read())
    }

    fn with_nodes_mut<F, U>(&self, f: F) -> U
    where
        F: FnOnce(&mut Nodes<T>) -> U,
    {
        f(&mut self.nodes.write())
    }

    /// Add a new node to the tree, returning its `qid`.
    ///
    /// Errors if `parent` is not a known node.
    pub fn try_add_node(
        &self,
        parent: u64,
        name: &str,
        perms: Perm,
        ty: FileType,
        aux: T,
    ) -> Result<Qid> {
        self.with_nodes_mut(|nodes| nodes.try_insert(parent, name, perms, ty, aux))
    }

    /// Add a new node to the tree, returning its `qid`.
    ///
    /// Errors if `parent` is not a known node.
    ///
    /// Panics if `qid` is already in the tree.
    pub fn try_add_node_with_qid(
        &self,
        parent: u64,
        qid: u64,
        name: &str,
        perms: Perm,
        ty: FileType,
        aux: T,
    ) -> Result<Qid> {
        self.with_nodes_mut(|nodes| {
            if nodes.entries.contains_key(&qid) || qid <= nodes.next_qid {
                panic!("qid={qid} already exists within this tree");
            }

            nodes.try_insert_with_qid(parent, qid, name, perms, ty, aux)
        })
    }

    /// Remove a node and all of its children from the tree.
    pub fn remove(&self, qid: u64) {
        self.with_nodes_mut(|nodes| nodes.remove(qid))
    }

    /// Attempt to map a path within this file tree to a qid.
    ///
    /// Returns `Some(qid)` for a known path, otherwise `None`.
    pub fn qid_for_path(&self, path: &str) -> Option<u64> {
        if !path.starts_with('/') {
            return None;
        }

        let mut qid = 0;

        // Need to skip the empty string from the leading slash
        for elem in path.split('/').skip(1) {
            qid = self.walk_one(qid, elem).ok()?.path;
        }

        Some(qid)
    }

    /// Whether or not this file tree contains the given qid
    pub fn contains_qid(&self, qid: u64) -> bool {
        self.with_nodes(|nodes| nodes.entries.contains_key(&qid))
    }

    /// Run a closure with access to the [File] associated with the given `qid`.
    pub fn with_file<F, U>(&self, qid: u64, f: F) -> Result<U>
    where
        F: FnOnce(&File<T>) -> U,
    {
        self.with_nodes(|nodes| nodes.entries.get(&qid).map(f))
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())
    }

    /// Run a closure with mutable access to the [File] associated with the given `qid`.
    pub fn with_file_mut<F, U>(&self, qid: u64, f: F) -> Result<U>
    where
        F: FnOnce(&mut File<T>) -> U,
    {
        self.with_nodes_mut(|nodes| nodes.entries.get_mut(&qid).map(f))
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())
    }

    /// Calls a closure on each [File] contained within this [FileTree].
    pub fn for_each_file<F>(&self, f: F)
    where
        F: FnMut(&mut File<T>),
    {
        self.with_nodes_mut(|nodes| nodes.entries.values_mut().for_each(f))
    }

    /// Walk from `parent_qid` to `child` (see [Serve9p::walk_one][0]).
    ///
    /// [0]: crate::sync::server::Serve9p::walk_one
    pub fn walk_one(&self, parent_qid: u64, child: &str) -> Result<Qid> {
        self.with_nodes(|nodes| nodes.walk_one(parent_qid, child))
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())
    }

    /// Request the [Stat] for the given `qid` (see [Serve9p::stat][0]).
    ///
    /// [0]: crate::sync::server::Serve9p::stat
    pub fn stat(&self, qid: u64) -> Result<Stat> {
        self.with_nodes(|nodes| nodes.stat(qid))
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())
    }

    /// Handle a [Serve9p::read_dir][0] request for `qid`.
    ///
    /// [0]: crate::sync::server::Serve9p::read_dir
    pub fn read_dir(&self, qid: u64) -> Result<Vec<Stat>> {
        self.with_nodes(|nodes| {
            if !nodes.entries.contains_key(&qid) {
                return Err(E_UNKNOWN_FILE.to_string());
            }

            Ok(nodes.read_dir(qid).unwrap_or_default())
        })
    }
}

/// A simple file implementation for use in a [FileTree].
#[derive(Debug, Clone)]
pub struct File<T> {
    parent: Option<u64>,
    /// The stat associated with this [File] node.
    pub stat: Stat,
    /// User defined additional data per [File] node.
    pub aux: T,
}

impl<T> File<T> {
    fn new(
        qid: Qid,
        name: &str,
        owner: &str,
        group: &str,
        perms: Perm,
        aux: T,
        parent: Option<u64>,
    ) -> Self {
        File {
            stat: Stat {
                qid,
                name: name.into(),
                owner: owner.into(),
                group: group.into(),
                perms,
                n_bytes: 0,
                last_accessed: Timestamp::now(),
                last_modified: Timestamp::now(),
                last_modified_by: owner.into(),
            },
            aux,
            parent,
        }
    }

    /// The `qid` of the parent node for this file.
    ///
    /// Returns [None] for the root node.
    pub fn parent(&self) -> Option<u64> {
        self.parent
    }

    /// Attempt to apply a [WStat] to the [Stat] of this file.
    ///
    /// See [WStat::try_apply] for more details.
    pub fn try_apply_wstat(&mut self, wstat: WStat) -> std::result::Result<(), Box<WStat>> {
        match wstat.try_apply(&self.stat) {
            Ok(new) => self.stat = new,
            Err(wstat) => return Err(wstat),
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Nodes<T>
where
    T: Send + Sync + 'static,
{
    entries: BTreeMap<u64, File<T>>,
    children: BTreeMap<u64, Vec<u64>>,
    next_qid: u64,
}

impl<T> Nodes<T>
where
    T: Send + Sync + 'static,
{
    fn new(owner: &str, group: &str, perms: Perm, aux: T) -> Self {
        let root = File::new(Qid::dir(QID_ROOT), "/", owner, group, perms, aux, None);

        Self {
            entries: BTreeMap::from_iter([(QID_ROOT, root)]),
            children: BTreeMap::new(),
            next_qid: 1,
        }
    }

    fn try_insert(
        &mut self,
        parent: u64,
        name: &str,
        perms: Perm,
        ty: FileType,
        aux: T,
    ) -> Result<Qid> {
        let qid = self.try_insert_with_qid(parent, self.next_qid + 1, name, perms, ty, aux)?;
        self.next_qid += 1;

        Ok(qid)
    }

    fn try_insert_with_qid(
        &mut self,
        parent: u64,
        qid_path: u64,
        name: &str,
        perms: Perm,
        ty: FileType,
        aux: T,
    ) -> Result<Qid> {
        if name == "." || name == ".." {
            return Err(E_ILLEGAL_CREATE_NAME.to_string());
        }

        let pstat = &self
            .entries
            .get(&parent)
            .ok_or_else(|| E_UNKNOWN_FILE.to_string())?
            .stat
            .clone();

        if pstat.qid.ty != FileType::DIRECTORY {
            return Err(E_CREATE_NON_DIR.to_string());
        }

        if let Some(siblings) = self.children.get(&parent) {
            for qid in siblings.iter() {
                if name == self.entries.get(qid).unwrap().stat.name {
                    return Err(E_ALREADY_EXISTS.to_string());
                }
            }
        }

        let perms = perms.apply_create_mask(pstat.perms);

        let qid = Qid {
            ty,
            version: 0,
            path: qid_path,
        };

        self.entries.insert(
            qid_path,
            File::new(
                qid,
                name,
                &pstat.owner,
                &pstat.group,
                perms,
                aux,
                Some(parent),
            ),
        );
        self.children
            .entry(pstat.qid.path)
            .or_default()
            .push(qid_path);

        Ok(qid)
    }

    fn remove(&mut self, qid: u64) {
        let mut to_remove = vec![qid];

        while let Some(qid) = to_remove.pop() {
            self.entries.remove(&qid);
            for child_list in self.children.values_mut() {
                child_list.retain(|child| *child != qid);
            }
            if let Some(children) = self.children.remove(&qid) {
                to_remove.extend(children);
            }
        }
    }

    fn walk_one(&self, parent_qid: u64, child: &str) -> Option<Qid> {
        for qid in self.children.get(&parent_qid)?.iter() {
            let child_node = self.entries.get(qid)?;
            if child_node.stat.name == child {
                return Some(child_node.stat.qid);
            }
        }

        None
    }

    fn stat(&self, qid: u64) -> Option<Stat> {
        Some(self.entries.get(&qid)?.stat.clone())
    }

    fn read_dir(&self, qid: u64) -> Option<Vec<Stat>> {
        let children = self.children.get(&qid)?;
        let mut stats = Vec::with_capacity(children.len());
        for qid in children.iter() {
            stats.push(self.entries.get(qid)?.stat.clone());
        }

        Some(stats)
    }
}
