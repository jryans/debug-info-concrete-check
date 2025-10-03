use std::collections::{HashSet, VecDeque};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

use once_cell::sync::Lazy;
use regex::Regex;

/// Computes 1-based depth of a single line.
/// Assumes 2 space indentation is used.
// JRS: This should probably move to the `Trace` module
pub fn line_depth(line: &str) -> usize {
    static INDENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ *").unwrap());
    INDENT_RE.captures(line).map_or(0, |c| c[0].len() / 2) + 1
}

/// A separate `Root` value is reserved for the root node.
/// This allows node indices to remain in sync with those for the separate data array.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum TreeNodeIndex {
    Root,
    Node(usize),
}

impl TreeNodeIndex {
    pub fn unwrap(self) -> usize {
        match self {
            TreeNodeIndex::Node(i) => i,
            TreeNodeIndex::Root => panic!("Called `TreeNodeIndex::unwrap` on `Root`"),
        }
    }

    pub fn is_root(&self) -> bool {
        match self {
            TreeNodeIndex::Node(_) => false,
            TreeNodeIndex::Root => true,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TreeNode {
    pub(crate) index: TreeNodeIndex,
    pub(crate) parent: Option<TreeNodeIndex>,
    pub(crate) children: Vec<TreeNodeIndex>,
}

impl TreeNode {
    pub fn new(index: usize, parent: TreeNodeIndex) -> TreeNode {
        TreeNode {
            index: TreeNodeIndex::Node(index),
            parent: Some(parent),
            children: Vec::new(),
        }
    }

    pub fn new_root() -> TreeNode {
        TreeNode {
            index: TreeNodeIndex::Root,
            parent: None,
            children: Vec::new(),
        }
    }

    pub fn parent<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        self.parent.as_ref().map(|index| &tree[index])
    }

    pub fn child<'tree>(&self, tree: &'tree Tree, nth: usize) -> Option<&'tree TreeNode> {
        self.children.get(nth).map(|index| &tree[index])
    }

    pub fn child_position<'tree>(&self, index: &TreeNodeIndex) -> Option<usize> {
        self.children
            .iter()
            .position(|child_index| child_index == index)
    }

    pub fn first_child<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        self.children.first().map(|index| &tree[index])
    }

    pub fn last_child<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        self.children.last().map(|index| &tree[index])
    }

    pub fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    pub fn is_branch(&self) -> bool {
        !self.is_leaf()
    }

    pub fn data<'container, T>(&self, container: &'container [T]) -> &'container T {
        match self.index {
            TreeNodeIndex::Node(i) => &container[i],
            TreeNodeIndex::Root => unimplemented!(),
        }
    }

    pub fn data_mut<'container, T>(&self, container: &'container mut [T]) -> &'container mut T {
        match self.index {
            TreeNodeIndex::Node(i) => &mut container[i],
            TreeNodeIndex::Root => unimplemented!(),
        }
    }

    pub fn bfs<'tree>(&'tree self, tree: &'tree Tree) -> TreeBfs {
        TreeBfs {
            tree,
            queue: VecDeque::from([self]),
        }
    }

    pub fn dfs_pre_order<'tree>(&'tree self, tree: &'tree Tree) -> TreeDfsPreOrder {
        TreeDfsPreOrder {
            tree,
            stack: Vec::from([self]),
        }
    }

    pub fn dfs_post_order<'tree>(&'tree self, tree: &'tree Tree) -> TreeDfsPostOrder {
        TreeDfsPostOrder {
            tree,
            stack: Vec::from([self]),
            visited: HashSet::new(),
        }
    }

    pub fn subtree_len<'tree>(&'tree self, tree: &'tree Tree) -> usize {
        self.dfs_pre_order(tree).count()
    }
}

/// Tree built from / overlaid onto a separate array.
/// Node data is accessed by indexing into that array.
#[derive(Clone, Debug)]
pub struct Tree {
    root: TreeNode,
    /// All non-root nodes.
    /// `nodes` indices correspond to the same items as those
    /// with the same index in the separate array.
    pub(crate) nodes: Vec<TreeNode>,
}

impl Tree {
    pub fn new() -> Tree {
        let root = TreeNode::new_root();
        Tree {
            root,
            nodes: Vec::new(),
        }
    }

    pub fn root_index() -> TreeNodeIndex {
        TreeNodeIndex::Root
    }

    pub fn root(&self) -> &TreeNode {
        &self[&Tree::root_index()]
    }

    pub fn register(&mut self, node: TreeNode) -> TreeNodeIndex {
        let index = node.index;
        self.nodes.push(node);
        index
    }

    /// Build a tree from indented items
    // JRS: This should probably move to the `Trace` module
    pub fn from_indented_items(items: &[&str]) -> Tree {
        let mut tree = Tree::new();

        if items.is_empty() {
            return tree;
        }
        assert!(line_depth(items[0]) == 1);

        // Temporary stack tracking indentation as we build the tree
        let mut stack: Vec<TreeNodeIndex> = Vec::new();
        stack.push(Tree::root_index());

        for i in 0..items.len() {
            let item = items[i];
            let item_depth = line_depth(item);
            let stack_depth = stack.len();
            if item_depth > stack_depth {
                assert!(item_depth == stack_depth + 1);
                let stack_top = &tree[stack.last().unwrap()];
                assert!(stack_top.children.len() > 0);
                stack.push(stack_top.last_child(&tree).unwrap().index);
            } else if item_depth < stack_depth {
                for _ in 0..(stack_depth - item_depth) {
                    stack.pop();
                }
            }
            // TODO: Move this into `push_child` on `TreeNode` somehow
            let stack_top_index = tree[stack.last().unwrap()].index;
            let node = TreeNode::new(i, stack_top_index);
            let node_index = tree.register(node);
            let stack_top = &mut tree[stack.last().unwrap()];
            stack_top.children.push(node_index);
        }

        tree
    }

    pub fn bfs(&self) -> TreeBfs {
        TreeBfs {
            tree: self,
            queue: VecDeque::from([self.root()]),
        }
    }

    pub fn bfs_mut(&mut self) -> TreeBfsMut {
        TreeBfsMut {
            tree: self,
            queue: VecDeque::from([TreeBfsMutStep::VisitNode(Tree::root_index())]),
        }
    }

    pub fn dfs_pre_order(&self) -> TreeDfsPreOrder {
        TreeDfsPreOrder {
            tree: self,
            stack: Vec::from([self.root()]),
        }
    }

    pub fn dfs_pre_order_with_depth(&self) -> TreeDfsPreOrderWithDepth {
        TreeDfsPreOrderWithDepth {
            tree: self,
            // Depth is 1-based for actual events,
            // so root is one less than that.
            stack: Vec::from([(self.root(), 0)]),
        }
    }

    pub fn dfs_post_order(&self) -> TreeDfsPostOrder {
        TreeDfsPostOrder {
            tree: self,
            stack: Vec::from([self.root()]),
            visited: HashSet::new(),
        }
    }
}

impl Index<&TreeNodeIndex> for Tree {
    type Output = TreeNode;

    fn index(&self, index: &TreeNodeIndex) -> &Self::Output {
        match *index {
            TreeNodeIndex::Node(i) => &self.nodes[i],
            TreeNodeIndex::Root => &self.root,
        }
    }
}

impl IndexMut<&TreeNodeIndex> for Tree {
    fn index_mut(&mut self, index: &TreeNodeIndex) -> &mut Self::Output {
        match *index {
            TreeNodeIndex::Node(i) => &mut self.nodes[i],
            TreeNodeIndex::Root => &mut self.root,
        }
    }
}

pub struct TreeBfs<'tree> {
    tree: &'tree Tree,
    queue: VecDeque<&'tree TreeNode>,
}

impl<'tree> Iterator for TreeBfs<'tree> {
    type Item = &'tree TreeNode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            return None;
        }

        while !self.queue.is_empty() {
            // Pop from the back of the queue
            let node = self.queue.pop_back().unwrap();
            if node.is_leaf() {
                // If `node` is a leaf, stop here for now
                return Some(node);
            } else {
                // If `node` is a branch, push all children into the front of the queue
                for i in 0..node.children.len() {
                    self.queue.push_front(node.child(&self.tree, i).unwrap());
                }
                // For all non-root branches, stop here for now
                if node.index.is_root() {
                    continue;
                }
                return Some(node);
            }
        }

        return None;
    }
}

#[derive(Debug)]
enum TreeBfsMutStep {
    VisitNode(TreeNodeIndex),
    CollectChildren(TreeNodeIndex),
}

pub struct TreeBfsMut<'tree> {
    tree: &'tree mut Tree,
    queue: VecDeque<TreeBfsMutStep>,
}

impl<'tree> Iterator for TreeBfsMut<'tree> {
    type Item = (&'tree mut Tree, TreeNodeIndex);

    fn next(&mut self) -> Option<Self::Item> {
        if self.queue.is_empty() {
            return None;
        }

        while !self.queue.is_empty() {
            // Pop from the back of the queue
            let step = self.queue.pop_back().unwrap();
            match step {
                TreeBfsMutStep::VisitNode(node_index) => {
                    let node = &self.tree[&node_index];
                    if node.is_leaf() {
                        // If `node` is a leaf, stop here for now
                        // Unsafe: `tree` reference outlives the iterator,
                        // but can't see how to express that in safe Rust.
                        let tree: &mut Tree = unsafe { &mut *(self.tree as *mut Tree) };
                        return Some((tree, node_index));
                    } else {
                        // If `node` is a branch, push a future step to gather children.
                        // This allows visiting the current node to potentially change its children
                        // and see that reflected in the same iteration round (as long as they
                        // don't move up the tree to levels we've already visited).
                        self.queue
                            .push_front(TreeBfsMutStep::CollectChildren(node_index));
                        // For all non-root branches, stop here for now
                        if node_index.is_root() {
                            continue;
                        }
                        // Unsafe: `tree` reference outlives the iterator,
                        // but can't see how to express that in safe Rust.
                        let tree: &mut Tree = unsafe { &mut *(self.tree as *mut Tree) };
                        return Some((tree, node_index));
                    }
                }
                TreeBfsMutStep::CollectChildren(node_index) => {
                    // Now that this step has made it to the top of the queue,
                    // we finally commit to the current list of children for this node
                    let node = &self.tree[&node_index];
                    for i in (0..node.children.len()).rev() {
                        self.queue
                            .push_back(TreeBfsMutStep::VisitNode(node.children[i]));
                    }
                }
            }
        }

        return None;
    }
}

pub struct TreeDfsPreOrder<'tree> {
    tree: &'tree Tree,
    stack: Vec<&'tree TreeNode>,
}

impl<'tree> Iterator for TreeDfsPreOrder<'tree> {
    type Item = &'tree TreeNode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            return None;
        }

        while !self.stack.is_empty() {
            // Pop from the end of the stack
            let node = self.stack.pop().unwrap();
            if node.is_leaf() {
                // If `node` is a leaf, stop here for now
                return Some(node);
            } else {
                // If `node` is a branch, push all children (in reverse for expected ordering)
                for i in (0..node.children.len()).rev() {
                    self.stack.push(node.child(&self.tree, i).unwrap());
                }
                // For all non-root branches, stop here for now
                if node.index.is_root() {
                    continue;
                }
                return Some(node);
            }
        }

        return None;
    }
}

pub struct TreeDfsPreOrderWithDepth<'tree> {
    tree: &'tree Tree,
    stack: Vec<(&'tree TreeNode, usize)>,
}

impl<'tree> Iterator for TreeDfsPreOrderWithDepth<'tree> {
    type Item = (&'tree TreeNode, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            return None;
        }

        while !self.stack.is_empty() {
            // Pop from the end of the stack
            let (node, depth) = self.stack.pop().unwrap();
            if node.is_leaf() {
                // If `node` is a leaf, stop here for now
                return Some((node, depth));
            } else {
                // If `node` is a branch, push all children (in reverse for expected ordering)
                for i in (0..node.children.len()).rev() {
                    self.stack
                        .push((node.child(&self.tree, i).unwrap(), depth + 1));
                }
                // For all non-root branches, stop here for now
                if node.index.is_root() {
                    continue;
                }
                return Some((node, depth));
            }
        }

        return None;
    }
}

pub struct TreeDfsPostOrder<'tree> {
    tree: &'tree Tree,
    stack: Vec<&'tree TreeNode>,
    visited: HashSet<TreeNodeIndex>,
}

impl<'tree> Iterator for TreeDfsPostOrder<'tree> {
    type Item = &'tree TreeNode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            return None;
        }

        while !self.stack.is_empty() {
            // Pop from the end of the stack
            let node = self.stack.pop().unwrap();
            if node.is_leaf() {
                // If `node` is a leaf, stop here for now
                return Some(node);
            } else {
                // If `node` is a branch, first check if we've seen it before
                if self.visited.contains(&node.index) {
                    // For all non-root branches, stop here for now
                    if node.index.is_root() {
                        continue;
                    }
                    return Some(node);
                } else {
                    // Mark the branch as visited, then push for visiting after children
                    self.visited.insert(node.index);
                    self.stack.push(node);
                }
                // Push all children (in reverse for expected ordering)
                for i in (0..node.children.len()).rev() {
                    self.stack.push(node.child(&self.tree, i).unwrap());
                }
            }
        }

        return None;
    }
}

// TODO: Add `leaves` and `branches` filters to all tree iterators

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tree_from_indented_items() {
        let items: Vec<_> = "
0
  0.0
    0.0.0
  0.1
  0.2
1
  1.0
    1.0.0"
            .trim()
            .lines()
            .collect();
        let tree = Tree::from_indented_items(&items);
        let root = &tree.root;
        let node_0 = root.child(&tree, 0).unwrap();
        assert_eq!(node_0.data(&items).trim(), "0");
        let node_0_1 = node_0.child(&tree, 1).unwrap();
        assert_eq!(node_0_1.data(&items).trim(), "0.1");
        let node_1 = root.child(&tree, 1).unwrap();
        let node_1_0 = node_1.child(&tree, 0).unwrap();
        assert_eq!(node_1_0.data(&items).trim(), "1.0");
        let node_1_0_parent = node_1_0.parent(&tree).unwrap();
        assert_eq!(node_1_0_parent, node_1);
    }

    #[test]
    fn tree_bfs() {
        let items: Vec<_> = "
0
  0.0
    0.0.0
  0.1
  0.2
1
  1.0
    1.0.0"
            .trim()
            .lines()
            .collect();
        let tree = Tree::from_indented_items(&items);
        let leaves: Vec<&str> = tree.bfs().map(|node| node.data(&items).trim()).collect();
        assert_eq!(
            leaves,
            vec!["0", "1", "0.0", "0.1", "0.2", "1.0", "0.0.0", "1.0.0"]
        );
    }

    #[test]
    fn tree_dfs_pre_order_leaves() {
        let items: Vec<_> = "
0
  0.0
    0.0.0
  0.1
  0.2
1
  1.0
    1.0.0"
            .trim()
            .lines()
            .collect();
        let tree = Tree::from_indented_items(&items);
        let leaves: Vec<&str> = tree
            .dfs_pre_order()
            .filter(|node| node.is_leaf())
            .map(|node| node.data(&items).trim())
            .collect();
        assert_eq!(leaves, vec!["0.0.0", "0.1", "0.2", "1.0.0"]);
    }

    #[test]
    fn tree_dfs_pre_order_branches() {
        let items: Vec<_> = "
0
  0.0
    0.0.0
  0.1
  0.2
1
  1.0
    1.0.0"
            .trim()
            .lines()
            .collect();
        let tree = Tree::from_indented_items(&items);
        let branches: Vec<&str> = tree
            .dfs_pre_order()
            .filter(|node| node.is_branch())
            .map(|node| node.data(&items).trim())
            .collect();
        assert_eq!(branches, vec!["0", "0.0", "1", "1.0"]);
    }

    #[test]
    fn tree_dfs_pre_order_with_depth() {
        let items: Vec<_> = "
0
  0.0
    0.0.0
  0.1
  0.2
1
  1.0
    1.0.0"
            .trim()
            .lines()
            .collect();
        let tree = Tree::from_indented_items(&items);
        let depths: Vec<usize> = tree
            .dfs_pre_order_with_depth()
            .map(|(_, depth)| depth)
            .collect();
        assert_eq!(depths, vec![1, 2, 3, 2, 2, 1, 2, 3]);
    }

    #[test]
    fn tree_dfs_post_order_branches() {
        let items: Vec<_> = "
0
  0.0
    0.0.0
  0.1
  0.2
1
  1.0
    1.0.0"
            .trim()
            .lines()
            .collect();
        let tree = Tree::from_indented_items(&items);
        let branches: Vec<&str> = tree
            .dfs_post_order()
            .filter(|node| node.is_branch())
            .map(|node| node.data(&items).trim())
            .collect();
        assert_eq!(branches, vec!["0.0", "0", "1.0", "1"]);
    }
}
