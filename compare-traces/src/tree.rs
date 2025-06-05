use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::ops::{Index, IndexMut};

use bimap::BiHashMap;
use indexmap::IndexMap;
use similar::{capture_diff_slices, DiffOp, TextDiff};

use crate::event::{line_depth, Event, Eventable};

/// A separate `Root` value is reserved for the root node.
/// This allows node indices to remain in sync with those for the separate data array.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum TreeNodeIndex {
    Root,
    Node(usize),
}

#[derive(PartialEq, Eq, Debug)]
struct TreeNode {
    index: TreeNodeIndex,
    parent: Option<TreeNodeIndex>,
    children: Vec<TreeNodeIndex>,
}

impl TreeNode {
    fn new(index: usize, parent: TreeNodeIndex) -> TreeNode {
        TreeNode {
            index: TreeNodeIndex::Node(index),
            parent: Some(parent),
            children: Vec::new(),
        }
    }

    fn new_root() -> TreeNode {
        TreeNode {
            index: TreeNodeIndex::Root,
            parent: None,
            children: Vec::new(),
        }
    }

    fn parent<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        self.parent.as_ref().map(|index| &tree[index])
    }

    fn child<'tree>(&self, tree: &'tree Tree, nth: usize) -> Option<&'tree TreeNode> {
        self.children.get(nth).map(|index| &tree[index])
    }

    fn first_child<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        self.children.first().map(|index| &tree[index])
    }

    fn last_child<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        self.children.last().map(|index| &tree[index])
    }

    fn is_leaf(&self) -> bool {
        self.children.is_empty()
    }

    fn is_branch(&self) -> bool {
        !self.is_leaf()
    }

    fn data<'container, T>(&self, container: &'container [T]) -> &'container T {
        match self.index {
            TreeNodeIndex::Node(i) => &container[i],
            TreeNodeIndex::Root => unimplemented!(),
        }
    }

    fn data_mut<'container, T>(&self, container: &'container mut [T]) -> &'container mut T {
        match self.index {
            TreeNodeIndex::Node(i) => &mut container[i],
            TreeNodeIndex::Root => unimplemented!(),
        }
    }
}

/// Tree built from / overlaid onto a separate array.
/// Node data is accessed by indexing into that array.
struct Tree {
    root: TreeNode,
    /// All non-root nodes.
    /// `nodes` indices correspond to the same items as those
    /// with the same index in the separate array.
    nodes: Vec<TreeNode>,
}

impl Tree {
    fn new() -> Tree {
        let root = TreeNode::new_root();
        Tree {
            root,
            nodes: Vec::new(),
        }
    }

    fn root_index() -> TreeNodeIndex {
        TreeNodeIndex::Root
    }

    fn root(&self) -> &TreeNode {
        &self[&Tree::root_index()]
    }

    fn register(&mut self, node: TreeNode) -> TreeNodeIndex {
        let index = node.index;
        self.nodes.push(node);
        index
    }

    /// Build a tree from indented items
    fn from_indented_items(items: &[&str]) -> Tree {
        assert!(items.len() > 0);
        assert!(line_depth(items[0]) == 1);

        let mut tree = Tree::new();
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
                assert!(item_depth == stack_depth - 1);
                stack.pop();
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

    fn bfs(&self) -> TreeBfs {
        TreeBfs {
            tree: &self,
            queue: VecDeque::from([self.root()]),
        }
    }

    fn dfs(&self) -> TreeDfs {
        TreeDfs {
            tree: &self,
            stack: Vec::from([self.root()]),
        }
    }

    // JRS: Consider implementing all edits here,
    // if we can navigate the lifetime rules to do so...
    fn edit(&mut self, edit: &TreeEditOp) {
        match edit {
            TreeEditOp::Remove { before_index } => {
                let parent_index = self[before_index].parent.unwrap();
                let parent = &mut self[&parent_index];
                let child_position = parent
                    .children
                    .iter()
                    .position(|child_index| child_index == before_index)
                    .unwrap();
                parent.children.remove(child_position);
            }
            TreeEditOp::Move {
                before_index,
                parent_index,
                child_position,
            } => {
                let current_parent_index = self[before_index].parent.unwrap();
                // Changing parents is not expected with our tree semantics,
                // but it may of course happen in general tree diffing.
                // We support it here to allow testing the algorithm's examples.
                let same_parent = current_parent_index == *parent_index;
                let child = {
                    let current_parent = &mut self[&current_parent_index];
                    let current_child_position = current_parent
                        .children
                        .iter()
                        .position(|child_index| child_index == before_index)
                        .unwrap();
                    current_parent.children.remove(current_child_position)
                };
                let target_parent = &mut self[parent_index];
                let mut new_child_position = *child_position;
                // Move op child position describes the position before any modification.
                // For same-parent moves, the `remove` just above means we need to adjust by 1.
                if same_parent {
                    new_child_position -= 1;
                }
                target_parent.children.insert(new_child_position, child);
            }
            _ => unimplemented!(),
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

struct TreeBfs<'tree> {
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
                if let TreeNodeIndex::Root = node.index {
                    continue;
                }
                return Some(node);
            }
        }

        return None;
    }
}

struct TreeDfs<'tree> {
    tree: &'tree Tree,
    stack: Vec<&'tree TreeNode>,
}

impl<'tree> Iterator for TreeDfs<'tree> {
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
                if let TreeNodeIndex::Root = node.index {
                    continue;
                }
                return Some(node);
            }
        }

        return None;
    }
}

// TODO: Add `leaves` and `branches` filters to all tree iterators

struct TreeLcs {
    matched: Vec<(TreeNodeIndex, TreeNodeIndex)>,
    unmatched: Vec<(Option<TreeNodeIndex>, Option<TreeNodeIndex>)>,
}

trait TreeNodeIndexable {
    fn index(&self) -> &TreeNodeIndex;
}

impl TreeNodeIndexable for TreeNodeIndex {
    fn index(&self) -> &TreeNodeIndex {
        self
    }
}

impl TreeNodeIndexable for TreeNode {
    fn index(&self) -> &TreeNodeIndex {
        &self.index
    }
}

/// Finds a longest common subsequence (LCS) by comparing
/// a subset of tree data items that know their own index.
/// Equality is determined by the item's `Eq` implementation (not the index).
/// The `subset` arrays contain only the (indexable) items of interest.
fn tree_indexable_subset_lcs<T>(subset_a: &[T], subset_b: &[T]) -> TreeLcs
where
    // `Hash` and `Ord` do not appear to actually be used by the diff algorithm
    T: TreeNodeIndexable + Eq + Hash + Ord,
{
    // TODO: Consider using `similar::IdentifyDistinct` for large inputs
    let diff_ops = capture_diff_slices(similar::Algorithm::Myers, &subset_a, &subset_b);
    let mut matched: Vec<(TreeNodeIndex, TreeNodeIndex)> = Vec::new();
    let mut unmatched: Vec<(Option<TreeNodeIndex>, Option<TreeNodeIndex>)> = Vec::new();
    for diff_op in diff_ops {
        // Translate subset indices back up to tree indices
        // JRS: Should keep these small summary representations,
        // instead of inflating them to cover each item...?
        match diff_op {
            DiffOp::Equal {
                old_index,
                new_index,
                len,
            } => {
                for i in 0..len {
                    matched.push((
                        *subset_a[old_index + i].index(),
                        *subset_b[new_index + i].index(),
                    ));
                }
            }
            DiffOp::Delete {
                old_index,
                old_len,
                new_index: _,
            } => {
                for i in 0..old_len {
                    unmatched.push((Some(*subset_a[old_index + i].index()), None));
                }
            }
            DiffOp::Insert {
                old_index: _,
                new_index,
                new_len,
            } => {
                for i in 0..new_len {
                    unmatched.push((None, Some(*subset_b[new_index + i].index())));
                }
            }
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => {
                for i in 0..old_len {
                    unmatched.push((Some(*subset_a[old_index + i].index()), None));
                }
                for i in 0..new_len {
                    unmatched.push((None, Some(*subset_b[new_index + i].index())));
                }
            }
        }
    }
    TreeLcs { matched, unmatched }
}

struct TreeNodeBundledItem<'items, T> {
    index: TreeNodeIndex,
    item: &'items T,
}

impl<T> TreeNodeIndexable for TreeNodeBundledItem<'_, T> {
    fn index(&self) -> &TreeNodeIndex {
        &self.index
    }
}

impl<T: PartialEq> PartialEq for TreeNodeBundledItem<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.item.eq(other.item)
    }
}

impl<T: Eq> Eq for TreeNodeBundledItem<'_, T> {}

// JRS: May need to adjust `Ord` and `Hash` to match `Eq`...
// So far, they appear to not be used by the diff algorithms we're applying.

impl<T: PartialEq> PartialOrd for TreeNodeBundledItem<'_, T> {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}

impl<T: Eq> Ord for TreeNodeBundledItem<'_, T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl<T> Hash for TreeNodeBundledItem<'_, T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

/// Finds a longest common subsequence (LCS) by comparing
/// a subset of separately stored tree data items.
/// The `items` arrays cover the entire tree.
/// The `subset` arrays contain only the indices of interest.
fn tree_items_subset_lcs<T>(
    tree_a: &Tree,
    tree_b: &Tree,
    items_a: &[T],
    items_b: &[T],
    subset_a: &[TreeNodeIndex],
    subset_b: &[TreeNodeIndex],
) -> TreeLcs
where
    // `Hash` and `Ord` do not appear to actually be used by the diff algorithm
    T: Eq + Hash + Ord,
{
    let subset_a_bundled_items: Vec<TreeNodeBundledItem<T>> = subset_a
        .iter()
        .map(|index| TreeNodeBundledItem {
            index: *index,
            item: tree_a[index].data(&items_a),
        })
        .collect();
    let subset_b_bundled_items: Vec<TreeNodeBundledItem<T>> = subset_b
        .iter()
        .map(|index| TreeNodeBundledItem {
            index: *index,
            item: tree_b[index].data(&items_b),
        })
        .collect();
    tree_indexable_subset_lcs(&subset_a_bundled_items, &subset_b_bundled_items)
}

// Tree edits that transform the before tree into the after tree
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum TreeEditOp {
    Add {
        /// Node index in after tree
        after_index: TreeNodeIndex,
        /// New parent node index in before tree
        parent_index: TreeNodeIndex,
        /// Position in new parent node's children
        child_position: usize,
    },
    Remove {
        /// Node index in before tree
        before_index: TreeNodeIndex,
    },
    Replace {
        /// Node index in before tree
        before_index: TreeNodeIndex,
        /// Replacing node index in after tree
        after_index: TreeNodeIndex,
    },
    Move {
        /// Node index in before tree
        before_index: TreeNodeIndex,
        /// New parent node index in before tree
        parent_index: TreeNodeIndex,
        /// Position in new parent node's children
        /// Note that this is an _insertion_ position into the parent as it currently exists.
        /// When applying a move within the same parent,
        /// you would need to decrement this by 1 if you remove before inserting.
        child_position: usize,
    },
}

// Diff result in terms of lines
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum TreeDiffOp {
    Add {
        /// Index of added line in after content
        after_index: usize,
    },
    Remove {
        /// Index of removed line in before content
        before_index: usize,
    },
    Replace {
        /// Index of replaced line in before content
        before_index: usize,
        /// Index of replaced line in after content
        after_index: usize,
    },
    Move {
        /// Index of reordered line in before content
        before_index: usize,
        /// Index of reordered line in after content
        after_index: usize,
    },
}

#[derive(Debug)]
pub struct TreeDiff<'content> {
    before_lines: Vec<&'content str>,
    after_lines: Vec<&'content str>,
    edit_ops: Vec<TreeEditOp>,
    diff_ops: Vec<TreeDiffOp>,
}

fn compare_frames(
    before_frame: Vec<(usize, &str)>,
    after_frame: Vec<(usize, &str)>,
) -> Vec<TreeDiffOp> {
    let mut tree_diff_ops = Vec::new();

    // Create faux documents for text diffing
    // JRS: There's surely a better way to diff the frames than this...
    let before_frame_content = before_frame
        .iter()
        .map(|(_, s)| *s)
        .collect::<Vec<_>>()
        .join("\n");
    let after_frame_content = after_frame
        .iter()
        .map(|(_, s)| *s)
        .collect::<Vec<_>>()
        .join("\n");

    let diff = TextDiff::configure()
        .algorithm(similar::Algorithm::Patience)
        .diff_lines(&before_frame_content, &after_frame_content);

    // Transform text diffs of frames into to tree diffs,
    // converting indices from our faux documents into those for the entire tree
    for text_diff_op_group in diff.grouped_ops(0) {
        for text_diff_op in text_diff_op_group {
            match text_diff_op {
                DiffOp::Insert {
                    new_index, new_len, ..
                } => {
                    for i in 0..new_len {
                        tree_diff_ops.push(TreeDiffOp::Add {
                            after_index: after_frame[new_index + i].0,
                        });
                    }
                }
                DiffOp::Delete {
                    old_index, old_len, ..
                } => {
                    for i in 0..old_len {
                        tree_diff_ops.push(TreeDiffOp::Remove {
                            before_index: before_frame[old_index + i].0,
                        });
                    }
                }
                DiffOp::Replace {
                    old_index,
                    old_len,
                    new_index,
                    new_len,
                } => {
                    assert!(old_len == new_len);
                    for i in 0..old_len {
                        tree_diff_ops.push(TreeDiffOp::Replace {
                            before_index: before_frame[old_index + i].0,
                            after_index: after_frame[new_index + i].0,
                        });
                    }
                }
                DiffOp::Equal { .. } => {}
            }
        }
    }

    tree_diff_ops
}

pub fn diff_tree<'content>(
    before_content: &'content str,
    after_content: &'content str,
) -> TreeDiff<'content> {
    let before_lines: Vec<_> = before_content.lines().collect();
    let after_lines: Vec<_> = after_content.lines().collect();

    let mut before_index: usize = 0;
    let mut after_index: usize = 0;

    // For each side in the comparison,
    // we track a list of events seen at each stack depth.
    let mut before_stack: Vec<Vec<(usize, &str)>> = Vec::new();
    let mut after_stack: Vec<Vec<(usize, &str)>> = Vec::new();

    let mut ops = Vec::new();

    // TODO: Compare lists when a stack frame ends...
    // ...but likely to end at different times on each side if moving line by line...
    // TODO: If one side returns and re-calls same function,
    // don't just let that go like nothing happened...!

    // Gather stack frames from each side of the tree.
    // When both sides return from a frame, compare to look for differences.
    // TODO: Test all cases below
    let before_len = before_lines.len();
    let after_len = after_lines.len();
    while before_index < before_len && after_index < after_len {
        let before_line = before_lines[before_index];
        let after_line = after_lines[after_index];

        // Depth is 1-based
        let before_line_depth = line_depth(before_line);
        let after_line_depth = line_depth(after_line);

        assert!(before_stack.len() == after_stack.len());
        if before_line_depth == after_line_depth {
            let line_depth = before_line_depth;
            let stack_depth = before_stack.len();
            if line_depth > stack_depth {
                before_stack.push(Vec::new());
                after_stack.push(Vec::new());
            } else if line_depth < stack_depth {
                let complete_before_frame = before_stack.pop().unwrap();
                let complete_after_frame = after_stack.pop().unwrap();
                ops.append(&mut compare_frames(
                    complete_before_frame,
                    complete_after_frame,
                ));
            }
            let before_frame = before_stack.last_mut().unwrap();
            before_frame.push((before_index, before_line));
            before_index += 1;
            let after_frame = after_stack.last_mut().unwrap();
            after_frame.push((after_index, after_line));
            after_index += 1;
        } else {
            // We ignore deeper sub-trees when depth is not matched.
            // Every descent in our tree is starts with a "call from"
            // at the current depth, so we make use of this semantic here.
            // TODO: Test one side having more elements at level below the root
            if before_line_depth < after_line_depth {
                after_index += 1;
            } else {
                before_index += 1;
            }
        }
        assert!(before_stack.len() == after_stack.len());
    }

    // For now, let's just assume before is the longer side
    // TODO: Support longer after trace as well
    // JRS: Try to merge this into the twinned loop above
    assert!(after_index == after_len);
    while before_index < before_len {
        let before_line = before_lines[before_index];

        let line_depth = line_depth(before_line);
        let stack_depth = before_stack.len();

        // JRS: Essentially the same code as the twinned loop,
        // but only thinking about one side
        assert!(before_stack.len() == after_stack.len());
        if line_depth > stack_depth {
            before_stack.push(Vec::new());
            after_stack.push(Vec::new());
        } else if line_depth < stack_depth {
            let complete_before_frame = before_stack.pop().unwrap();
            let complete_after_frame = after_stack.pop().unwrap();
            ops.append(&mut compare_frames(
                complete_before_frame,
                complete_after_frame,
            ));
        }
        let before_frame = before_stack.last_mut().unwrap();
        before_frame.push((before_index, before_line));
        before_index += 1;
        assert!(before_stack.len() == after_stack.len());
    }

    // Traces often trail off without returning to starting depth,
    // so pop and compare any remaining frames
    assert!(before_stack.len() == after_stack.len());
    while !before_stack.is_empty() {
        let complete_before_frame = before_stack.pop().unwrap();
        let complete_after_frame = after_stack.pop().unwrap();
        ops.append(&mut compare_frames(
            complete_before_frame,
            complete_after_frame,
        ));
    }

    TreeDiff {
        before_lines,
        after_lines,
        edit_ops: Default::default(),
        diff_ops: ops,
    }
}

/// Override `Event` equality to allow for some source coordinate drift
#[derive(Clone)]
struct FuzzyEvent(Event);

impl PartialEq for FuzzyEvent {
    fn eq(&self, other: &Self) -> bool {
        if self.0 == other.0 {
            return true;
        }

        if self.0.depth != other.0.depth || self.0.event_type != other.0.event_type {
            return false;
        }

        let self_loc = &self.0.location;
        let other_loc = &other.0.location;
        if self_loc.function != other_loc.function || self_loc.file != other_loc.file {
            return false;
        }

        if self_loc.line.is_some() != other_loc.line.is_some() {
            return false;
        }
        if self_loc.line.is_none() && other_loc.line.is_none() {
            return true;
        }
        let self_line = self_loc.line.unwrap();
        let other_line = other_loc.line.unwrap();
        if self_line.abs_diff(other_line) > 3 {
            return false;
        }

        true
    }
}

impl Eq for FuzzyEvent {}

// JRS: May need to adjust `Ord` and `Hash` to match `Eq`...
// So far, they appear to not be used by the diff algorithms we're applying.

impl PartialOrd for FuzzyEvent {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}

impl Ord for FuzzyEvent {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl Hash for FuzzyEvent {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl Eventable for FuzzyEvent {
    fn as_event(&self) -> &Event {
        &self.0
    }
}

/// Label each event by hashing all function names along its path in the tree.
fn tree_event_labels<E: Eventable>(tree: &Tree, events: &[E]) -> Vec<u64> {
    let mut hashers: Vec<DefaultHasher> = Vec::with_capacity(events.len());

    // For each event, look for parent's previously computed hasher,
    // and then add that event's own function name.
    for i in 0..events.len() {
        let event = events[i].as_event();
        let node = &tree[&TreeNodeIndex::Node(i)];
        let parent_index = node.parent(tree).unwrap().index;
        let parent_hasher = match parent_index {
            TreeNodeIndex::Node(p) => Some(&hashers[p]),
            TreeNodeIndex::Root => None,
        };
        let mut hasher: DefaultHasher =
            parent_hasher.map_or(DefaultHasher::new(), |hasher| hasher.clone());
        event.location.function.hash(&mut hasher);
        hashers.push(hasher);
    }

    hashers.into_iter().map(|hasher| hasher.finish()).collect()
}

fn matching_bimap<T>(
    before_tree: &Tree,
    after_tree: &Tree,
    before_items: &[T],
    after_items: &[T],
    before_labels: &[u64],
    after_labels: &[u64],
) -> BiHashMap<TreeNodeIndex, TreeNodeIndex>
where
    // `Hash` and `Ord` do not appear to actually be used by the diff algorithm
    T: Eq + Hash + Ord,
{
    let mut matching = BiHashMap::new();

    // Group leaves by label
    let mut before_leaves_by_label: HashMap<u64, Vec<TreeNodeIndex>> = HashMap::new();
    for leaf in before_tree.dfs().filter(|node| node.is_leaf()) {
        let label = leaf.data(before_labels);
        before_leaves_by_label
            .entry(*label)
            .or_default()
            .push(leaf.index);
    }
    let mut after_leaves_by_label: HashMap<u64, Vec<TreeNodeIndex>> = HashMap::new();
    for leaf in after_tree.dfs().filter(|node| node.is_leaf()) {
        let label = leaf.data(after_labels);
        after_leaves_by_label
            .entry(*label)
            .or_default()
            .push(leaf.index);
    }

    // For each leaf label, find the LCS of the leaves and add this to the matching bimap
    let default_vec: Vec<TreeNodeIndex> = Vec::new();
    for leaf_label in before_leaves_by_label.keys() {
        // TODO: Extract and share as much of this with branch loop below as possible
        let before_leaves = &before_leaves_by_label[leaf_label];
        let after_leaves = after_leaves_by_label
            .get(leaf_label)
            .unwrap_or(&default_vec);
        let leaves_lcs = tree_items_subset_lcs(
            before_tree,
            after_tree,
            before_items,
            after_items,
            before_leaves,
            after_leaves,
        );
        matching.extend(leaves_lcs.matched.into_iter());

        // Match any remaining leaves to their first trace-order match
        let before_leaves_unmatched: Vec<TreeNodeIndex> = leaves_lcs
            .unmatched
            .iter()
            .map(|index_pair| index_pair.0)
            .filter(|index| index.is_some())
            .map(|index| index.unwrap())
            .collect();
        // Use `VecDeque` with the after side for efficient removal when a match is found
        let mut after_leaves_unmatched: VecDeque<TreeNodeIndex> = leaves_lcs
            .unmatched
            .iter()
            .map(|index_pair| index_pair.1)
            .filter(|index| index.is_some())
            .map(|index| index.unwrap())
            .collect();
        for before_leaf_index in &before_leaves_unmatched {
            let mut to_remove: Option<usize> = None;
            for (position, after_leaf_index) in after_leaves_unmatched.iter().enumerate() {
                let before_leaf_item = before_tree[before_leaf_index].data(&before_items);
                let after_leaf_item = after_tree[after_leaf_index].data(&after_items);
                if before_leaf_item == after_leaf_item {
                    matching.insert(*before_leaf_index, *after_leaf_index);
                    to_remove = Some(position);
                    break;
                }
            }
            if let Some(position) = to_remove {
                after_leaves_unmatched.remove(position);
            }
        }
    }

    // Group branches by label
    // Use `IndexMap` with the before side to support iteration in insertion order
    let mut before_branches_by_label: IndexMap<u64, Vec<TreeNodeIndex>> = IndexMap::new();
    for branch in before_tree.dfs().filter(|node| node.is_branch()) {
        let label = branch.data(before_labels);
        before_branches_by_label
            .entry(*label)
            .or_default()
            .push(branch.index);
    }
    let mut after_branches_by_label: HashMap<u64, Vec<TreeNodeIndex>> = HashMap::new();
    for branch in after_tree.dfs().filter(|node| node.is_branch()) {
        let label = branch.data(after_labels);
        after_branches_by_label
            .entry(*label)
            .or_default()
            .push(branch.index);
    }

    // For each branch label, find the LCS of the branches and add this to the matching bimap
    // Look for branch labels somewhat naively by walking backwards through the trace labels
    // (since branches at the end of the trace are deeper and likely to ready for comparison)
    let mut before_visited: HashSet<TreeNodeIndex> = HashSet::new();
    before_visited.extend(
        before_tree
            .dfs()
            .filter(|node| node.is_leaf())
            .map(|node| node.index),
    );
    for branch_label in before_branches_by_label.keys().rev() {
        let before_branches = &before_branches_by_label[branch_label];
        let after_branches = after_branches_by_label
            .get(branch_label)
            .unwrap_or(&default_vec);
        // Ensure we've tried to match all children of these branches
        let all_before_children_visited: bool = before_branches
            .iter()
            .flat_map(|branch_index| before_tree[branch_index].children.iter())
            .all(|child_index| before_visited.contains(child_index));
        assert!(all_before_children_visited);

        // JRS: Need to extend equality to check portion of children in common...
        // Or alternatively, skip LCS and compare pair-wise only...
        // For now, let's try pair-wise only.

        // Match any remaining branches to their first trace-order match
        let before_branches_unmatched: Vec<TreeNodeIndex> =
            before_branches.iter().cloned().collect();
        // Use `VecDeque` with the after side for efficient removal when a match is found
        let mut after_branches_unmatched: VecDeque<TreeNodeIndex> =
            after_branches.iter().cloned().collect();
        for before_branch_index in &before_branches_unmatched {
            let mut to_remove: Option<usize> = None;
            for (position, after_branch_index) in after_branches_unmatched.iter().enumerate() {
                let before_branch = &before_tree[before_branch_index];
                let after_branch = &after_tree[after_branch_index];
                let before_branch_item = before_branch.data(&before_items);
                let after_branch_item = after_branch.data(&after_items);
                if before_branch_item == after_branch_item {
                    // With branches, we also check children in common
                    let before_children: &Vec<TreeNodeIndex> = &before_branch.children;
                    let after_children: HashSet<TreeNodeIndex> =
                        after_branch.children.iter().cloned().collect();
                    let max_children: f64 = before_children.len().max(after_children.len()) as f64;
                    let common_children: f64 = before_children
                        .iter()
                        .map(|before_index| matching.get_by_left(before_index))
                        .filter(|possible_match| possible_match.is_some())
                        .map(|after_index| after_children.contains(after_index.unwrap()))
                        .filter(|child_found| *child_found)
                        .count() as f64;
                    let common_ratio: f64 = common_children / max_children;
                    if common_ratio >= 0.5 {
                        matching.insert(*before_branch_index, *after_branch_index);
                        to_remove = Some(position);
                        break;
                    }
                }
            }
            if let Some(position) = to_remove {
                after_branches_unmatched.remove(position);
            }
        }

        // Add these branches to the visited set
        before_visited.extend(before_branches);
    }

    matching
}

#[derive(PartialEq, Eq)]
enum DiffSide {
    Before,
    After,
}

/// Override `TreeNodeIndex` equality to mean partner from matching bimap
struct MatchingTreeNodeIndex {
    side: DiffSide,
    index: TreeNodeIndex,
    partner: TreeNodeIndex,
}

impl PartialEq for MatchingTreeNodeIndex {
    fn eq(&self, other: &Self) -> bool {
        assert!(self.side != other.side);
        other.index == self.partner
    }
}

impl Eq for MatchingTreeNodeIndex {}

// JRS: May need to adjust `Ord` and `Hash` to match `Eq`...
// So far, they appear to not be used by the diff algorithms we're applying.

impl PartialOrd for MatchingTreeNodeIndex {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}

impl Ord for MatchingTreeNodeIndex {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        todo!()
    }
}

impl Hash for MatchingTreeNodeIndex {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {
        todo!()
    }
}

impl TreeNodeIndexable for MatchingTreeNodeIndex {
    fn index(&self) -> &TreeNodeIndex {
        &self.index
    }
}

fn align_children(
    before_tree: &mut Tree,
    after_tree: &Tree,
    matching: &BiHashMap<TreeNodeIndex, TreeNodeIndex>,
    before_parent_index: &TreeNodeIndex,
    after_parent_index: &TreeNodeIndex,
    before_in_order: &mut HashSet<TreeNodeIndex>,
    after_in_order: &mut HashSet<TreeNodeIndex>,
) -> Vec<TreeEditOp> {
    let mut ops = Vec::new();

    let before_parent = &before_tree[before_parent_index];
    let after_parent = &after_tree[after_parent_index];

    // Mark all children of before and after nodes as out of order
    for before_child_index in &before_parent.children {
        before_in_order.remove(before_child_index);
    }
    for after_child_index in &after_parent.children {
        after_in_order.remove(after_child_index);
    }

    // Look for children that have a match in each matched parent
    let mut before_children_matched_to_after_children: Vec<MatchingTreeNodeIndex> = Vec::new();
    for before_child_index in &before_parent.children {
        if let Some(partner_index) = matching.get_by_left(before_child_index) {
            let partner_node = &after_tree[partner_index];
            if partner_node.parent.unwrap() != *after_parent_index {
                continue;
            }
            before_children_matched_to_after_children.push(MatchingTreeNodeIndex {
                side: DiffSide::Before,
                index: *before_child_index,
                partner: *partner_index,
            });
        }
    }
    let mut after_children_matched_to_before_children: Vec<MatchingTreeNodeIndex> = Vec::new();
    for after_child_index in &after_parent.children {
        if let Some(partner_index) = matching.get_by_right(after_child_index) {
            let partner_node = &before_tree[partner_index];
            if partner_node.parent.unwrap() != *before_parent_index {
                continue;
            }
            after_children_matched_to_before_children.push(MatchingTreeNodeIndex {
                side: DiffSide::After,
                index: *after_child_index,
                partner: *partner_index,
            });
        }
    }

    // Find LCS of these cross-matched children
    let children_lcs = tree_indexable_subset_lcs(
        &before_children_matched_to_after_children,
        &after_children_matched_to_before_children,
    );

    // Mark children in the LCS as in order
    for (before_child_index, after_child_index) in &children_lcs.matched {
        before_in_order.insert(*before_child_index);
        after_in_order.insert(*after_child_index);
    }

    // For each matched child outside the LCS,
    // move to expected position by examining after parent
    let after_children_outside_lcs = children_lcs
        .unmatched
        .iter()
        .map(|(_, after)| after)
        .filter(|possible_after| possible_after.is_some())
        .map(|after| after.as_ref().unwrap());
    for after_child_index in after_children_outside_lcs {
        let before_child_index = matching.get_by_right(after_child_index).unwrap();
        let child_position = find_position_in_parent(
            before_tree,
            after_tree,
            matching,
            after_child_index,
            before_in_order,
            after_in_order,
        );
        let op = TreeEditOp::Move {
            before_index: *before_child_index,
            parent_index: *before_parent_index,
            child_position,
        };
        // Apply move to before tree and add to ops
        before_tree.edit(&op);
        ops.push(op);
        // Mark children as in order
        before_in_order.insert(*before_child_index);
        after_in_order.insert(*after_child_index);
    }

    ops
}

/// Returns intended 0-based position of node in parent.
/// Note that this is an _insertion_ position into the parent as it currently exists.
/// When applying a move within the same parent,
/// you would need to decrement this by 1 if you remove before inserting.
fn find_position_in_parent(
    before_tree: &Tree,
    after_tree: &Tree,
    matching: &BiHashMap<TreeNodeIndex, TreeNodeIndex>,
    after_child_index: &TreeNodeIndex,
    before_in_order: &HashSet<TreeNodeIndex>,
    after_in_order: &HashSet<TreeNodeIndex>,
) -> usize {
    let after_child = &after_tree[after_child_index];
    let after_parent = after_child.parent(after_tree).unwrap();
    // Check if after child is left-most child marked as in order
    for after_index_to_check in &after_parent.children {
        if !after_in_order.contains(after_index_to_check) {
            continue;
        }
        if after_index_to_check == after_child_index {
            return 0;
        }
        break;
    }
    // Find right-most sibling to the left of after child that is in order
    let after_child_position = after_parent
        .children
        .iter()
        .position(|child_index| child_index == after_child_index)
        .unwrap();
    let mut after_sibling_index = None;
    for after_index_to_check in after_parent.children[0..after_child_position].iter().rev() {
        if !after_in_order.contains(after_index_to_check) {
            continue;
        }
        after_sibling_index = Some(*after_index_to_check);
        break;
    }
    assert!(after_sibling_index.is_some());
    // Find position of its matched before node, counting _in-order_ siblings only
    let before_sibling_index = matching
        .get_by_right(&after_sibling_index.unwrap())
        .unwrap();
    let before_sibling = &before_tree[before_sibling_index];
    let before_parent = before_sibling.parent(before_tree).unwrap();
    // JRS: Below seems to be what the algorithm says to do,
    // but doesn't appear to work in practice.
    // let mut before_in_order_siblings: usize = 0;
    // for before_index_to_check in &before_parent.children {
    //     if before_in_order.contains(before_index_to_check) {
    //         before_in_order_siblings += 1;
    //     }
    //     if before_index_to_check == before_sibling_index {
    //         // Count of in-order siblings also gives the position node should move to
    //         return before_in_order_siblings;
    //     }
    // }
    // JRS: After trying a few examples,
    // I think the position just after the before sibling is what we want
    let before_sibling_position = before_parent
        .children
        .iter()
        .position(|child_index| child_index == before_sibling_index)
        .unwrap();
    // Return position after sibling
    before_sibling_position + 1
}

// TODO: Separate diff algorithm from tree events to simplify testing and reuse
pub fn diff_tree_chawathe<'content>(
    before_content: &'content str,
    after_content: &'content str,
) -> TreeDiff<'content> {
    let before_lines: Vec<_> = before_content.lines().collect();
    let after_lines: Vec<_> = after_content.lines().collect();

    // Parse lines into fuzzy events
    let mut before_events: Vec<_> = before_lines
        .iter()
        .map(|line| FuzzyEvent(Event::parse(line).unwrap()))
        .collect();
    let after_events: Vec<_> = after_lines
        .iter()
        .map(|line| FuzzyEvent(Event::parse(line).unwrap()))
        .collect();

    // Convert lines into trees
    // JRS: Do we want to save a copy of the before tree without edits...?
    let mut before_tree = Tree::from_indented_items(&before_lines);
    let after_tree = Tree::from_indented_items(&after_lines);

    // Collect tree event labels from function names along each node's tree path
    let mut before_labels = tree_event_labels(&before_tree, &before_events);
    let after_labels = tree_event_labels(&after_tree, &after_events);

    // Build initial matching bimap
    let mut matching = matching_bimap(
        &before_tree,
        &after_tree,
        &before_events,
        &after_events,
        &before_labels,
        &after_labels,
    );

    let mut edit_ops: Vec<TreeEditOp> = Vec::new();

    // Temporary state used to check child alignment
    let mut before_in_order: HashSet<TreeNodeIndex> = HashSet::new();
    let mut after_in_order: HashSet<TreeNodeIndex> = HashSet::new();

    // Visit after nodes in breadth-first order
    for after_node in after_tree.bfs() {
        let after_index = &after_node.index;
        if matching.contains_right(after_index) {
            // Match exists, check if replacements or moves are needed
            let before_index = matching.get_by_right(after_index).unwrap();
            let before_node = &before_tree[before_index];
            // Compare events without fuzzy wrapper
            let before_event = before_node.data(&before_events);
            let after_event = after_node.data(&after_events);
            if before_event.as_event() != after_event.as_event() {
                let op = TreeEditOp::Replace {
                    before_index: *before_index,
                    after_index: *after_index,
                };
                // Replace event in before tree
                *before_node.data_mut(&mut before_events) = after_event.clone();
                // Add to ops
                edit_ops.push(op);
            }
            // Transcribing algorithm's naming in something a bit more readable
            // x: after_node
            // y: after_parent (from `after_node` parent)
            // z: target_before_parent (from matching `after_parent`)
            // w: before_node (from matching `after_node`)
            // v: current_before_parent (from `before_node` parent)
            let after_parent_index = &after_node.parent(&after_tree).unwrap().index;
            let current_before_parent_index = &before_node.parent(&before_tree).unwrap().index;
            if let Some(target_before_parent_index) = matching.get_by_right(after_parent_index) {
                if current_before_parent_index != target_before_parent_index {
                    // Parent match not found, move node to target parent
                    // JRS: We don't expect parent changes with our tree semantics,
                    // though they do of course appear for general tree diffs.
                    // Perhaps optionally disable this ability for our trees...?
                    let child_position = find_position_in_parent(
                        &before_tree,
                        &after_tree,
                        &matching,
                        after_index,
                        &mut before_in_order,
                        &mut after_in_order,
                    );
                    let op = TreeEditOp::Move {
                        before_index: *before_index,
                        parent_index: *target_before_parent_index,
                        child_position,
                    };
                    // Apply move to before tree and add to ops
                    before_tree.edit(&op);
                    edit_ops.push(op);
                }
            }
        } else {
            // Match not found, need to add after node
            let child_position = find_position_in_parent(
                &before_tree,
                &after_tree,
                &matching,
                after_index,
                &mut before_in_order,
                &mut after_in_order,
            );
            let after_parent_index = &after_node.parent(&after_tree).unwrap().index;
            let before_parent_index = matching.get_by_right(after_parent_index).unwrap();
            let op = TreeEditOp::Add {
                after_index: *after_index,
                parent_index: *before_parent_index,
                child_position,
            };
            // Add to the before tree
            let before_tree_position = before_events.len();
            before_events.push(after_node.data(&after_events).clone());
            // JRS: This might not be correct...
            before_labels.push(after_node.data(&after_labels).clone());
            let before_node = TreeNode::new(before_tree_position, *before_parent_index);
            let before_index = before_tree.register(before_node);
            let before_parent = &mut before_tree[before_parent_index];
            before_parent.children.insert(child_position, before_index);
            // Add to matching
            matching.insert(before_index, *after_index);
            // Add to ops
            edit_ops.push(op);
        }
        // Re-check matching, as `else` branch above may have updated it
        let before_index = matching.get_by_right(after_index).unwrap();
        // Align children
        let move_ops = align_children(
            &mut before_tree,
            &after_tree,
            &matching,
            before_index,
            after_index,
            &mut before_in_order,
            &mut after_in_order,
        );
        // Merge moves into ops
        edit_ops.extend(move_ops);
    }

    // Visit before nodes in the depth-first order
    let mut delete_ops: Vec<TreeEditOp> = Vec::new();
    for before_node in before_tree.dfs() {
        let before_index = &before_node.index;
        if !matching.contains_left(before_index) {
            // Match not found, need to remove before node
            let op = TreeEditOp::Remove {
                before_index: *before_index,
            };
            // Add to temporary delete ops
            delete_ops.push(op);
        }
    }
    // Now that we're done iterating the before tree,
    // apply any delete ops found
    for op in &delete_ops {
        // Remove from the before tree
        before_tree.edit(&op);
    }
    // Merge deletes into ops
    edit_ops.extend(delete_ops);

    // TODO: Convert edit ops to diff ops
    let mut diff_ops: Vec<TreeDiffOp> = Vec::new();

    TreeDiff {
        before_lines,
        after_lines,
        edit_ops,
        diff_ops,
    }
}

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
    fn tree_dfs_leaves() {
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
            .dfs()
            .filter(|node| node.is_leaf())
            .map(|node| node.data(&items).trim())
            .collect();
        assert_eq!(leaves, vec!["0.0.0", "0.1", "0.2", "1.0.0"]);
    }

    #[test]
    fn tree_dfs_branches() {
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
            .dfs()
            .filter(|node| node.is_branch())
            .map(|node| node.data(&items).trim())
            .collect();
        assert_eq!(branches, vec!["0", "0.0", "1", "1.0"]);
    }

    #[test]
    fn tree_leaves_lcs_matching() {
        let items_1: Vec<_> = "
D
  P
    Sa
    Sb
    Sc
  P
    Sd
    Se
  P
    Sf"
        .trim()
        .lines()
        .collect();
        let items_2: Vec<_> = "
D
  P
    Sa
    Sc
  P
    Sf
  P
    Sd
    Se
    Sg"
        .trim()
        .lines()
        .collect();
        let tree_1 = Tree::from_indented_items(&items_1);
        let tree_2 = Tree::from_indented_items(&items_2);
        let leaves_1: Vec<TreeNodeIndex> = tree_1
            .dfs()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_2: Vec<TreeNodeIndex> = tree_2
            .dfs()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_lcs =
            tree_items_subset_lcs(&tree_1, &tree_2, &items_1, &items_2, &leaves_1, &leaves_2);
        let items_1_lcs_matched: Vec<&str> = leaves_lcs
            .matched
            .iter()
            .map(|index_pair| tree_1[&index_pair.0].data(&items_1).trim())
            .collect();
        let items_2_lcs_matched: Vec<&str> = leaves_lcs
            .matched
            .iter()
            .map(|index_pair| tree_2[&index_pair.1].data(&items_2).trim())
            .collect();
        assert_eq!(items_1_lcs_matched, items_2_lcs_matched);
        assert_eq!(items_1_lcs_matched, vec!["Sa", "Sc", "Sd", "Se"]);
        let items_1_lcs_unmatched: Vec<&str> = leaves_lcs
            .unmatched
            .iter()
            .map(|index_pair| index_pair.0)
            .filter(|index| index.is_some())
            .map(|index| index.unwrap())
            .map(|index| tree_1[&index].data(&items_1).trim())
            .collect();
        let items_2_lcs_unmatched: Vec<&str> = leaves_lcs
            .unmatched
            .iter()
            .map(|index_pair| index_pair.1)
            .filter(|index| index.is_some())
            .map(|index| index.unwrap())
            .map(|index| tree_2[&index].data(&items_2).trim())
            .collect();
        assert_eq!(items_1_lcs_unmatched, vec!["Sb", "Sf"]);
        assert_eq!(items_2_lcs_unmatched, vec!["Sf", "Sg"]);
    }

    #[test]
    fn tree_leaves_lcs_fuzzy_matching() {
        let items_1: Vec<_> = "
CF: system_path at exec-cmd.c:265:6
  CT: is_absolute_path at cache.h:1275:0
  RF: is_absolute_path at cache.h:1276:9"
            .trim()
            .lines()
            .collect();
        // Leaf 1 differs by 1 line
        // Leaf 2 differs by many lines
        let items_2: Vec<_> = "
CF: system_path at exec-cmd.c:265:6
  CT: is_absolute_path at cache.h:1274:0
  RF: is_absolute_path at cache.h:1290:9"
            .trim()
            .lines()
            .collect();
        let events_1: Vec<_> = items_1
            .iter()
            .map(|line| FuzzyEvent(Event::parse(line).unwrap()))
            .collect();
        let events_2: Vec<_> = items_2
            .iter()
            .map(|line| FuzzyEvent(Event::parse(line).unwrap()))
            .collect();
        let tree_1 = Tree::from_indented_items(&items_1);
        let tree_2 = Tree::from_indented_items(&items_2);
        let leaves_1: Vec<TreeNodeIndex> = tree_1
            .dfs()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_2: Vec<TreeNodeIndex> = tree_2
            .dfs()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_lcs =
            tree_items_subset_lcs(&tree_1, &tree_2, &events_1, &events_2, &leaves_1, &leaves_2);
        assert_eq!(
            leaves_lcs.matched,
            vec![(TreeNodeIndex::Node(1), TreeNodeIndex::Node(1))]
        );
    }

    #[test]
    fn tree_indexable_lcs() {
        let indices_1: Vec<TreeNodeIndex> = (0..6).map(|i| TreeNodeIndex::Node(i)).collect();
        let indices_2: Vec<TreeNodeIndex> = (2..10).map(|i| TreeNodeIndex::Node(i)).collect();
        let lcs = tree_indexable_subset_lcs(&indices_1, &indices_2);
        assert_eq!(lcs.matched.len(), 4);
        assert!(lcs.matched.iter().all(|(l, r)| l == r));
        assert_eq!(
            lcs.matched.iter().map(|(l, _)| *l).collect::<Vec<_>>(),
            (2..6).map(|i| TreeNodeIndex::Node(i)).collect::<Vec<_>>()
        );
    }

    #[test]
    fn tree_events_with_labels() {
        let items: Vec<_> = "
CF: system_path at exec-cmd.c:265:6
  CT: is_absolute_path at cache.h:1275:0
  CF: is_absolute_path at cache.h:1276:9
    CT: git_is_dir_sep at git-compat-util.h:447:0
    RF: git_is_dir_sep at git-compat-util.h:448:2"
            .trim()
            .lines()
            .collect();
        let events: Vec<_> = items
            .iter()
            .map(|line| FuzzyEvent(Event::parse(line).unwrap()))
            .collect();
        let tree = Tree::from_indented_items(&items);
        let labels = tree_event_labels(&tree, &events);
        assert_eq!(labels[1], labels[2]);
        assert_eq!(labels[3], labels[4]);
    }

    #[test]
    fn tree_matching_bimap() {
        let items_1: Vec<_> = "
D
  P
    Sa
    Sb
    Sc
  P
    Sd
    Se
  P
    Sf"
        .trim()
        .lines()
        .collect();
        let items_2: Vec<_> = "
D
  P
    Sa
    Sc
  P
    Sf
  P
    Sd
    Se
    Sg"
        .trim()
        .lines()
        .collect();
        let tree_1 = Tree::from_indented_items(&items_1);
        let tree_2 = Tree::from_indented_items(&items_2);
        let labels_1 = vec![0, 1, 2, 2, 2, 1, 2, 2, 1, 2];
        let labels_2 = vec![0, 1, 2, 2, 1, 2, 1, 2, 2, 2];
        let matching = matching_bimap(&tree_1, &tree_2, &items_1, &items_2, &labels_1, &labels_2);
        assert_eq!(matching.len(), 9);
        let mut items_1_matched: Vec<&str> = matching
            .left_values()
            .map(|index| tree_1[&index].data(&items_1).trim())
            .collect();
        items_1_matched.sort();
        let mut items_2_matched: Vec<&str> = matching
            .right_values()
            .map(|index| tree_2[&index].data(&items_2).trim())
            .collect();
        items_2_matched.sort();
        assert_eq!(items_1_matched, items_2_matched);
        assert_eq!(
            items_1_matched,
            vec!["D", "P", "P", "P", "Sa", "Sc", "Sd", "Se", "Sf"]
        );
    }

    #[test]
    fn tree_align_children() {
        let items_1: Vec<_> = "
1
  2
  3
  4
  5
  6"
        .trim()
        .lines()
        .collect();
        let items_2: Vec<_> = "
1
  3
  5
  6
  2
  4"
        .trim()
        .lines()
        .collect();
        let mut tree_1 = Tree::from_indented_items(&items_1);
        let tree_2 = Tree::from_indented_items(&items_2);
        let labels_1 = vec![0, 1, 1, 1, 1, 1];
        let labels_2 = vec![0, 1, 1, 1, 1, 1];
        let matching = matching_bimap(&tree_1, &tree_2, &items_1, &items_2, &labels_1, &labels_2);
        assert_eq!(matching.len(), 6);
        let ops = align_children(
            &mut tree_1,
            &tree_2,
            &matching,
            &TreeNodeIndex::Node(0),
            &TreeNodeIndex::Node(0),
            &mut Default::default(),
            &mut Default::default(),
        );
        assert_eq!(
            ops,
            vec![
                TreeEditOp::Move {
                    before_index: TreeNodeIndex::Node(1),
                    parent_index: TreeNodeIndex::Node(0),
                    child_position: 5,
                },
                TreeEditOp::Move {
                    before_index: TreeNodeIndex::Node(3),
                    parent_index: TreeNodeIndex::Node(0),
                    child_position: 5,
                },
            ]
        );
        assert_eq!(
            tree_1.nodes[0].children,
            vec![
                TreeNodeIndex::Node(2),
                TreeNodeIndex::Node(4),
                TreeNodeIndex::Node(5),
                TreeNodeIndex::Node(1),
                TreeNodeIndex::Node(3),
            ]
        );
    }

    #[test]
    fn tree_diff_chawathe() {
        let before_content = "
CF: D at file.tex
  CF: P at file.tex
    CF: Sa at file.tex
    CF: Sb at file.tex
    CF: Sc at file.tex
  CF: P at file.tex
    CF: Sd at file.tex
    CF: Se at file.tex
  CF: P at file.tex
    CF: Sf at file.tex"
            .trim();
        let after_content = "
CF: D at file.tex
  CF: P at file.tex
    CF: Sa at file.tex
    CF: Sc at file.tex
  CF: P at file.tex
    CF: Sf at file.tex
  CF: P at file.tex
    CF: Sd at file.tex
    CF: Se at file.tex
    CF: Sg at file.tex"
            .trim();
        let diff = diff_tree_chawathe(before_content, after_content);
        assert_eq!(
            diff.edit_ops,
            vec![
                TreeEditOp::Move {
                    before_index: TreeNodeIndex::Node(8),
                    parent_index: TreeNodeIndex::Node(0),
                    child_position: 1,
                },
                TreeEditOp::Add {
                    after_index: TreeNodeIndex::Node(9),
                    parent_index: TreeNodeIndex::Node(5),
                    child_position: 2,
                },
                TreeEditOp::Remove {
                    before_index: TreeNodeIndex::Node(3),
                },
            ]
        );
    }

    #[test]
    fn remove_follows_tree_semantics() {
        // Adapted from case where text diffing fails
        // with Git's `t1007-hash-object` test
        // (removes lines 2 - 4, which does not make sense according to tree semantics)
        // TODO: Fix our algorithm to get this working
        // (currently removes lines 1, 5, 6, the opposite of text diffing)

        let before_content = "
CF: all_attrs_init at attr.c:155:3
  CT: container_of_or_null_offset at git-compat-util.h:1580:0
  RF: container_of_or_null_offset at git-compat-util.h:1581:2
CF: all_attrs_init at attr.c:155:3
  CT: hashmap_iter_next at hashmap.c:295:0
  RF: hashmap_iter_next at hashmap.c:308:1"
            .trim();
        let after_content = "
CF: all_attrs_init at attr.c:155:3
  CT: hashmap_iter_next at hashmap.c:295:0
  RF: hashmap_iter_next at hashmap.c:308:1"
            .trim();

        let diff = diff_tree(before_content, after_content);

        assert!(diff.diff_ops.len() > 0);
        let diff_op = diff.diff_ops.last().unwrap();
        if let TreeDiffOp::Remove { before_index } = diff_op {
            assert!(*before_index == 3);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn remove_nested_tree() {
        // Adapted from Git's `log` trace
        // Nested call to `git_has_dos_drive_prefix` removed
        // TODO: Fix our algorithm to get this working
        // (currently replaces line 5 instead of removing)

        let before_content = "
CF: system_path at exec-cmd.c:265:6
  CT: is_absolute_path at cache.h:1275:0
  CF: is_absolute_path at cache.h:1276:9
    CT: git_is_dir_sep at git-compat-util.h:447:0
    RF: git_is_dir_sep at git-compat-util.h:448:2
  CF: is_absolute_path at cache.h:1276:32
    CT: git_has_dos_drive_prefix at git-compat-util.h:432:0
    RF: git_has_dos_drive_prefix at git-compat-util.h:433:2
  RF: is_absolute_path at cache.h:1276:2
CF: system_path at exec-cmd.c:268:27
  CT: system_prefix at exec-cmd.c:247:0
  RF: system_prefix at exec-cmd.c:248:2"
            .trim();
        let after_content = "
CF: system_path at exec-cmd.c:265:6
  CT: is_absolute_path at cache.h:1275:0
  CF: is_absolute_path at cache.h:1276:9
    CT: git_is_dir_sep at git-compat-util.h:447:0
    RF: git_is_dir_sep at git-compat-util.h:448:2
  RF: is_absolute_path at cache.h:1276:2
CF: system_path at exec-cmd.c:268:27
  CT: system_prefix at exec-cmd.c:247:0
  RF: system_prefix at exec-cmd.c:248:2"
            .trim();

        let diff = diff_tree(before_content, after_content);

        assert!(diff.diff_ops.len() > 0);
        let diff_op = diff.diff_ops.last().unwrap();
        if let TreeDiffOp::Remove { before_index } = diff_op {
            assert!(*before_index == 5);
        } else {
            assert!(false);
        }
    }
}
