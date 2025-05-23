use std::hash::Hash;
use std::ops::{Index, IndexMut};

use similar::{capture_diff_slices, DiffOp, TextDiff};

use crate::event::{line_depth, Event};

/// `None` is reserved for the root node.
/// This allows nodes indices to remain in sync with those for the separate data array.
/// JRS: This should be a proper wrapper type...
/// A few places are using `None` to mean no value,
/// which may cause confusion.
type TreeNodeIndex = Option<usize>;

#[derive(PartialEq, Eq, Debug)]
struct TreeNode {
    index: TreeNodeIndex,
    parent: TreeNodeIndex,
    children: Vec<TreeNodeIndex>,
}

impl TreeNode {
    fn new(index: usize, parent: TreeNodeIndex) -> TreeNode {
        TreeNode {
            index: Some(index),
            parent,
            children: Vec::new(),
        }
    }

    fn new_root() -> TreeNode {
        TreeNode {
            index: None,
            parent: None,
            children: Vec::new(),
        }
    }

    fn parent<'tree>(&self, tree: &'tree Tree) -> Option<&'tree TreeNode> {
        match &self.index {
            Some(_) => Some(&tree[&self.parent]),
            None => None,
        }
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

    fn data<'container, T>(&self, container: &'container [T]) -> &'container T {
        &container[self.index.unwrap()]
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
        None
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

    fn leaves(&self) -> TreeLeaves {
        TreeLeaves {
            tree: &self,
            stack: vec![self.root()],
        }
    }
}

impl Index<&TreeNodeIndex> for Tree {
    type Output = TreeNode;

    fn index(&self, index: &TreeNodeIndex) -> &Self::Output {
        match *index {
            Some(i) => &self.nodes[i],
            None => &self.root,
        }
    }
}

impl IndexMut<&TreeNodeIndex> for Tree {
    fn index_mut(&mut self, index: &TreeNodeIndex) -> &mut Self::Output {
        match *index {
            Some(i) => &mut self.nodes[i],
            None => &mut self.root,
        }
    }
}

struct TreeLeaves<'tree> {
    tree: &'tree Tree,
    stack: Vec<&'tree TreeNode>,
}

impl<'tree> Iterator for TreeLeaves<'tree> {
    type Item = &'tree TreeNode;

    /// Advance to the next leaf node
    fn next(&mut self) -> Option<Self::Item> {
        if self.stack.is_empty() {
            return None;
        }

        while !self.stack.is_empty() {
            // Pop from the end of the stack
            let node = self.stack.pop().unwrap();
            if node.children.is_empty() {
                // If `node` is a leaf, we can stop here for now.
                return Some(node);
            } else {
                // If `node` is a branch, push all children (in reverse for expected ordering).
                for i in (0..node.children.len()).rev() {
                    self.stack.push(node.child(&self.tree, i).unwrap());
                }
            }
        }

        return None;
    }
}

struct TreeLcs {
    matched: Vec<(TreeNodeIndex, TreeNodeIndex)>,
    unmatched: Vec<(TreeNodeIndex, TreeNodeIndex)>,
}

fn tree_leaves_lcs<T>(tree_a: &Tree, tree_b: &Tree, items_a: &[T], items_b: &[T]) -> TreeLcs
where
    // `Hash` and `Ord` do not appear to actually be used by the diff algorithm
    T: Eq + Hash + Ord,
{
    let leaves_a: Vec<&TreeNode> = tree_a.leaves().collect();
    let leaves_a_items: Vec<&T> = leaves_a.iter().map(|node| node.data(&items_a)).collect();
    let leaves_b: Vec<&TreeNode> = tree_b.leaves().collect();
    let leaves_b_items: Vec<&T> = leaves_b.iter().map(|node| node.data(&items_b)).collect();
    // TODO: Consider using `similar::IdentifyDistinct` for large inputs
    let diff_ops = capture_diff_slices(similar::Algorithm::Myers, &leaves_a_items, &leaves_b_items);
    let mut matched = Vec::new();
    let mut unmatched = Vec::new();
    for diff_op in diff_ops {
        // Translate leaf indices back up to tree indices
        // JRS: Should keep these small summary representations,
        // instead of inflating them to cover each item...?
        match diff_op {
            DiffOp::Equal {
                old_index,
                new_index,
                len,
            } => {
                for i in 0..len {
                    matched.push((leaves_a[old_index + i].index, leaves_b[new_index + i].index));
                }
            }
            DiffOp::Delete {
                old_index,
                old_len,
                new_index: _,
            } => {
                for i in 0..old_len {
                    unmatched.push((leaves_a[old_index + i].index, None));
                }
            }
            DiffOp::Insert {
                old_index: _,
                new_index,
                new_len,
            } => {
                for i in 0..new_len {
                    unmatched.push((None, leaves_b[new_index + i].index));
                }
            }
            DiffOp::Replace {
                old_index,
                old_len,
                new_index,
                new_len,
            } => {
                for i in 0..old_len {
                    unmatched.push((leaves_a[old_index + i].index, None));
                }
                for i in 0..new_len {
                    unmatched.push((None, leaves_b[new_index + i].index));
                }
            }
        }
    }
    TreeLcs { matched, unmatched }
}

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
    // // JRS: Unclear if we'll actually support this...
    // Reorder {
    //     /// Index of reordered line in before content
    //     before_index: usize,
    //     /// Index of reordered line in after content
    //     after_index: usize,
    //     // JRS: Do we also want indices within their stack frame?
    // },
}

#[derive(Debug)]
pub struct TreeDiff<'content> {
    before_lines: Vec<&'content str>,
    after_lines: Vec<&'content str>,
    ops: Vec<TreeDiffOp>,
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
        ops,
    }
}

/// Override `Event` equality to allow for some source coordinate drift
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
    fn tree_leaves() {
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
        let leaves: Vec<&str> = tree.leaves().map(|node| node.data(&items).trim()).collect();
        assert_eq!(leaves, vec!["0.0.0", "0.1", "0.2", "1.0.0"]);
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
        let leaves_lcs = tree_leaves_lcs(&tree_1, &tree_2, &items_1, &items_2);
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
            .map(|index| tree_1[&index].data(&items_1).trim())
            .collect();
        let items_2_lcs_unmatched: Vec<&str> = leaves_lcs
            .unmatched
            .iter()
            .map(|index_pair| index_pair.1)
            .filter(|index| index.is_some())
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
        let leaves_lcs = tree_leaves_lcs(&tree_1, &tree_2, &events_1, &events_2);
        assert_eq!(leaves_lcs.matched, vec![(Some(1), Some(1))]);
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
        println!("{:?}", &diff);

        assert!(diff.ops.len() > 0);
        let diff_op = diff.ops.last().unwrap();
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
        println!("{:?}", &diff);

        assert!(diff.ops.len() > 0);
        let diff_op = diff.ops.last().unwrap();
        if let TreeDiffOp::Remove { before_index } = diff_op {
            assert!(*before_index == 5);
        } else {
            assert!(false);
        }
    }
}
