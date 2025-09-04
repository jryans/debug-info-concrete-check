use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{DefaultHasher, Hash, Hasher};

use bimap::BiHashMap;
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use similar::{capture_diff_slices, DiffOp, DiffTag};
use strsim::normalized_levenshtein;

use crate::event::{Event, Eventable};
use crate::trace::Trace;
use crate::tree::{Tree, TreeNode, TreeNodeIndex};

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

/// Tree edits that transform the before tree into the after tree.
/// Indices are 0-based.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum TreeEditOp {
    Add {
        /// New parent node index in before tree
        parent_index: TreeNodeIndex,
        /// Position in new parent node's children
        child_position: usize,
        /// Node index in after tree
        after_index: TreeNodeIndex,
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
        /// Node index in after tree
        after_index: TreeNodeIndex,
    },
}

impl TreeEditOp {
    fn to_diff_ops(&self, before_tree: &Tree, after_tree: &Tree) -> Vec<DiffOp> {
        // JRS: Ideally the zeros below would actually be `None`,
        // since `0` points to an actual line, which is not what we mean here.
        match self {
            TreeEditOp::Add {
                parent_index: _,
                child_position: _,
                after_index,
            } => vec![DiffOp::Insert {
                old_index: {
                    // let mut index = parent_index.unwrap();
                    // let before_parent = &before_tree[parent_index];
                    // // JRS: Child position was only accurate at the time of the edit...
                    // index + 1
                    // JRS: Not sure if we actually use this...?
                    // Let's force this to 0 for now...
                    0
                },
                // JRS: Maybe we can't use after index at all, and instead
                // we need to count in the before tree...?
                // Keep in mind the before tree hsa now been edited to match after...
                new_index: after_index.unwrap(),
                new_len: 1,
            }],
            TreeEditOp::Remove { before_index } => {
                let node = &before_tree[before_index];
                let subtree_len = node.subtree_len(before_tree);
                vec![DiffOp::Delete {
                    old_index: before_index.unwrap(),
                    old_len: subtree_len,
                    new_index: {
                        // JRS: Not sure if we actually use this...?
                        // Let's force this to 0 for now...
                        0
                    },
                }]
            }
            TreeEditOp::Replace {
                before_index,
                after_index,
            } => vec![DiffOp::Replace {
                old_index: before_index.unwrap(),
                old_len: 1,
                new_index: after_index.unwrap(),
                new_len: 1,
            }],
            TreeEditOp::Move {
                before_index,
                after_index,
                ..
            } => {
                // JRS: Viewing a move as if it were a text diff is more noise than signal...
                // Divergence reporting then thinks all nested lines have changed.
                // We should instead think of these explicitly in tree edit terms.
                let before_node = &before_tree[before_index];
                let before_subtree_len = before_node.dfs_pre_order(before_tree).count();
                let after_node = &after_tree[after_index];
                let after_subtree_len = after_node.dfs_pre_order(after_tree).count();
                vec![
                    DiffOp::Delete {
                        old_index: before_index.unwrap(),
                        old_len: before_subtree_len,
                        new_index: {
                            // JRS: Not sure if we actually use this...?
                            // Let's force this to 0 for now...
                            0
                        },
                    },
                    DiffOp::Insert {
                        old_index: {
                            // JRS: Not sure if we actually use this...?
                            // Let's force this to 0 for now...
                            0
                        },
                        new_index: after_index.unwrap(),
                        new_len: after_subtree_len,
                    },
                ]
            }
        }
    }
}

// JRS: Consider implementing all edits here,
// if we can navigate the lifetime rules to do so...
fn edit_tree(tree: &mut Tree, edit: &TreeEditOp) {
    match edit {
        TreeEditOp::Remove { before_index } => {
            let parent_index = tree[before_index].parent.unwrap();
            let parent = &mut tree[&parent_index];
            let child_position = parent.child_position(before_index).unwrap();
            parent.children.remove(child_position);
        }
        TreeEditOp::Move {
            before_index,
            parent_index,
            child_position,
            ..
        } => {
            let current_parent_index = tree[before_index].parent.unwrap();
            // Changing parents is not expected with our tree semantics,
            // but it may of course happen in general tree diffing.
            // We support it here to allow testing the algorithm's examples.
            let same_parent = current_parent_index == *parent_index;
            let current_parent = &mut tree[&current_parent_index];
            let current_child_position = current_parent.child_position(before_index).unwrap();
            let child = current_parent.children.remove(current_child_position);
            let target_parent = &mut tree[parent_index];
            let mut new_child_position = *child_position;
            // Move op child position describes the position before any modification.
            // For same-parent moves, the `remove` just above means we need to adjust by 1
            // if the previous position is before the new position (as removal will have
            // shifted it over).
            if same_parent && current_child_position < new_child_position {
                new_child_position -= 1;
            }
            target_parent.children.insert(new_child_position, child);
            // If parent did change, update the child's `parent` field as well
            if !same_parent {
                tree[before_index].parent = Some(*parent_index);
            }
        }
        _ => unimplemented!(),
    }
}

fn compact_diff_ops(grouped_diff_ops: &mut Vec<Vec<DiffOp>>) {
    for i in 0..(grouped_diff_ops.len() - 1) {
        // Double check end condition, as we may remove items during compaction
        if i > grouped_diff_ops.len() - 2 {
            break;
        }
        let current_op_group = &grouped_diff_ops[i];
        let next_op_group = &grouped_diff_ops[i + 1];
        let current_op_group_tags = current_op_group.iter().map(|op| op.tag());
        let next_op_group_tags = next_op_group.iter().map(|op| op.tag());
        // If both op groups have the same tags, compaction may be possible
        if current_op_group_tags.ne(next_op_group_tags) {
            continue;
        }
        // Compaction supports groups with a single op only
        if current_op_group.len() > 1 {
            continue;
        }
        let current_op = current_op_group[0];
        let next_op = next_op_group[0];
        // If ops are adjacent, compaction can proceed
        if current_op.old_range().end != next_op.old_range().start
            || current_op.new_range().end != next_op.new_range().start
        {
            continue;
        }
        let compacted_op = match current_op.tag() {
            DiffTag::Delete => DiffOp::Delete {
                old_index: current_op.old_range().start,
                old_len: current_op.old_range().len() + next_op.old_range().len(),
                new_index: current_op.new_range().start,
            },
            DiffTag::Insert => DiffOp::Insert {
                old_index: current_op.old_range().start,
                new_index: current_op.new_range().start,
                new_len: current_op.new_range().len() + next_op.new_range().len(),
            },
            DiffTag::Replace => DiffOp::Replace {
                old_index: current_op.old_range().start,
                old_len: current_op.old_range().len() + next_op.old_range().len(),
                new_index: current_op.new_range().start,
                new_len: current_op.new_range().len() + next_op.new_range().len(),
            },
            DiffTag::Equal => unreachable!(),
        };
        grouped_diff_ops[i] = vec![compacted_op];
        // TODO: Use a different data structure to make this more efficient...?
        grouped_diff_ops.remove(i + 1);
    }
}

#[derive(Debug)]
pub struct TreeDiff<'content> {
    pub before_trace: Trace<'content>,
    pub after_trace: Trace<'content>,
    pub matching: BiHashMap<TreeNodeIndex, TreeNodeIndex>,
    pub edit_ops: Vec<TreeEditOp>,
    pub grouped_diff_ops: Vec<Vec<DiffOp>>,
}

fn is_known_library_function_replacement(before: &str, after: &str) -> bool {
    static KNOWN_REPLACEMENTS: Lazy<HashMap<&str, &str>> =
        Lazy::new(|| HashMap::from([("printf", "puts")]));
    KNOWN_REPLACEMENTS
        .get(before)
        .map_or(false, |expected| after == *expected)
}

/// Override `Event` equality to allow for some source coordinate drift
#[derive(Clone, Debug)]
struct FuzzyEvent(Event);

impl FuzzyEvent {
    fn self_eq(a: &Event, b: &Event) -> bool {
        if a == b {
            return true;
        }

        if a.event_type != b.event_type {
            return false;
        }

        let a_loc = &a.location;
        let b_loc = &b.location;

        if a_loc.function.is_some()
            && a_loc.file.is_none()
            && b_loc.function.is_some()
            && b_loc.file.is_none()
        {
            // Most likely an external code event
            let a_func = a_loc.function.as_ref().unwrap();
            let b_func = b_loc.function.as_ref().unwrap();

            // Allow some function name edits to detect call replacement
            if normalized_levenshtein(a_func, b_func) >= 0.5 {
                return true;
            }

            // Check for known function name changes that exceed Levenshtein limit
            return is_known_library_function_replacement(a_func, b_func);
        }

        if a_loc.function.is_some()
            && b_loc.function.is_some()
            && (a_loc.file.is_none() || b_loc.file.is_none())
        {
            // Could be an inlined external code event
            // Allow this to match as long as the function is the same
            return a_loc.function == b_loc.function;
        }

        if a_loc.function != b_loc.function || a_loc.file != b_loc.file {
            return false;
        }

        if a_loc.line.is_some() != b_loc.line.is_some() {
            return false;
        }

        true
    }
}

impl PartialEq for FuzzyEvent {
    fn eq(&self, other: &Self) -> bool {
        if !FuzzyEvent::self_eq(&self.0, &other.0) {
            return false;
        }

        // JRS: Doesn't feel right to repeat this here,
        // partner check should be generic across event kind somehow
        if self.0.partner.is_some() != other.0.partner.is_some() {
            return false;
        }

        if self.0.partner.is_some() {
            let self_partner = self.0.partner.as_ref().unwrap();
            let other_partner = other.0.partner.as_ref().unwrap();
            if !FuzzyEvent::self_eq(self_partner, other_partner) {
                return false;
            }
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
    // For each event, look for parent's previously computed hasher,
    // and then add that event's own function name (in case it has children)
    let mut hashers_as_parents: Vec<DefaultHasher> = Vec::with_capacity(events.len());
    for i in 0..events.len() {
        let event = events[i].as_event();
        let node = &tree[&TreeNodeIndex::Node(i)];
        let parent_index = node.parent(tree).unwrap().index;
        let parent_hasher = match parent_index {
            TreeNodeIndex::Node(p) => Some(&hashers_as_parents[p]),
            TreeNodeIndex::Root => None,
        };
        let mut hasher: DefaultHasher =
            parent_hasher.map_or(DefaultHasher::new(), |hasher| hasher.clone());
        event.location.function.hash(&mut hasher);
        hashers_as_parents.push(hasher);
    }

    // For each event, use its _parent's_ hash as the label
    // (excluding its own function name from the label)
    let labels_as_parents: Vec<u64> = hashers_as_parents
        .into_iter()
        .map(|hasher| hasher.finish())
        .collect();
    let mut labels: Vec<u64> = Vec::with_capacity(events.len());
    for i in 0..events.len() {
        let node = &tree[&TreeNodeIndex::Node(i)];
        let parent_index = node.parent(tree).unwrap().index;
        let label = match parent_index {
            TreeNodeIndex::Node(p) => labels_as_parents[p],
            TreeNodeIndex::Root => 0,
        };
        labels.push(label);
    }

    labels
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
    for leaf in before_tree.dfs_pre_order().filter(|node| node.is_leaf()) {
        let label = leaf.data(before_labels);
        before_leaves_by_label
            .entry(*label)
            .or_default()
            .push(leaf.index);
    }
    let mut after_leaves_by_label: HashMap<u64, Vec<TreeNodeIndex>> = HashMap::new();
    for leaf in after_tree.dfs_pre_order().filter(|node| node.is_leaf()) {
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
    for branch in before_tree.dfs_pre_order().filter(|node| node.is_branch()) {
        let label = branch.data(before_labels);
        before_branches_by_label
            .entry(*label)
            .or_default()
            .push(branch.index);
    }
    let mut after_branches_by_label: HashMap<u64, Vec<TreeNodeIndex>> = HashMap::new();
    for branch in after_tree.dfs_pre_order().filter(|node| node.is_branch()) {
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
            .dfs_pre_order()
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

    // Roots are assumed to match by design
    matching.insert(Tree::root_index(), Tree::root_index());

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
            after_index: *after_child_index,
        };
        // Apply move to before tree and add to ops
        edit_tree(before_tree, &op);
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
    let after_child_position = after_parent.child_position(after_child_index).unwrap();
    let mut after_sibling_index = None;
    for after_index_to_check in after_parent.children[0..after_child_position].iter().rev() {
        if !after_in_order.contains(after_index_to_check) {
            continue;
        }
        after_sibling_index = Some(*after_index_to_check);
        break;
    }
    // If there aren't any in-order siblings to the left, assume we should use first position
    if after_sibling_index.is_none() {
        return 0;
    }
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
    let before_sibling_position = before_parent.child_position(before_sibling_index).unwrap();
    // Return position after sibling
    before_sibling_position + 1
}

/// Diff tree using the Chawathe et al. approach
// TODO: Separate diff algorithm from tree events to simplify testing and reuse
pub fn diff_tree<'content>(
    mut before: Trace<'content>,
    after: Trace<'content>,
) -> TreeDiff<'content> {
    // Copy before tree, as we'll be modifying it as we go along,
    // bringing it incrementally closer to the after tree
    let before_unmodified = before.clone();

    // Morph trace events into fuzzy events
    let mut before_events: Vec<_> = before
        .events
        .into_iter()
        .map(|event| FuzzyEvent(event))
        .collect();
    let after_events: Vec<_> = after
        .events
        .iter()
        .cloned()
        .map(|event| FuzzyEvent(event))
        .collect();

    // Collect tree event labels from function names along each node's tree path
    let before_labels = tree_event_labels(&before.tree, &before_events);
    let after_labels = tree_event_labels(&after.tree, &after_events);

    // Build initial matching bimap
    let mut matching = matching_bimap(
        &before.tree,
        &after.tree,
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
    for after_node in after.tree.bfs() {
        let after_index = &after_node.index;
        if matching.contains_right(after_index) {
            // Match exists, check if replacements or moves are needed
            let before_index = matching.get_by_right(after_index).unwrap();
            let before_node = &before.tree[before_index];
            // Compare events without fuzzy wrappers and partners
            let before_event = before_node.data(&before_events);
            let after_event = after_node.data(&after_events);
            if !Event::self_eq(before_event.as_event(), after_event.as_event()) {
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
            let after_parent_index = &after_node.parent(&after.tree).unwrap().index;
            let current_before_parent_index = &before_node.parent(&before.tree).unwrap().index;
            if let Some(target_before_parent_index) = matching.get_by_right(after_parent_index) {
                if current_before_parent_index != target_before_parent_index {
                    // Parent match not found, move node to target parent
                    // JRS: We don't expect parent changes with our tree semantics,
                    // though they do of course appear for general tree diffs.
                    // Perhaps optionally disable this ability for our trees...?
                    let child_position = find_position_in_parent(
                        &before.tree,
                        &after.tree,
                        &matching,
                        after_index,
                        &mut before_in_order,
                        &mut after_in_order,
                    );
                    let op = TreeEditOp::Move {
                        before_index: *before_index,
                        parent_index: *target_before_parent_index,
                        child_position,
                        after_index: *after_index,
                    };
                    // Apply move to before tree and add to ops
                    edit_tree(&mut before.tree, &op);
                    edit_ops.push(op);
                }
            }
        } else {
            // Match not found, need to add after node
            let child_position = find_position_in_parent(
                &before.tree,
                &after.tree,
                &matching,
                after_index,
                &mut before_in_order,
                &mut after_in_order,
            );
            let after_parent_index = &after_node.parent(&after.tree).unwrap().index;
            let before_parent_index = matching.get_by_right(after_parent_index).unwrap();
            let op = TreeEditOp::Add {
                parent_index: *before_parent_index,
                child_position,
                after_index: *after_index,
            };
            // Add to the before tree
            let before_tree_position = before_events.len();
            before.lines.push(*after_node.data(&after.lines));
            before_events.push(after_node.data(&after_events).clone());
            // JRS: Do we need to attach partners when applying add operations...?
            // We don't need to worry about labels at this stage,
            // as they're only used to create the initial bimap
            let before_node = TreeNode::new(before_tree_position, *before_parent_index);
            let before_index = before.tree.register(before_node);
            let before_parent = &mut before.tree[before_parent_index];
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
            &mut before.tree,
            &after.tree,
            &matching,
            before_index,
            after_index,
            &mut before_in_order,
            &mut after_in_order,
        );
        // Merge moves into ops
        edit_ops.extend(move_ops);
    }

    // Visit before nodes using depth-first, post-order search
    let mut deleted_before_nodes: HashSet<TreeNodeIndex> = HashSet::new();
    let mut delete_ops: Vec<TreeEditOp> = Vec::new();
    for before_node in before.tree.dfs_post_order() {
        let before_index = &before_node.index;
        if !matching.contains_left(before_index) {
            // Add to deleted node set
            deleted_before_nodes.insert(*before_index);
            // Match not found, need to remove before node
            let op = TreeEditOp::Remove {
                before_index: *before_index,
            };
            // Add to temporary delete ops
            delete_ops.push(op);
        }
    }

    // JRS: We used to apply the delete ops to the before tree here,
    // but this complicates counting the size of before sub-trees.
    // Since we don't actually need to keep editing the before tree
    // for correct positions (since the delete phase is last),
    // we skip applying these edits for now.

    // For diffing purposes, we'd rather have a single delete op for a sub-tree
    // (instead of separate ops for each node in the sub-tree)
    let filtered_delete_ops = delete_ops.into_iter().filter(|op| {
        let before_index = match op {
            TreeEditOp::Remove { before_index } => before_index,
            _ => unreachable!(),
        };
        let parent_index = match before.tree[before_index].parent(&before.tree) {
            Some(parent_node) => &parent_node.index,
            None => return true,
        };
        // If parent node is also deleted, filter out child node
        !deleted_before_nodes.contains(parent_index)
    });

    // Merge filtered deletes into ops
    edit_ops.extend(filtered_delete_ops);

    // Convert edit ops to grouped diff ops
    let mut grouped_diff_ops: Vec<Vec<DiffOp>> = edit_ops
        .iter()
        // JRS: Divergence reporting is not move-aware yet,
        // and converting moves to text diffs creates too much noise,
        // so for now we filter them out
        .filter(|edit_op| !matches!(edit_op, TreeEditOp::Move { .. }))
        .map(|edit_op| edit_op.to_diff_ops(&before.tree, &after.tree))
        .collect();
    // Sort to enable more compaction opportunities, then compact adjacent ops
    grouped_diff_ops.sort_unstable_by_key(|diff_op_group| {
        let op = &diff_op_group[0];
        op.old_range().start.max(op.new_range().start)
    });
    compact_diff_ops(&mut grouped_diff_ops);

    TreeDiff {
        before_trace: before_unmodified,
        after_trace: after,
        matching,
        edit_ops,
        grouped_diff_ops,
    }
}

#[cfg(test)]
mod tests {
    use crate::tree::{Tree, TreeNodeIndex};

    use super::*;

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
            .dfs_pre_order()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_2: Vec<TreeNodeIndex> = tree_2
            .dfs_pre_order()
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
            .dfs_pre_order()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_2: Vec<TreeNodeIndex> = tree_2
            .dfs_pre_order()
            .filter(|node| node.is_leaf())
            .map(|node| node.index)
            .collect();
        let leaves_lcs =
            tree_items_subset_lcs(&tree_1, &tree_2, &events_1, &events_2, &leaves_1, &leaves_2);
        assert_eq!(
            leaves_lcs.matched,
            vec![
                (TreeNodeIndex::Node(1), TreeNodeIndex::Node(1)),
                (TreeNodeIndex::Node(2), TreeNodeIndex::Node(2)),
            ]
        );
    }

    #[test]
    fn tree_fuzzy_external_function_replacement() {
        let event_1 = FuzzyEvent(Event::parse("CT: Jump to external code for memcmp").unwrap());
        let event_2 = FuzzyEvent(Event::parse("CT: Jump to external code for bcmp").unwrap());
        assert_eq!(event_1, event_2);
        let event_3 = FuzzyEvent(Event::parse("CT: Jump to external code for pomelo").unwrap());
        assert_ne!(event_1, event_3);
    }

    #[test]
    fn tree_fuzzy_ne_partner() {
        let mut event_1 = Event::parse("CF: system_path at exec-cmd.c:265:6").unwrap();
        let event_1p = Event::parse("CT: foo at foo.c:0:0").unwrap();
        event_1.attach_partner(&event_1p);
        let mut event_2 = Event::parse("CF: system_path at exec-cmd.c:265:6").unwrap();
        let event_2p = Event::parse("CT: bob at bob.c:0:0").unwrap();
        event_2.attach_partner(&event_2p);
        assert_ne!(FuzzyEvent(event_1), FuzzyEvent(event_2));
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
        assert_eq!(matching.len(), 10); // Actual nodes plus implicit root
        let mut items_1_matched: Vec<&str> = matching
            .left_values()
            .filter(|index| !index.is_root())
            .map(|index| tree_1[&index].data(&items_1).trim())
            .collect();
        items_1_matched.sort();
        let mut items_2_matched: Vec<&str> = matching
            .right_values()
            .filter(|index| !index.is_root())
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
        assert_eq!(matching.len(), 7); // Actual nodes plus implicit root
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
                    after_index: TreeNodeIndex::Node(4),
                },
                TreeEditOp::Move {
                    before_index: TreeNodeIndex::Node(3),
                    parent_index: TreeNodeIndex::Node(0),
                    child_position: 5,
                    after_index: TreeNodeIndex::Node(5),
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
        let diff = diff_tree(
            Trace::parse_str(before_content),
            Trace::parse_str(after_content),
        );
        assert_eq!(
            diff.edit_ops,
            vec![
                TreeEditOp::Move {
                    before_index: TreeNodeIndex::Node(8),
                    parent_index: TreeNodeIndex::Node(0),
                    child_position: 1,
                    after_index: TreeNodeIndex::Node(4),
                },
                TreeEditOp::Add {
                    parent_index: TreeNodeIndex::Node(5),
                    child_position: 2,
                    after_index: TreeNodeIndex::Node(9),
                },
                TreeEditOp::Remove {
                    before_index: TreeNodeIndex::Node(3),
                },
            ]
        );
        assert_eq!(
            diff.grouped_diff_ops,
            vec![
                vec![DiffOp::Delete {
                    old_index: 3,
                    old_len: 1,
                    new_index: 0,
                }],
                vec![DiffOp::Insert {
                    old_index: 0,
                    new_index: 9,
                    new_len: 1,
                }],
            ]
        );
    }

    #[test]
    fn remove_follows_tree_semantics() {
        // Adapted from case where text diffing fails
        // with Git's `t1007-hash-object` test
        // (removes lines 2 - 4, which does not make sense according to tree semantics)
        // Chawathe et al. tree algorithm removes lines 1 - 3, following tree semantics
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
        let diff = diff_tree(
            Trace::parse_str(before_content),
            Trace::parse_str(after_content),
        );
        assert_eq!(
            diff.edit_ops,
            vec![TreeEditOp::Remove {
                before_index: TreeNodeIndex::Node(0),
            }]
        );
        assert_eq!(
            diff.grouped_diff_ops,
            vec![vec![DiffOp::Delete {
                old_index: 0,
                old_len: 3,
                new_index: 0,
            }]]
        );
    }

    #[test]
    fn remove_nested_tree() {
        // Adapted from Git's `log` trace
        // Nested call to `git_has_dos_drive_prefix` removed
        // Chawathe et al. tree algorithm removes lines 6 - 8, following tree semantics
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
        let diff = diff_tree(
            Trace::parse_str(before_content),
            Trace::parse_str(after_content),
        );
        assert_eq!(
            diff.edit_ops,
            vec![TreeEditOp::Remove {
                before_index: TreeNodeIndex::Node(5),
            }]
        );
        assert_eq!(
            diff.grouped_diff_ops,
            vec![vec![DiffOp::Delete {
                old_index: 5,
                old_len: 3,
                new_index: 0,
            }]]
        );
    }

    #[test]
    fn compact_adjacent_diff_ops() {
        let before_content = "
CF: strbuf_vaddf at strbuf.c:397:8
  CT: Jump to external code for ___vsnprintf_chk
  RF: Jump to external code for ___vsnprintf_chk"
            .trim();
        let after_content = "
CF: strbuf_vaddf at strbuf.c:397:8
  CT: Jump to external code for _vsnprintf
  RF: Jump to external code for _vsnprintf"
            .trim();
        let diff = diff_tree(
            Trace::parse_str(before_content),
            Trace::parse_str(after_content),
        );
        assert_eq!(
            diff.edit_ops,
            vec![
                TreeEditOp::Replace {
                    before_index: TreeNodeIndex::Node(1),
                    after_index: TreeNodeIndex::Node(1),
                },
                TreeEditOp::Replace {
                    before_index: TreeNodeIndex::Node(2),
                    after_index: TreeNodeIndex::Node(2),
                }
            ]
        );
        assert_eq!(
            diff.grouped_diff_ops,
            vec![vec![DiffOp::Replace {
                old_index: 1,
                old_len: 2,
                new_index: 1,
                new_len: 2,
            }]]
        );
    }

    #[test]
    fn root_parent_findable() {
        // Adding call to partners to call from events
        // and checking them as part of equality
        // incidentally means some root-level call from events
        // became non-matching, which revealed the hidden root node
        // was not in the matching bimap.
        // This test then quickly became of less use because equality was relaxed
        // to permit large coordinate changes, which allows the
        // root-level nodes here to be reused.
        let before_content = "
CF: main at ffmpeg.c:4521:5
  CT: init_dynload at cmdutils.c:78:0
  RF: init_dynload at cmdutils.c:84:1"
            .trim();
        let after_content = "
CF: main at ffmpeg.c:4521:5
  CT: init_dynload at cmdutils.c:84:1
  RF: init_dynload at cmdutils.c:84:1"
            .trim();
        let diff = diff_tree(
            Trace::parse_str(before_content),
            Trace::parse_str(after_content),
        );
        assert_eq!(
            diff.edit_ops,
            vec![TreeEditOp::Replace {
                before_index: TreeNodeIndex::Node(1),
                after_index: TreeNodeIndex::Node(1),
            }]
        );
        assert_eq!(
            diff.grouped_diff_ops,
            vec![vec![DiffOp::Replace {
                old_index: 1,
                old_len: 1,
                new_index: 1,
                new_len: 1,
            }]]
        );
    }

    #[test]
    fn uncategorised_ex1() {
        // From `git/program/clang/13/O1/divergences/t1007-hash-object`
        // Uncategorised
        //   After events:
        //     CF: _ at gettext.h:48:9
        //   Occurrences: 4
        //   Example trace lines: -1, +304078
        // Individual small replacements, seems like ideal diff output
        let before_content = "
CF: error_builtin at usage.c:81:11
  CT: _ at gettext.h:45:0
  CF: _ at gettext.h:48:9
    CT: Jump to external code for gettext
    RF: Jump to external code for gettext
  RF: _ at gettext.h:49:1"
            .trim();
        let after_content = "
ICF: error_builtin at usage.c:81:11
  ICT: _ at gettext.h:44:0
  CF: _ at gettext.h:48:9
    CT: Jump to external code for dcgettext
    RF: Jump to external code for dcgettext
  IRF: _ at gettext.h:0:0"
            .trim();
        let diff = diff_tree(
            Trace::parse_str(before_content),
            Trace::parse_str(after_content),
        );
        assert_eq!(
            diff.edit_ops,
            vec![
                TreeEditOp::Replace {
                    before_index: TreeNodeIndex::Node(1),
                    after_index: TreeNodeIndex::Node(1)
                },
                TreeEditOp::Replace {
                    before_index: TreeNodeIndex::Node(5),
                    after_index: TreeNodeIndex::Node(5)
                },
                TreeEditOp::Replace {
                    before_index: TreeNodeIndex::Node(3),
                    after_index: TreeNodeIndex::Node(3)
                },
                TreeEditOp::Replace {
                    before_index: TreeNodeIndex::Node(4),
                    after_index: TreeNodeIndex::Node(4)
                }
            ]
        );
        assert_eq!(
            diff.grouped_diff_ops,
            vec![
                vec![DiffOp::Replace {
                    old_index: 1,
                    old_len: 1,
                    new_index: 1,
                    new_len: 1
                }],
                vec![DiffOp::Replace {
                    old_index: 3,
                    old_len: 2,
                    new_index: 3,
                    new_len: 2
                }],
                vec![DiffOp::Replace {
                    old_index: 5,
                    old_len: 1,
                    new_index: 5,
                    new_len: 1
                }],
            ]
        );
    }
}
