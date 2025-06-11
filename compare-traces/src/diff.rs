use similar::{group_diff_ops, DiffOp, TextDiff};

use crate::tree::TreeDiff;

#[derive(Debug)]
// JRS: Could be a trait instead of struct...?
pub struct Diff<'content> {
    pub before_lines: Vec<&'content str>,
    pub after_lines: Vec<&'content str>,
    pub grouped_diff_ops: Vec<Vec<DiffOp>>,
}

impl<'content> From<TextDiff<'content, 'content, 'content, str>> for Diff<'content> {
    fn from(text_diff: TextDiff<'content, 'content, 'content, str>) -> Self {
        Diff {
            before_lines: text_diff.old_slices().to_vec(),
            after_lines: text_diff.new_slices().to_vec(),
            grouped_diff_ops: group_diff_ops(text_diff.ops().to_vec(), 1),
        }
    }
}

impl<'content> From<TreeDiff<'content>> for Diff<'content> {
    fn from(tree_diff: TreeDiff<'content>) -> Self {
        Diff {
            before_lines: tree_diff.before_lines,
            after_lines: tree_diff.after_lines,
            grouped_diff_ops: tree_diff.grouped_diff_ops,
        }
    }
}
