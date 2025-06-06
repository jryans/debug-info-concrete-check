use similar::{DiffOp, TextDiff};

use crate::tree::TreeDiff;

#[derive(Debug)]
// JRS: Could be a trait instead of struct...?
pub struct Diff<'content> {
    pub before_lines: Vec<&'content str>,
    pub after_lines: Vec<&'content str>,
    pub ops: Vec<DiffOp>,
}

impl<'content> From<TextDiff<'content, 'content, 'content, str>> for Diff<'content> {
    fn from(text_diff: TextDiff<'content, 'content, 'content, str>) -> Self {
        Diff {
            before_lines: text_diff.old_slices().to_vec(),
            after_lines: text_diff.new_slices().to_vec(),
            ops: text_diff.ops().to_vec(),
        }
    }
}

impl<'content> From<TreeDiff<'content>> for Diff<'content> {
    fn from(tree_diff: TreeDiff<'content>) -> Self {
        Diff {
            before_lines: tree_diff.before_lines,
            after_lines: tree_diff.after_lines,
            ops: tree_diff.diff_ops,
        }
    }
}

impl Diff<'_> {
    // JRS: Maybe return borrowed op instead...?
    pub fn grouped_ops(&self, n: usize) -> Vec<Vec<DiffOp>> {
        // TODO: Actually group things properly
        self.ops.iter().map(|op| vec![op.clone()]).collect()
    }
}
