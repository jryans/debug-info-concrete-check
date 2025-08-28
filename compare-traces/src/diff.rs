use std::path::PathBuf;

use similar::{group_diff_ops, DiffOp, TextDiff};

use crate::trace::Trace;
use crate::tree_diff::TreeDiff;

#[derive(Debug)]
// JRS: Could be a trait instead of struct...?
pub struct Diff<'content> {
    pub before_file_path: Option<PathBuf>,
    pub after_file_path: Option<PathBuf>,
    pub before_trace: Trace<'content>,
    pub after_trace: Trace<'content>,
    pub grouped_diff_ops: Vec<Vec<DiffOp>>,
}

impl<'content> Diff<'content> {
    pub fn new(
        before_trace: Trace<'content>,
        after_trace: Trace<'content>,
        grouped_diff_ops: Vec<Vec<DiffOp>>,
    ) -> Self {
        Diff {
            before_file_path: None,
            after_file_path: None,
            before_trace,
            after_trace,
            grouped_diff_ops,
        }
    }
}

impl<'content> From<TextDiff<'content, 'content, 'content, str>> for Diff<'content> {
    fn from(text_diff: TextDiff<'content, 'content, 'content, str>) -> Self {
        Diff::new(
            Trace::parse_lines(text_diff.old_slices().to_vec()),
            Trace::parse_lines(text_diff.new_slices().to_vec()),
            group_diff_ops(text_diff.ops().to_vec(), 1),
        )
    }
}

impl<'content> From<TreeDiff<'content>> for Diff<'content> {
    fn from(tree_diff: TreeDiff<'content>) -> Self {
        Diff::new(
            tree_diff.before_trace,
            tree_diff.after_trace,
            tree_diff.grouped_diff_ops,
        )
    }
}
