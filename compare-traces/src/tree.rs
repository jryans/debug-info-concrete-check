use once_cell::sync::Lazy;
use regex::Regex;
use similar::TextDiff;

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

/// Computes 1-based depth of a single line.
/// Assumes 2 space indentation is used.
fn line_depth(line: &str) -> usize {
    static INDENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^ *").unwrap());
    INDENT_RE.captures(line).map_or(0, |c| c[0].len() / 2) + 1
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
                similar::DiffOp::Insert {
                    new_index, new_len, ..
                } => {
                    for i in 0..new_len {
                        tree_diff_ops.push(TreeDiffOp::Add {
                            after_index: after_frame[new_index + i].0,
                        });
                    }
                }
                similar::DiffOp::Delete {
                    old_index, old_len, ..
                } => {
                    for i in 0..old_len {
                        tree_diff_ops.push(TreeDiffOp::Remove {
                            before_index: before_frame[old_index + i].0,
                        });
                    }
                }
                similar::DiffOp::Replace {
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
                similar::DiffOp::Equal { .. } => {}
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
