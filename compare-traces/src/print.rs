use console::Style;
use similar::{ChangeTag, DiffOp, DiffTag, DiffableStr};

use crate::diff::Diff;

pub fn print_change(op: &DiffOp, change_tuples: &[(ChangeTag, &[&str])]) {
    if op.tag() == DiffTag::Replace {
        assert!(change_tuples.len() == 2);
        for (tag, slices) in change_tuples {
            let (sign, style) = match tag {
                ChangeTag::Delete => ("<", Style::new().magenta()),
                ChangeTag::Insert => (">", Style::new().yellow()),
                ChangeTag::Equal => unreachable!(),
            };
            for slice in *slices {
                print!("{}{}", style.apply_to(sign).bold(), style.apply_to(slice));
                if !slice.ends_with_newline() {
                    println!();
                }
            }
        }
    } else {
        assert!(change_tuples.len() == 1);
        for (tag, slices) in change_tuples {
            let (sign, style) = match tag {
                ChangeTag::Delete => ("-", Style::new().red()),
                ChangeTag::Insert => ("+", Style::new().green()),
                ChangeTag::Equal => (" ", Style::new()),
            };
            for slice in *slices {
                print!("{}{}", style.apply_to(sign).bold(), style.apply_to(slice));
                if !slice.ends_with_newline() {
                    println!();
                }
            }
        }
    }
}

pub fn print_change_group(diff: &Diff<'_>, op_group: &Vec<DiffOp>) {
    for op in op_group {
        let change_tuples: Vec<_> = op
            .iter_slices(&diff.before_trace.lines, &diff.after_trace.lines)
            .collect();
        print_change(op, &change_tuples);
    }
}

pub fn print_diff(diff: &Diff<'_>) {
    // TODO: Add `context` option to reveal surrounding lines when desired
    for op_group in &diff.grouped_diff_ops {
        print_change_group(diff, op_group);
        println!("---");
    }
}
