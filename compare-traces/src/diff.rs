use console::Style;
use similar::{ChangeTag, DiffTag, TextDiff};

pub fn print_diff(diff: TextDiff<'_, '_, '_, str>) {
    // TODO: Add `context` option to reveal surrounding lines when desired
    for op_group in diff.grouped_ops(0) {
        for op in op_group {
            let change_tuples: Vec<_> = op
                .iter_slices(diff.old_slices(), diff.new_slices())
                .collect();
            if op.tag() == DiffTag::Replace {
                assert!(change_tuples.len() == 2);
                for (tag, slices) in change_tuples {
                    let (sign, style) = match tag {
                        ChangeTag::Delete => ("<", Style::new().magenta()),
                        ChangeTag::Insert => (">", Style::new().yellow()),
                        ChangeTag::Equal => unreachable!(),
                    };
                    for slice in slices {
                        print!("{}{}", style.apply_to(sign).bold(), style.apply_to(slice));
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
                    for slice in slices {
                        print!("{}{}", style.apply_to(sign).bold(), style.apply_to(slice));
                    }
                }
            }
        }
        println!("---");
    }
}
