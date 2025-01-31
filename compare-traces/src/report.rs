use similar::TextDiff;

pub fn analyse_and_print_report(diff: &TextDiff<'_, '_, '_, str>) {
    println!("Analysing divergences…");
    println!();

    let divergences: Vec<&str> = vec![]; // TODO: Change to parsed type

    for op_group in diff.grouped_ops(0) {
        for op in op_group {
            let change_tuples: Vec<_> = op
                .iter_slices(diff.old_slices(), diff.new_slices())
                .collect();
            // TODO: Parse strings into events
        }
    }

    println!("{} divergences found", divergences.len());
}
