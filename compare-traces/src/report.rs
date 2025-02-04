use anyhow::{anyhow, Ok, Result};
use similar::{DiffOp, DiffTag, TextDiff};

use crate::diff::print_change;

#[derive(Debug)]
enum EventType {
    CallFrom,
    CallTo,
    ReturnFrom,
    // Verbose,
}

#[derive(Debug)]
struct Event {
    event_type: EventType,
    // TODO: Maybe store reference instead...?
    detail: String,
}

impl Event {
    // See `printEventFromLineInfo` in `collect-trace.cpp` for output path
    fn parse(event_str: &str) -> Result<Self> {
        let mut rest = event_str;

        // Remove any leading indentation
        rest = rest.trim_start();

        // Ignore inlined chain source tag if present
        if rest.starts_with("I") {
            rest = &rest[1..];
        }

        // Parse event type
        let event_type_str = &rest[0..2];
        let event_type = match event_type_str {
            "CF" => EventType::CallFrom,
            "CT" => EventType::CallTo,
            "RF" => EventType::ReturnFrom,
            _ => return Err(anyhow!("Unexpected event type {}", event_type_str)),
        };
        // Advance past event type and ": " separator
        rest = &rest[4..];

        // For now, just stash the rest as event detail
        let detail = rest.to_owned();

        Ok(Self { event_type, detail })
    }
}

pub fn analyse_and_print_report(diff: &TextDiff<'_, '_, '_, str>) {
    println!("Analysing divergences…");
    println!();

    let divergences: Vec<&str> = vec![]; // TODO: Change to parsed type

    for op_group in diff.grouped_ops(0) {
        for op in op_group {
            // Skip matching regions
            if op.tag() == DiffTag::Equal {
                continue;
            }

            // TODO: Skip unnecessary collects
            let change_tuples_raw: Vec<_> = op
                .iter_slices(diff.old_slices(), diff.new_slices())
                .collect();
            let change_tuples: Vec<_> = change_tuples_raw
                .iter()
                .map(|(tag, strings)| {
                    (
                        tag,
                        strings
                            .iter()
                            .map(|str| Event::parse(str).unwrap())
                            .collect::<Vec<_>>(),
                    )
                })
                .collect();
            println!("{:?}", change_tuples);

            // For debugging
            print_change(&op, &change_tuples_raw);
        }
        println!();
    }

    println!("{} divergences found", divergences.len());
}
