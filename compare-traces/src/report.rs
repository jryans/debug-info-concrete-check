use std::{collections::VecDeque, vec};

use anyhow::{anyhow, Ok, Result};
use similar::{ChangeTag, DiffOp, DiffTag, TextDiff};

use crate::diff::print_change;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum DivergenceType {
    RemovedLibraryCall,
    Unknown,
}

#[derive(Debug)]
struct Divergence {
    divergence_type: DivergenceType,
    events: Vec<Event>,
}

// Example diff:
// - CF: strbuf_init at strbuf.c:57:2
// -   CT: Jump to external code
// -   CF: Jump to external code
// -     CT: External code
// -   RF: Jump to external code
fn check_for_removed_library_call(
    op: &DiffOp,
    change_tuples_event: &mut [(ChangeTag, VecDeque<Event>)],
) -> Option<Divergence> {
    // Diff op for this region should be delete
    if op.tag() != DiffTag::Delete {
        return None;
    }

    // Should have a single tuple with deleted lines
    if change_tuples_event.len() != 1 {
        return None;
    }
    let (change_tag, events) = &mut change_tuples_event[0];
    if *change_tag != ChangeTag::Delete {
        return None;
    }

    // First event should be call from traced binary
    if events[0].event_type != EventType::CallFrom {
        return None;
    }

    // Second event should be call to external library
    if events[1].event_type != EventType::CallTo {
        return None;
    }
    if !events[1].detail.to_lowercase().contains("external code") {
        return None;
    }

    // Extract related events
    let mut related_events = vec![];

    // First two are known to match
    related_events.push(events.pop_front().unwrap());
    related_events.push(events.pop_front().unwrap());

    // Also take any more contiguous events about external code
    while !events.is_empty() {
        if !events[0].detail.to_lowercase().contains("external code") {
            break;
        }
        related_events.push(events.pop_front().unwrap());
    }

    Some(Divergence {
        divergence_type: DivergenceType::RemovedLibraryCall,
        events: related_events,
    })
}

fn check_for_known_divergences(
    op: &DiffOp,
    change_tuples_event: &mut [(ChangeTag, VecDeque<Event>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    // println!("{:?}", op);
    // println!("{:?}", change_tuples_event);
    // println!();

    // TODO: Fix logic for multiple divergences in a single change region

    if let Some(divergence) = check_for_removed_library_call(op, change_tuples_event) {
        divergences.push(divergence);
    }

    divergences
}

pub fn analyse_and_print_report(diff: &TextDiff<'_, '_, '_, str>) {
    println!("Analysing divergences…");
    println!();

    let mut divergences = vec![];

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

            // For debugging
            if false {
                print_change(&op, &change_tuples_raw);
            }

            // Parse raw text into events
            let mut change_tuples_event: Vec<_> = change_tuples_raw
                .iter()
                .map(|(tag, strings)| {
                    (
                        tag.clone(),
                        strings
                            .iter()
                            .map(|str| Event::parse(str).unwrap())
                            .collect::<VecDeque<_>>(),
                    )
                })
                .collect();

            // Check events against known divergence patterns
            // TODO: Deduplicate divergences at same source location
            divergences.append(&mut check_for_known_divergences(
                &op,
                &mut change_tuples_event,
            ));
        }
    }

    for divergence in &divergences {
        println!("{:?}", divergence);
        println!();
    }

    println!("{} divergences found", divergences.len());
}
