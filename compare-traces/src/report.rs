use std::{collections::VecDeque, vec};

use anyhow::{anyhow, Ok, Result};
use log::log_enabled;
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
        let detail = rest.trim_end().to_owned();

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
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Option<Divergence> {
    // Diff op for this region should be delete
    if op.tag() != DiffTag::Delete {
        return None;
    }

    // Should have a single tuple with deleted lines
    if change_tuples_events.len() != 1 {
        return None;
    }
    let (change_tag, events) = &mut change_tuples_events[0];
    if *change_tag != ChangeTag::Delete {
        return None;
    }

    // Must have at least 2 events
    if events.len() < 2 {
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
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    if log_enabled!(log::Level::Debug) {
        println!("{:?}", op);
        println!("{:?}", change_tuples_events);
        println!();
    }

    // Check for divergences at least once and keep going each time more are found
    let mut continue_checking = true;
    while continue_checking {
        continue_checking = false;
        if let Some(divergence) = check_for_removed_library_call(op, change_tuples_events) {
            divergences.push(divergence);
            continue_checking = true;
        }
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
            let change_tuples_strings: Vec<_> = op
                .iter_slices(diff.old_slices(), diff.new_slices())
                .collect();

            if log_enabled!(log::Level::Debug) {
                print_change(&op, &change_tuples_strings);
                println!();
            }

            // Parse raw text into events
            let mut change_tuples_events: Vec<_> = change_tuples_strings
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
            let mut new_divergences = check_for_known_divergences(&op, &mut change_tuples_events);
            for divergence in &new_divergences {
                if log_enabled!(log::Level::Debug) {
                    println!("{:?}", divergence);
                    println!();
                }
            }
            divergences.append(&mut new_divergences);
        }
    }

    println!("{} divergences found", divergences.len());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn removed_library_call_single() {
        // Example diff:
        // - CF: strbuf_init at strbuf.c:57:2
        // -   CT: Jump to external code
        // -   CF: Jump to external code
        // -     CT: External code
        // -   RF: Jump to external code
        let op = DiffOp::Delete {
            old_index: 134,
            old_len: 5,
            new_index: 125,
        };
        let mut change_tuples_events = [(
            ChangeTag::Delete,
            VecDeque::from([
                Event {
                    event_type: EventType::CallFrom,
                    detail: "strbuf_init at strbuf.c:57:2".to_owned(),
                },
                Event {
                    event_type: EventType::CallTo,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallFrom,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallTo,
                    detail: "External code".to_owned(),
                },
                Event {
                    event_type: EventType::ReturnFrom,
                    detail: "Jump to external code".to_owned(),
                },
            ]),
        )];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::RemovedLibraryCall
        );
        assert_eq!(divergence.events.len(), 5);
    }

    #[test]
    fn removed_library_call_multiple() {
        // Example diff:
        // - CF: init_repository_format at setup.c:710:33
        // -   CT: Jump to external code
        // -   CF: Jump to external code
        // -     CT: External code
        // -   RF: Jump to external code
        // - CF: init_repository_format at setup.c:712:2
        // -   CT: Jump to external code
        // -   CF: Jump to external code
        // -     CT: External code
        // -   RF: Jump to external code
        let op = DiffOp::Delete {
            old_index: 25260,
            old_len: 10,
            new_index: 25114,
        };
        let mut change_tuples_events = [(
            ChangeTag::Delete,
            VecDeque::from([
                Event {
                    event_type: EventType::CallFrom,
                    detail: "init_repository_format at setup.c:710:33".to_owned(),
                },
                Event {
                    event_type: EventType::CallTo,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallFrom,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallTo,
                    detail: "External code".to_owned(),
                },
                Event {
                    event_type: EventType::ReturnFrom,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallFrom,
                    detail: "init_repository_format at setup.c:712:2".to_owned(),
                },
                Event {
                    event_type: EventType::CallTo,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallFrom,
                    detail: "Jump to external code".to_owned(),
                },
                Event {
                    event_type: EventType::CallTo,
                    detail: "External code".to_owned(),
                },
                Event {
                    event_type: EventType::ReturnFrom,
                    detail: "Jump to external code".to_owned(),
                },
            ]),
        )];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 2);
        for divergence in &divergences {
            assert_eq!(
                divergence.divergence_type,
                DivergenceType::RemovedLibraryCall
            );
            assert_eq!(divergence.events.len(), 5);
        }
    }
}
