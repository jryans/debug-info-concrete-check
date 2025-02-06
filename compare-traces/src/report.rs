use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

use anyhow::{anyhow, Ok, Result};
use log::log_enabled;
use similar::{ChangeTag, DiffOp, DiffTag, TextDiff};

use crate::diff::print_change;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum EventType {
    CallFrom,
    CallTo,
    ReturnFrom,
    // Verbose,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct Event {
    event_type: EventType,
    // TODO: Maybe store reference instead...?
    detail: String,
    function: Option<String>,
    file: Option<String>,
    line: Option<u64>,
    column: Option<u64>,
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

        // Stash the rest as event detail
        let detail = rest.trim_end().to_owned();

        // Attempt to parse coordinates further if present
        let mut function = None;
        let mut file = None;
        let mut line = None;
        let mut column = None;
        if rest.contains(" at ") {
            // Example: getnanotime at trace.c:397:18
            // Function and file should always be present, but line and column may not be
            let mut segments = rest.split_ascii_whitespace();
            function = segments.next().map(|s| s.to_owned());
            let mut components = segments.nth(1).unwrap().split(':');
            file = components.next().map(|s| s.to_owned());
            line = components.next().map(|s| s.parse::<u64>().unwrap());
            column = components.next().map(|s| s.parse::<u64>().unwrap());
        }

        Ok(Self {
            event_type,
            detail,
            function,
            file,
            line,
            column,
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
enum DivergenceType {
    LibraryCallRemoved,
    Unknown,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct Divergence {
    divergence_type: DivergenceType,
    events: Vec<Event>,
}

impl Divergence {
    fn coordinates(&self) -> &str {
        assert!(!self.events.is_empty());
        // Use first event to provide approximate coordinates for divergence
        &self.events[0].detail
    }
}

// Example diff:
// - CF: strbuf_init at strbuf.c:57:2
// -   CT: Jump to external code
// -   CF: Jump to external code
// -     CT: External code
// -   RF: Jump to external code
fn check_for_library_call_removed(
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
        divergence_type: DivergenceType::LibraryCallRemoved,
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
        if let Some(divergence) = check_for_library_call_removed(op, change_tuples_events) {
            divergences.push(divergence);
            continue_checking = true;
        }
    }

    // If any events remain, record an unknown divergence
    if change_tuples_events
        .iter()
        .any(|(_tag, events)| !events.is_empty())
    {
        // TODO: Keep events separated by tuple...?
        let mut merged_events = vec![];
        for (_tag, events) in change_tuples_events {
            merged_events.append(&mut events.drain(..).collect::<Vec<_>>());
        }
        divergences.push(Divergence {
            divergence_type: DivergenceType::Unknown,
            events: merged_events,
        });
    }

    divergences
}

pub fn analyse_and_print_report(diff: &TextDiff<'_, '_, '_, str>) {
    println!("Analysing divergences…");
    println!();

    let mut divergence_stats_by_coordinates: HashMap<Divergence, u64> = HashMap::new();

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
            let new_divergences = check_for_known_divergences(&op, &mut change_tuples_events);
            for divergence in &new_divergences {
                if log_enabled!(log::Level::Debug) {
                    println!("{:?}", divergence);
                    println!();
                }

                // Insert or update stats for these source coordinates
                if let Some(occurrences) = divergence_stats_by_coordinates.get_mut(divergence) {
                    *occurrences += 1;
                } else {
                    divergence_stats_by_coordinates.insert(divergence.clone(), 1);
                }
            }
        }
    }

    println!("Divergence analysis complete!");
    println!();

    println!("## Divergences by source coordinates");
    println!();

    let mut divergence_coordinates_count_by_type: HashMap<DivergenceType, u64> = HashMap::new();
    let mut occurrences_total: u64 = 0;
    for (divergence, occurrences) in &divergence_stats_by_coordinates {
        println!("{:?}", divergence.divergence_type);
        println!("  Coordinates: {}", divergence.coordinates());
        println!("  Occurrences: {}", occurrences);
        println!();
        if let Some(count) =
            divergence_coordinates_count_by_type.get_mut(&divergence.divergence_type)
        {
            *count += 1;
        } else {
            divergence_coordinates_count_by_type.insert(divergence.divergence_type.clone(), 1);
        }
        occurrences_total += occurrences;
    }

    println!("## Divergences with unique coordinates by type");
    println!();

    for (divergence_type, count) in &divergence_coordinates_count_by_type {
        println!("{:?}", divergence_type);
        println!("  Unique divergence coordinates: {}", count);
        println!();
    }

    println!("## Summary");
    println!();

    println!(
        "{} unique divergence coordinates",
        divergence_stats_by_coordinates.len()
    );
    println!("{} divergence occurrences", occurrences_total);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn library_call_removed_single() {
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
                Event::parse("CF: strbuf_init at strbuf.c:57:2").unwrap(),
                Event::parse("CT: Jump to external code").unwrap(),
                Event::parse("CF: Jump to external code").unwrap(),
                Event::parse("CT: External code").unwrap(),
                Event::parse("RF: Jump to external code").unwrap(),
            ]),
        )];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::LibraryCallRemoved
        );
        assert_eq!(divergence.events.len(), 5);
    }

    #[test]
    fn library_call_removed_multiple() {
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
                Event::parse("CF: init_repository_format at setup.c:710:33").unwrap(),
                Event::parse("CT: Jump to external code").unwrap(),
                Event::parse("CF: Jump to external code").unwrap(),
                Event::parse("CT: External code").unwrap(),
                Event::parse("RF: Jump to external code").unwrap(),
                Event::parse("CF: init_repository_format at setup.c:712:2").unwrap(),
                Event::parse("CT: Jump to external code").unwrap(),
                Event::parse("CF: Jump to external code").unwrap(),
                Event::parse("CT: External code").unwrap(),
                Event::parse("RF: Jump to external code").unwrap(),
            ]),
        )];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 2);
        for divergence in &divergences {
            assert_eq!(
                divergence.divergence_type,
                DivergenceType::LibraryCallRemoved
            );
            assert_eq!(divergence.events.len(), 5);
        }
    }
}
