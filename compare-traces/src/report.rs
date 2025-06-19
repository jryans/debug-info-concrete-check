use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    fs::File,
    hash::Hash,
    io::Write,
    path::PathBuf,
};

use anyhow::{Context, Ok, Result};
use enum_iterator::Sequence;
use log::log_enabled;
use once_cell::sync::Lazy;
use regex::Regex;
use similar::{ChangeTag, DiffOp, DiffTag};

use crate::{
    diff::Diff,
    event::{Event, EventType, Location},
    print::{print_change_group, print_change_vec},
    remarks::Remark,
};

#[derive(Sequence, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum DivergenceType {
    CoordinatesRemoved,
    CoordinatesChangedSmall,
    CoordinatesChangedLarge,
    LibraryCallAdded,
    LibraryCallReplaced,
    LibraryCallRemoved,
    // TODO: Refine this by pass, similar to the paper
    ProgramCallRemoved,
    Uncategorised,
}

impl DivergenceType {
    fn to_file_name(&self) -> &str {
        match self {
            DivergenceType::CoordinatesRemoved => "coordinates-removed",
            DivergenceType::CoordinatesChangedSmall => "coordinates-changed-small",
            DivergenceType::CoordinatesChangedLarge => "coordinates-changed-large",
            DivergenceType::LibraryCallAdded => "library-call-added",
            DivergenceType::LibraryCallReplaced => "library-call-replaced",
            DivergenceType::LibraryCallRemoved => "library-call-removed",
            DivergenceType::ProgramCallRemoved => "program-call-removed",
            DivergenceType::Uncategorised => "uncategorised",
        }
    }
}

#[derive(Clone, Debug)]
pub struct Divergence {
    divergence_type: DivergenceType,
    before_events: Vec<Event>,
    after_events: Vec<Event>,
    pass_responsible: Option<String>,

    // Excluded from key (intended for trace debugging)
    // 1-based line indices
    old_index: usize,
    new_index: usize,
}

impl Divergence {
    fn new(
        divergence_type: DivergenceType,
        before_events: Vec<Event>,
        after_events: Vec<Event>,
        diff_op: &DiffOp,
    ) -> Divergence {
        Divergence {
            divergence_type,
            before_events,
            after_events,
            pass_responsible: None,
            old_index: diff_op.old_range().start + 1,
            new_index: diff_op.new_range().start + 1,
        }
    }

    // JRS: Should the key really just be `location` below...?
    fn key(&self) -> (&DivergenceType, &Vec<Event>, &Vec<Event>, &Option<String>) {
        (
            &self.divergence_type,
            &self.before_events,
            &self.after_events,
            &self.pass_responsible,
        )
    }

    fn location(&self) -> &Location {
        assert!(!self.before_events.is_empty() || !self.after_events.is_empty());
        // Use first event to provide approximate coordinates for divergence
        if !self.before_events.is_empty() {
            &self.before_events[0].location
        } else {
            &self.after_events[0].location
        }
    }
}

impl PartialEq for Divergence {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl Eq for Divergence {}

impl PartialOrd for Divergence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Divergence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.key().cmp(&other.key())
    }
}

// Example diff:
// < CF: getnanotime at trace.c:397:18
// > CF: getnanotime at trace.c:0:0
fn check_for_coordinates_removed(
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_events) in grouped_events {
        // Diff op for this region should be replace
        if diff_op.tag() != DiffTag::Replace {
            continue;
        }

        // Should have two tuples with deleted and inserted lines
        assert!(change_tuples_events.len() == 2);
        let (befores, afters) = change_tuples_events.split_at_mut(1);
        let (before_change_tag, before_events) = &mut befores[0];
        let (after_change_tag, after_events) = &mut afters[0];
        assert!(*before_change_tag == ChangeTag::Delete);
        assert!(*after_change_tag == ChangeTag::Insert);

        // Must have at least one event on both sides
        if before_events.len() < 1 || after_events.len() < 1 {
            continue;
        }

        // Function and file must match
        let before_event = &before_events[0];
        let after_event = &after_events[0];
        if before_event.location.function != after_event.location.function {
            continue;
        }
        if before_event.location.file != after_event.location.file {
            continue;
        }

        // After event must have at least missing line coordinate
        // Some transformations seem to set line 0 but non-0 column,
        // which is effectively still removal, so we now only examine line here.
        if after_event.location.line != Some(0) {
            continue;
        }

        // Extract related events
        let mut related_before_events = vec![];
        let mut related_after_events = vec![];

        related_before_events.push(before_events.pop_front().unwrap());
        related_after_events.push(after_events.pop_front().unwrap());

        divergences.push(Divergence::new(
            DivergenceType::CoordinatesRemoved,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// < CT: xstrdup_or_null at git-compat-util.h:1168:0
// > CT: xstrdup_or_null at git-compat-util.h:1169:9
fn check_for_coordinates_changed(
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_events) in grouped_events {
        // Diff op for this region should be replace
        if diff_op.tag() != DiffTag::Replace {
            continue;
        }

        // Should have two tuples with deleted and inserted lines
        assert!(change_tuples_events.len() == 2);
        let (befores, afters) = change_tuples_events.split_at_mut(1);
        let (before_change_tag, before_events) = &mut befores[0];
        let (after_change_tag, after_events) = &mut afters[0];
        assert!(*before_change_tag == ChangeTag::Delete);
        assert!(*after_change_tag == ChangeTag::Insert);

        // Must have at least one event on both sides
        if before_events.len() < 1 || after_events.len() < 1 {
            continue;
        }

        // Function and file must match
        let before_event = &before_events[0];
        let after_event = &after_events[0];
        if before_event.location.function != after_event.location.function {
            continue;
        }
        if before_event.location.file != after_event.location.file {
            continue;
        }

        // Line and column coordinates must be present
        if before_event.location.line.is_none()
            || before_event.location.column.is_none()
            || after_event.location.line.is_none()
            || after_event.location.column.is_none()
        {
            continue;
        }

        // Line or column coordinates must differ
        if before_event.location.line == after_event.location.line
            && before_event.location.column == after_event.location.column
        {
            continue;
        }

        // Determine divergence type using line delta
        let before_line = before_event.location.line.unwrap();
        let after_line = after_event.location.line.unwrap();
        let line_delta = if before_line < after_line {
            after_line - before_line
        } else {
            before_line - after_line
        };
        let divergence_type = if line_delta <= 3 {
            DivergenceType::CoordinatesChangedSmall
        } else {
            DivergenceType::CoordinatesChangedLarge
        };

        // Extract related events
        let mut related_before_events = vec![];
        let mut related_after_events = vec![];

        related_before_events.push(before_events.pop_front().unwrap());
        related_after_events.push(after_events.pop_front().unwrap());

        divergences.push(Divergence::new(
            divergence_type,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// + CF: strbuf_init at strbuf.c:57:2
// +   CT: Jump to external code
// +   RF: Jump to external code
fn check_for_library_call_added(
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_events) in grouped_events {
        // Diff op for this region should be insert
        if diff_op.tag() != DiffTag::Insert {
            continue;
        }

        // Should have a single tuple with inserted lines
        assert!(change_tuples_events.len() == 1);
        let (change_tag, events) = &mut change_tuples_events[0];
        assert!(*change_tag == ChangeTag::Insert);

        // Must have at least 3 events
        if events.len() < 3 {
            continue;
        }

        // First event should be call from traced binary
        if events[0].event_type != EventType::CallFrom {
            continue;
        }

        // Second event should be call to external library
        if events[1].event_type != EventType::CallTo {
            continue;
        }
        if !events[1].detail.to_lowercase().contains("external code") {
            continue;
        }

        // Third event should be return from external library
        if events[2].event_type != EventType::ReturnFrom {
            continue;
        }
        if !events[2].detail.to_lowercase().contains("external code") {
            continue;
        }

        // Extract related events
        let related_before_events = vec![];
        let mut related_after_events = vec![];

        // First 3 are known to match
        related_after_events.push(events.pop_front().unwrap());
        related_after_events.push(events.pop_front().unwrap());
        related_after_events.push(events.pop_front().unwrap());

        divergences.push(Divergence::new(
            DivergenceType::LibraryCallAdded,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// TODO: Break this up into smaller functions
// Example diff:
//   CF: strbuf_vaddf at strbuf.c:397:8
// <   CT: Jump to external code for ___vsnprintf_chk
// >   CT: Jump to external code for _vsnprintf
// <   RF: Jump to external code for ___vsnprintf_chk
// >   RF: Jump to external code for _vsnprintf
fn check_for_library_call_replaced(
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    let mut unchanged_call_from_slot = None;
    let mut changed_call_return_slot = None;

    for (diff_op, change_tuples_events) in grouped_events {
        // Look for an unchanged call from
        if unchanged_call_from_slot.is_none() {
            if diff_op.tag() != DiffTag::Equal {
                continue;
            }
            assert!(change_tuples_events.len() == 1);
            let (change_tag, events) = &mut change_tuples_events[0];
            assert!(*change_tag == ChangeTag::Equal);
            if events.len() != 1 {
                continue;
            }
            let event = &events[0];
            if event.event_type != EventType::CallFrom {
                continue;
            }
            unchanged_call_from_slot = Some(change_tuples_events);
            continue;
        }

        // Look for changed external call to and return from
        if changed_call_return_slot.is_none() {
            if diff_op.tag() != DiffTag::Replace {
                unchanged_call_from_slot = None;
                continue;
            }
            assert!(change_tuples_events.len() == 2);
            let (befores, afters) = change_tuples_events.split_at_mut(1);
            let (before_change_tag, before_events) = &mut befores[0];
            let (after_change_tag, after_events) = &mut afters[0];
            assert!(*before_change_tag == ChangeTag::Delete);
            assert!(*after_change_tag == ChangeTag::Insert);
            // Ensure events mention external call
            if before_events.len() < 2 || after_events.len() < 2 {
                unchanged_call_from_slot = None;
                continue;
            }
            if before_events
                .iter()
                .any(|e| !e.detail.to_lowercase().contains("external code"))
                || after_events
                    .iter()
                    .any(|e| !e.detail.to_lowercase().contains("external code"))
            {
                unchanged_call_from_slot = None;
                continue;
            }
            changed_call_return_slot = Some(change_tuples_events);
        }

        // Extract related events
        let mut related_before_events = vec![];
        let mut related_after_events = vec![];

        {
            let events = &mut unchanged_call_from_slot.unwrap()[0].1;
            related_before_events.push(events.pop_front().unwrap());
            related_after_events.push(related_before_events.last().unwrap().clone());
        }

        {
            let tuples_events = changed_call_return_slot.unwrap();
            let (befores, afters) = tuples_events.split_at_mut(1);
            let (_, before_events) = &mut befores[0];
            let (_, after_events) = &mut afters[0];
            while !before_events.is_empty() {
                related_before_events.push(before_events.pop_front().unwrap());
            }
            while !after_events.is_empty() {
                related_after_events.push(after_events.pop_front().unwrap());
            }
        }

        divergences.push(Divergence::new(
            DivergenceType::LibraryCallReplaced,
            related_before_events,
            related_after_events,
            diff_op,
        ));

        unchanged_call_from_slot = None;
        changed_call_return_slot = None;
    }

    divergences
}

// Example diff:
// - CF: strbuf_init at strbuf.c:57:2
// -   CT: Jump to external code
// -   RF: Jump to external code
fn check_for_library_call_removed(
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_events) in grouped_events {
        // Diff op for this region should be delete
        if diff_op.tag() != DiffTag::Delete {
            continue;
        }

        // Should have a single tuple with deleted lines
        assert!(change_tuples_events.len() == 1);
        let (change_tag, events) = &mut change_tuples_events[0];
        assert!(*change_tag == ChangeTag::Delete);

        // Must have at least 3 events
        if events.len() < 3 {
            continue;
        }

        // First event should be call from traced binary
        if events[0].event_type != EventType::CallFrom {
            continue;
        }

        // Second event should be call to external library
        if events[1].event_type != EventType::CallTo {
            continue;
        }
        if !events[1].detail.to_lowercase().contains("external code") {
            continue;
        }

        // Third event should be return from external library
        if events[2].event_type != EventType::ReturnFrom {
            continue;
        }
        if !events[2].detail.to_lowercase().contains("external code") {
            continue;
        }

        // Extract related events
        let mut related_before_events = vec![];
        let related_after_events = vec![];

        // First 3 are known to match
        related_before_events.push(events.pop_front().unwrap());
        related_before_events.push(events.pop_front().unwrap());
        related_before_events.push(events.pop_front().unwrap());

        divergences.push(Divergence::new(
            DivergenceType::LibraryCallRemoved,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// - CF: is_absolute_path at cache.h:1276:32
// -   CT: git_has_dos_drive_prefix at git-compat-util.h:432:0
// -   RF: git_has_dos_drive_prefix at git-compat-util.h:433:2
fn check_for_program_call_removed(
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_events) in grouped_events {
        // Diff op for this region should be delete
        if diff_op.tag() != DiffTag::Delete {
            continue;
        }

        // Should have a single tuple with deleted lines
        assert!(change_tuples_events.len() == 1);
        let (change_tag, events) = &mut change_tuples_events[0];
        assert!(*change_tag == ChangeTag::Delete);

        // Must have at least 3 events
        if events.len() < 3 {
            continue;
        }

        // Event sequence should be call from, call to, return from
        // Call to and return from should not mention external code
        // TODO: Support more complex trees with nested calls
        if events[0].event_type != EventType::CallFrom {
            continue;
        }
        if events[1].event_type != EventType::CallTo {
            continue;
        }
        if events[1].detail.to_lowercase().contains("external code") {
            continue;
        }
        if events[2].event_type != EventType::ReturnFrom {
            continue;
        }
        if events[2].detail.to_lowercase().contains("external code") {
            continue;
        }

        // Extract related events
        let mut related_before_events = vec![];
        let related_after_events = vec![];
        related_before_events.push(events.pop_front().unwrap());
        related_before_events.push(events.pop_front().unwrap());
        related_before_events.push(events.pop_front().unwrap());

        divergences.push(Divergence::new(
            DivergenceType::ProgramCallRemoved,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

fn check_for_known_divergences(
    diff: &Diff<'_>,
    grouped_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    // Check for divergences at least once and keep going each time more are found
    // JRS: Change patterns to produce all matching divergences up front...?
    loop {
        {
            let mut divergences_found = check_for_coordinates_removed(grouped_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_coordinates_changed(grouped_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_added(grouped_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_replaced(grouped_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_removed(grouped_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_program_call_removed(grouped_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        break;
    }

    // If any events remain, record an unknown divergence
    for (diff_op, change_tuples_events) in grouped_events {
        if diff_op.tag() == DiffTag::Equal {
            continue;
        }
        let events_present = change_tuples_events
            .iter()
            .any(|(_, events)| !events.is_empty());
        if events_present {
            // Look for certain terms that suggest non-determinism
            static ND_RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r"(sig|command|hash|alloc|env)").unwrap());
            let nondeterminism_found = change_tuples_events
                .iter()
                .any(|(_, events)| events.iter().any(|e| ND_RE.is_match(&e.detail)));
            if !nondeterminism_found {
                let mut remaining_before_events = vec![];
                let mut remaining_after_events = vec![];
                for (change_tag, events) in change_tuples_events {
                    if *change_tag == ChangeTag::Equal {
                        continue;
                    }
                    let mut collected_events = events.drain(..).collect::<Vec<_>>();
                    if *change_tag == ChangeTag::Delete {
                        remaining_before_events.append(&mut collected_events);
                    } else {
                        remaining_after_events.append(&mut collected_events);
                    }
                }
                divergences.push(Divergence::new(
                    DivergenceType::Uncategorised,
                    remaining_before_events,
                    remaining_after_events,
                    diff_op,
                ));
            }
        }
    }

    divergences
}

fn tweak_alignment(op: &DiffOp, change_tuples_strings: &mut [(ChangeTag, Vec<&str>)]) -> bool {
    let mut changed: bool = false;

    if op.tag() == DiffTag::Delete && change_tuples_strings.len() == 1 {
        let change_strings = &change_tuples_strings[0].1;
        let string_count = change_strings.len();
        // Limit to regions of at most 5 lines to accommodate both:
        // - Internal call (3 lines)
        // - External call (5 lines)
        if string_count > 1 && string_count <= 5 {
            let last_string = *change_strings.last().unwrap();
            // Repeated sequences where one part is deleted sometimes appear as
            // 2-N lines of one part, 1 line of next part
            if last_string.contains("CF:") {
                let mut change_strings_reordered = vec![last_string];
                let last_string_index = change_strings.len() - 1;
                change_strings_reordered.extend_from_slice(&change_strings[..last_string_index]);
                change_tuples_strings[0].1 = change_strings_reordered;
                changed = true;
            }
        }
    }

    changed
}

fn print_events(events: &Vec<Event>) {
    let event_count = events.len();
    if event_count <= 30 {
        for event in events {
            println!("    {}", event);
        }
    } else {
        let first_events = &events[..30];
        for event in first_events {
            println!("    {}", event);
        }
        println!("    [...{} more events...]", event_count - 30);
    }
}

pub fn analyse_and_print_report(
    diff: &Diff<'_>,
    remarks_by_location: &Option<HashMap<Location, Remark>>,
    tweak_event_alignment: bool,
) -> BTreeMap<Divergence, u64> {
    println!("Analysing divergences…");
    println!();

    let mut divergence_stats_by_coordinates: BTreeMap<Divergence, u64> = BTreeMap::new();

    for op_group in &diff.grouped_diff_ops {
        if log_enabled!(log::Level::Debug) {
            print_change_group(diff, &op_group);
            println!();
            // println!("{:#?}", &op_group);
            // println!();
        }

        let mut grouped_events: Vec<(DiffOp, Vec<(ChangeTag, VecDeque<Event>)>)> = vec![];

        for op in op_group {
            // TODO: Skip unnecessary collects / copies
            let mut change_tuples_strings: Vec<_> = op
                .iter_slices(&diff.before_lines, &diff.after_lines)
                .map(|(tag, slices)| (tag, Vec::from(slices)))
                .collect();

            // Fix up alignment where possible
            // JRS: Maybe remove this by using the previous line from context...?
            if tweak_event_alignment {
                if tweak_alignment(&op, &mut change_tuples_strings) {
                    if log_enabled!(log::Level::Debug) {
                        println!("Alignment tweaked, new ordering:");
                        print_change_vec(&op, &change_tuples_strings);
                        println!();
                    }
                }
            }

            // Parse raw text into events
            let mut parse_errors = vec![];
            let change_tuples_events: Vec<_> = change_tuples_strings
                .iter()
                .map(|(tag, strings)| {
                    (
                        tag.clone(),
                        strings
                            .iter()
                            .map(|str| Event::parse(str))
                            .filter_map(|r| r.map_err(|e| parse_errors.push(e)).ok())
                            .collect::<VecDeque<_>>(),
                    )
                })
                .collect();
            for error in parse_errors {
                println!("{}", error);
                println!();
            }

            grouped_events.push((*op, change_tuples_events));
        }

        // Check events against known divergence patterns
        let mut new_divergences = check_for_known_divergences(diff, &mut grouped_events);
        for divergence in &mut new_divergences {
            if log_enabled!(log::Level::Debug) {
                println!("{:#?}", divergence);
                println!();
            }

            // Look for optimisation remarks at divergence coordinates
            if let Some(remarks) = remarks_by_location {
                // TODO: Enable for other types where possible
                if divergence.divergence_type == DivergenceType::ProgramCallRemoved {
                    if let Some(remark) = remarks.get(divergence.location()) {
                        if log_enabled!(log::Level::Debug) {
                            println!("Matching remark: {:#?}", remark);
                            println!();
                        }
                        // Record pass responsible in divergence
                        divergence.pass_responsible = Some(remark.pass.clone());
                    }
                }
            }

            // Insert or update stats for these source coordinates
            if let Some(occurrences) = divergence_stats_by_coordinates.get_mut(divergence) {
                *occurrences += 1;
            } else {
                divergence_stats_by_coordinates.insert(divergence.clone(), 1);
            }
        }
    }

    println!("Divergence analysis complete!");
    println!();

    println!("## Divergences by source coordinates");
    println!();

    let mut divergence_coordinates_count_by_type: BTreeMap<DivergenceType, u64> = BTreeMap::new();
    let mut occurrences_total: u64 = 0;
    for (divergence, occurrences) in &divergence_stats_by_coordinates {
        println!("{:?}", divergence.divergence_type);
        if !divergence.before_events.is_empty() {
            println!("  Before events:");
            print_events(&divergence.before_events);
        }
        if !divergence.after_events.is_empty() {
            println!("  After events:");
            print_events(&divergence.after_events);
        }
        println!("  Occurrences: {}", occurrences);
        if let Some(pass) = &divergence.pass_responsible {
            println!("  Pass responsible: {}", pass);
        }
        if log_enabled!(log::Level::Info) {
            println!(
                "  Example trace lines: -{}, +{}",
                divergence.old_index, divergence.new_index
            );
        }
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

    divergence_stats_by_coordinates
}

pub fn print_before_events_by_type(
    divergence_stats_by_coordinates: &BTreeMap<Divergence, u64>,
    events_by_type_dir: &PathBuf,
) -> Result<()> {
    let mut files_by_type: HashMap<DivergenceType, File> = HashMap::new();
    for divergence_type in enum_iterator::all::<DivergenceType>() {
        let file_path = events_by_type_dir.join(divergence_type.to_file_name());
        let file = File::create(&file_path).with_context(|| {
            format!(
                "Unable to create events by type file ({})",
                file_path.display()
            )
        })?;
        files_by_type.insert(divergence_type, file);
    }

    for divergence in divergence_stats_by_coordinates.keys() {
        let mut file = &files_by_type[&divergence.divergence_type];
        let printable_events = match &divergence.divergence_type {
            // In the less common case of only added events,
            // let's use those so we at least have something to count
            DivergenceType::LibraryCallAdded => &divergence.after_events,
            _ => &divergence.before_events,
        };
        for event in printable_events {
            writeln!(file, "{}", event)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_error() {
        let result = Event::parse("RF: do_xmalloc at     CT: External code");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Corrupt event"));
    }

    #[test]
    fn coordinates_removed() {
        // Example diff:
        // < CF: getnanotime at trace.c:397:18
        // > CF: getnanotime at trace.c:0:0
        let diff = Diff {
            before_lines: Vec::from(["CF: getnanotime at trace.c:397:18"]),
            after_lines: Vec::from(["CF: getnanotime at trace.c:0:0"]),
            grouped_diff_ops: Vec::from([Vec::from([DiffOp::Replace {
                old_index: 0,
                old_len: 1,
                new_index: 0,
                new_len: 1,
            }])]),
        };
        let change_tuples_events = Vec::from([
            (
                ChangeTag::Delete,
                diff.before_lines
                    .iter()
                    .map(|str| Event::parse(str).unwrap())
                    .collect::<VecDeque<_>>(),
            ),
            (
                ChangeTag::Insert,
                diff.after_lines
                    .iter()
                    .map(|str| Event::parse(str).unwrap())
                    .collect::<VecDeque<_>>(),
            ),
        ]);
        let mut grouped_events = [(diff.grouped_diff_ops[0][0], change_tuples_events)];
        let divergences = check_for_known_divergences(&diff, &mut grouped_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::CoordinatesRemoved
        );
        assert_eq!(divergence.before_events.len(), 1);
        assert_eq!(divergence.after_events.len(), 1);
        assert_eq!(
            divergence.before_events[0].detail,
            "getnanotime at trace.c:397:18"
        );
    }

    #[test]
    fn coordinates_changed() {
        // Example diff:
        // < CT: xstrdup_or_null at git-compat-util.h:1168:0
        // > CT: xstrdup_or_null at git-compat-util.h:1169:9
        let diff = Diff {
            before_lines: Vec::from(["CT: xstrdup_or_null at git-compat-util.h:1168:0"]),
            after_lines: Vec::from(["CT: xstrdup_or_null at git-compat-util.h:1169:9"]),
            grouped_diff_ops: Vec::from([Vec::from([DiffOp::Replace {
                old_index: 0,
                old_len: 1,
                new_index: 0,
                new_len: 1,
            }])]),
        };
        let change_tuples_events = Vec::from([
            (
                ChangeTag::Delete,
                diff.before_lines
                    .iter()
                    .map(|str| Event::parse(str).unwrap())
                    .collect::<VecDeque<_>>(),
            ),
            (
                ChangeTag::Insert,
                diff.after_lines
                    .iter()
                    .map(|str| Event::parse(str).unwrap())
                    .collect::<VecDeque<_>>(),
            ),
        ]);
        let mut grouped_events = [(diff.grouped_diff_ops[0][0], change_tuples_events)];
        let divergences = check_for_known_divergences(&diff, &mut grouped_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::CoordinatesChangedSmall
        );
        assert_eq!(divergence.before_events.len(), 1);
        assert_eq!(divergence.after_events.len(), 1);
        assert_eq!(
            divergence.before_events[0].detail,
            "xstrdup_or_null at git-compat-util.h:1168:0"
        );
    }

    #[test]
    fn library_call_removed_single() {
        // Example diff:
        // - CF: strbuf_init at strbuf.c:57:2
        // -   CT: Jump to external code
        // -   RF: Jump to external code
        let diff = Diff {
            before_lines: Vec::from([
                "CF: strbuf_init at strbuf.c:57:2",
                "  CT: Jump to external code",
                "  RF: Jump to external code",
            ]),
            after_lines: Vec::from([]),
            grouped_diff_ops: Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 3,
                new_index: 0,
            }])]),
        };
        let change_tuples_events = Vec::from([(
            ChangeTag::Delete,
            diff.before_lines
                .iter()
                .map(|str| Event::parse(str).unwrap())
                .collect::<VecDeque<_>>(),
        )]);
        let mut grouped_events = [(diff.grouped_diff_ops[0][0], change_tuples_events)];
        let divergences = check_for_known_divergences(&diff, &mut grouped_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::LibraryCallRemoved
        );
        assert_eq!(divergence.before_events.len(), 3);
    }

    #[test]
    fn library_call_removed_multiple() {
        // Example diff:
        // - CF: init_repository_format at setup.c:710:33
        // -   CT: Jump to external code
        // -   RF: Jump to external code
        // - CF: init_repository_format at setup.c:712:2
        // -   CT: Jump to external code
        // -   RF: Jump to external code
        let diff = Diff {
            before_lines: Vec::from([
                "CF: init_repository_format at setup.c:710:33",
                "  CT: Jump to external code",
                "  RF: Jump to external code",
                "CF: init_repository_format at setup.c:712:2",
                "  CT: Jump to external code",
                "  RF: Jump to external code",
            ]),
            after_lines: Vec::from([]),
            grouped_diff_ops: Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 6,
                new_index: 0,
            }])]),
        };
        let change_tuples_events = Vec::from([(
            ChangeTag::Delete,
            diff.before_lines
                .iter()
                .map(|str| Event::parse(str).unwrap())
                .collect::<VecDeque<_>>(),
        )]);
        let mut grouped_events = [(diff.grouped_diff_ops[0][0], change_tuples_events)];
        let divergences = check_for_known_divergences(&diff, &mut grouped_events);
        assert_eq!(divergences.len(), 2);
        for divergence in &divergences {
            assert_eq!(
                divergence.divergence_type,
                DivergenceType::LibraryCallRemoved
            );
            assert_eq!(divergence.before_events.len(), 3);
        }
    }

    #[test]
    fn program_call_removed() {
        // Example diff:
        // - CF: is_absolute_path at cache.h:1276:32
        // -   CT: git_has_dos_drive_prefix at git-compat-util.h:432:0
        // -   RF: git_has_dos_drive_prefix at git-compat-util.h:433:2
        let diff = Diff {
            before_lines: Vec::from([
                "CF: is_absolute_path at cache.h:1276:32",
                "  CT: git_has_dos_drive_prefix at git-compat-util.h:432:0",
                "  RF: git_has_dos_drive_prefix at git-compat-util.h:433:2",
            ]),
            after_lines: Vec::from([]),
            grouped_diff_ops: Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 3,
                new_index: 0,
            }])]),
        };
        let change_tuples_events = Vec::from([(
            ChangeTag::Delete,
            diff.before_lines
                .iter()
                .map(|str| Event::parse(str).unwrap())
                .collect::<VecDeque<_>>(),
        )]);
        let mut grouped_events = [(diff.grouped_diff_ops[0][0], change_tuples_events)];
        let divergences = check_for_known_divergences(&diff, &mut grouped_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::ProgramCallRemoved
        );
        assert_eq!(divergence.before_events.len(), 3);
    }
}
