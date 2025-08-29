use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fs::File;
use std::hash::Hash;
use std::io::Write;
use std::path::PathBuf;

use anyhow::{Context, Ok, Result};
use enum_iterator::Sequence;
use log::log_enabled;
use once_cell::sync::Lazy;
use regex::Regex;
use similar::{ChangeTag, DiffOp, DiffTag};

use crate::diff::Diff;
use crate::event::{Event, EventSource, EventType, Location};
use crate::print::print_change_group;
use crate::remarks::Remark;

#[derive(Sequence, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum DivergenceType {
    CoordinatesRemoved,
    CoordinatesChangedSmall,
    CoordinatesChangedLarge,
    LibraryCallAdded,
    LibraryCallReplaced,
    LibraryCallInlined,
    LibraryCallRemoved,
    // TODO: Refine this by pass, similar to the paper
    ProgramCallRemoved,
    InlinedReentryAdded,
    InlinedNoiseAdded,
    InlinedReturnAdded,
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
            DivergenceType::LibraryCallInlined => "library-call-inlined",
            DivergenceType::LibraryCallRemoved => "library-call-removed",
            DivergenceType::ProgramCallRemoved => "program-call-removed",
            DivergenceType::InlinedReentryAdded => "inlined-reentry-added",
            DivergenceType::InlinedNoiseAdded => "inlined-noise-added",
            DivergenceType::InlinedReturnAdded => "inlined-return-added",
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
    before_file_path: Option<PathBuf>,
    after_file_path: Option<PathBuf>,
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
            before_file_path: None,
            after_file_path: None,
            // JRS: Some tree diff ops use zeros, when really they mean `None`.
            // For now, we put up with it, as these indices are only used for debugging.
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
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be replace
        if diff_op.tag() != DiffTag::Replace {
            continue;
        }

        // Should have two tuples with deleted and inserted lines
        assert!(change_tuples_indexed_events.len() == 2);
        let (befores, afters) = change_tuples_indexed_events.split_at_mut(1);
        let (before_change_tag, before_indexed_events) = &mut befores[0];
        let (after_change_tag, after_indexed_events) = &mut afters[0];
        assert!(*before_change_tag == ChangeTag::Delete);
        assert!(*after_change_tag == ChangeTag::Insert);

        // Must have at least one event on both sides
        if before_indexed_events.len() < 1 || after_indexed_events.len() < 1 {
            continue;
        }

        // Function and file must match
        let before_event = &before_indexed_events[0].event;
        let after_event = &after_indexed_events[0].event;
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

        related_before_events.push(before_indexed_events.pop_front().unwrap().event);
        related_after_events.push(after_indexed_events.pop_front().unwrap().event);

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
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be replace
        if diff_op.tag() != DiffTag::Replace {
            continue;
        }

        // Should have two tuples with deleted and inserted lines
        assert!(change_tuples_indexed_events.len() == 2);
        let (befores, afters) = change_tuples_indexed_events.split_at_mut(1);
        let (before_change_tag, before_indexed_events) = &mut befores[0];
        let (after_change_tag, after_indexed_events) = &mut afters[0];
        assert!(*before_change_tag == ChangeTag::Delete);
        assert!(*after_change_tag == ChangeTag::Insert);

        // Must have at least one event on both sides
        if before_indexed_events.len() < 1 || after_indexed_events.len() < 1 {
            continue;
        }

        // Function and file must match
        let before_event = &before_indexed_events[0].event;
        let after_event = &after_indexed_events[0].event;
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

        related_before_events.push(before_indexed_events.pop_front().unwrap().event);
        related_after_events.push(after_indexed_events.pop_front().unwrap().event);

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
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be insert
        if diff_op.tag() != DiffTag::Insert {
            continue;
        }

        // Should have a single tuple with inserted lines
        assert!(change_tuples_indexed_events.len() == 1);
        let (change_tag, indexed_events) = &mut change_tuples_indexed_events[0];
        assert!(*change_tag == ChangeTag::Insert);

        // Must have at least 3 events
        if indexed_events.len() < 3 {
            continue;
        }

        // First event should be call from traced binary
        let first_event = &indexed_events[0].event;
        if first_event.event_type != EventType::CallFrom {
            continue;
        }

        // Second event should be call to external library
        let second_event = &indexed_events[1].event;
        if second_event.event_type != EventType::CallTo {
            continue;
        }
        if !second_event.detail.to_lowercase().contains("external code") {
            continue;
        }

        // Third event should be return from external library
        let third_event = &indexed_events[2].event;
        if third_event.event_type != EventType::ReturnFrom {
            continue;
        }
        if !third_event.detail.to_lowercase().contains("external code") {
            continue;
        }

        // Extract related events
        let related_before_events = vec![];
        let mut related_after_events = vec![];

        // First 3 are known to match
        related_after_events.push(indexed_events.pop_front().unwrap().event);
        related_after_events.push(indexed_events.pop_front().unwrap().event);
        related_after_events.push(indexed_events.pop_front().unwrap().event);

        divergences.push(Divergence::new(
            DivergenceType::LibraryCallAdded,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
//   CF: strbuf_vaddf at strbuf.c:397:8
// <   CT: Jump to external code for ___vsnprintf_chk
// >   CT: Jump to external code for _vsnprintf
// <   RF: Jump to external code for ___vsnprintf_chk
// >   RF: Jump to external code for _vsnprintf
fn check_for_library_call_replaced(
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Look for changed external call events
        if diff_op.tag() != DiffTag::Replace {
            continue;
        }
        assert!(change_tuples_indexed_events.len() == 2);
        let (befores, afters) = change_tuples_indexed_events.split_at_mut(1);
        let (before_change_tag, before_indexed_events) = &mut befores[0];
        let (after_change_tag, after_indexed_events) = &mut afters[0];
        assert!(*before_change_tag == ChangeTag::Delete);
        assert!(*after_change_tag == ChangeTag::Insert);

        // Extract related events
        let mut related_before_events = vec![];
        let mut related_after_events = vec![];

        // Any number of adjacent events accepted
        while !before_indexed_events.is_empty() && !after_indexed_events.is_empty() {
            let before_event = &before_indexed_events[0].event;
            let after_event = &after_indexed_events[0].event;
            // Ensure before and after events mentions external call
            if !before_event.detail.to_lowercase().contains("external code")
                || !after_event.detail.to_lowercase().contains("external code")
            {
                break;
            }
            // Ensure the event type matches
            if before_event.event_type != after_event.event_type {
                break;
            }
            // Ensure the function name does _not_ match
            if before_event.location.function == after_event.location.function {
                break;
            }
            related_before_events.push(before_indexed_events.pop_front().unwrap().event);
            related_after_events.push(after_indexed_events.pop_front().unwrap().event);
        }

        if related_before_events.is_empty() || related_after_events.is_empty() {
            continue;
        }

        divergences.push(Divergence::new(
            DivergenceType::LibraryCallReplaced,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// < RF: Jump to external code for bsearch
// > IRF: bsearch at stdlib-bsearch.h:0:0
// < CT: Jump to external code for bsearch
// > ICT: bsearch at stdlib-bsearch.h:20:0
fn check_for_library_call_inlined(
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Look for inlined external call events
        if diff_op.tag() != DiffTag::Replace {
            continue;
        }
        assert!(change_tuples_indexed_events.len() == 2);
        let (befores, afters) = change_tuples_indexed_events.split_at_mut(1);
        let (before_change_tag, before_indexed_events) = &mut befores[0];
        let (after_change_tag, after_indexed_events) = &mut afters[0];
        assert!(*before_change_tag == ChangeTag::Delete);
        assert!(*after_change_tag == ChangeTag::Insert);

        // Extract related events
        let mut related_before_events = vec![];
        let mut related_after_events = vec![];

        // Any number of adjacent events accepted
        while !before_indexed_events.is_empty() && !after_indexed_events.is_empty() {
            let before_event = &before_indexed_events[0].event;
            let after_event = &after_indexed_events[0].event;
            // Ensure before event mentions external call
            if !before_event.detail.to_lowercase().contains("external code") {
                break;
            }
            // Ensure at least the event type and function name matches
            if before_event.event_type != after_event.event_type {
                break;
            }
            if before_event.location.function != after_event.location.function {
                break;
            }
            // Ensure after event is inlined
            if after_event.event_source != EventSource::InlinedChain {
                break;
            }
            related_before_events.push(before_indexed_events.pop_front().unwrap().event);
            related_after_events.push(after_indexed_events.pop_front().unwrap().event);
        }

        if related_before_events.is_empty() || related_after_events.is_empty() {
            continue;
        }

        divergences.push(Divergence::new(
            DivergenceType::LibraryCallInlined,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// - CF: strbuf_init at strbuf.c:57:2
// -   CT: Jump to external code
// -   RF: Jump to external code
fn check_for_library_call_removed(
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be delete
        if diff_op.tag() != DiffTag::Delete {
            continue;
        }

        // Should have a single tuple with deleted lines
        assert!(change_tuples_indexed_events.len() == 1);
        let (change_tag, indexed_events) = &mut change_tuples_indexed_events[0];
        assert!(*change_tag == ChangeTag::Delete);

        // Must have at least 3 events
        if indexed_events.len() < 3 {
            continue;
        }

        // First event should be call from traced binary
        let first_event = &indexed_events[0].event;
        if first_event.event_type != EventType::CallFrom {
            continue;
        }

        // Second event should be call to external library
        let second_event = &indexed_events[1].event;
        if second_event.event_type != EventType::CallTo {
            continue;
        }
        if !second_event.detail.to_lowercase().contains("external code") {
            continue;
        }

        // Third event should be return from external library
        let third_event = &indexed_events[2].event;
        if third_event.event_type != EventType::ReturnFrom {
            continue;
        }
        if !third_event.detail.to_lowercase().contains("external code") {
            continue;
        }

        // Extract related events
        let mut related_before_events = vec![];
        let related_after_events = vec![];

        // First 3 are known to match
        related_before_events.push(indexed_events.pop_front().unwrap().event);
        related_before_events.push(indexed_events.pop_front().unwrap().event);
        related_before_events.push(indexed_events.pop_front().unwrap().event);

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
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be delete
        if diff_op.tag() != DiffTag::Delete {
            continue;
        }

        // Should have a single tuple with deleted lines
        assert!(change_tuples_indexed_events.len() == 1);
        let (change_tag, indexed_events) = &mut change_tuples_indexed_events[0];
        assert!(*change_tag == ChangeTag::Delete);

        // Must have at least 3 events
        if indexed_events.len() < 3 {
            continue;
        }

        // Event sequence should be call from, call to, return from.
        // Call to and return from should not mention external code.
        // There may be any number of additionally removed events
        // between the call to and return from.
        let first_event = &indexed_events[0].event;
        if first_event.event_type != EventType::CallFrom {
            continue;
        }
        let second_event = &indexed_events[1].event;
        if second_event.event_type != EventType::CallTo {
            continue;
        }
        if second_event.detail.to_lowercase().contains("external code") {
            continue;
        }
        // Look for last return from the called function in this block
        let called_function = &second_event.location.function;
        if called_function.is_none() {
            continue;
        }
        let mut last_return_from: Option<usize> = None;
        for i in 2..indexed_events.len() {
            let event = &indexed_events[i].event;
            if event.event_type != EventType::ReturnFrom {
                continue;
            }
            if event.detail.to_lowercase().contains("external code") {
                continue;
            }
            if event.location.function != *called_function {
                continue;
            }
            last_return_from = Some(i);
        }
        if last_return_from.is_none() {
            continue;
        }

        // Extract related events
        let related_before_events = indexed_events
            .drain(0..=last_return_from.unwrap())
            .map(|ie| ie.event)
            .collect();
        let related_after_events = vec![];

        divergences.push(Divergence::new(
            DivergenceType::ProgramCallRemoved,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// + IRF: get_builtin at git.c:0:0
// + ICT: get_builtin at git.c:635:0
fn check_for_inlined_reentry_added(
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be insert
        if diff_op.tag() != DiffTag::Insert {
            continue;
        }

        // Should have a single tuple with inserted lines
        assert!(change_tuples_indexed_events.len() == 1);
        let (change_tag, indexed_events) = &mut change_tuples_indexed_events[0];
        assert!(*change_tag == ChangeTag::Insert);

        // Must have at least 2 events
        if indexed_events.len() < 2 {
            continue;
        };

        // First event should be inlined return from
        let first_event = &indexed_events[0].event;
        if first_event.event_source != EventSource::InlinedChain {
            continue;
        }
        if first_event.event_type != EventType::ReturnFrom {
            continue;
        }

        // Second event should be inlined call to
        let second_event = &indexed_events[1].event;
        if second_event.event_source != EventSource::InlinedChain {
            continue;
        }
        if second_event.event_type != EventType::CallTo {
            continue;
        }

        // Events should have the same function name
        if first_event.location.function.is_none() || second_event.location.function.is_none() {
            continue;
        }
        if first_event.location.function != second_event.location.function {
            continue;
        }

        // Extract related events
        let related_before_events = vec![];
        let mut related_after_events = vec![];
        related_after_events.push(indexed_events.pop_front().unwrap().event);
        related_after_events.push(indexed_events.pop_front().unwrap().event);

        divergences.push(Divergence::new(
            DivergenceType::InlinedReentryAdded,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// + ICT: get_builtin at git.c:635:0
// + IRF: get_builtin at git.c:0:0
fn check_for_inlined_noise_added(
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be insert
        if diff_op.tag() != DiffTag::Insert {
            continue;
        }

        // Should have a single tuple with inserted lines
        assert!(change_tuples_indexed_events.len() == 1);
        let (change_tag, indexed_events) = &mut change_tuples_indexed_events[0];
        assert!(*change_tag == ChangeTag::Insert);

        // Must have at least 2 events
        if indexed_events.len() < 2 {
            continue;
        };

        // First event should be inlined call to
        let first_event = &indexed_events[0].event;
        if first_event.event_source != EventSource::InlinedChain {
            continue;
        }
        if first_event.event_type != EventType::CallTo {
            continue;
        }

        // Second event should be inlined return from
        let second_event = &indexed_events[1].event;
        if second_event.event_source != EventSource::InlinedChain {
            continue;
        }
        if second_event.event_type != EventType::ReturnFrom {
            continue;
        }

        // Events should have the same function name
        if first_event.location.function.is_none() || second_event.location.function.is_none() {
            continue;
        }
        if first_event.location.function != second_event.location.function {
            continue;
        }

        // Extract related events
        let related_before_events = vec![];
        let mut related_after_events = vec![];
        related_after_events.push(indexed_events.pop_front().unwrap().event);
        related_after_events.push(indexed_events.pop_front().unwrap().event);

        divergences.push(Divergence::new(
            DivergenceType::InlinedNoiseAdded,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

// Example diff:
// + IRF: check_commit at object-file.c:0:0
fn check_for_inlined_return_added(
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    for (diff_op, change_tuples_indexed_events) in grouped_indexed_events {
        // Diff op for this region should be insert
        if diff_op.tag() != DiffTag::Insert {
            continue;
        }

        // Should have a single tuple with inserted lines
        assert!(change_tuples_indexed_events.len() == 1);
        let (change_tag, indexed_events) = &mut change_tuples_indexed_events[0];
        assert!(*change_tag == ChangeTag::Insert);

        // Must have at least 1 event
        if indexed_events.len() < 1 {
            continue;
        };

        // Event should be inlined return from
        let first_event = &indexed_events[0].event;
        if first_event.event_source != EventSource::InlinedChain {
            continue;
        }
        if first_event.event_type != EventType::ReturnFrom {
            continue;
        }

        // Extract related events
        let related_before_events = vec![];
        let mut related_after_events = vec![];
        related_after_events.push(indexed_events.pop_front().unwrap().event);

        divergences.push(Divergence::new(
            DivergenceType::InlinedReturnAdded,
            related_before_events,
            related_after_events,
            diff_op,
        ));
    }

    divergences
}

fn check_for_known_divergences(
    diff: &Diff<'_>,
    grouped_indexed_events: &mut [(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];
    let mut uncategorised_grouped_events: HashMap<DiffOp, Vec<(ChangeTag, VecDeque<Event>)>> =
        HashMap::new();

    // Check for divergences at least once and keep going each time more are found
    // JRS: Change patterns to produce all matching divergences up front...?
    loop {
        {
            let mut divergences_found = check_for_coordinates_removed(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_coordinates_changed(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_added(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_replaced(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_inlined(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_library_call_removed(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_program_call_removed(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_inlined_reentry_added(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_inlined_noise_added(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }
        {
            let mut divergences_found = check_for_inlined_return_added(grouped_indexed_events);
            if !divergences_found.is_empty() {
                divergences.append(&mut divergences_found);
                continue;
            }
        }

        // If any events remain, collect one from each tuple in preparation for
        // assembling an uncategorised divergence after checking what remains
        let mut moved_to_uncategorised = false;
        for (diff_op, change_tuples_indexed_events) in &mut *grouped_indexed_events {
            if diff_op.tag() == DiffTag::Equal {
                continue;
            }
            let events_present = change_tuples_indexed_events
                .iter()
                .any(|(_, events)| !events.is_empty());
            if !events_present {
                continue;
            }
            // Look for certain terms that suggest non-determinism
            static ND_RE: Lazy<Regex> =
                Lazy::new(|| Regex::new(r"(sig|command|hash|alloc|env)").unwrap());
            let nondeterminism_found =
                change_tuples_indexed_events
                    .iter()
                    .any(|(_, indexed_events)| {
                        indexed_events
                            .iter()
                            .any(|ie| ND_RE.is_match(&ie.event.detail))
                    });
            if nondeterminism_found {
                // Return what we have and ignore the rest
                return divergences;
            }
            // Retrieve or create the uncategorised change tuples with events for this diff op
            let uncategorised_ctes =
                uncategorised_grouped_events
                    .entry(*diff_op)
                    .or_insert_with(|| {
                        let mut ctes: Vec<(ChangeTag, VecDeque<Event>)> = Vec::new();
                        for (change_tag, _) in &*change_tuples_indexed_events {
                            ctes.push((*change_tag, VecDeque::new()));
                        }
                        ctes
                    });
            // Move one event from each tuple, then retry matching
            for i in 0..change_tuples_indexed_events.len() {
                let (change_tag, indexed_events) = &mut change_tuples_indexed_events[i];
                if *change_tag == ChangeTag::Equal {
                    continue;
                }
                let (_, uncategorised_events) = &mut uncategorised_ctes[i];
                uncategorised_events.push_back(indexed_events.pop_front().unwrap().event);
                moved_to_uncategorised = true;
            }
        }
        if moved_to_uncategorised {
            continue;
        }

        break;
    }

    // Assemble uncategorised divergences from whatever may remain
    for (diff_op, change_tuples_events) in uncategorised_grouped_events {
        let mut remaining_before_events = vec![];
        let mut remaining_after_events = vec![];
        for (change_tag, mut events) in change_tuples_events {
            if change_tag == ChangeTag::Equal {
                continue;
            }
            let mut collected_events = events.drain(..).collect::<Vec<_>>();
            if change_tag == ChangeTag::Delete {
                remaining_before_events.append(&mut collected_events);
            } else {
                remaining_after_events.append(&mut collected_events);
            }
        }
        divergences.push(Divergence::new(
            DivergenceType::Uncategorised,
            remaining_before_events,
            remaining_after_events,
            &diff_op,
        ));
    }

    divergences
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

fn to_change_tuples_indices(op: &DiffOp) -> Vec<(ChangeTag, VecDeque<usize>)> {
    let mut change_tuples_indices = Vec::new();
    match *op {
        DiffOp::Equal { .. } => {
            change_tuples_indices.push((ChangeTag::Equal, op.old_range().collect()));
        }
        DiffOp::Insert { .. } => {
            change_tuples_indices.push((ChangeTag::Insert, op.new_range().collect()));
        }
        DiffOp::Delete { .. } => {
            change_tuples_indices.push((ChangeTag::Delete, op.old_range().collect()));
        }
        DiffOp::Replace { .. } => {
            change_tuples_indices.push((ChangeTag::Delete, op.old_range().collect()));
            change_tuples_indices.push((ChangeTag::Insert, op.new_range().collect()));
        }
    }
    change_tuples_indices
}

struct IndexedEvent {
    index: usize,
    event: Event,
}

fn collect_grouped_indexed_events(
    diff: &Diff<'_>,
    op_group: &Vec<DiffOp>,
) -> Vec<(DiffOp, Vec<(ChangeTag, VecDeque<IndexedEvent>)>)> {
    let mut grouped_indexed_events = vec![];

    for op in op_group {
        let change_tuples_indices = to_change_tuples_indices(op);
        // TODO: Skip unnecessary collects / copies
        let change_tuples_events: Vec<_> = op
            .iter_slices(&diff.before_trace.events, &diff.after_trace.events)
            .map(|(tag, events)| (tag, events.iter().cloned().collect::<VecDeque<_>>()))
            .collect();
        let change_tuples_indexed_events: Vec<(ChangeTag, VecDeque<IndexedEvent>)> =
            change_tuples_indices
                .into_iter()
                .zip(change_tuples_events)
                .map(|(it, et)| {
                    assert!(it.0 == et.0);
                    let indexed_events: VecDeque<IndexedEvent> =
                        it.1.into_iter()
                            .zip(et.1)
                            .map(|(index, event)| IndexedEvent { index, event })
                            .collect();
                    (it.0, indexed_events)
                })
                .collect();
        grouped_indexed_events.push((*op, change_tuples_indexed_events));
    }

    grouped_indexed_events
}

pub struct DivergenceAnalysis {
    remarks_by_location: Option<HashMap<Location, Remark>>,
    divergence_stats_by_coordinates: BTreeMap<Divergence, u64>,
}

impl DivergenceAnalysis {
    pub fn new(remarks_by_location: Option<HashMap<Location, Remark>>) -> DivergenceAnalysis {
        DivergenceAnalysis {
            remarks_by_location,
            divergence_stats_by_coordinates: BTreeMap::new(),
        }
    }

    pub fn analyse_diff(&mut self, diff: &Diff<'_>) {
        for op_group in &diff.grouped_diff_ops {
            if log_enabled!(log::Level::Debug) {
                println!("{:#?}", &op_group);
                println!();
                print_change_group(diff, &op_group);
                println!();
            }

            let mut grouped_indexed_events = collect_grouped_indexed_events(diff, op_group);

            // Check events against known divergence patterns
            let mut new_divergences =
                check_for_known_divergences(diff, &mut grouped_indexed_events);
            for divergence in &mut new_divergences {
                // If we'll print example trace lines, record trace path
                if log_enabled!(log::Level::Info) {
                    divergence.before_file_path = diff.before_file_path.clone();
                    divergence.after_file_path = diff.after_file_path.clone();
                }

                // Look for optimisation remarks at divergence coordinates
                if let Some(remarks) = &self.remarks_by_location {
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

                if log_enabled!(log::Level::Debug) {
                    println!("{:#?}", divergence);
                    println!();
                }

                // Insert or update stats for these source coordinates
                if let Some(occurrences) = self.divergence_stats_by_coordinates.get_mut(divergence)
                {
                    *occurrences += 1;
                } else {
                    self.divergence_stats_by_coordinates
                        .insert(divergence.clone(), 1);
                }
            }
        }
    }

    pub fn print_report(&self) {
        println!("## Divergences by source coordinates");
        println!();

        let mut divergence_coordinates_count_by_type: BTreeMap<DivergenceType, u64> =
            BTreeMap::new();
        let mut occurrences_total: u64 = 0;
        for (divergence, occurrences) in &self.divergence_stats_by_coordinates {
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
                println!("  Example trace lines:");
                println!(
                    "    -{}:{}",
                    divergence.before_file_path.as_ref().unwrap().display(),
                    divergence.old_index
                );
                println!(
                    "    +{}:{}",
                    divergence.after_file_path.as_ref().unwrap().display(),
                    divergence.new_index
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
            self.divergence_stats_by_coordinates.len()
        );
        println!("{} divergence occurrences", occurrences_total);
    }

    pub fn print_countable_events_by_type(&self, events_by_type_dir: &PathBuf) -> Result<()> {
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

        for divergence in self.divergence_stats_by_coordinates.keys() {
            let mut file = &files_by_type[&divergence.divergence_type];
            let mut printable_events = &divergence.before_events;
            // In the less common case of only added events,
            // let's use those so we at least have something to count
            if printable_events.is_empty() {
                printable_events = &divergence.after_events;
            }
            for event in printable_events {
                writeln!(file, "{}", event)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::trace::Trace;

    use super::*;

    #[test]
    fn parse_error() {
        let result = Event::parse("RF: do_xmalloc at     CT: External code");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Corrupt event"));
    }

    #[test]
    fn coordinates_removed() {
        let diff = Diff::new(
            Trace::parse_lines(Vec::from(["CF: getnanotime at trace.c:397:18"])),
            Trace::parse_lines(Vec::from(["CF: getnanotime at trace.c:0:0"])),
            Vec::from([Vec::from([DiffOp::Replace {
                old_index: 0,
                old_len: 1,
                new_index: 0,
                new_len: 1,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences = check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
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
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CT: xstrdup_or_null at git-compat-util.h:1168:0",
            ])),
            Trace::parse_lines(Vec::from([
                "CT: xstrdup_or_null at git-compat-util.h:1169:9",
            ])),
            Vec::from([Vec::from([DiffOp::Replace {
                old_index: 0,
                old_len: 1,
                new_index: 0,
                new_len: 1,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
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
    fn library_call_replaced() {
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CF: strbuf_vaddf at strbuf.c:397:8",
                "  CT: Jump to external code for ___vsnprintf_chk",
                "  RF: Jump to external code for ___vsnprintf_chk",
            ])),
            Trace::parse_lines(Vec::from([
                "CF: strbuf_vaddf at strbuf.c:397:8",
                "  CT: Jump to external code for _vsnprintf",
                "  RF: Jump to external code for _vsnprintf",
            ])),
            Vec::from([Vec::from([DiffOp::Replace {
                old_index: 1,
                old_len: 2,
                new_index: 1,
                new_len: 2,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::LibraryCallReplaced
        );
        assert_eq!(divergence.before_events.len(), 2);
    }

    #[test]
    fn library_call_removed_single() {
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CF: strbuf_init at strbuf.c:57:2",
                "  CT: Jump to external code",
                "  RF: Jump to external code",
            ])),
            Trace::parse_lines(Vec::from([])),
            Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 3,
                new_index: 0,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
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
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CF: init_repository_format at setup.c:710:33",
                "  CT: Jump to external code",
                "  RF: Jump to external code",
                "CF: init_repository_format at setup.c:712:2",
                "  CT: Jump to external code",
                "  RF: Jump to external code",
            ])),
            Trace::parse_lines(Vec::from([])),
            Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 6,
                new_index: 0,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
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
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CF: is_absolute_path at cache.h:1276:32",
                "  CT: git_has_dos_drive_prefix at git-compat-util.h:432:0",
                "  RF: git_has_dos_drive_prefix at git-compat-util.h:433:2",
            ])),
            Trace::parse_lines(Vec::from([])),
            Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 3,
                new_index: 0,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::ProgramCallRemoved
        );
        assert_eq!(divergence.before_events.len(), 3);
    }

    #[test]
    fn program_call_subtree_removed() {
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CF: clear_repository_format at setup.c:730:2",
                "  CT: init_repository_format at setup.c:709:0",
                "  CF: init_repository_format at setup.c:710:33",
                "    CT: Jump to external code for memset",
                "    RF: Jump to external code for memset",
                "  CF: init_repository_format at setup.c:712:2",
                "    CT: Jump to external code for memcpy",
                "    RF: Jump to external code for memcpy",
                "  RF: init_repository_format at setup.c:713:1",
            ])),
            Trace::parse_lines(Vec::from([])),
            Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 9,
                new_index: 0,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::ProgramCallRemoved
        );
        assert_eq!(divergence.before_events.len(), 9);
    }

    #[test]
    fn program_call_removed_after_uncategorised() {
        let diff = Diff::new(
            Trace::parse_lines(Vec::from([
                "CT: uncategorised at in-the-way.c:0:0",
                "CF: is_absolute_path at cache.h:1276:32",
                "  CT: git_has_dos_drive_prefix at git-compat-util.h:432:0",
                "  RF: git_has_dos_drive_prefix at git-compat-util.h:433:2",
            ])),
            Trace::parse_lines(Vec::from([])),
            Vec::from([Vec::from([DiffOp::Delete {
                old_index: 0,
                old_len: 4,
                new_index: 0,
            }])]),
        );
        let mut divergences: Vec<Divergence> = Vec::new();
        for op_group in &diff.grouped_diff_ops {
            let mut grouped_indexed_events = collect_grouped_indexed_events(&diff, op_group);
            let mut new_divergences =
                check_for_known_divergences(&diff, &mut grouped_indexed_events);
            divergences.append(&mut new_divergences);
        }
        assert_eq!(divergences.len(), 2);
        assert_eq!(
            divergences[0].divergence_type,
            DivergenceType::ProgramCallRemoved
        );
        assert_eq!(divergences[0].before_events.len(), 3);
        assert_eq!(
            divergences[1].divergence_type,
            DivergenceType::Uncategorised
        );
        assert_eq!(divergences[1].before_events.len(), 1);
    }
}
