use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    fmt::Display,
    fs::File,
    hash::Hash,
    io::Write,
    path::PathBuf,
};

use anyhow::{anyhow, Context, Ok, Result};
use enum_iterator::Sequence;
use log::log_enabled;
use once_cell::sync::Lazy;
use regex::Regex;
use similar::{ChangeTag, DiffOp, DiffTag, TextDiff};

use crate::{diff::print_change_vec, remarks::Remark};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum EventType {
    CallFrom,
    CallTo,
    ReturnFrom,
    Warning,
    // Verbose,
}

impl Display for EventType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let abbreviation = match self {
            EventType::CallFrom => "CF",
            EventType::CallTo => "CT",
            EventType::ReturnFrom => "RF",
            EventType::Warning => "🔔",
        };
        write!(f, "{}", abbreviation)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Location {
    pub function: Option<String>,
    pub file: Option<String>,
    pub line: Option<u64>,
    pub column: Option<u64>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
struct Event {
    event_type: EventType,
    // TODO: Maybe store reference instead...?
    detail: String,
    location: Location,
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

        // Parse event type after advancing 2 Unicode characters
        let (event_type_end, _) = rest.char_indices().nth(2).unwrap();
        let event_type_str = &rest[..event_type_end];
        let event_type = match event_type_str {
            "CF" => EventType::CallFrom,
            "CT" => EventType::CallTo,
            "RF" => EventType::ReturnFrom,
            "🔔 " => EventType::Warning,
            _ => return Err(anyhow!("Unexpected event type: `{}`", event_type_str)),
        };
        // Advance past event type
        rest = &rest[event_type_end..];
        // Advance past ": " separator if used
        if event_type != EventType::Warning {
            rest = &rest[2..];
        }

        // Check for corrupt lines containing multiple events
        static EVENT_TYPES_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(CF|CT|RF):").unwrap());
        if EVENT_TYPES_RE.is_match(rest) {
            return Err(anyhow!("Corrupt event: `{}`", event_str.trim()));
        }

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
            location: Location {
                function,
                file,
                line,
                column,
            },
        })
    }
}

impl Display for Event {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.event_type == EventType::Warning {
            return write!(f, "{} {}", self.event_type, self.detail);
        }
        write!(f, "{}: {}", self.event_type, self.detail)
    }
}

#[derive(Sequence, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum DivergenceType {
    TailCallWithoutInfo,
    CoordinatesRemoved,
    CoordinatesChangedSmall,
    CoordinatesChangedLarge,
    LibraryCallRemoved,
    // TODO: Refine this by pass, similar to the paper
    ProgramCallRemoved,
    Uncategorised,
}

impl DivergenceType {
    fn to_file_name(&self) -> &str {
        match self {
            DivergenceType::TailCallWithoutInfo => "tail-call-without-info",
            DivergenceType::CoordinatesRemoved => "coordinates-removed",
            DivergenceType::CoordinatesChangedSmall => "coordinates-changed-small",
            DivergenceType::CoordinatesChangedLarge => "coordinates-changed-large",
            DivergenceType::LibraryCallRemoved => "library-call-removed",
            DivergenceType::ProgramCallRemoved => "program-call-removed",
            DivergenceType::Uncategorised => "uncategorised",
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct Divergence {
    divergence_type: DivergenceType,
    events: Vec<Event>,
    pass_responsible: Option<String>,
}

impl Divergence {
    fn new(divergence_type: DivergenceType, events: Vec<Event>) -> Divergence {
        Divergence {
            divergence_type,
            events,
            pass_responsible: None,
        }
    }

    fn location(&self) -> &Location {
        assert!(!self.events.is_empty());
        // Use first event to provide approximate coordinates for divergence
        &self.events[0].location
    }
}

// Example diff:
// < CT: xstrdup_or_null at git-compat-util.h:1168:0
// > CT: xstrdup_or_null at git-compat-util.h:1168:0 (TWCI)
fn check_for_tail_call_without_info(
    op: &DiffOp,
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Option<Divergence> {
    // Diff op for this region should be replace
    if op.tag() != DiffTag::Replace {
        return None;
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
        return None;
    }

    // Function and file must match
    let before_event = &before_events[0];
    let after_event = &after_events[0];
    if before_event.location.function != after_event.location.function {
        return None;
    }
    if before_event.location.file != after_event.location.file {
        return None;
    }

    // Detail must contain "(TCWI)"
    if !before_event.detail.contains("(TCWI)") && !after_event.detail.contains("(TCWI)") {
        return None;
    }

    // Extract related events
    let mut related_events = vec![];

    // TODO: Keep events from each side separate...?
    related_events.push(before_events.pop_front().unwrap());
    related_events.push(after_events.pop_front().unwrap());

    Some(Divergence::new(
        DivergenceType::TailCallWithoutInfo,
        related_events,
    ))
}

// Example diff:
// < CF: getnanotime at trace.c:397:18
// > CF: getnanotime at trace.c:0:0
fn check_for_coordinates_removed(
    op: &DiffOp,
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Option<Divergence> {
    // Diff op for this region should be replace
    if op.tag() != DiffTag::Replace {
        return None;
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
        return None;
    }

    // Function and file must match
    let before_event = &before_events[0];
    let after_event = &after_events[0];
    if before_event.location.function != after_event.location.function {
        return None;
    }
    if before_event.location.file != after_event.location.file {
        return None;
    }

    // After event must have missing line and column coordinates
    if after_event.location.line != Some(0) || after_event.location.column != Some(0) {
        return None;
    }

    // Extract related events
    let mut related_events = vec![];

    // TODO: Keep events from each side separate...?
    related_events.push(before_events.pop_front().unwrap());
    related_events.push(after_events.pop_front().unwrap());

    Some(Divergence::new(
        DivergenceType::CoordinatesRemoved,
        related_events,
    ))
}

// Example diff:
// < CT: xstrdup_or_null at git-compat-util.h:1168:0
// > CT: xstrdup_or_null at git-compat-util.h:1169:9
fn check_for_coordinates_changed(
    op: &DiffOp,
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Option<Divergence> {
    // Diff op for this region should be replace
    if op.tag() != DiffTag::Replace {
        return None;
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
        return None;
    }

    // Function and file must match
    let before_event = &before_events[0];
    let after_event = &after_events[0];
    if before_event.location.function != after_event.location.function {
        return None;
    }
    if before_event.location.file != after_event.location.file {
        return None;
    }

    // Line and column coordinates must be present
    if before_event.location.line.is_none()
        || before_event.location.column.is_none()
        || after_event.location.line.is_none()
        || after_event.location.column.is_none()
    {
        return None;
    }

    // Line or column coordinates must differ
    if before_event.location.line == after_event.location.line
        && before_event.location.column == after_event.location.column
    {
        return None;
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
    let mut related_events = vec![];

    // TODO: Keep events from each side separate...?
    related_events.push(before_events.pop_front().unwrap());
    related_events.push(after_events.pop_front().unwrap());

    Some(Divergence::new(
        divergence_type,
        related_events,
    ))
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
    assert!(change_tuples_events.len() == 1);
    let (change_tag, events) = &mut change_tuples_events[0];
    assert!(*change_tag == ChangeTag::Delete);

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

    Some(Divergence::new(
        DivergenceType::LibraryCallRemoved,
        related_events,
    ))
}

// Example diff:
// - CF: is_absolute_path at cache.h:1276:32
// -   CT: git_has_dos_drive_prefix at git-compat-util.h:432:0
// -   RF: git_has_dos_drive_prefix at git-compat-util.h:433:2
fn check_for_program_call_removed(
    op: &DiffOp,
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Option<Divergence> {
    // Diff op for this region should be delete
    if op.tag() != DiffTag::Delete {
        return None;
    }

    // Should have a single tuple with deleted lines
    assert!(change_tuples_events.len() == 1);
    let (change_tag, events) = &mut change_tuples_events[0];
    assert!(*change_tag == ChangeTag::Delete);

    // Must have at least 3 events
    if events.len() < 3 {
        return None;
    }

    // Event sequence should be call from, call to, return from
    // TODO: Support more complex trees with nested calls
    if events[0].event_type != EventType::CallFrom {
        return None;
    }
    if events[1].event_type != EventType::CallTo {
        return None;
    }
    if events[2].event_type != EventType::ReturnFrom {
        return None;
    }

    // Extract related events
    let mut related_events = vec![];
    related_events.push(events.pop_front().unwrap());
    related_events.push(events.pop_front().unwrap());
    related_events.push(events.pop_front().unwrap());

    Some(Divergence::new(
        DivergenceType::ProgramCallRemoved,
        related_events,
    ))
}

fn check_for_known_divergences(
    op: &DiffOp,
    change_tuples_events: &mut [(ChangeTag, VecDeque<Event>)],
) -> Vec<Divergence> {
    let mut divergences = vec![];

    if log_enabled!(log::Level::Debug) {
        println!("{:#?}", op);
        println!("{:#?}", change_tuples_events);
        println!();
    }

    // Check for divergences at least once and keep going each time more are found
    let mut continue_checking = true;
    while continue_checking {
        continue_checking = false;
        if let Some(divergence) = check_for_tail_call_without_info(op, change_tuples_events) {
            divergences.push(divergence);
            continue_checking = true;
        }
        if let Some(divergence) = check_for_coordinates_removed(op, change_tuples_events) {
            divergences.push(divergence);
            continue_checking = true;
        }
        if let Some(divergence) = check_for_coordinates_changed(op, change_tuples_events) {
            divergences.push(divergence);
            continue_checking = true;
        }
        if let Some(divergence) = check_for_library_call_removed(op, change_tuples_events) {
            divergences.push(divergence);
            continue_checking = true;
        }
        if let Some(divergence) = check_for_program_call_removed(op, change_tuples_events) {
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
        divergences.push(Divergence::new(
            DivergenceType::Uncategorised,
            merged_events,
        ));
    }

    divergences
}

fn tweak_alignment(op: &DiffOp, change_tuples_strings: &mut [(ChangeTag, Vec<&str>)]) {
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
            }
        }
    }
}

pub fn analyse_and_print_report(
    diff: &TextDiff<'_, '_, '_, str>,
    remarks_by_location: &Option<HashMap<Location, Remark>>,
) -> BTreeMap<Divergence, u64> {
    println!("Analysing divergences…");
    println!();

    let mut divergence_stats_by_coordinates: BTreeMap<Divergence, u64> = BTreeMap::new();

    for op_group in diff.grouped_ops(0) {
        for op in op_group {
            // Skip matching regions
            if op.tag() == DiffTag::Equal {
                continue;
            }

            // TODO: Skip unnecessary collects / copies
            let mut change_tuples_strings: Vec<_> = op
                .iter_slices(diff.old_slices(), diff.new_slices())
                .map(|(tag, slices)| (tag, Vec::from(slices)))
                .collect();

            // Fix up alignment where possible
            tweak_alignment(&op, &mut change_tuples_strings);

            if log_enabled!(log::Level::Debug) {
                print_change_vec(&op, &change_tuples_strings);
                println!();
            }

            // Parse raw text into events
            let mut parse_errors = vec![];
            let mut change_tuples_events: Vec<_> = change_tuples_strings
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

            // Check events against known divergence patterns
            let mut new_divergences = check_for_known_divergences(&op, &mut change_tuples_events);
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
    }

    println!("Divergence analysis complete!");
    println!();

    println!("## Divergences by source coordinates");
    println!();

    let mut divergence_coordinates_count_by_type: BTreeMap<DivergenceType, u64> = BTreeMap::new();
    let mut occurrences_total: u64 = 0;
    for (divergence, occurrences) in &divergence_stats_by_coordinates {
        println!("{:?}", divergence.divergence_type);
        println!("  Events:");
        let event_count = divergence.events.len();
        if event_count <= 30 {
            for event in &divergence.events {
                println!("    {}", event);
            }
        } else {
            let first_events = &divergence.events[..30];
            for event in first_events {
                println!("    {}", event);
            }
            println!("    [...{} more events...]", event_count - 30);
        }
        println!("  Occurrences: {}", occurrences);
        if let Some(pass) = &divergence.pass_responsible {
            println!("  Pass responsible: {}", pass);
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

pub fn print_events_by_type(
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
        for event in &divergence.events {
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
        let op = DiffOp::Replace {
            old_index: 7,
            old_len: 1,
            new_index: 7,
            new_len: 1,
        };
        let mut change_tuples_events = [
            (
                ChangeTag::Delete,
                VecDeque::from([Event::parse("CF: getnanotime at trace.c:397:18").unwrap()]),
            ),
            (
                ChangeTag::Insert,
                VecDeque::from([Event::parse("CF: getnanotime at trace.c:0:0").unwrap()]),
            ),
        ];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::CoordinatesRemoved
        );
        assert_eq!(divergence.events.len(), 2);
        assert_eq!(divergence.events[0].detail, "getnanotime at trace.c:397:18");
    }

    #[test]
    fn coordinates_changed() {
        // Example diff:
        // < CT: xstrdup_or_null at git-compat-util.h:1168:0
        // > CT: xstrdup_or_null at git-compat-util.h:1169:9
        let op = DiffOp::Replace {
            old_index: 444,
            old_len: 1,
            new_index: 390,
            new_len: 1,
        };
        let mut change_tuples_events = [
            (
                ChangeTag::Delete,
                VecDeque::from([
                    Event::parse("CT: xstrdup_or_null at git-compat-util.h:1168:0").unwrap(),
                ]),
            ),
            (
                ChangeTag::Insert,
                VecDeque::from([
                    Event::parse("CT: xstrdup_or_null at git-compat-util.h:1169:9").unwrap(),
                ]),
            ),
        ];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::CoordinatesChangedSmall
        );
        assert_eq!(divergence.events.len(), 2);
        assert_eq!(
            divergence.events[0].detail,
            "xstrdup_or_null at git-compat-util.h:1168:0"
        );
    }

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

    #[test]
    fn program_call_removed() {
        // Example diff:
        // - CF: is_absolute_path at cache.h:1276:32
        // -   CT: git_has_dos_drive_prefix at git-compat-util.h:432:0
        // -   RF: git_has_dos_drive_prefix at git-compat-util.h:433:2
        let op = DiffOp::Delete {
            old_index: 75,
            old_len: 3,
            new_index: 72,
        };
        let mut change_tuples_events = [(
            ChangeTag::Delete,
            VecDeque::from([
                Event::parse("CF: is_absolute_path at cache.h:1276:32").unwrap(),
                Event::parse("CT: git_has_dos_drive_prefix at git-compat-util.h:432:0").unwrap(),
                Event::parse("RF: git_has_dos_drive_prefix at git-compat-util.h:433:2").unwrap(),
            ]),
        )];
        let divergences = check_for_known_divergences(&op, &mut change_tuples_events);
        assert_eq!(divergences.len(), 1);
        let divergence = &divergences[0];
        assert_eq!(
            divergence.divergence_type,
            DivergenceType::ProgramCallRemoved
        );
        assert_eq!(divergence.events.len(), 3);
    }
}
