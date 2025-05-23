use std::fmt::Display;

use anyhow::{anyhow, Ok, Result};
use once_cell::sync::Lazy;
use regex::Regex;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum EventType {
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
pub struct Event {
    pub event_type: EventType,
    // TODO: Maybe store reference instead...?
    pub detail: String,
    pub location: Location,
}

impl Event {
    // See `printEventFromLineInfo` in `collect-trace.cpp` for output path
    pub fn parse(event_str: &str) -> Result<Self> {
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
