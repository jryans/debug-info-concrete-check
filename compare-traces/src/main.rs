use std::collections::HashMap;
use std::time::Duration;
use std::{fs, path::PathBuf};

use anyhow::{anyhow, Context, Ok, Result};
use clap::Parser;
use similar::TextDiff;

use crate::print::print_diff;
use crate::remarks::{load_remarks, Remark};
use crate::report::{analyse_and_print_report, print_before_events_by_type, Location};

mod print;
mod remarks;
mod report;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// Trace before program transformations
    before_file: PathBuf,
    /// Trace after program transformations
    after_file: PathBuf,

    /// LLVM optimisation remarks (YAML)
    #[arg(long = "remarks")]
    remarks_file: Option<PathBuf>,

    /// Record events found in each divergence category
    #[arg(long)]
    events_by_type_dir: Option<PathBuf>,

    /// Show trace diff
    #[arg(long)]
    diff: bool,

    /// Analyse and report trace divergences
    #[arg(long, default_value_t = true)]
    report: bool,

    /// Tweak event alignment to improve text diffing results
    #[arg(long)]
    tweak_event_alignment: bool,

    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,

    /// Whether to use terminal colors
    #[arg(long)]
    color: Option<bool>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    env_logger::Builder::new()
        .filter_level(cli.verbose.log_level_filter())
        .init();

    if let Some(colors_enabled) = cli.color {
        console::set_colors_enabled(colors_enabled);
    }

    let before_content = fs::read_to_string(&cli.before_file).with_context(|| {
        format!(
            "Unable to read before trace ({})",
            cli.before_file.display()
        )
    })?;
    let after_content = fs::read_to_string(&cli.after_file)
        .with_context(|| format!("Unable to read after trace ({})", cli.after_file.display()))?;

    let mut remarks_by_location: Option<HashMap<Location, Remark>> = None;
    if let Some(remarks_file) = cli.remarks_file {
        remarks_by_location = Some(load_remarks(&remarks_file)?);
    }

    let diff = TextDiff::configure()
        .algorithm(similar::Algorithm::Patience)
        .timeout(Duration::from_secs(10 * 60))
        .diff_lines(&before_content, &after_content);

    if cli.diff {
        print_diff(&diff);
    }

    if cli.report {
        let divergence_stats_by_coordinates =
            analyse_and_print_report(&diff, &remarks_by_location, cli.tweak_event_alignment);
        if let Some(events_by_type_dir) = cli.events_by_type_dir {
            if !events_by_type_dir.is_dir() {
                return Err(anyhow!(
                    "Events by type path `{}` is not a directory",
                    events_by_type_dir.display(),
                ));
            }
            print_before_events_by_type(&divergence_stats_by_coordinates, &events_by_type_dir)?;
        }
    }

    Ok(())
}
