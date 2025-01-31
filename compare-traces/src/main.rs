use std::{fs, path::PathBuf};

use crate::diff::print_diff;
use crate::report::analyse_and_print_report;
use anyhow::{Context, Ok, Result};
use clap::Parser;
use similar::TextDiff;

mod diff;
mod report;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// Trace before program transformations
    before_file: PathBuf,
    /// Trace after program transformations
    after_file: PathBuf,

    /// Whether to use terminal colors
    #[arg(long)]
    color: Option<bool>,

    /// Show trace diff
    #[arg(long)]
    diff: bool,

    /// Analyse and report trace divergences
    #[arg(long, default_value_t = true)]
    report: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if let Some(colors_enabled) = cli.color {
        console::set_colors_enabled(colors_enabled);
    }

    let before_content = fs::read_to_string(&cli.before_file).with_context(|| {
        format!(
            "Unable to read before trace ({})",
            cli.before_file.display()
        )
    })?;
    let after_content = fs::read_to_string(&cli.after_file).with_context(|| {
        format!(
            "Unable to read after trace ({})",
            cli.after_file.display()
        )
    })?;

    let diff = TextDiff::configure()
        .algorithm(similar::Algorithm::Patience)
        .diff_lines(&before_content,&after_content);

    if cli.diff {
        print_diff(&diff);
    }

    if cli.report {
        analyse_and_print_report(&diff);
    }

    Ok(())
}
