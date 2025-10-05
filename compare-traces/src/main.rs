use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::panic;
use std::path::PathBuf;
use std::time::Duration;

use anyhow::{anyhow, Context, Ok, Result};
use clap::{Parser, ValueEnum};
use diff::Diff;
use indicatif::{ProgressBar, ProgressStyle};
use log::log_enabled;
use rayon::prelude::*;
use similar::TextDiff;
use tree_diff::diff_tree;
use walkdir::WalkDir;

use crate::event::Location;
use crate::inlining::{preprocess_inlining, InliningTransform};
use crate::print::print_diff;
use crate::remarks::{load_remarks, Remark};
use crate::report::DivergenceAnalysis;
use crate::trace::Trace;

mod diff;
mod event;
mod inlining;
mod print;
mod remarks;
mod report;
mod trace;
mod tree;
mod tree_diff;

#[derive(ValueEnum, Clone, Debug)]
enum DiffStrategy {
    /// Textual diff using the patience algorithm
    Text,
    /// Tree / structural diff
    Tree,
}

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// Trace file or directory before program transformations
    before_file_or_dir: PathBuf,
    /// Trace file or directory after program transformations
    after_file_or_dir: PathBuf,

    /// LLVM optimisation remarks (YAML)
    #[arg(long = "remarks")]
    remarks_file: Option<PathBuf>,

    /// Record events found in each divergence category
    #[arg(long)]
    events_by_type_dir: Option<PathBuf>,

    /// Stop after parsing traces
    #[arg(long)]
    parse_only: bool,

    /// Show trace diff
    #[arg(long)]
    diff: bool,

    /// Disable analysis and reporting of trace divergences
    #[arg(long)]
    no_report: bool,

    /// Strategy to use when comparing traces to reveal divergences
    #[arg(long, value_enum, default_value_t = DiffStrategy::Tree)]
    diff_strategy: DiffStrategy,

    /// Approach to use when transforming inlined traces
    #[arg(long, value_enum, default_value_t = InliningTransform::Clustered)]
    inlining_transform: InliningTransform,

    /// Whether to save traces to new files just after the inlining transform
    #[arg(long)]
    save_after_inlining_transform: bool,

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

    let mut before_files: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(&cli.before_file_or_dir).sort_by_file_name() {
        let entry = entry?;
        if entry.file_type().is_dir() {
            continue;
        }
        before_files.push(entry.into_path());
    }
    alphanumeric_sort::sort_path_slice(&mut before_files);
    let mut after_files: Vec<PathBuf> = Vec::new();
    for entry in WalkDir::new(&cli.after_file_or_dir).sort_by_file_name() {
        let entry = entry?;
        if entry.file_type().is_dir() {
            continue;
        }
        after_files.push(entry.into_path());
    }
    alphanumeric_sort::sort_path_slice(&mut after_files);

    // Verify that we have same file hierarchy on both sides
    if before_files.len() != after_files.len() {
        return Err(anyhow!(
            "Mismatched file hierarchies: {} before files, {} after files",
            before_files.len(),
            after_files.len()
        ));
    }

    let mut remarks_by_location: Option<HashMap<Location, Remark>> = None;
    if let Some(remarks_file) = &cli.remarks_file {
        remarks_by_location = Some(load_remarks(&remarks_file)?);
    }

    let progress = ProgressBar::new(before_files.len().try_into().unwrap());
    progress.set_style(
        ProgressStyle::with_template(
            "{spinner} [{elapsed_precise}] [{wide_bar}] {pos}/{len} ({eta})",
        )
        .unwrap(),
    );
    progress.force_draw();

    // Collect separate analyses per file pair in parallel
    let separate_analyses: Vec<_> = (0..before_files.len())
        .into_par_iter()
        .map(|i| {
            let mut divergence_analysis: Option<DivergenceAnalysis> = None;
            if !cli.no_report {
                divergence_analysis = Some(DivergenceAnalysis::new());
            }

            let before_file = &before_files[i];
            let after_file = &after_files[i];

            let before_content = fs::read_to_string(before_file).with_context(|| {
                format!("Unable to read before trace ({})", before_file.display())
            })?;
            let after_content = fs::read_to_string(after_file).with_context(|| {
                format!("Unable to read after trace ({})", after_file.display())
            })?;

            if before_content.is_empty() && after_content.is_empty() {
                progress.inc(1);
                return Ok(None);
            }

            let result = panic::catch_unwind(|| {
                let mut diff: Diff = match cli.diff_strategy {
                    DiffStrategy::Text => Diff::from(
                        TextDiff::configure()
                            .algorithm(similar::Algorithm::Patience)
                            .timeout(Duration::from_secs(10 * 60))
                            .diff_lines(&before_content, &after_content),
                    ),
                    DiffStrategy::Tree => {
                        let mut before =
                            Trace::parse_str_with_context(&before_content, before_file.to_str())?;
                        let mut after =
                            Trace::parse_str_with_context(&after_content, after_file.to_str())?;

                        if cli.parse_only {
                            progress.inc(1);
                            return Ok(None);
                        }

                        let transform = cli.inlining_transform;
                        preprocess_inlining(&mut before, &mut after, transform);

                        // Compacting adjacent events depends on their indices reflecting
                        // lines they'd have in printed trace, so re-numbering is required
                        // for multi-line patterns to match after the inlining transform.
                        // In addition, it makes the verbose trace easier to understand
                        // (at least when also saving the inlined transforms).
                        before.renumber();
                        after.renumber();

                        if cli.save_after_inlining_transform {
                            assert!(cli.before_file_or_dir.is_dir());
                            assert!(cli.after_file_or_dir.is_dir());
                            let mut before_file_inlining_dir =
                                before_file.parent().unwrap().to_path_buf();
                            before_file_inlining_dir.set_file_name(format!(
                                "{}-inlining-{}",
                                before_file_inlining_dir
                                    .file_name()
                                    .unwrap()
                                    .to_str()
                                    .unwrap()
                                    .to_string(),
                                transform.to_file_name(),
                            ));
                            fs::create_dir_all(&before_file_inlining_dir)?;
                            let before_file_inlining_path =
                                before_file_inlining_dir.join(before_file.file_name().unwrap());
                            let mut before_file_inlining =
                                BufWriter::new(File::create(before_file_inlining_path)?);
                            write!(&mut before_file_inlining, "{}", before)?;
                            before_file_inlining.flush()?;
                            let mut after_file_inlining_dir =
                                after_file.parent().unwrap().to_path_buf();
                            after_file_inlining_dir.set_file_name(format!(
                                "{}-inlining-{}",
                                after_file_inlining_dir
                                    .file_name()
                                    .unwrap()
                                    .to_str()
                                    .unwrap()
                                    .to_string(),
                                transform.to_file_name(),
                            ));
                            fs::create_dir_all(&after_file_inlining_dir)?;
                            let after_file_inlining_path =
                                after_file_inlining_dir.join(after_file.file_name().unwrap());
                            let mut after_file_inlining =
                                BufWriter::new(File::create(after_file_inlining_path)?);
                            write!(&mut after_file_inlining, "{}", after)?;
                            after_file_inlining.flush()?;
                        }

                        Diff::from(diff_tree(before, after))
                    }
                };

                diff.before_file_path = Some(before_file.clone());
                diff.after_file_path = Some(after_file.clone());

                if cli.diff {
                    if log_enabled!(log::Level::Debug) {
                        println!("{:#?}", &diff);
                        println!();
                    }
                    print_diff(&diff);
                }

                if let Some(divergence_analysis) = &mut divergence_analysis {
                    divergence_analysis.analyse_diff(remarks_by_location.as_ref(), &diff);
                }

                progress.inc(1);

                return Ok(divergence_analysis);
            });
            if result.is_err() {
                return Err(anyhow!("Panic ({})", before_file.display()));
            }
            result.unwrap()
        })
        .map(|r| r.unwrap())
        .collect();

    progress.finish();

    if separate_analyses.iter().all(|analysis| analysis.is_none()) {
        return Ok(());
    }

    progress.reset();

    // Merge separate analyses together sequentially (for deterministic output)
    let merged_analysis =
        separate_analyses
            .into_iter()
            .fold(DivergenceAnalysis::new(), |mut merged, analysis| {
                if let Some(analysis) = analysis {
                    DivergenceAnalysis::into_merged(&mut merged, analysis);
                }
                progress.inc(1);
                merged
            });

    // Print merged results
    merged_analysis.print_report()?;

    if let Some(events_by_type_dir) = &cli.events_by_type_dir {
        if !events_by_type_dir.is_dir() {
            return Err(anyhow!(
                "Events by type path `{}` is not a directory",
                events_by_type_dir.display(),
            ));
        }
        merged_analysis.print_countable_events_by_type(&events_by_type_dir)?;
    }

    progress.finish();

    Ok(())
}
