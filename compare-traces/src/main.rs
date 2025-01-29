use std::{fs, path::PathBuf};

use anyhow::{Context, Ok, Result};
use clap::Parser;
use console::Style;
use similar::{ChangeTag, DiffTag, TextDiff};

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

    for op in diff.ops() {
        let change_tuples: Vec<_> = op
            .iter_slices(diff.old_slices(), diff.new_slices())
            .collect();
        if op.tag() == DiffTag::Replace {
            assert!(change_tuples.len() == 2);
            for (tag, slices) in change_tuples {
                let (sign, style) = match tag {
                    ChangeTag::Delete => ("<", Style::new().magenta()),
                    ChangeTag::Insert => (">", Style::new().yellow()),
                    ChangeTag::Equal => unreachable!(),
                };
                for slice in slices {
                    print!("{}{}", style.apply_to(sign).bold(), style.apply_to(slice));
                }
            }
        } else {
            assert!(change_tuples.len() == 1);
            for (tag, slices) in change_tuples {
                let (sign, style) = match tag {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new()),
                };
                for slice in slices {
                    print!("{}{}", style.apply_to(sign).bold(), style.apply_to(slice));
                }
            }
        }
    }

    Ok(())
}
