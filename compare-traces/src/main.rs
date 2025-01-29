use std::{fs, path::PathBuf};

use anyhow::{Context, Ok, Result};
use clap::Parser;
use similar::{ChangeTag, TextDiff};

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// Trace before program transformations
    before_file: PathBuf,
    /// Trace after program transformations
    after_file: PathBuf,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

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

    let diff = TextDiff::from_lines(&before_content,&after_content);

    for change in diff.iter_all_changes() {
        let sign = match change.tag() {
            ChangeTag::Delete => "-",
            ChangeTag::Insert => "+",
            ChangeTag::Equal => " ",
        };
        print!("{}{}", sign, change);
    }

    Ok(())
}
