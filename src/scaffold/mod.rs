//! Defines the `poinciana scaffold` command.
//!
//! This command scaffolds a Cairo file from a spec `.tree` file.

use std::{
    fs,
    path::{Path, PathBuf},
};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_formatter::format_string;
use clap::Parser;
use owo_colors::OwoColorize;
use serde::{Deserialize, Serialize};

use crate::{config::Config, hir::translate, syntax::get_database};

pub mod emitter;
pub mod modifiers;

/// Generate Cairo tests based on your spec.
#[doc(hidden)]
#[derive(Parser, Debug, Clone, Serialize, Deserialize)]
pub struct Scaffold {
    /// The set of tree files to generate from.
    ///
    /// Each Cairo test file will be named after its matching
    /// tree spec.
    pub files: Vec<PathBuf>,
    /// Whether to write to files instead of stdout.
    ///
    /// This will write the output for each input file to the file
    /// specified at the root of the input file if the output file
    /// doesn't already exist. To overwrite, use `--force-write`
    /// together with `--write-files`.
    #[arg(short = 'w', long, group = "file-handling", default_value_t = false)]
    pub write_files: bool,
    /// When `--write-files` is passed, use `--force-write` to
    /// overwrite the output files.
    #[arg(
        short = 'f',
        long,
        requires = "file-handling",
        default_value_t = false
    )]
    pub force_write: bool,
    /// Whether to emit modifiers.
    #[arg(short = 'm', long, default_value_t = false)]
    pub skip_modifiers: bool,
    /// The number of spaces to indent by in the resulting cairo code.
    #[arg(short = 'i', long, requires = "file-handling", default_value_t = 2)]
    pub indent: usize,
}

impl Default for Scaffold {
    fn default() -> Self {
        Scaffold::parse_from(Vec::<String>::new())
    }
}

impl Scaffold {
    /// Runs the scaffold command, processing all specified files.
    ///
    /// This method iterates through all input files, processes them, and either
    /// writes the output to files or prints to stdout based on the config.
    ///
    /// If any errors occur during processing, they are collected and reported.
    pub(crate) fn run(&self, cfg: &Config) -> anyhow::Result<()> {
        let db = get_database();
        let errors: Vec<_> = self
            .files
            .iter()
            .filter_map(|file| {
                self.process_file(&db, file, cfg)
                    .map_err(|e| (file.as_path(), e))
                    .err()
            })
            .collect();

        if !errors.is_empty() {
            self.report_errors(&errors);
            std::process::exit(1);
        }

        Ok(())
    }

    /// Processes a single input file.
    ///
    /// This method reads the input file, scaffolds the Cairo code, formats
    /// it, and either writes it to a file or prints it to stdout.
    fn process_file(
        &self,
        db: &RootDatabase,
        file: &Path,
        cfg: &Config,
    ) -> anyhow::Result<()> {
        let text = fs::read_to_string(file)?;
        let emitted = scaffold(&text, cfg)?;
        let formatted = format_string(db, emitted);

        if self.write_files {
            let file = file.with_extension("t.cairo");
            self.write_file(&formatted, &file);
        } else {
            println!("{formatted}");
        }

        Ok(())
    }

    /// Writes the provided `text` to `file`.
    ///
    /// If the file doesn't exist it will create it. If it exists,
    /// and `--force-write` was not passed, it will skip writing to the file.
    fn write_file(&self, text: &str, file: &PathBuf) {
        // Don't overwrite files unless `--force-write` was passed.
        if file.exists() && !self.force_write {
            eprintln!(
                "{}: Skipped emitting {:?}",
                "warn".yellow(),
                file.as_path().blue()
            );
            eprintln!(
                "    {} The corresponding `.t.cairo` file already exists",
                "=".blue()
            );
            return;
        }

        if let Err(err) = fs::write(file, text) {
            eprintln!("{}: {err}", "error".red());
        };
    }

    /// Reports errors that occurred during file processing.
    ///
    /// This method prints error messages for each file that failed to process,
    /// along with a summary of the total number of failed files.
    fn report_errors(&self, errors: &[(&Path, anyhow::Error)]) {
        for (file, err) in errors {
            eprintln!("{err}");
            eprintln!("file: {}", file.display());
        }

        eprintln!(
            "\n{}: Could not scaffold {} files. Check the output above.",
            "warn".yellow(),
            errors.len().yellow()
        );
    }
}

/// Generates Cairo code from a `.tree` file.
///
/// This function takes the content of a `.tree` file and a configuration,
/// translates it to an intermediate representation, and then to Cairo.
pub fn scaffold(text: &str, cfg: &Config) -> anyhow::Result<String> {
    let hir = translate(text, cfg)?;
    let cairo_code = emitter::Emitter::new(cfg).emit(&hir);
    Ok(cairo_code)
}
