//! Fastbreak CLI - formal specification tool

use clap::Parser;
use fastbreak::cli::{run, Cli};
use std::process::ExitCode;

fn main() -> ExitCode {
    let cli = Cli::parse();

    match run(cli) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("Error: {e}");
            ExitCode::FAILURE
        }
    }
}
