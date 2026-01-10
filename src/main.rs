//! Fastbreak CLI - formal specification tool

use clap::{Parser, Subcommand};
use miette::Result;

#[derive(Parser)]
#[command(name = "fb")]
#[command(author, version, about = "Fastbreak: A formal methods-inspired specification language")]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new Fastbreak project
    Init {
        /// Project name
        name: Option<String>,
    },
    /// Build the project (check + doc + diagram)
    Build {
        /// Path to project or specification file
        #[arg(default_value = ".")]
        path: String,
    },
    /// Check specifications for errors
    Check {
        /// Path to project or specification file
        #[arg(default_value = ".")]
        path: String,
    },
    /// Generate documentation
    Doc {
        /// Path to project or specification file
        #[arg(default_value = ".")]
        path: String,
        /// Output directory
        #[arg(short, long, default_value = "docs")]
        output: String,
    },
    /// Generate Mermaid diagrams
    Diagram {
        /// Path to project or specification file
        #[arg(default_value = ".")]
        path: String,
        /// Output directory
        #[arg(short, long, default_value = "diagrams")]
        output: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Init { name } => {
            let project_name = name.unwrap_or_else(|| "my-spec".to_string());
            println!("Initializing Fastbreak project: {project_name}");
            // TODO: Implement project initialization
        }
        Commands::Build { path } => {
            println!("Building specifications in: {path}");
            // TODO: Implement build
        }
        Commands::Check { path } => {
            println!("Checking specifications in: {path}");
            // TODO: Implement check
        }
        Commands::Doc { path, output } => {
            println!("Generating documentation from: {path} to: {output}");
            // TODO: Implement doc generation
        }
        Commands::Diagram { path, output } => {
            println!("Generating diagrams from: {path} to: {output}");
            // TODO: Implement diagram generation
        }
    }

    Ok(())
}
