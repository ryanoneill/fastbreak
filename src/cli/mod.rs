//! Command-line interface module
//!
//! This module provides the CLI for Fastbreak:
//!
//! - `fb build` - Check, generate docs, and generate diagrams
//! - `fb check` - Validate specifications
//! - `fb doc` - Generate Markdown documentation
//! - `fb diagram` - Generate Mermaid diagrams
//! - `fb init` - Create a new project

mod commands;

pub use commands::{run, Cli, Commands};
