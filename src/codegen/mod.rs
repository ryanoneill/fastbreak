//! Code generation module
//!
//! This module provides output generation for Fastbreak specifications:
//!
//! - `markdown`: Generate Markdown documentation
//! - `mermaid`: Generate Mermaid diagrams (state diagrams, ERDs)

mod markdown;
mod mermaid;

pub use markdown::MarkdownGenerator;
pub use mermaid::{MermaidGenerator, DiagramType};
