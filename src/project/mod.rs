//! Project management module
//!
//! This module handles Fastbreak project configuration and multi-file loading:
//!
//! - `Manifest`: Project configuration from `fastbreak.toml`
//! - `Project`: Loaded project with all specifications
//! - `Loader`: Multi-file project loading with import resolution

mod loader;
mod manifest;

pub use loader::{LoadError, Loader, Project};
pub use manifest::{Manifest, OutputConfig};
