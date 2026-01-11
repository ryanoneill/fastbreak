//! Project manifest (fastbreak.toml)
//!
//! Defines the project configuration format for Fastbreak projects.

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Project manifest loaded from `fastbreak.toml`
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    /// Project metadata
    pub project: ProjectInfo,

    /// Source configuration
    #[serde(default)]
    pub source: SourceConfig,

    /// Output configuration
    #[serde(default)]
    pub output: OutputConfig,
}

impl Manifest {
    /// Load a manifest from a file path
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed.
    pub fn load(path: &Path) -> Result<Self, ManifestError> {
        let content = std::fs::read_to_string(path).map_err(|e| ManifestError::Io {
            path: path.to_path_buf(),
            source: e,
        })?;

        toml::from_str(&content).map_err(|e| ManifestError::Parse {
            path: path.to_path_buf(),
            source: e,
        })
    }

    /// Save a manifest to a file path
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be written.
    pub fn save(&self, path: &Path) -> Result<(), ManifestError> {
        let content = toml::to_string_pretty(self).map_err(|e| ManifestError::Serialize {
            path: path.to_path_buf(),
            source: e,
        })?;

        std::fs::write(path, content).map_err(|e| ManifestError::Io {
            path: path.to_path_buf(),
            source: e,
        })
    }

    /// Create a new manifest with default values
    #[must_use]
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            project: ProjectInfo {
                name: name.into(),
                version: "0.1.0".to_string(),
                description: None,
                authors: Vec::new(),
            },
            source: SourceConfig::default(),
            output: OutputConfig::default(),
        }
    }

    /// Find a manifest by searching upward from the current directory
    ///
    /// # Errors
    ///
    /// Returns an error if no manifest is found.
    pub fn find(start: &Path) -> Result<PathBuf, ManifestError> {
        let mut current = start.to_path_buf();

        loop {
            let manifest_path = current.join("fastbreak.toml");
            if manifest_path.exists() {
                return Ok(manifest_path);
            }

            if !current.pop() {
                return Err(ManifestError::NotFound {
                    searched_from: start.to_path_buf(),
                });
            }
        }
    }
}

/// Project metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectInfo {
    /// Project name
    pub name: String,
    /// Project version
    pub version: String,
    /// Project description
    pub description: Option<String>,
    /// Project authors
    #[serde(default)]
    pub authors: Vec<String>,
}

/// Source file configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceConfig {
    /// Root directory for source files (relative to manifest)
    #[serde(default = "default_src_dir")]
    pub dir: PathBuf,
    /// File extension for specification files
    #[serde(default = "default_extension")]
    pub extension: String,
    /// Entry point file (relative to source dir)
    pub entry: Option<PathBuf>,
}

impl Default for SourceConfig {
    fn default() -> Self {
        Self {
            dir: default_src_dir(),
            extension: default_extension(),
            entry: None,
        }
    }
}

fn default_src_dir() -> PathBuf {
    PathBuf::from("specs")
}

fn default_extension() -> String {
    "fbs".to_string()
}

/// Output configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputConfig {
    /// Output directory for generated files (relative to manifest)
    #[serde(default = "default_output_dir")]
    pub dir: PathBuf,
    /// Generate Markdown documentation
    #[serde(default = "default_true")]
    pub markdown: bool,
    /// Generate Mermaid diagrams
    #[serde(default = "default_true")]
    pub diagrams: bool,
    /// Diagram types to generate
    #[serde(default)]
    pub diagram_types: DiagramTypes,
}

impl Default for OutputConfig {
    fn default() -> Self {
        Self {
            dir: default_output_dir(),
            markdown: true,
            diagrams: true,
            diagram_types: DiagramTypes::default(),
        }
    }
}

fn default_output_dir() -> PathBuf {
    PathBuf::from("docs")
}

const fn default_true() -> bool {
    true
}

/// Which diagram types to generate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagramTypes {
    /// Generate ERD diagrams
    #[serde(default = "default_true")]
    pub erd: bool,
    /// Generate state diagrams
    #[serde(default = "default_true")]
    pub state: bool,
    /// Generate sequence diagrams
    #[serde(default = "default_true")]
    pub sequence: bool,
}

impl Default for DiagramTypes {
    fn default() -> Self {
        Self {
            erd: true,
            state: true,
            sequence: true,
        }
    }
}

/// Errors that can occur when working with manifests
#[derive(Debug)]
pub enum ManifestError {
    /// IO error reading/writing manifest
    Io {
        /// Path that caused the error
        path: PathBuf,
        /// Underlying IO error
        source: std::io::Error,
    },
    /// Parse error in TOML
    Parse {
        /// Path that caused the error
        path: PathBuf,
        /// Underlying parse error
        source: toml::de::Error,
    },
    /// Serialization error
    Serialize {
        /// Path that caused the error
        path: PathBuf,
        /// Underlying serialization error
        source: toml::ser::Error,
    },
    /// No manifest found
    NotFound {
        /// Directory searched from
        searched_from: PathBuf,
    },
}

impl std::fmt::Display for ManifestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ManifestError::Io { path, source } => {
                write!(f, "IO error reading {}: {}", path.display(), source)
            }
            ManifestError::Parse { path, source } => {
                write!(f, "Parse error in {}: {}", path.display(), source)
            }
            ManifestError::Serialize { path, source } => {
                write!(f, "Serialization error for {}: {}", path.display(), source)
            }
            ManifestError::NotFound { searched_from } => {
                write!(
                    f,
                    "No fastbreak.toml found searching from {}",
                    searched_from.display()
                )
            }
        }
    }
}

impl std::error::Error for ManifestError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ManifestError::Io { source, .. } => Some(source),
            ManifestError::Parse { source, .. } => Some(source),
            ManifestError::Serialize { source, .. } => Some(source),
            ManifestError::NotFound { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manifest_new() {
        let manifest = Manifest::new("test-project");

        assert_eq!(manifest.project.name, "test-project");
        assert_eq!(manifest.project.version, "0.1.0");
        assert_eq!(manifest.source.dir, PathBuf::from("specs"));
        assert_eq!(manifest.source.extension, "fbs");
        assert_eq!(manifest.output.dir, PathBuf::from("docs"));
        assert!(manifest.output.markdown);
        assert!(manifest.output.diagrams);
    }

    #[test]
    fn test_manifest_serialize() {
        let manifest = Manifest::new("test-project");
        let toml = toml::to_string_pretty(&manifest).unwrap();

        assert!(toml.contains("[project]"));
        assert!(toml.contains("name = \"test-project\""));
        assert!(toml.contains("version = \"0.1.0\""));
    }

    #[test]
    fn test_manifest_deserialize() {
        let toml = r#"
            [project]
            name = "my-spec"
            version = "1.0.0"
            description = "A test specification"
            authors = ["Test Author"]

            [source]
            dir = "src/specs"
            extension = "fbs"

            [output]
            dir = "build/docs"
            markdown = true
            diagrams = true
        "#;

        let manifest: Manifest = toml::from_str(toml).unwrap();

        assert_eq!(manifest.project.name, "my-spec");
        assert_eq!(manifest.project.version, "1.0.0");
        assert_eq!(
            manifest.project.description,
            Some("A test specification".to_string())
        );
        assert_eq!(manifest.project.authors, vec!["Test Author"]);
        assert_eq!(manifest.source.dir, PathBuf::from("src/specs"));
        assert_eq!(manifest.output.dir, PathBuf::from("build/docs"));
    }

    #[test]
    fn test_manifest_defaults() {
        let toml = r#"
            [project]
            name = "minimal"
            version = "0.1.0"
        "#;

        let manifest: Manifest = toml::from_str(toml).unwrap();

        assert_eq!(manifest.source.dir, PathBuf::from("specs"));
        assert_eq!(manifest.source.extension, "fbs");
        assert_eq!(manifest.output.dir, PathBuf::from("docs"));
        assert!(manifest.output.markdown);
        assert!(manifest.output.diagrams);
    }
}
