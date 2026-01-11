//! Project loader
//!
//! Handles loading multi-file Fastbreak projects with import resolution.

use super::manifest::{Manifest, ManifestError};
use crate::ast::Specification;
use crate::model::{compile, CompiledSpec};
use crate::parser::{self, ParseError};
use crate::semantic::{self, Analyzer};
use indexmap::IndexMap;
use smol_str::SmolStr;
use std::path::{Path, PathBuf};

// Re-export SemanticError for error handling
pub use crate::semantic::SemanticError;

/// A loaded Fastbreak project
#[derive(Debug)]
pub struct Project {
    /// The project manifest
    pub manifest: Manifest,
    /// Root directory of the project
    pub root: PathBuf,
    /// Loaded source files
    pub sources: IndexMap<PathBuf, SourceFile>,
    /// Combined compiled specification
    pub spec: CompiledSpec,
    /// Semantic analyzer results
    pub analyzer: Analyzer,
}

impl Project {
    /// Get the project name
    #[must_use]
    pub fn name(&self) -> &str {
        &self.manifest.project.name
    }

    /// Get the project version
    #[must_use]
    pub fn version(&self) -> &str {
        &self.manifest.project.version
    }

    /// Check if the project has any errors
    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.analyzer.has_errors()
    }

    /// Get all semantic errors
    #[must_use]
    pub fn errors(&self) -> &[SemanticError] {
        self.analyzer.errors()
    }
}

/// A loaded source file
#[derive(Debug)]
pub struct SourceFile {
    /// File path
    pub path: PathBuf,
    /// File contents
    pub source: String,
    /// Parsed AST
    pub ast: Specification,
    /// Module name (if declared)
    pub module: Option<SmolStr>,
}

/// Project loader
pub struct Loader {
    /// Root directory for resolution (reserved for future import resolution)
    #[allow(dead_code)]
    root: PathBuf,
    /// Source configuration
    source_dir: PathBuf,
    /// File extension
    extension: String,
}

impl Loader {
    /// Create a new loader for a project root
    #[must_use]
    pub fn new(root: PathBuf) -> Self {
        Self {
            source_dir: root.join("specs"),
            extension: "fbs".to_string(),
            root,
        }
    }

    /// Create a loader from a manifest
    #[must_use]
    pub fn from_manifest(manifest: &Manifest, root: PathBuf) -> Self {
        Self {
            source_dir: root.join(&manifest.source.dir),
            extension: manifest.source.extension.clone(),
            root,
        }
    }

    /// Load a project from a manifest file
    ///
    /// # Errors
    ///
    /// Returns an error if the manifest cannot be loaded or files cannot be parsed.
    pub fn load_project(manifest_path: &Path) -> Result<Project, LoadError> {
        let manifest = Manifest::load(manifest_path).map_err(LoadError::Manifest)?;

        let root = manifest_path
            .parent()
            .map_or_else(|| PathBuf::from("."), Path::to_path_buf);

        let loader = Self::from_manifest(&manifest, root.clone());
        loader.load_with_manifest(manifest, root)
    }

    /// Load a single file without a manifest
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be loaded or parsed.
    pub fn load_file(path: &Path) -> Result<Project, LoadError> {
        let source = std::fs::read_to_string(path).map_err(|e| LoadError::Io {
            path: path.to_path_buf(),
            source: e,
        })?;

        let ast = parser::parse(&source).map_err(|e| LoadError::Parse {
            path: path.to_path_buf(),
            error: e,
        })?;

        let module = ast.module.as_ref().map(|m| m.name.name.clone());

        let analyzer = semantic::analyze(&ast);
        let spec = compile(&ast, &analyzer);

        let mut sources = IndexMap::new();
        sources.insert(
            path.to_path_buf(),
            SourceFile {
                path: path.to_path_buf(),
                source,
                ast,
                module,
            },
        );

        // Create a minimal manifest
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unnamed");

        let manifest = Manifest::new(name);

        let root = path
            .parent()
            .map_or_else(|| PathBuf::from("."), Path::to_path_buf);

        Ok(Project {
            manifest,
            root,
            sources,
            spec,
            analyzer,
        })
    }

    fn load_with_manifest(self, manifest: Manifest, root: PathBuf) -> Result<Project, LoadError> {
        let mut sources = IndexMap::new();

        // Collect all source files
        let source_files = self.collect_source_files()?;

        if source_files.is_empty() {
            return Err(LoadError::NoSources {
                dir: self.source_dir,
            });
        }

        // Load and parse each file
        for path in source_files {
            let source = std::fs::read_to_string(&path).map_err(|e| LoadError::Io {
                path: path.clone(),
                source: e,
            })?;

            let ast = parser::parse(&source).map_err(|e| LoadError::Parse {
                path: path.clone(),
                error: e,
            })?;

            let module = ast.module.as_ref().map(|m| m.name.name.clone());

            sources.insert(
                path.clone(),
                SourceFile {
                    path,
                    source,
                    ast,
                    module,
                },
            );
        }

        // Combine all ASTs and analyze
        // For now, we just use the first file's AST
        // TODO: Implement proper multi-file merging with import resolution
        let combined_ast = if let Some(first) = sources.values().next() {
            first.ast.clone()
        } else {
            Specification::default()
        };

        let analyzer = semantic::analyze(&combined_ast);
        let spec = compile(&combined_ast, &analyzer);

        Ok(Project {
            manifest,
            root,
            sources,
            spec,
            analyzer,
        })
    }

    fn collect_source_files(&self) -> Result<Vec<PathBuf>, LoadError> {
        let mut files = Vec::new();

        if !self.source_dir.exists() {
            return Ok(files);
        }

        self.collect_files_recursive(&self.source_dir, &mut files)?;

        // Sort for deterministic ordering
        files.sort();

        Ok(files)
    }

    fn collect_files_recursive(
        &self,
        dir: &Path,
        files: &mut Vec<PathBuf>,
    ) -> Result<(), LoadError> {
        let entries = std::fs::read_dir(dir).map_err(|e| LoadError::Io {
            path: dir.to_path_buf(),
            source: e,
        })?;

        for entry in entries {
            let entry = entry.map_err(|e| LoadError::Io {
                path: dir.to_path_buf(),
                source: e,
            })?;

            let path = entry.path();

            if path.is_dir() {
                self.collect_files_recursive(&path, files)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some(&self.extension) {
                files.push(path);
            }
        }

        Ok(())
    }
}

/// Errors that can occur when loading a project
#[derive(Debug)]
pub enum LoadError {
    /// Manifest error
    Manifest(ManifestError),
    /// IO error
    Io {
        /// Path that caused the error
        path: PathBuf,
        /// Underlying IO error
        source: std::io::Error,
    },
    /// Parse error
    Parse {
        /// Path that caused the error
        path: PathBuf,
        /// Parse error
        error: ParseError,
    },
    /// No source files found
    NoSources {
        /// Directory that was searched
        dir: PathBuf,
    },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Manifest(e) => write!(f, "{e}"),
            LoadError::Io { path, source } => {
                write!(f, "IO error reading {}: {}", path.display(), source)
            }
            LoadError::Parse { path, error } => {
                write!(f, "Parse error in {}: {}", path.display(), error)
            }
            LoadError::NoSources { dir } => {
                write!(f, "No source files found in {}", dir.display())
            }
        }
    }
}

impl std::error::Error for LoadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            LoadError::Manifest(e) => Some(e),
            LoadError::Io { source, .. } => Some(source),
            LoadError::Parse { .. } | LoadError::NoSources { .. } => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_project() -> TempDir {
        let dir = TempDir::new().unwrap();

        // Create manifest
        let manifest = Manifest::new("test-project");
        manifest.save(&dir.path().join("fastbreak.toml")).unwrap();

        // Create source directory
        fs::create_dir(dir.path().join("specs")).unwrap();

        // Create a source file
        fs::write(
            dir.path().join("specs/main.fbs"),
            r#"
                module main

                type User {
                    id: Int,
                    name: String,
                }
            "#,
        )
        .unwrap();

        dir
    }

    #[test]
    fn test_load_project() {
        let dir = create_test_project();
        let manifest_path = dir.path().join("fastbreak.toml");

        let project = Loader::load_project(&manifest_path).unwrap();

        assert_eq!(project.name(), "test-project");
        assert_eq!(project.sources.len(), 1);
        assert!(!project.has_errors());
    }

    #[test]
    fn test_load_single_file() {
        let dir = TempDir::new().unwrap();
        let file_path = dir.path().join("test.fbs");

        fs::write(
            &file_path,
            r#"
                type User {
                    id: Int,
                }
            "#,
        )
        .unwrap();

        let project = Loader::load_file(&file_path).unwrap();

        assert_eq!(project.name(), "test");
        assert_eq!(project.sources.len(), 1);
        assert!(!project.has_errors());
    }

    #[test]
    fn test_load_project_no_sources() {
        let dir = TempDir::new().unwrap();

        // Create manifest but no sources
        let manifest = Manifest::new("empty-project");
        manifest.save(&dir.path().join("fastbreak.toml")).unwrap();

        // Create empty source directory
        fs::create_dir(dir.path().join("specs")).unwrap();

        let manifest_path = dir.path().join("fastbreak.toml");
        let result = Loader::load_project(&manifest_path);

        assert!(matches!(result, Err(LoadError::NoSources { .. })));
    }

    #[test]
    fn test_load_nested_files() {
        let dir = TempDir::new().unwrap();

        // Create manifest
        let manifest = Manifest::new("nested-project");
        manifest.save(&dir.path().join("fastbreak.toml")).unwrap();

        // Create nested source directories
        fs::create_dir_all(dir.path().join("specs/models")).unwrap();
        fs::create_dir_all(dir.path().join("specs/actions")).unwrap();

        // Create source files
        fs::write(dir.path().join("specs/main.fbs"), "module main").unwrap();

        fs::write(
            dir.path().join("specs/models/user.fbs"),
            "module user\ntype User { id: Int }",
        )
        .unwrap();

        fs::write(
            dir.path().join("specs/actions/auth.fbs"),
            "module auth\naction login(user: String) -> Bool",
        )
        .unwrap();

        let manifest_path = dir.path().join("fastbreak.toml");
        let project = Loader::load_project(&manifest_path).unwrap();

        assert_eq!(project.sources.len(), 3);
    }
}
