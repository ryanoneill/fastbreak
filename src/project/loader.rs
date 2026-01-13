//! Project loader
//!
//! Handles loading multi-file Fastbreak projects with import resolution.

use super::manifest::{Manifest, ManifestError};
use crate::ast::{Module, Specification};
use crate::model::{compile, CompiledSpec};
use crate::parser::{self, ParseError};
use crate::semantic::{self, Analyzer, ModuleRegistry};
use crate::SourceLocation;
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
    /// Module registry for tracking definitions across files
    pub modules: ModuleRegistry,
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
            extension: "fbrk".to_string(),
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
            source_text: source.clone(),
            error: e,
        })?;

        let module = ast.module.as_ref().map(Module::name);

        // Build module registry from the AST
        let modules = ModuleRegistry::from_spec(&ast);

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
            modules,
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
                source_text: source.clone(),
                error: e,
            })?;

            let module = ast.module.as_ref().map(Module::name);

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

        // Merge all ASTs into a combined specification
        let combined_ast = Self::merge_specifications(&sources);

        // Build module registry from the combined AST
        let modules = ModuleRegistry::from_spec(&combined_ast);

        let analyzer = semantic::analyze(&combined_ast);
        let spec = compile(&combined_ast, &analyzer);

        Ok(Project {
            manifest,
            root,
            sources,
            spec,
            analyzer,
            modules,
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

    /// Merge multiple source files into a combined specification
    fn merge_specifications(sources: &IndexMap<PathBuf, SourceFile>) -> Specification {
        let mut combined = Specification::new();

        for source_file in sources.values() {
            let spec = &source_file.ast;

            // Use the first module declaration found
            if combined.module.is_none() {
                combined.module.clone_from(&spec.module);
            }

            // Merge all imports
            combined.imports.extend(spec.imports.iter().cloned());

            // Merge all type definitions
            combined.types.extend(spec.types.iter().cloned());

            // Merge all type aliases
            combined.type_aliases.extend(spec.type_aliases.iter().cloned());

            // Merge all enums
            combined.enums.extend(spec.enums.iter().cloned());

            // Merge all relations
            combined.relations.extend(spec.relations.iter().cloned());

            // Merge all states
            combined.states.extend(spec.states.iter().cloned());

            // Merge all actions
            combined.actions.extend(spec.actions.iter().cloned());

            // Merge all scenarios
            combined.scenarios.extend(spec.scenarios.iter().cloned());

            // Merge all properties
            combined.properties.extend(spec.properties.iter().cloned());

            // Merge all qualities
            combined.qualities.extend(spec.qualities.iter().cloned());
        }

        combined
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
        /// Source text (for computing line numbers)
        source_text: String,
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
            LoadError::Parse {
                path,
                source_text,
                error,
            } => {
                let location = SourceLocation::from_offset(source_text, error.span().start);
                write!(
                    f,
                    "Parse error in {}:{}: {}",
                    path.display(),
                    location,
                    error
                )
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
            dir.path().join("specs/main.fbrk"),
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
        let file_path = dir.path().join("test.fbrk");

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
        fs::write(dir.path().join("specs/main.fbrk"), "module main").unwrap();

        fs::write(
            dir.path().join("specs/models/user.fbrk"),
            "module user\ntype User { id: Int }",
        )
        .unwrap();

        fs::write(
            dir.path().join("specs/actions/auth.fbrk"),
            "module auth\naction login(user: String) -> Bool",
        )
        .unwrap();

        let manifest_path = dir.path().join("fastbreak.toml");
        let project = Loader::load_project(&manifest_path).unwrap();

        assert_eq!(project.sources.len(), 3);
    }

    #[test]
    fn test_merge_multiple_files() {
        let dir = TempDir::new().unwrap();

        // Create manifest
        let manifest = Manifest::new("merge-project");
        manifest.save(&dir.path().join("fastbreak.toml")).unwrap();

        // Create source directory
        fs::create_dir(dir.path().join("specs")).unwrap();

        // Create first file with a type
        fs::write(
            dir.path().join("specs/types.fbrk"),
            r#"
                type User {
                    id: Int,
                    name: String,
                }

                type Product {
                    id: Int,
                    name: String,
                }
            "#,
        )
        .unwrap();

        // Create second file with an enum and action
        fs::write(
            dir.path().join("specs/actions.fbrk"),
            r#"
                enum Status { Active, Inactive }

                action create_user(name: String) -> Bool
            "#,
        )
        .unwrap();

        // Create third file with a scenario
        fs::write(
            dir.path().join("specs/scenarios.fbrk"),
            r#"
                scenario "test scenario" {
                    given { x = 1 }
                    when { y = x + 1 }
                    then { y == 2 }
                }
            "#,
        )
        .unwrap();

        let manifest_path = dir.path().join("fastbreak.toml");
        let project = Loader::load_project(&manifest_path).unwrap();

        // Verify all definitions were merged
        assert_eq!(project.sources.len(), 3);
        assert_eq!(project.spec.structs.len(), 2); // User and Product
        assert_eq!(project.spec.enums.len(), 1); // Status
        assert_eq!(project.spec.actions.len(), 1); // create_user
        assert_eq!(project.spec.scenarios.len(), 1); // test scenario
    }

    #[test]
    fn test_cross_file_type_references() {
        let dir = TempDir::new().unwrap();

        // Create manifest
        let manifest = Manifest::new("cross-ref-project");
        manifest.save(&dir.path().join("fastbreak.toml")).unwrap();

        // Create source directory
        fs::create_dir(dir.path().join("specs")).unwrap();

        // Create file with base types
        fs::write(
            dir.path().join("specs/01_types.fbrk"),
            r#"
                type User {
                    id: Int,
                    name: String,
                }
            "#,
        )
        .unwrap();

        // Create file that references the User type
        fs::write(
            dir.path().join("specs/02_actions.fbrk"),
            r#"
                action create_user(name: String) -> User
            "#,
        )
        .unwrap();

        let manifest_path = dir.path().join("fastbreak.toml");
        let project = Loader::load_project(&manifest_path).unwrap();

        // Verify both definitions are present
        assert_eq!(project.spec.structs.len(), 1);
        assert_eq!(project.spec.actions.len(), 1);

        // Verify the module registry tracks the type
        assert!(project.modules.is_type_available("User"));

        // Verify there are no semantic errors (cross-file reference should work)
        assert!(
            !project.has_errors(),
            "Should not have errors: {:?}",
            project.errors()
        );
    }

    #[test]
    fn test_module_info_from_multi_file() {
        let dir = TempDir::new().unwrap();

        // Create manifest
        let manifest = Manifest::new("module-info-project");
        manifest.save(&dir.path().join("fastbreak.toml")).unwrap();

        // Create source directory
        fs::create_dir(dir.path().join("specs")).unwrap();

        // Create first file
        fs::write(
            dir.path().join("specs/a.fbrk"),
            r#"
                type TypeA { id: Int }
                enum EnumA { X, Y }
            "#,
        )
        .unwrap();

        // Create second file
        fs::write(
            dir.path().join("specs/b.fbrk"),
            r#"
                type TypeB { name: String }
                action do_something(x: Int) -> Bool
            "#,
        )
        .unwrap();

        let manifest_path = dir.path().join("fastbreak.toml");
        let project = Loader::load_project(&manifest_path).unwrap();

        // Verify module registry has all types
        assert!(project.modules.is_type_available("TypeA"));
        assert!(project.modules.is_type_available("TypeB"));
        assert!(project.modules.is_enum_available("EnumA"));
    }
}
