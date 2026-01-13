//! Module registry for multi-file specifications
//!
//! Tracks which definitions belong to which module and resolves imports.

use crate::ast::{Import, Module, Specification};
use indexmap::{IndexMap, IndexSet};
use smol_str::SmolStr;

/// Registry of modules and their definitions
#[derive(Debug, Default)]
pub struct ModuleRegistry {
    /// All known modules and their exported definitions
    modules: IndexMap<SmolStr, ModuleInfo>,
    /// The current module being analyzed
    current_module: Option<SmolStr>,
}

/// Information about a single module
#[derive(Debug, Default)]
pub struct ModuleInfo {
    /// Module name
    pub name: SmolStr,
    /// Types defined in this module
    pub types: IndexSet<SmolStr>,
    /// Enums defined in this module
    pub enums: IndexSet<SmolStr>,
    /// States defined in this module
    pub states: IndexSet<SmolStr>,
    /// Actions defined in this module
    pub actions: IndexSet<SmolStr>,
    /// Relations defined in this module
    pub relations: IndexSet<SmolStr>,
    /// Imports from other modules
    pub imports: Vec<ResolvedImport>,
}

/// A resolved import statement
#[derive(Debug, Clone)]
pub struct ResolvedImport {
    /// The source module path
    pub source_module: SmolStr,
    /// Specific items imported (empty means wildcard import)
    pub items: Vec<ImportedItem>,
}

/// An imported item with optional alias
#[derive(Debug, Clone)]
pub struct ImportedItem {
    /// Original name in the source module
    pub original_name: SmolStr,
    /// Local name (may be aliased)
    pub local_name: SmolStr,
}

/// Error during import resolution
#[derive(Debug, Clone, PartialEq)]
pub enum ImportError {
    /// The source module doesn't exist
    ModuleNotFound {
        /// Module that contains the import
        importing_module: SmolStr,
        /// Module that was not found
        source_module: SmolStr,
    },
    /// An imported item doesn't exist in the source module
    ItemNotFound {
        /// Module that contains the import
        importing_module: SmolStr,
        /// Module being imported from
        source_module: SmolStr,
        /// Item that was not found
        item_name: SmolStr,
    },
}

impl std::fmt::Display for ImportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportError::ModuleNotFound {
                importing_module,
                source_module,
            } => write!(
                f,
                "module '{importing_module}' imports from '{source_module}', but module '{source_module}' does not exist"
            ),
            ImportError::ItemNotFound {
                importing_module,
                source_module,
                item_name,
            } => write!(
                f,
                "module '{importing_module}' imports '{item_name}' from '{source_module}', but '{item_name}' is not defined in '{source_module}'"
            ),
        }
    }
}

impl ModuleRegistry {
    /// Create a new empty module registry
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a registry from a single specification
    #[must_use]
    pub fn from_spec(spec: &Specification) -> Self {
        let mut registry = Self::new();
        registry.register_spec(spec);
        registry
    }

    /// Build a registry from multiple specifications (for multi-file projects)
    #[must_use]
    pub fn from_specs(specs: &[&Specification]) -> Self {
        let mut registry = Self::new();
        for spec in specs {
            registry.register_spec(spec);
        }
        registry
    }

    /// Register a specification's definitions in this registry
    pub fn register_spec(&mut self, spec: &Specification) {
        // Determine the module name
        let module_name = spec
            .module
            .as_ref()
            .map_or_else(|| SmolStr::new("default"), Module::name);

        // Set current module if not already set
        if self.current_module.is_none() {
            self.current_module = Some(module_name.clone());
        }

        // Get or create module info
        let info = self
            .modules
            .entry(module_name.clone())
            .or_insert_with(|| ModuleInfo {
                name: module_name,
                ..Default::default()
            });

        // Register all types
        for type_def in &spec.types {
            info.types.insert(type_def.name.name.clone());
        }

        // Register all type aliases
        for alias in &spec.type_aliases {
            info.types.insert(alias.name.name.clone());
        }

        // Register all enums
        for enum_def in &spec.enums {
            info.enums.insert(enum_def.name.name.clone());
        }

        // Register all states
        for state in &spec.states {
            info.states.insert(state.name.name.clone());
        }

        // Register all actions
        for action in &spec.actions {
            info.actions.insert(action.name.name.clone());
        }

        // Register all relations
        for relation in &spec.relations {
            info.relations.insert(relation.name.name.clone());
        }

        // Process imports
        for import in &spec.imports {
            info.imports.push(Self::resolve_import(import));
        }
    }

    /// Set the current module context for type resolution
    pub fn set_current_module(&mut self, module_name: &str) {
        self.current_module = Some(SmolStr::new(module_name));
    }

    /// Resolve an import statement to a `ResolvedImport`
    fn resolve_import(import: &Import) -> ResolvedImport {
        // Use `.` as separator to match Module::name() format
        let source_module = import
            .path
            .segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        let items = import
            .items
            .as_ref()
            .map(|items| {
                items
                    .iter()
                    .map(|item| ImportedItem {
                        original_name: item.name.name.clone(),
                        local_name: item
                            .alias
                            .as_ref()
                            .map_or_else(|| item.name.name.clone(), |a| a.name.clone()),
                    })
                    .collect()
            })
            .unwrap_or_default();

        ResolvedImport {
            source_module: SmolStr::new(source_module),
            items,
        }
    }

    /// Get the current module name
    #[must_use]
    pub fn current_module(&self) -> Option<&SmolStr> {
        self.current_module.as_ref()
    }

    /// Get information about a module
    #[must_use]
    pub fn get_module(&self, name: &str) -> Option<&ModuleInfo> {
        self.modules.get(name)
    }

    /// Get the current module info
    #[must_use]
    pub fn current_module_info(&self) -> Option<&ModuleInfo> {
        self.current_module
            .as_ref()
            .and_then(|name| self.modules.get(name))
    }

    /// Check if a type is defined in the current module or imported
    #[must_use]
    pub fn is_type_available(&self, name: &str) -> bool {
        self.is_type_available_in(self.current_module.as_deref(), name)
    }

    /// Check if a type is available in a specific module
    #[must_use]
    pub fn is_type_available_in(&self, module_name: Option<&str>, name: &str) -> bool {
        let module_name = module_name.unwrap_or("default");
        let Some(info) = self.modules.get(module_name) else {
            return false;
        };

        // Check local definitions
        if info.types.contains(name) {
            return true;
        }

        // Check imports
        for import in &info.imports {
            // Get the source module info to verify the type exists
            let source_module = self.modules.get(import.source_module.as_str());

            if import.items.is_empty() {
                // Wildcard import - check if the type exists in source module
                if let Some(source) = source_module {
                    if source.types.contains(name) || source.enums.contains(name) {
                        return true;
                    }
                }
            } else {
                // Named import - check if this name is imported
                for item in &import.items {
                    if item.local_name == name {
                        // Verify the original name exists in the source module
                        if let Some(source) = source_module {
                            if source.types.contains(item.original_name.as_str())
                                || source.enums.contains(item.original_name.as_str())
                            {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        false
    }

    /// Check if an enum is defined in the current module or imported
    #[must_use]
    pub fn is_enum_available(&self, name: &str) -> bool {
        self.is_enum_available_in(self.current_module.as_deref(), name)
    }

    /// Check if an enum is available in a specific module
    #[must_use]
    pub fn is_enum_available_in(&self, module_name: Option<&str>, name: &str) -> bool {
        let module_name = module_name.unwrap_or("default");
        let Some(info) = self.modules.get(module_name) else {
            return false;
        };

        // Check local definitions
        if info.enums.contains(name) {
            return true;
        }

        // Check imports
        for import in &info.imports {
            let source_module = self.modules.get(import.source_module.as_str());

            if import.items.is_empty() {
                // Wildcard import
                if let Some(source) = source_module {
                    if source.enums.contains(name) {
                        return true;
                    }
                }
            } else {
                // Named import
                for item in &import.items {
                    if item.local_name == name {
                        if let Some(source) = source_module {
                            if source.enums.contains(item.original_name.as_str()) {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        false
    }

    /// Validate all imports in the registry and return unresolved imports
    #[must_use]
    pub fn validate_imports(&self) -> Vec<ImportError> {
        let mut errors = Vec::new();

        for (module_name, info) in &self.modules {
            for import in &info.imports {
                // Check if source module exists
                let Some(source) = self.modules.get(import.source_module.as_str()) else {
                    errors.push(ImportError::ModuleNotFound {
                        importing_module: module_name.clone(),
                        source_module: import.source_module.clone(),
                    });
                    continue;
                };

                // Validate each imported item exists in the source module
                for item in &import.items {
                    let exists = source.types.contains(item.original_name.as_str())
                        || source.enums.contains(item.original_name.as_str())
                        || source.states.contains(item.original_name.as_str())
                        || source.actions.contains(item.original_name.as_str());

                    if !exists {
                        errors.push(ImportError::ItemNotFound {
                            importing_module: module_name.clone(),
                            source_module: import.source_module.clone(),
                            item_name: item.original_name.clone(),
                        });
                    }
                }
            }
        }

        errors
    }

    /// Resolve a qualified name like `auth::User`
    #[must_use]
    pub fn resolve_qualified_name(&self, path: &[&str]) -> Option<QualifiedName> {
        if path.is_empty() {
            return None;
        }

        if path.len() == 1 {
            // Simple name - check current module
            let name = path[0];
            if let Some(info) = self.current_module_info() {
                if info.types.contains(name) || info.enums.contains(name) {
                    return Some(QualifiedName {
                        module: info.name.clone(),
                        name: SmolStr::new(name),
                    });
                }
            }
            None
        } else {
            // Qualified name - module::name
            let module_path = &path[..path.len() - 1];
            let name = path[path.len() - 1];
            let module_name = module_path.join("::");

            if let Some(info) = self.modules.get(module_name.as_str()) {
                if info.types.contains(name) || info.enums.contains(name) {
                    return Some(QualifiedName {
                        module: SmolStr::new(module_name),
                        name: SmolStr::new(name),
                    });
                }
            }
            None
        }
    }
}

/// A fully qualified name with module path
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualifiedName {
    /// The module containing the definition
    pub module: SmolStr,
    /// The name of the definition
    pub name: SmolStr,
}

impl std::fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.module, self.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn test_module_registry_basic() {
        let spec = parse(
            r#"
            module auth

            type User {
                id: Int,
                name: String,
            }

            enum Status { Active, Inactive }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_spec(&spec);

        assert_eq!(registry.current_module(), Some(&SmolStr::new("auth")));

        let info = registry.get_module("auth").unwrap();
        assert!(info.types.contains("User"));
        assert!(info.enums.contains("Status"));
    }

    #[test]
    fn test_module_registry_with_imports() {
        let spec = parse(
            r#"
            module main

            use common.types.Email
            use common.types.UserId
            use auth.User

            type Profile {
                id: Int,
            }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_spec(&spec);

        let info = registry.get_module("main").unwrap();
        assert_eq!(info.imports.len(), 3);

        // Check imports - paths use . separator to match Module::name() format
        assert_eq!(info.imports[0].source_module.as_str(), "common.types.Email");
        assert_eq!(info.imports[1].source_module.as_str(), "common.types.UserId");
        assert_eq!(info.imports[2].source_module.as_str(), "auth.User");
    }

    #[test]
    fn test_is_type_available() {
        let spec = parse(
            r#"
            module test

            type LocalType { id: Int }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_spec(&spec);

        assert!(registry.is_type_available("LocalType"));
        assert!(!registry.is_type_available("NonExistent"));
    }

    #[test]
    fn test_default_module() {
        let spec = parse("type Foo { id: Int }").unwrap();
        let registry = ModuleRegistry::from_spec(&spec);

        assert_eq!(registry.current_module(), Some(&SmolStr::new("default")));
    }

    #[test]
    fn test_cross_file_imports() {
        // First file defines shared types
        let types_spec = parse(
            r#"
            module types

            type UserId { value: Int }
            type Email { address: String }
            "#,
        )
        .unwrap();

        // Second file imports and uses the types
        let users_spec = parse(
            r#"
            module users

            use types::{UserId, Email}

            type User {
                id: UserId,
                email: Email,
            }
            "#,
        )
        .unwrap();

        // Build registry from both specs
        let registry = ModuleRegistry::from_specs(&[&types_spec, &users_spec]);

        // Check that both modules are registered
        assert!(registry.get_module("types").is_some());
        assert!(registry.get_module("users").is_some());

        // Check types are defined in the types module
        let types_info = registry.get_module("types").unwrap();
        assert!(types_info.types.contains("UserId"));
        assert!(types_info.types.contains("Email"));

        // Check that imported types are available in users module
        assert!(registry.is_type_available_in(Some("users"), "UserId"));
        assert!(registry.is_type_available_in(Some("users"), "Email"));
        assert!(registry.is_type_available_in(Some("users"), "User"));

        // Validate imports should succeed
        let errors = registry.validate_imports();
        assert!(errors.is_empty(), "Expected no import errors: {errors:?}");
    }

    #[test]
    fn test_import_validation_missing_module() {
        let spec = parse(
            r#"
            module test

            use nonexistent::{Foo}

            type Bar { id: Int }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_spec(&spec);
        let errors = registry.validate_imports();

        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            ImportError::ModuleNotFound { source_module, .. }
            if source_module == "nonexistent"
        ));
    }

    #[test]
    fn test_import_validation_missing_item() {
        let types_spec = parse(
            r#"
            module types

            type UserId { value: Int }
            "#,
        )
        .unwrap();

        let users_spec = parse(
            r#"
            module users

            use types::{UserId, NonExistent}

            type User { id: UserId }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_specs(&[&types_spec, &users_spec]);
        let errors = registry.validate_imports();

        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0],
            ImportError::ItemNotFound { item_name, .. }
            if item_name == "NonExistent"
        ));
    }

    #[test]
    fn test_import_with_alias() {
        let types_spec = parse(
            r#"
            module types

            type Identifier { value: String }
            "#,
        )
        .unwrap();

        let users_spec = parse(
            r#"
            module users

            use types::{Identifier as Id}

            type User { id: Id }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_specs(&[&types_spec, &users_spec]);

        // Id should be available (aliased from Identifier)
        assert!(registry.is_type_available_in(Some("users"), "Id"));

        // Identifier should NOT be directly available (it's aliased)
        assert!(!registry.is_type_available_in(Some("users"), "Identifier"));

        // No import errors
        let errors = registry.validate_imports();
        assert!(errors.is_empty());
    }

    #[test]
    fn test_import_with_dotted_path() {
        // Test imports with dotted module paths like `use abc.catalog::{Item}`
        let types_spec = parse(
            r#"
            module abc.catalog

            type Item { id: Int }
            type Category { name: String }
            "#,
        )
        .unwrap();

        let users_spec = parse(
            r#"
            module users

            use abc.catalog::{Item, Category}

            type Cart {
                items: Set<Item>,
            }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_specs(&[&types_spec, &users_spec]);

        // Check the catalog module with dotted name
        assert!(registry.get_module("abc.catalog").is_some());
        let catalog_info = registry.get_module("abc.catalog").unwrap();
        assert!(catalog_info.types.contains("Item"));
        assert!(catalog_info.types.contains("Category"));

        // Check imported types are available in users module
        assert!(registry.is_type_available_in(Some("users"), "Item"));
        assert!(registry.is_type_available_in(Some("users"), "Category"));

        // No import errors
        let errors = registry.validate_imports();
        assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
    }
}
