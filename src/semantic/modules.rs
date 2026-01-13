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

impl ModuleRegistry {
    /// Create a new empty module registry
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a registry from a specification
    #[must_use]
    pub fn from_spec(spec: &Specification) -> Self {
        let mut registry = Self::new();

        // Determine the module name
        let module_name = spec
            .module
            .as_ref()
            .map_or_else(|| SmolStr::new("default"), Module::name);

        registry.current_module = Some(module_name.clone());

        // Create module info
        let mut info = ModuleInfo {
            name: module_name.clone(),
            ..Default::default()
        };

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

        registry.modules.insert(module_name, info);
        registry
    }

    /// Resolve an import statement to a `ResolvedImport`
    fn resolve_import(import: &Import) -> ResolvedImport {
        let source_module = import
            .path
            .segments
            .iter()
            .map(|s| s.name.as_str())
            .collect::<Vec<_>>()
            .join("::");

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
        if let Some(info) = self.current_module_info() {
            // Check local definitions
            if info.types.contains(name) {
                return true;
            }

            // Check imports
            for import in &info.imports {
                // Wildcard import - would need to check source module
                if import.items.is_empty() {
                    // For now, assume it might be available
                    return true;
                }

                // Named import
                if import.items.iter().any(|item| item.local_name == name) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if an enum is defined in the current module or imported
    #[must_use]
    pub fn is_enum_available(&self, name: &str) -> bool {
        if let Some(info) = self.current_module_info() {
            if info.enums.contains(name) {
                return true;
            }

            for import in &info.imports {
                if import.items.is_empty() {
                    return true;
                }
                if import.items.iter().any(|item| item.local_name == name) {
                    return true;
                }
            }
        }
        false
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

            use common::types::Email
            use common::types::UserId
            use auth::User

            type Profile {
                id: Int,
            }
            "#,
        )
        .unwrap();

        let registry = ModuleRegistry::from_spec(&spec);

        let info = registry.get_module("main").unwrap();
        assert_eq!(info.imports.len(), 3);

        // Check imports - they're full paths now
        assert_eq!(info.imports[0].source_module.as_str(), "common::types::Email");
        assert_eq!(info.imports[1].source_module.as_str(), "common::types::UserId");
        assert_eq!(info.imports[2].source_module.as_str(), "auth::User");
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
}
