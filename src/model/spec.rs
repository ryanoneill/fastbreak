//! Compiled specification model
//!
//! This module defines the fully resolved specification model that is ready
//! for code generation, documentation, and verification.

use crate::ast::{Attribute, AttributeArg};
use crate::semantic::{ActionInfo, EnumInfo, RelationInfo, StateInfo, StructInfo, Type};
use crate::Span;
use indexmap::IndexMap;
use smol_str::SmolStr;
use std::sync::Arc;

/// A compiled attribute
#[derive(Debug, Clone)]
pub struct CompiledAttribute {
    /// Attribute name
    pub name: SmolStr,
    /// Attribute arguments
    pub args: Vec<CompiledAttributeArg>,
}

/// A compiled attribute argument
#[derive(Debug, Clone)]
pub enum CompiledAttributeArg {
    /// String value
    String(SmolStr),
    /// Identifier value
    Ident(SmolStr),
    /// Integer value
    Int(i64),
}

impl CompiledAttribute {
    /// Create from AST `Attribute`
    #[must_use]
    pub fn from_ast(attr: &Attribute) -> Self {
        Self {
            name: attr.name.name.clone(),
            args: attr.args.iter().map(CompiledAttributeArg::from_ast).collect(),
        }
    }

    /// Get the first argument as a string if it exists
    #[must_use]
    pub fn first_arg_string(&self) -> Option<&str> {
        self.args.first().and_then(|arg| match arg {
            CompiledAttributeArg::String(s) | CompiledAttributeArg::Ident(s) => Some(s.as_str()),
            CompiledAttributeArg::Int(_) => None,
        })
    }
}

impl CompiledAttributeArg {
    /// Create from AST `AttributeArg`
    #[must_use]
    pub fn from_ast(arg: &AttributeArg) -> Self {
        match arg {
            AttributeArg::String(s, _) => CompiledAttributeArg::String(s.clone()),
            AttributeArg::Ident(ident) => CompiledAttributeArg::Ident(ident.name.clone()),
            AttributeArg::Int(n, _) => CompiledAttributeArg::Int(*n),
        }
    }
}

impl std::fmt::Display for CompiledAttributeArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompiledAttributeArg::String(s) => write!(f, "\"{s}\""),
            CompiledAttributeArg::Ident(s) => write!(f, "{s}"),
            CompiledAttributeArg::Int(n) => write!(f, "{n}"),
        }
    }
}

/// A fully compiled and resolved specification
#[derive(Debug, Clone)]
pub struct CompiledSpec {
    /// Module name (if any)
    pub module: Option<SmolStr>,
    /// Imports from other modules
    pub imports: Vec<Import>,
    /// User-defined struct types
    pub structs: IndexMap<SmolStr, CompiledStruct>,
    /// User-defined enum types
    pub enums: IndexMap<SmolStr, CompiledEnum>,
    /// State definitions
    pub states: IndexMap<SmolStr, CompiledState>,
    /// Action definitions
    pub actions: IndexMap<SmolStr, CompiledAction>,
    /// Relation definitions
    pub relations: IndexMap<SmolStr, CompiledRelation>,
    /// Scenario definitions
    pub scenarios: Vec<CompiledScenario>,
    /// Property definitions
    pub properties: Vec<CompiledProperty>,
}

impl CompiledSpec {
    /// Create a new empty compiled specification
    #[must_use]
    pub fn new() -> Self {
        Self {
            module: None,
            imports: Vec::new(),
            structs: IndexMap::new(),
            enums: IndexMap::new(),
            states: IndexMap::new(),
            actions: IndexMap::new(),
            relations: IndexMap::new(),
            scenarios: Vec::new(),
            properties: Vec::new(),
        }
    }

    /// Check if the specification is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.structs.is_empty()
            && self.enums.is_empty()
            && self.states.is_empty()
            && self.actions.is_empty()
            && self.relations.is_empty()
            && self.scenarios.is_empty()
            && self.properties.is_empty()
    }

    /// Get the total number of definitions
    #[must_use]
    pub fn definition_count(&self) -> usize {
        self.structs.len()
            + self.enums.len()
            + self.states.len()
            + self.actions.len()
            + self.relations.len()
    }
}

impl Default for CompiledSpec {
    fn default() -> Self {
        Self::new()
    }
}

/// An import statement
#[derive(Debug, Clone)]
pub struct Import {
    /// The module path being imported
    pub path: Vec<SmolStr>,
    /// Specific items being imported (empty means import all)
    pub items: Vec<SmolStr>,
    /// Source span
    pub span: Span,
}

/// A compiled struct type
#[derive(Debug, Clone)]
pub struct CompiledStruct {
    /// Struct name
    pub name: SmolStr,
    /// Type parameters
    pub type_params: Vec<SmolStr>,
    /// Fields with their resolved types
    pub fields: IndexMap<SmolStr, Type>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

impl CompiledStruct {
    /// Create from semantic `StructInfo`
    #[must_use]
    pub fn from_info(info: &StructInfo, attributes: Vec<CompiledAttribute>, span: Span) -> Self {
        Self {
            name: info.id.name.clone(),
            type_params: info.type_params.clone(),
            fields: info.fields.clone(),
            attributes,
            doc: None,
            span,
        }
    }
}

/// A compiled enum type
#[derive(Debug, Clone)]
pub struct CompiledEnum {
    /// Enum name
    pub name: SmolStr,
    /// Type parameters
    pub type_params: Vec<SmolStr>,
    /// Variants
    pub variants: IndexMap<SmolStr, CompiledVariant>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

impl CompiledEnum {
    /// Create from semantic `EnumInfo`
    #[must_use]
    pub fn from_info(info: &EnumInfo, attributes: Vec<CompiledAttribute>, span: Span) -> Self {
        let variants = info
            .variants
            .iter()
            .map(|(name, v)| {
                (
                    name.clone(),
                    CompiledVariant {
                        name: v.name.clone(),
                        fields: v.fields.clone(),
                        is_tuple: v.is_tuple,
                    },
                )
            })
            .collect();

        Self {
            name: info.id.name.clone(),
            type_params: info.type_params.clone(),
            variants,
            attributes,
            doc: None,
            span,
        }
    }
}

/// A compiled enum variant
#[derive(Debug, Clone)]
pub struct CompiledVariant {
    /// Variant name
    pub name: SmolStr,
    /// Fields (named or positional)
    pub fields: IndexMap<SmolStr, Type>,
    /// Whether this is a tuple variant
    pub is_tuple: bool,
}

impl CompiledVariant {
    /// Check if this is a unit variant (no fields)
    #[must_use]
    pub fn is_unit(&self) -> bool {
        self.fields.is_empty()
    }
}

/// A compiled state definition
#[derive(Debug, Clone)]
pub struct CompiledState {
    /// State name
    pub name: SmolStr,
    /// State fields with their resolved types
    pub fields: IndexMap<SmolStr, Type>,
    /// State invariants
    pub invariants: Vec<CompiledInvariant>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

impl CompiledState {
    /// Create from semantic `StateInfo`
    #[must_use]
    pub fn from_info(info: &StateInfo, attributes: Vec<CompiledAttribute>, span: Span) -> Self {
        Self {
            name: info.name.clone(),
            fields: info.fields.clone(),
            invariants: Vec::new(),
            attributes,
            doc: None,
            span,
        }
    }
}

/// A compiled invariant
#[derive(Debug, Clone)]
pub struct CompiledInvariant {
    /// Invariant name/description
    pub name: Option<String>,
    /// The invariant expression (stored as AST for now)
    pub expr: Arc<crate::ast::Expr>,
    /// Source span
    pub span: Span,
}

/// A compiled action definition
#[derive(Debug, Clone)]
pub struct CompiledAction {
    /// Action name
    pub name: SmolStr,
    /// Parameters with their types
    pub params: IndexMap<SmolStr, Type>,
    /// Return type
    pub return_type: Type,
    /// Preconditions (requires clauses)
    pub requires: Vec<CompiledContract>,
    /// Postconditions (ensures clauses)
    pub ensures: Vec<CompiledContract>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

impl CompiledAction {
    /// Create from semantic `ActionInfo`
    #[must_use]
    pub fn from_info(info: &ActionInfo, attributes: Vec<CompiledAttribute>, span: Span) -> Self {
        Self {
            name: info.name.clone(),
            params: info.params.clone(),
            return_type: info.return_type.clone(),
            requires: Vec::new(),
            ensures: Vec::new(),
            attributes,
            doc: None,
            span,
        }
    }
}

/// A compiled contract (requires/ensures clause)
#[derive(Debug, Clone)]
pub struct CompiledContract {
    /// The contract expression
    pub expr: Arc<crate::ast::Expr>,
    /// Source span
    pub span: Span,
}

/// A compiled relation definition
#[derive(Debug, Clone)]
pub struct CompiledRelation {
    /// Relation name
    pub name: SmolStr,
    /// Source type
    pub source: Type,
    /// Target type
    pub target: Type,
    /// Relation properties
    pub properties: Vec<RelationProperty>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

impl CompiledRelation {
    /// Create from semantic `RelationInfo`
    #[must_use]
    pub fn from_info(info: &RelationInfo, attributes: Vec<CompiledAttribute>, span: Span) -> Self {
        Self {
            name: info.name.clone(),
            source: info.source.clone(),
            target: info.target.clone(),
            properties: Vec::new(),
            attributes,
            doc: None,
            span,
        }
    }
}

/// A relation property constraint
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelationProperty {
    /// The relation is symmetric (a->b implies b->a)
    Symmetric,
    /// The relation is reflexive (a->a for all a)
    Reflexive,
    /// The relation is irreflexive (never a->a)
    Irreflexive,
    /// The relation is transitive (a->b and b->c implies a->c)
    Transitive,
    /// The relation is antisymmetric (a->b and b->a implies a=b)
    Antisymmetric,
}

/// A compiled scenario
#[derive(Debug, Clone)]
pub struct CompiledScenario {
    /// Scenario name/description
    pub name: String,
    /// Given clauses (initial state setup)
    pub given: Vec<CompiledGiven>,
    /// When clauses (actions to perform)
    pub when: Vec<CompiledWhen>,
    /// Then clauses (expected outcomes)
    pub then: Vec<CompiledThen>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

/// A compiled given clause
#[derive(Debug, Clone)]
pub struct CompiledGiven {
    /// Assignments in the given clause
    pub assignments: Vec<CompiledAssignment>,
    /// Source span
    pub span: Span,
}

/// A compiled when clause
#[derive(Debug, Clone)]
pub struct CompiledWhen {
    /// Actions/expressions in the when clause
    pub actions: Vec<CompiledWhenAction>,
    /// Source span
    pub span: Span,
}

/// An action in a when clause
#[derive(Debug, Clone)]
pub struct CompiledWhenAction {
    /// Optional result binding
    pub binding: Option<SmolStr>,
    /// The action expression
    pub expr: Arc<crate::ast::Expr>,
    /// Source span
    pub span: Span,
}

/// A compiled then clause
#[derive(Debug, Clone)]
pub struct CompiledThen {
    /// Assertions in the then clause
    pub assertions: Vec<CompiledAssertion>,
    /// Source span
    pub span: Span,
}

/// An assignment in a given clause
#[derive(Debug, Clone)]
pub struct CompiledAssignment {
    /// The variable being assigned
    pub name: SmolStr,
    /// The value expression
    pub value: Arc<crate::ast::Expr>,
    /// Source span
    pub span: Span,
}

/// An assertion in a then clause
#[derive(Debug, Clone)]
pub struct CompiledAssertion {
    /// The assertion expression
    pub expr: Arc<crate::ast::Expr>,
    /// Source span
    pub span: Span,
}

/// A compiled property
#[derive(Debug, Clone)]
pub struct CompiledProperty {
    /// Property name/description
    pub name: String,
    /// The property expression
    pub expr: Arc<crate::ast::Expr>,
    /// Temporal operator (if any)
    pub temporal: Option<TemporalOp>,
    /// Attributes
    pub attributes: Vec<CompiledAttribute>,
    /// Documentation comment
    pub doc: Option<String>,
    /// Source span
    pub span: Span,
}

/// Temporal operators for properties
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TemporalOp {
    /// The property always holds
    Always,
    /// The property eventually holds
    Eventually,
    /// The property holds until another property holds
    Until,
    /// The property holds in the next state
    Next,
}
