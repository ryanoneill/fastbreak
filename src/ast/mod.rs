//! Abstract Syntax Tree for Fastbreak specifications
//!
//! This module defines the AST types that represent parsed Fastbreak specifications.
//! The AST is designed to closely mirror the source syntax while being easy to
//! analyze and transform.

mod attribute;
mod expr;
mod property;
mod quality;
mod scenario;
mod state;
mod types;

pub use attribute::{Attribute, AttributeArg};
pub use expr::{
    BinaryOp, Expr, ExprKind, FieldInit, FieldPattern, LambdaParam, Literal, MatchArm, Pattern,
    PatternKind, QuantBinding, UnaryOp,
};
pub use property::{Property, TemporalOp};
pub use quality::{
    AppliesTo, AppliesToKind, DurationUnit, Quality, QualityCategory, QualityOp, QualityProperty,
    QualityPropertyValue, QualityTarget, QualityValue, RateUnit, SizeUnit,
};
pub use scenario::{Alternative, Assertion, Binding, GivenClause, Scenario, ThenClause, WhenClause};
pub use state::{Action, ActionParam, Contract, ContractKind, Invariant, StateBlock, StateField};
pub use types::{
    BuiltInType, EnumDef, EnumVariant, Field, GenericArg, Import, ImportItem, Module, Relation,
    RelationConstraint, TypeAlias, TypeDef, TypeRef, TypeRefKind,
};

use crate::Span;
use smol_str::SmolStr;

/// An identifier with source location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    /// The identifier name
    pub name: SmolStr,
    /// Source location
    pub span: Span,
}

impl Ident {
    /// Create a new identifier
    #[must_use]
    pub fn new(name: impl Into<SmolStr>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }

    /// Get the name as a string slice
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// A qualified path like `common::types::Email`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    /// Path segments
    pub segments: Vec<Ident>,
    /// Full span of the path
    pub span: Span,
}

impl Path {
    /// Create a new path
    #[must_use]
    pub fn new(segments: Vec<Ident>, span: Span) -> Self {
        Self { segments, span }
    }

    /// Create a simple single-segment path
    #[must_use]
    pub fn simple(ident: Ident) -> Self {
        let span = ident.span;
        Self {
            segments: vec![ident],
            span,
        }
    }

    /// Check if this is a simple single-segment path
    #[must_use]
    pub fn is_simple(&self) -> bool {
        self.segments.len() == 1
    }

    /// Get the last segment (the "name" of the path)
    #[must_use]
    pub fn name(&self) -> Option<&Ident> {
        self.segments.last()
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for segment in &self.segments {
            if !first {
                write!(f, "::")?;
            }
            write!(f, "{segment}")?;
            first = false;
        }
        Ok(())
    }
}

/// A complete Fastbreak specification
#[derive(Debug, Clone, PartialEq)]
pub struct Specification {
    /// Module declaration (optional for single-file specs)
    pub module: Option<Module>,
    /// Import statements
    pub imports: Vec<Import>,
    /// Type definitions (structs with fields)
    pub types: Vec<TypeDef>,
    /// Type aliases (e.g., `type PositiveInt = Int where self > 0`)
    pub type_aliases: Vec<TypeAlias>,
    /// Enum definitions
    pub enums: Vec<EnumDef>,
    /// Relation definitions
    pub relations: Vec<Relation>,
    /// State definitions
    pub states: Vec<StateBlock>,
    /// Action definitions
    pub actions: Vec<Action>,
    /// Scenario definitions
    pub scenarios: Vec<Scenario>,
    /// Property definitions
    pub properties: Vec<Property>,
    /// Quality requirements (NFRs)
    pub qualities: Vec<Quality>,
}

impl Default for Specification {
    fn default() -> Self {
        Self::new()
    }
}

impl Specification {
    /// Create an empty specification
    #[must_use]
    pub fn new() -> Self {
        Self {
            module: None,
            imports: Vec::new(),
            types: Vec::new(),
            type_aliases: Vec::new(),
            enums: Vec::new(),
            relations: Vec::new(),
            states: Vec::new(),
            actions: Vec::new(),
            scenarios: Vec::new(),
            properties: Vec::new(),
            qualities: Vec::new(),
        }
    }

    /// Check if the specification is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
            && self.type_aliases.is_empty()
            && self.enums.is_empty()
            && self.relations.is_empty()
            && self.states.is_empty()
            && self.actions.is_empty()
            && self.scenarios.is_empty()
            && self.properties.is_empty()
            && self.qualities.is_empty()
    }
}
