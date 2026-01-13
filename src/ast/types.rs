//! Type-related AST nodes

use super::{Attribute, Expr, Ident, Path};
use crate::Span;
use smol_str::SmolStr;

/// Module declaration: `module auth` or `module abc.def.ghi`
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// Module path segments (supports dotted names like "abc.def.ghi")
    pub path: Path,
    /// Span of the entire declaration
    pub span: Span,
}

impl Module {
    /// Get the full module name as a dotted string
    #[must_use]
    pub fn name(&self) -> SmolStr {
        SmolStr::new(
            self.path
                .segments
                .iter()
                .map(Ident::as_str)
                .collect::<Vec<_>>()
                .join("."),
        )
    }
}

/// Import statement: `use common::types::{Email, UserId}`
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    /// Base path
    pub path: Path,
    /// Specific items to import (if any)
    pub items: Option<Vec<ImportItem>>,
    /// Span of the entire import
    pub span: Span,
}

/// A single item in an import list
#[derive(Debug, Clone, PartialEq)]
pub struct ImportItem {
    /// The imported name
    pub name: Ident,
    /// Optional alias: `Email as E`
    pub alias: Option<Ident>,
}

/// Type definition: `type User { id: UserId, email: Email } where ...`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    /// Attributes (e.g., `@id(TYPE-001)`)
    pub attributes: Vec<Attribute>,
    /// Type name
    pub name: Ident,
    /// Type parameters
    pub type_params: Vec<Ident>,
    /// Fields
    pub fields: Vec<Field>,
    /// Refinement predicate (optional): `where self.age >= 0`
    pub refinement: Option<Expr>,
    /// Span of the entire definition
    pub span: Span,
}

/// Type alias with optional refinement: `type Email = String where ...`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    /// Attributes (e.g., `@id(ALIAS-001)`)
    pub attributes: Vec<Attribute>,
    /// Alias name
    pub name: Ident,
    /// Type parameters (for generic aliases)
    pub type_params: Vec<Ident>,
    /// The underlying type
    pub base: TypeRef,
    /// Refinement predicate (optional): `where self > 0`
    pub refinement: Option<Expr>,
    /// Span of the entire definition
    pub span: Span,
}

/// A field in a type definition
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    /// Field name
    pub name: Ident,
    /// Field type
    pub ty: TypeRef,
    /// Span of the field
    pub span: Span,
}

/// Enum definition: `enum UserStatus { Active, Suspended }`
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    /// Attributes (e.g., `@id(ENUM-001)`)
    pub attributes: Vec<Attribute>,
    /// Enum name
    pub name: Ident,
    /// Type parameters
    pub type_params: Vec<Ident>,
    /// Variants
    pub variants: Vec<EnumVariant>,
    /// Span of the entire definition
    pub span: Span,
}

/// An enum variant
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    /// Variant name
    pub name: Ident,
    /// Associated data (if any)
    pub fields: Vec<Field>,
    /// Span of the variant
    pub span: Span,
}

/// A type reference like `User`, `Set<User>`, or `Map<String, Int>`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeRef {
    /// The kind of type reference
    pub kind: TypeRefKind,
    /// Span of the type reference
    pub span: Span,
}

/// Kind of type reference
#[derive(Debug, Clone, PartialEq)]
pub enum TypeRefKind {
    /// A simple type name or path
    Named(Path),
    /// A built-in type
    BuiltIn(BuiltInType),
    /// A generic type with arguments
    Generic {
        /// Base type
        base: Box<TypeRef>,
        /// Type arguments
        args: Vec<GenericArg>,
    },
    /// A function type: `(A, B) -> C`
    Function {
        /// Parameter types
        params: Vec<TypeRef>,
        /// Return type
        ret: Box<TypeRef>,
    },
    /// A tuple type: `(A, B)`
    Tuple(Vec<TypeRef>),
    /// Unit type: `()`
    Unit,
}

/// Built-in types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltInType {
    /// `Int`
    Int,
    /// `String`
    String,
    /// `Bool`
    Bool,
    /// `Set<T>`
    Set,
    /// `Map<K, V>`
    Map,
    /// `List<T>`
    List,
    /// `Option<T>`
    Option,
    /// `Result<T, E>`
    Result,
}

impl std::fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltInType::Int => write!(f, "Int"),
            BuiltInType::String => write!(f, "String"),
            BuiltInType::Bool => write!(f, "Bool"),
            BuiltInType::Set => write!(f, "Set"),
            BuiltInType::Map => write!(f, "Map"),
            BuiltInType::List => write!(f, "List"),
            BuiltInType::Option => write!(f, "Option"),
            BuiltInType::Result => write!(f, "Result"),
        }
    }
}

/// A generic type argument
#[derive(Debug, Clone, PartialEq)]
pub struct GenericArg {
    /// The type argument
    pub ty: TypeRef,
}

/// Relation definition: `relation friends: User -> Set<User> { symmetric }`
#[derive(Debug, Clone, PartialEq)]
pub struct Relation {
    /// Attributes (e.g., `@id(REL-001)`)
    pub attributes: Vec<Attribute>,
    /// Relation name
    pub name: Ident,
    /// Source type
    pub source: TypeRef,
    /// Target type
    pub target: TypeRef,
    /// Constraints (symmetric, transitive, etc.)
    pub constraints: Vec<RelationConstraint>,
    /// Span of the entire definition
    pub span: Span,
}

/// Relation constraint
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RelationConstraint {
    /// The relation is symmetric: a R b implies b R a
    Symmetric,
    /// The relation is reflexive: a R a for all a
    Reflexive,
    /// The relation is irreflexive: not (a R a) for all a
    Irreflexive,
    /// The relation is transitive: a R b and b R c implies a R c
    Transitive,
    /// The relation is antisymmetric: a R b and b R a implies a = b
    Antisymmetric,
}

impl std::fmt::Display for RelationConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelationConstraint::Symmetric => write!(f, "symmetric"),
            RelationConstraint::Reflexive => write!(f, "reflexive"),
            RelationConstraint::Irreflexive => write!(f, "irreflexive"),
            RelationConstraint::Transitive => write!(f, "transitive"),
            RelationConstraint::Antisymmetric => write!(f, "antisymmetric"),
        }
    }
}
