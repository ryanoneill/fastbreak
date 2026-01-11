//! Semantic type representation
//!
//! These types represent resolved types after name resolution,
//! as opposed to AST `TypeRef` which is the syntactic representation.

use indexmap::IndexMap;
use smol_str::SmolStr;
use std::fmt;
use std::sync::Arc;

/// A resolved semantic type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Primitive integer type
    Int,
    /// Primitive string type
    String,
    /// Primitive boolean type
    Bool,
    /// Unit type (empty tuple)
    Unit,

    /// A user-defined type (struct)
    Struct(TypeId),
    /// A user-defined enum
    Enum(TypeId),

    /// Set type
    Set(Arc<Type>),
    /// List type
    List(Arc<Type>),
    /// Map type
    Map(Arc<Type>, Arc<Type>),
    /// Option type
    Option(Arc<Type>),
    /// Result type
    Result(Arc<Type>, Arc<Type>),

    /// Tuple type
    Tuple(Vec<Type>),

    /// Function type
    Function {
        /// Parameter types
        params: Vec<Type>,
        /// Return type
        ret: Arc<Type>,
    },

    /// A type variable (for generics)
    Var(TypeVarId),

    /// Unknown type (used during inference)
    Unknown,

    /// Error type (used when type checking fails)
    Error,
}

impl Type {
    /// Check if this is a primitive type
    #[must_use]
    pub fn is_primitive(&self) -> bool {
        matches!(self, Type::Int | Type::String | Type::Bool | Type::Unit)
    }

    /// Check if this is a collection type
    #[must_use]
    pub fn is_collection(&self) -> bool {
        matches!(self, Type::Set(_) | Type::List(_) | Type::Map(_, _))
    }

    /// Check if this is an error type
    #[must_use]
    pub fn is_error(&self) -> bool {
        matches!(self, Type::Error)
    }

    /// Get the element type if this is a collection
    #[must_use]
    pub fn element_type(&self) -> Option<&Type> {
        match self {
            Type::Set(t) | Type::List(t) | Type::Option(t) => Some(t),
            _ => None,
        }
    }

    /// Check if this type can be compared for equality
    #[must_use]
    pub fn is_comparable(&self) -> bool {
        match self {
            Type::Int
            | Type::String
            | Type::Bool
            | Type::Unit
            | Type::Struct(_)
            | Type::Enum(_)
            | Type::Var(_)
            | Type::Unknown
            | Type::Error => true,
            Type::Tuple(types) => types.iter().all(Type::is_comparable),
            Type::Option(t) | Type::Set(t) | Type::List(t) => t.is_comparable(),
            Type::Result(ok, err) => ok.is_comparable() && err.is_comparable(),
            Type::Map(k, v) => k.is_comparable() && v.is_comparable(),
            Type::Function { .. } => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::String => write!(f, "String"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "()"),
            Type::Struct(id) | Type::Enum(id) => write!(f, "{}", id.name),
            Type::Set(t) => write!(f, "Set<{t}>"),
            Type::List(t) => write!(f, "List<{t}>"),
            Type::Map(k, v) => write!(f, "Map<{k}, {v}>"),
            Type::Option(t) => write!(f, "Option<{t}>"),
            Type::Result(ok, err) => write!(f, "Result<{ok}, {err}>"),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                }
                write!(f, ")")
            }
            Type::Function { params, ret } => {
                write!(f, "(")?;
                for (i, t) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                }
                write!(f, ") -> {ret}")
            }
            Type::Var(id) => write!(f, "'{}", id.0),
            Type::Unknown => write!(f, "?"),
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Unique identifier for a user-defined type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeId {
    /// The type name
    pub name: SmolStr,
    /// Unique index in the type registry
    pub index: usize,
}

impl TypeId {
    /// Create a new type ID
    #[must_use]
    pub fn new(name: impl Into<SmolStr>, index: usize) -> Self {
        Self {
            name: name.into(),
            index,
        }
    }
}

/// Unique identifier for a type variable
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub usize);

/// Information about a struct type
#[derive(Debug, Clone)]
pub struct StructInfo {
    /// Type ID
    pub id: TypeId,
    /// Type parameters
    pub type_params: Vec<SmolStr>,
    /// Fields (name -> type)
    pub fields: IndexMap<SmolStr, Type>,
}

impl StructInfo {
    /// Create new struct info
    #[must_use]
    pub fn new(id: TypeId) -> Self {
        Self {
            id,
            type_params: Vec::new(),
            fields: IndexMap::new(),
        }
    }

    /// Get a field type by name
    #[must_use]
    pub fn field(&self, name: &str) -> Option<&Type> {
        self.fields.get(name)
    }
}

/// Information about an enum type
#[derive(Debug, Clone)]
pub struct EnumInfo {
    /// Type ID
    pub id: TypeId,
    /// Type parameters
    pub type_params: Vec<SmolStr>,
    /// Variants (name -> variant info)
    pub variants: IndexMap<SmolStr, VariantInfo>,
}

impl EnumInfo {
    /// Create new enum info
    #[must_use]
    pub fn new(id: TypeId) -> Self {
        Self {
            id,
            type_params: Vec::new(),
            variants: IndexMap::new(),
        }
    }

    /// Get a variant by name
    #[must_use]
    pub fn variant(&self, name: &str) -> Option<&VariantInfo> {
        self.variants.get(name)
    }
}

/// Information about an enum variant
#[derive(Debug, Clone)]
pub struct VariantInfo {
    /// Variant name
    pub name: SmolStr,
    /// Fields (for struct variants) or positional types (for tuple variants)
    pub fields: IndexMap<SmolStr, Type>,
    /// Whether this is a tuple variant (positional fields)
    pub is_tuple: bool,
}

impl VariantInfo {
    /// Create a unit variant (no data)
    #[must_use]
    pub fn unit(name: impl Into<SmolStr>) -> Self {
        Self {
            name: name.into(),
            fields: IndexMap::new(),
            is_tuple: false,
        }
    }

    /// Create a tuple variant
    #[must_use]
    pub fn tuple(name: impl Into<SmolStr>, types: Vec<Type>) -> Self {
        let mut fields = IndexMap::new();
        for (i, ty) in types.into_iter().enumerate() {
            fields.insert(SmolStr::new(i.to_string()), ty);
        }
        Self {
            name: name.into(),
            fields,
            is_tuple: true,
        }
    }

    /// Check if this is a unit variant
    #[must_use]
    pub fn is_unit(&self) -> bool {
        self.fields.is_empty()
    }
}

/// Information about a state definition
#[derive(Debug, Clone)]
pub struct StateInfo {
    /// State name
    pub name: SmolStr,
    /// Fields (name -> type)
    pub fields: IndexMap<SmolStr, Type>,
}

impl StateInfo {
    /// Create new state info
    #[must_use]
    pub fn new(name: impl Into<SmolStr>) -> Self {
        Self {
            name: name.into(),
            fields: IndexMap::new(),
        }
    }
}

/// Information about an action
#[derive(Debug, Clone)]
pub struct ActionInfo {
    /// Action name
    pub name: SmolStr,
    /// Parameter types (name -> type)
    pub params: IndexMap<SmolStr, Type>,
    /// Return type
    pub return_type: Type,
}

impl ActionInfo {
    /// Create new action info
    #[must_use]
    pub fn new(name: impl Into<SmolStr>, return_type: Type) -> Self {
        Self {
            name: name.into(),
            params: IndexMap::new(),
            return_type,
        }
    }
}

/// Information about a relation
#[derive(Debug, Clone)]
pub struct RelationInfo {
    /// Relation name
    pub name: SmolStr,
    /// Source type
    pub source: Type,
    /// Target type
    pub target: Type,
}

impl RelationInfo {
    /// Create new relation info
    #[must_use]
    pub fn new(name: impl Into<SmolStr>, source: Type, target: Type) -> Self {
        Self {
            name: name.into(),
            source,
            target,
        }
    }
}
