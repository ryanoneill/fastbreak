//! State and action AST nodes

use super::{Attribute, Expr, Ident, TypeRef};
use crate::Span;
use smol_str::SmolStr;

/// State definition block
///
/// ```fbrk
/// state AuthSystem {
///     users: Set<User>,
///     sessions: Map<SessionId, UserId>,
///
///     invariant "Sessions reference existing users" {
///         forall sid in sessions.keys() =>
///             sessions[sid] in users.map(u => u.id)
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct StateBlock {
    /// Attributes (e.g., `@id(STATE-001)`)
    pub attributes: Vec<Attribute>,
    /// State name
    pub name: Ident,
    /// State fields
    pub fields: Vec<StateField>,
    /// Invariants
    pub invariants: Vec<Invariant>,
    /// Span of the entire block
    pub span: Span,
}

/// A field in a state definition
#[derive(Debug, Clone, PartialEq)]
pub struct StateField {
    /// Field name
    pub name: Ident,
    /// Field type
    pub ty: TypeRef,
    /// Optional initial value
    pub init: Option<Expr>,
    /// Span of the field
    pub span: Span,
}

/// An invariant in a state definition
///
/// ```fbrk
/// invariant "Users have unique emails" {
///     forall u1, u2 in users where u1 != u2 =>
///         u1.email != u2.email
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Invariant {
    /// Attributes (e.g., `@id(INV-001)`)
    pub attributes: Vec<Attribute>,
    /// Optional description
    pub description: Option<SmolStr>,
    /// The invariant expression (must evaluate to Bool)
    pub expr: Expr,
    /// Span of the entire invariant
    pub span: Span,
}

/// Action definition with contracts
///
/// ```fbrk
/// action register(email: Email) -> Result<User, RegisterError>
///     requires {
///         not exists u in users where u.email == email
///     }
///     ensures {
///         match result {
///             Ok(user) => user in users' and user.email == email,
///             Err(_) => users' == users,
///         }
///     }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Action {
    /// Attributes (e.g., `@id(ACTION-001)`)
    pub attributes: Vec<Attribute>,
    /// Action name
    pub name: Ident,
    /// Parameters
    pub params: Vec<ActionParam>,
    /// Return type (if any)
    pub return_type: Option<TypeRef>,
    /// Contracts (requires/ensures)
    pub contracts: Vec<Contract>,
    /// Optional body for executable actions
    pub body: Option<Expr>,
    /// Span of the entire action
    pub span: Span,
}

/// An action parameter
#[derive(Debug, Clone, PartialEq)]
pub struct ActionParam {
    /// Parameter name
    pub name: Ident,
    /// Parameter type
    pub ty: TypeRef,
    /// Span of the parameter
    pub span: Span,
}

/// A contract (precondition or postcondition)
#[derive(Debug, Clone, PartialEq)]
pub struct Contract {
    /// Kind of contract
    pub kind: ContractKind,
    /// The contract expression
    pub expr: Expr,
    /// Span of the contract
    pub span: Span,
}

/// Kind of contract
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContractKind {
    /// Precondition: `requires { ... }`
    Requires,
    /// Postcondition: `ensures { ... }`
    Ensures,
}

impl std::fmt::Display for ContractKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ContractKind::Requires => write!(f, "requires"),
            ContractKind::Ensures => write!(f, "ensures"),
        }
    }
}
