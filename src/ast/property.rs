//! Property AST nodes (for future model checking)

use super::{Attribute, Expr};
use crate::Span;
use smol_str::SmolStr;

/// A property definition for model checking
///
/// ```fbs
/// property "Users are unique by email" {
///     always {
///         forall u1, u2 in users where u1 != u2 =>
///             u1.email != u2.email
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Property {
    /// Attributes (e.g., `@id(PROP-001)`)
    pub attributes: Vec<Attribute>,
    /// Property description
    pub description: SmolStr,
    /// Temporal operator (if any)
    pub temporal_op: Option<TemporalOp>,
    /// The property expression
    pub expr: Expr,
    /// Span of the entire property
    pub span: Span,
}

/// Temporal operators for property specifications
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TemporalOp {
    /// `always` - The property holds in all states
    Always,
    /// `eventually` - The property holds in some future state
    Eventually,
}

impl std::fmt::Display for TemporalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TemporalOp::Always => write!(f, "always"),
            TemporalOp::Eventually => write!(f, "eventually"),
        }
    }
}
