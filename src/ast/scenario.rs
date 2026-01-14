//! Scenario AST nodes (Cucumber-inspired)

use super::{Attribute, Expr, Path};
use crate::Span;
use smol_str::SmolStr;

/// A scenario definition
///
/// ```fbrk
/// scenario "New user registration" {
///     given {
///         users = {}
///         sessions = {}
///     }
///     when {
///         result = register("test@example.com")
///     }
///     then {
///         result is Ok
///         users.len() == 1
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Scenario {
    /// Attributes (e.g., `@id(SCENARIO-001)`)
    pub attributes: Vec<Attribute>,
    /// Scenario description
    pub description: SmolStr,
    /// Given clause (initial state setup)
    pub given: GivenClause,
    /// When clause (action to perform)
    pub when: WhenClause,
    /// Then clause (expected outcomes)
    pub then: ThenClause,
    /// Alternative flows (error cases, extensions)
    pub alternatives: Vec<Alternative>,
    /// Span of the entire scenario
    pub span: Span,
}

/// An alternative flow within a scenario
///
/// Alternative flows describe error cases, exceptions, or optional behavior
/// that diverge from the main (happy) path.
///
/// ```fbrk
/// alt "email already exists" when { exists u in users where u.email == email } {
///     then {
///         result is Err(DuplicateEmail)
///         users' == users
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Alternative {
    /// Attributes (e.g., `@id(ALT-001)`)
    pub attributes: Vec<Attribute>,
    /// Alternative name/description
    pub name: SmolStr,
    /// Condition that triggers this alternative (optional)
    pub condition: Option<Expr>,
    /// Additional given bindings (optional, extends base)
    pub given: Option<GivenClause>,
    /// Different when clause (optional, replaces base)
    pub when: Option<WhenClause>,
    /// Expected outcome (required)
    pub then: ThenClause,
    /// Span of the alternative
    pub span: Span,
}

/// Given clause - establishes initial state
///
/// ```fbrk
/// given {
///     users = {}
///     sessions = {}
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct GivenClause {
    /// Variable bindings
    pub bindings: Vec<Binding>,
    /// Span of the clause
    pub span: Span,
}

/// When clause - the action to perform
///
/// ```fbrk
/// when {
///     result = register("test@example.com")
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct WhenClause {
    /// Variable bindings (capturing results)
    pub bindings: Vec<Binding>,
    /// Span of the clause
    pub span: Span,
}

/// Then clause - assertions about the outcome
///
/// ```fbrk
/// then {
///     result is Ok
///     users.len() == 1
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct ThenClause {
    /// Assertions to check
    pub assertions: Vec<Assertion>,
    /// Span of the clause
    pub span: Span,
}

/// A variable binding: `name = expr` or `path.to.field = expr`
#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    /// Variable name (supports dotted paths like `catalog.entries`)
    pub name: Path,
    /// Value expression
    pub value: Expr,
    /// Span of the binding
    pub span: Span,
}

/// An assertion in the then clause
#[derive(Debug, Clone, PartialEq)]
pub struct Assertion {
    /// The assertion expression (must evaluate to Bool)
    pub expr: Expr,
    /// Optional description
    pub description: Option<SmolStr>,
    /// Span of the assertion
    pub span: Span,
}
