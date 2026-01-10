//! Scenario AST nodes (Cucumber-inspired)

use super::{Expr, Ident};
use crate::Span;
use smol_str::SmolStr;

/// A scenario definition
///
/// ```fbs
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
    /// Scenario description
    pub description: SmolStr,
    /// Given clause (initial state setup)
    pub given: GivenClause,
    /// When clause (action to perform)
    pub when: WhenClause,
    /// Then clause (expected outcomes)
    pub then: ThenClause,
    /// Span of the entire scenario
    pub span: Span,
}

/// Given clause - establishes initial state
///
/// ```fbs
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
/// ```fbs
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
/// ```fbs
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

/// A variable binding: `name = expr`
#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    /// Variable name
    pub name: Ident,
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
