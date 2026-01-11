//! Semantic analysis error types

#![allow(unused_assignments)] // Fields used by thiserror Display derive

use crate::Span;
use miette::Diagnostic;
use thiserror::Error;

/// Result type for semantic analysis
pub type SemanticResult<T> = Result<T, SemanticError>;

/// A semantic analysis error
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum SemanticError {
    /// Undefined name reference
    #[error("undefined {kind}: `{name}`")]
    Undefined {
        /// Kind of thing that's undefined
        kind: &'static str,
        /// The undefined name
        name: String,
        /// Location of the reference
        #[label("not found")]
        span: Span,
    },

    /// Duplicate definition
    #[error("duplicate {kind} definition: `{name}`")]
    Duplicate {
        /// Kind of definition
        kind: &'static str,
        /// The duplicated name
        name: String,
        /// Location of the duplicate
        #[label("duplicate")]
        span: Span,
        /// Location of the original
        #[label("original defined here")]
        original_span: Span,
    },

    /// Type mismatch
    #[error("type mismatch: expected `{expected}`, found `{found}`")]
    TypeMismatch {
        /// Expected type
        expected: String,
        /// Found type
        found: String,
        /// Location of the mismatch
        #[label("expected `{expected}`")]
        span: Span,
    },

    /// Invalid type
    #[error("invalid type: {message}")]
    InvalidType {
        /// Error message
        message: String,
        /// Location
        #[label("invalid type")]
        span: Span,
    },

    /// Arity mismatch (wrong number of arguments)
    #[error("{kind} expects {expected} arguments, found {found}")]
    ArityMismatch {
        /// Kind (function, type, etc.)
        kind: &'static str,
        /// Expected count
        expected: usize,
        /// Found count
        found: usize,
        /// Location
        #[label("wrong number of arguments")]
        span: Span,
    },

    /// Invalid operation
    #[error("invalid operation: {message}")]
    InvalidOperation {
        /// Error message
        message: String,
        /// Location
        #[label("invalid")]
        span: Span,
    },

    /// Cyclic dependency
    #[error("cyclic dependency in {kind}: {cycle}")]
    CyclicDependency {
        /// Kind of cycle
        kind: &'static str,
        /// Description of the cycle
        cycle: String,
        /// Location
        #[label("cycle here")]
        span: Span,
    },

    /// Missing required element
    #[error("missing {kind}: {name}")]
    Missing {
        /// Kind of missing element
        kind: &'static str,
        /// Name or description
        name: String,
        /// Location
        #[label("missing")]
        span: Span,
    },

    /// Invalid pattern
    #[error("invalid pattern: {message}")]
    InvalidPattern {
        /// Error message
        message: String,
        /// Location
        #[label("invalid pattern")]
        span: Span,
    },

    /// Not exhaustive pattern match
    #[error("non-exhaustive patterns: {missing}")]
    NonExhaustive {
        /// Missing patterns
        missing: String,
        /// Location
        #[label("patterns not covered")]
        span: Span,
    },
}

impl SemanticError {
    /// Create an undefined error
    #[must_use]
    pub fn undefined(kind: &'static str, name: impl Into<String>, span: Span) -> Self {
        SemanticError::Undefined {
            kind,
            name: name.into(),
            span,
        }
    }

    /// Create a duplicate error
    #[must_use]
    pub fn duplicate(
        kind: &'static str,
        name: impl Into<String>,
        span: Span,
        original_span: Span,
    ) -> Self {
        SemanticError::Duplicate {
            kind,
            name: name.into(),
            span,
            original_span,
        }
    }

    /// Create a type mismatch error
    #[must_use]
    pub fn type_mismatch(
        expected: impl Into<String>,
        found: impl Into<String>,
        span: Span,
    ) -> Self {
        SemanticError::TypeMismatch {
            expected: expected.into(),
            found: found.into(),
            span,
        }
    }

    /// Create an arity mismatch error
    #[must_use]
    pub fn arity_mismatch(
        kind: &'static str,
        expected: usize,
        found: usize,
        span: Span,
    ) -> Self {
        SemanticError::ArityMismatch {
            kind,
            expected,
            found,
            span,
        }
    }

    /// Get the span of the error
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            SemanticError::Undefined { span, .. }
            | SemanticError::Duplicate { span, .. }
            | SemanticError::TypeMismatch { span, .. }
            | SemanticError::InvalidType { span, .. }
            | SemanticError::ArityMismatch { span, .. }
            | SemanticError::InvalidOperation { span, .. }
            | SemanticError::CyclicDependency { span, .. }
            | SemanticError::Missing { span, .. }
            | SemanticError::InvalidPattern { span, .. }
            | SemanticError::NonExhaustive { span, .. } => *span,
        }
    }
}

/// Collection of semantic errors
#[derive(Debug, Default)]
pub struct Diagnostics {
    errors: Vec<SemanticError>,
}

impl Diagnostics {
    /// Create a new empty diagnostics collection
    #[must_use]
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Add an error
    pub fn error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }

    /// Check if there are any errors
    #[must_use]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all errors
    #[must_use]
    pub fn errors(&self) -> &[SemanticError] {
        &self.errors
    }

    /// Take all errors, consuming self
    #[must_use]
    pub fn into_errors(self) -> Vec<SemanticError> {
        self.errors
    }

    /// Get the number of errors
    #[must_use]
    pub fn len(&self) -> usize {
        self.errors.len()
    }

    /// Check if empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}
