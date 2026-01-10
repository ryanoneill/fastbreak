//! Parser error types

// The unused_assignments warnings are false positives - fields are used by thiserror Display derive
#![allow(unused_assignments)]

use crate::lexer::Token;
use crate::Span;
use miette::Diagnostic;
use thiserror::Error;

/// Result type for parser operations
pub type ParseResult<T> = Result<T, ParseError>;

/// A parse error with location and context
#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ParseError {
    /// Unexpected token
    #[error("unexpected token: expected {expected}, found {found}")]
    UnexpectedToken {
        /// What we expected
        expected: String,
        /// What we found
        found: String,
        /// Location of the error
        #[label("here")]
        span: Span,
    },

    /// Unexpected end of input
    #[error("unexpected end of input: expected {expected}")]
    UnexpectedEof {
        /// What we expected
        expected: String,
        /// Location of the error (end of file)
        #[label("end of input")]
        span: Span,
    },

    /// Invalid token (lexer error)
    #[error("invalid token")]
    InvalidToken {
        /// Location of the error
        #[label("invalid token")]
        span: Span,
    },

    /// Expected an identifier
    #[error("expected identifier, found {found}")]
    ExpectedIdent {
        /// What we found instead
        found: String,
        /// Location
        #[label("expected identifier")]
        span: Span,
    },

    /// Expected a type
    #[error("expected type, found {found}")]
    ExpectedType {
        /// What we found instead
        found: String,
        /// Location
        #[label("expected type")]
        span: Span,
    },

    /// Expected an expression
    #[error("expected expression, found {found}")]
    ExpectedExpr {
        /// What we found instead
        found: String,
        /// Location
        #[label("expected expression")]
        span: Span,
    },

    /// Expected a pattern
    #[error("expected pattern, found {found}")]
    ExpectedPattern {
        /// What we found instead
        found: String,
        /// Location
        #[label("expected pattern")]
        span: Span,
    },

    /// Duplicate definition
    #[error("duplicate {kind} definition: {name}")]
    DuplicateDefinition {
        /// Kind of definition
        kind: String,
        /// Name that was duplicated
        name: String,
        /// Location of the duplicate
        #[label("duplicate definition")]
        span: Span,
    },

    /// Invalid string literal (escape sequences, etc.)
    #[error("invalid string literal")]
    InvalidString {
        /// Location
        #[label("invalid string")]
        span: Span,
    },

    /// Invalid integer literal
    #[error("invalid integer literal")]
    InvalidInteger {
        /// Location
        #[label("invalid integer")]
        span: Span,
    },
}

impl ParseError {
    /// Create an unexpected token error
    pub fn unexpected(expected: impl Into<String>, found: &Token, span: Span) -> Self {
        ParseError::UnexpectedToken {
            expected: expected.into(),
            found: found.to_string(),
            span,
        }
    }

    /// Create an unexpected EOF error
    pub fn unexpected_eof(expected: impl Into<String>, span: Span) -> Self {
        ParseError::UnexpectedEof {
            expected: expected.into(),
            span,
        }
    }

    /// Create an expected identifier error
    #[must_use]
    pub fn expected_ident(found: &Token, span: Span) -> Self {
        ParseError::ExpectedIdent {
            found: found.to_string(),
            span,
        }
    }

    /// Create an expected type error
    #[must_use]
    pub fn expected_type(found: &Token, span: Span) -> Self {
        ParseError::ExpectedType {
            found: found.to_string(),
            span,
        }
    }

    /// Create an expected expression error
    #[must_use]
    pub fn expected_expr(found: &Token, span: Span) -> Self {
        ParseError::ExpectedExpr {
            found: found.to_string(),
            span,
        }
    }

    /// Get the span of the error
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            ParseError::UnexpectedToken { span, .. }
            | ParseError::UnexpectedEof { span, .. }
            | ParseError::InvalidToken { span }
            | ParseError::ExpectedIdent { span, .. }
            | ParseError::ExpectedType { span, .. }
            | ParseError::ExpectedExpr { span, .. }
            | ParseError::ExpectedPattern { span, .. }
            | ParseError::DuplicateDefinition { span, .. }
            | ParseError::InvalidString { span }
            | ParseError::InvalidInteger { span } => *span,
        }
    }
}
