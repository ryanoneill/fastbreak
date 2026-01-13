//! Fastbreak: A formal methods-inspired specification language
//!
//! Fastbreak combines ideas from Alloy, TLA+, Cucumber, and Design by Contract
//! to create a specification language that is both formal and readable.
//!
//! # Features
//!
//! - **Type definitions**: Define entities and their relationships (Alloy-inspired)
//! - **State machines**: Define system states and transitions (TLA+-inspired)
//! - **Scenarios**: Given/When/Then executable specifications (Cucumber-inspired)
//! - **Contracts**: Preconditions, postconditions, and invariants (DbC-inspired)
//!
//! # Example
//!
//! ```fbrk
//! module auth
//!
//! type User {
//!     id: UserId,
//!     email: Email,
//!     status: UserStatus,
//! }
//!
//! state AuthSystem {
//!     users: Set<User>,
//!
//!     invariant "Users have unique emails" {
//!         forall u1, u2 in users where u1 != u2 =>
//!             u1.email != u2.email
//!     }
//! }
//!
//! action register(email: Email) -> Result<User, Error>
//!     requires { not exists u in users where u.email == email }
//!     ensures { result is Ok implies result.unwrap() in users' }
//! ```

#![warn(missing_docs)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]

pub mod ast;
pub mod cli;
pub mod codegen;
pub mod lexer;
pub mod model;
pub mod parser;
pub mod project;
pub mod semantic;

/// Source location information for error reporting
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct Span {
    /// Start byte offset
    pub start: usize,
    /// End byte offset (exclusive)
    pub end: usize,
}

impl Span {
    /// Create a new span
    #[must_use]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Create a span covering both spans
    #[must_use]
    pub const fn merge(self, other: Self) -> Self {
        let start = if self.start < other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end > other.end {
            self.end
        } else {
            other.end
        };
        Self { start, end }
    }
}

impl From<logos::Span> for Span {
    fn from(span: logos::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        (span.start, span.end.saturating_sub(span.start)).into()
    }
}

/// A human-readable source location (1-indexed line and column)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
}

impl SourceLocation {
    /// Compute the line and column for a byte offset in source text
    #[must_use]
    pub fn from_offset(source: &str, offset: usize) -> Self {
        let offset = offset.min(source.len());
        let mut line = 1;
        let mut column = 1;

        for (i, ch) in source.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }

        Self { line, column }
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_location_first_line() {
        let source = "hello world";
        let loc = SourceLocation::from_offset(source, 0);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 1);

        let loc = SourceLocation::from_offset(source, 6);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 7);
    }

    #[test]
    fn test_source_location_multiline() {
        let source = "line1\nline2\nline3";
        // Start of line 1
        let loc = SourceLocation::from_offset(source, 0);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 1);

        // End of line 1 (before \n)
        let loc = SourceLocation::from_offset(source, 5);
        assert_eq!(loc.line, 1);
        assert_eq!(loc.column, 6);

        // Start of line 2 (after \n)
        let loc = SourceLocation::from_offset(source, 6);
        assert_eq!(loc.line, 2);
        assert_eq!(loc.column, 1);

        // Middle of line 3
        let loc = SourceLocation::from_offset(source, 14);
        assert_eq!(loc.line, 3);
        assert_eq!(loc.column, 3);
    }

    #[test]
    fn test_source_location_display() {
        let loc = SourceLocation { line: 10, column: 25 };
        assert_eq!(format!("{loc}"), "10:25");
    }
}
