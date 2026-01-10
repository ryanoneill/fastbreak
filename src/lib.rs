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
//! ```fbs
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
pub mod lexer;

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
