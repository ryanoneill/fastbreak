# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-01-12

### Added

- **Line and column numbers in error messages**: Parse errors now include precise location information (e.g., "error at line 3, column 5") for easier debugging.

- **Block syntax in match arms**: Match arm bodies can now use block expressions with braces, enabling multi-statement bodies:
  ```fbrk
  match x {
      0 => { false },
      _ => { x > 0 },
  }
  ```

- **Dotted module names**: Module declarations now support hierarchical naming with dot-separated paths:
  ```fbrk
  module myproject.auth.users
  ```

- **Multi-file module imports**: Specifications can now be split across multiple files with import support:
  ```fbrk
  // In types.fbrk
  module types
  type UserId { value: Int }
  type Email { address: String }

  // In users.fbrk
  module users
  use types::{UserId, Email}
  type User { id: UserId, email: Email }
  ```

- **Import aliases**: Imported items can be renamed locally:
  ```fbrk
  use types::{Identifier as Id}
  ```

- **Comprehensive self-specification**: The Fastbreak self-specification now documents all implementation types including operators, literals, tokens, semantic analysis types, and quality system types.

### Changed

- **Lowered MSRV to Rust 1.70**: Removed use of let-chains to support older Rust versions.

- **Split self-specification into modules**: The self-specification is now organized into `main.fbrk` (AST domain model) and `compiler.fbrk` (compiler pipeline types), demonstrating the multi-file import feature.

## [0.1.0] - 2026-01-11

### Added

- Initial release of Fastbreak specification language
- Type definitions with fields and refinement predicates
- Enum definitions
- Type aliases with optional refinements
- Relation definitions with constraints (symmetric, reflexive, transitive, etc.)
- State definitions with fields and invariants
- Action definitions with requires/ensures contracts
- Scenario definitions with given/when/then clauses and alternatives
- Property definitions with temporal operators (always/eventually)
- Quality requirement definitions for NFRs
- Attribute system for traceability (@id, @rationale, etc.)
- Rich expression language with quantifiers, pattern matching, lambdas
- CLI commands: init, check, build, doc, diagram
- Markdown documentation generation
- Mermaid diagram generation (ERD, state, sequence)
- Project manifest (fastbreak.toml) support
- Self-specification in `spec/` directory
