# Fastbreak

A formal methods-inspired specification language combining ideas from Alloy, TLA+, Cucumber, and Design by Contract.

## Overview

Fastbreak provides a unified language for specifying software systems that is both formal and readable. It allows you to define:

- **Type definitions** - Entities and their relationships (Alloy-inspired)
- **State machines** - System states, transitions, and invariants (TLA+-inspired)
- **Scenarios** - Given/When/Then executable specifications (Cucumber-inspired)
- **Contracts** - Preconditions, postconditions, and invariants (Design by Contract-inspired)

## Language Syntax

Fastbreak uses a Rust-like syntax with the `.fbs` file extension.

### Type Definitions

```fbs
type User {
    id: UserId,
    email: Email,
    status: UserStatus,
}

enum UserStatus {
    Pending,
    Active,
    Suspended,
}
```

### Relations

```fbs
relation friends: User -> Set<User> {
    symmetric
    irreflexive
}
```

### State Definitions

```fbs
state AuthSystem {
    users: Set<User>,
    sessions: Map<SessionId, UserId>,

    invariant "All sessions reference existing users" {
        forall sid in sessions.keys() =>
            sessions[sid] in users.map(u => u.id)
    }
}
```

### Actions with Contracts

```fbs
action register(email: Email) -> Result<User, RegisterError>
    requires {
        not exists u in users where u.email == email
    }
    ensures {
        match result {
            Ok(user) => user in users' and user.email == email,
            Err(_) => users' == users,
        }
    }
```

### Scenarios

```fbs
scenario "New user registration" {
    given {
        users = {}
        sessions = {}
    }
    when {
        result = register("test@example.com")
    }
    then {
        result is Ok
        users.len() == 1
    }
}
```

### Properties

```fbs
property "Users are unique by email" {
    always {
        forall u1, u2 in users where u1 != u2 =>
            u1.email != u2.email
    }
}
```

## Project Structure

```
fastbreak/
├── src/
│   ├── lib.rs           # Library root
│   ├── main.rs          # CLI entry point
│   ├── ast/             # Abstract Syntax Tree definitions
│   ├── lexer/           # Lexical analysis (logos-based)
│   ├── parser/          # Recursive descent parser
│   ├── semantic/        # Semantic analysis and type checking
│   ├── model/           # Compiled specification model
│   ├── codegen/         # Output generation (planned)
│   ├── project/         # Project management (planned)
│   └── cli/             # Command-line interface (planned)
└── tests/
```

## Implementation Status

| Phase | Component | Status |
|-------|-----------|--------|
| 1 | Lexer & AST | Complete |
| 2 | Parser | Complete |
| 3 | Semantic Analysis | Complete |
| 4 | Model Representation | Complete |
| 5 | Code Generation | Planned |
| 6 | Project Management & CLI | Planned |

### Completed Features

- **Lexer**: Full tokenization using logos with support for all language constructs
- **AST**: Complete abstract syntax tree for modules, types, states, actions, scenarios, and properties
- **Parser**: Recursive descent parser with comprehensive error messages
- **Semantic Analysis**: Name resolution, type checking, and validation
- **Model**: Compiled specification with runtime evaluation and property checking

### Planned Features

- **Code Generation**: Markdown documentation and Mermaid diagrams
- **CLI**: `fb build`, `fb check`, `fb doc`, `fb diagram`, `fb init` commands
- **Multi-file Projects**: Import system with `fastbreak.toml` manifest
- **Watch Mode**: Automatic rebuild on file changes

## Building

```bash
# Build the project
cargo build

# Run tests
cargo test

# Run with clippy
cargo clippy
```

## Usage (Planned)

```bash
# Initialize a new project
fb init my-spec

# Check specifications
fb check

# Generate documentation
fb doc

# Generate diagrams
fb diagram

# Build everything (check + doc + diagram)
fb build
```

## Design Principles

1. **Formal but Readable**: Specifications should be precise enough for verification yet readable as documentation
2. **Rust-like Syntax**: Familiar syntax for developers coming from Rust
3. **Incremental Verification**: Start with lightweight checks, architect for full model checking
4. **Multi-paradigm**: Combine structural (Alloy), behavioral (TLA+), and scenario-based (Cucumber) specifications

## Expression Language

Fastbreak includes a rich expression language supporting:

- **Literals**: Integers, strings, booleans, unit
- **Collections**: Sets, lists, maps with literals and comprehensions
- **Operators**: Arithmetic, comparison, logical, set operations (union, intersect, difference)
- **Quantifiers**: `forall` and `exists` with filters
- **Pattern Matching**: `match` expressions with exhaustive checking
- **Lambdas**: Anonymous functions for map/filter operations
- **Field Access**: Dot notation for struct fields
- **Method Calls**: Built-in methods on collections and optionals

## Temporal Operators

Properties can use temporal operators for specifying behavior over time:

- `always { expr }` - Expression holds in all states
- `eventually { expr }` - Expression holds in at least one state
- `next { expr }` - Expression holds in the next state (planned)
- `until` - Expression holds until another becomes true (planned)

## License

MIT
