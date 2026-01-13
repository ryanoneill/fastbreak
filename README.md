# Fastbreak

A formal methods-inspired specification language combining ideas from Alloy, TLA+, Cucumber, and Design by Contract.

## Overview

Fastbreak provides a unified language for specifying software systems that is both formal and readable. It allows you to define:

- **Type definitions** - Entities and their relationships (Alloy-inspired)
- **State machines** - System states, transitions, and invariants (TLA+-inspired)
- **Scenarios** - Given/When/Then executable specifications (Cucumber-inspired)
- **Contracts** - Preconditions, postconditions, and invariants (Design by Contract-inspired)
- **Properties** - Temporal properties with always/eventually operators
- **Quality requirements** - Non-functional requirements with measurable targets

## Installation

```bash
cargo install fastbreak
```

Or build from source:

```bash
git clone https://github.com/ryanoneill/fastbreak.git
cd fastbreak
cargo build --release
```

## Quick Start

```bash
# Initialize a new project
fastbreak init my-spec

# Check specifications for errors
fastbreak check

# Build everything (check + generate docs + diagrams)
fastbreak build
```

## Language Syntax

Fastbreak uses a Rust-like syntax with the `.fbrk` file extension.

### Modules and Imports

Specifications can be organized across multiple files using modules and imports:

```fbrk
// In types.fbrk
module myproject.types

type UserId { value: Int }
type Email { address: String }
```

```fbrk
// In users.fbrk
module myproject.users

use myproject.types::{UserId, Email}

type User {
    id: UserId,
    email: Email,
}
```

Import aliases allow renaming imported items:

```fbrk
use types::{Identifier as Id, Email as E}
```

### Type Definitions

```fbrk
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

### Type Aliases with Refinements

```fbrk
type Email = String where self.contains("@")
type PositiveInt = Int where self > 0
```

### Relations

```fbrk
relation friends: User -> Set<User> {
    symmetric
    irreflexive
}
```

### State Definitions

```fbrk
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

```fbrk
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

```fbrk
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

    alt "Email already exists" when exists u in users where u.email == email {
        then {
            result is Err
            users.len() == old_count
        }
    }
}
```

### Properties

```fbrk
property "Users are unique by email" {
    always {
        forall u1, u2 in users where u1 != u2 =>
            u1.email != u2.email
    }
}

property "Parsing is deterministic" {
    always {
        forall source: String =>
            parse(source) == parse(source)
    }
}
```

### Quality Requirements

```fbrk
@id("NFR-001")
quality performance "API response time" {
    metric: latency,
    target: < 100ms,
}

@id("NFR-002")
quality reliability "System uptime" {
    metric: availability,
    target: >= 99.9%,
}
```

### Attributes for Traceability

```fbrk
@id("REQ-001")
@rationale("Users must be uniquely identifiable")
type User {
    id: UserId,
    name: String,
}
```

## CLI Commands

```bash
# Initialize a new project
fastbreak init <name> [--path <dir>]

# Check specifications for errors
fastbreak check [<file>]

# Generate markdown documentation
fastbreak doc [<file>]

# Generate Mermaid diagrams
fastbreak diagram [<file>] [--type <erd|state|sequence>]

# Build everything (check + doc + diagram)
fastbreak build
```

## Project Structure

A Fastbreak project uses the following structure:

```
my-spec/
├── fastbreak.toml    # Project manifest
├── specs/            # Specification files
│   └── main.fbrk
└── docs/             # Generated output
    ├── my-spec.md
    ├── my-spec_erd.mmd
    ├── my-spec_state.mmd
    └── my-spec_sequence.mmd
```

### Project Manifest (fastbreak.toml)

```toml
[project]
name = "my-spec"
version = "0.1.0"

[source]
dir = "specs"
extension = "fbrk"

[output]
dir = "docs"
markdown = true
diagrams = true

[output.diagram_types]
erd = true
state = true
sequence = true
```

## Self-Specification

Fastbreak is specified using Fastbreak itself. The self-specification lives in `spec/` and serves as both documentation and validation:

```bash
cd spec
../target/release/fastbreak build
```

## Expression Language

Fastbreak includes a rich expression language supporting:

- **Literals**: Integers, floats, strings, booleans, unit
- **Collections**: Sets `{a, b}`, lists `[a, b]`, maps `{k: v}`
- **Operators**: Arithmetic, comparison, logical, set operations (union, intersect, difference)
- **Quantifiers**: `forall x in xs => pred(x)` and `exists x in xs where pred(x)`
- **Typed quantifiers**: `forall x: Type => pred(x)` for universal properties
- **Pattern Matching**: `match expr { Pat => result, ... }`
- **Lambdas**: `|x| x + 1` for map/filter operations
- **Field Access**: `user.email`
- **Method Calls**: `list.len()`, `set.contains(x)`, `option.unwrap()`
- **Range expressions**: `1..10`

## Temporal Operators

Properties can use temporal operators for specifying behavior over time:

- `always { expr }` - Expression holds in all states
- `eventually { expr }` - Expression holds in at least one state

## Generated Output

### Markdown Documentation

Generates comprehensive documentation including:
- Type definitions with field tables
- Enum variants
- State definitions with invariants
- Action signatures with contracts
- Scenarios with given/when/then steps
- Properties and quality requirements

### Mermaid Diagrams

- **ERD**: Entity-relationship diagrams showing types and their relationships
- **State**: State machine diagrams from state definitions
- **Sequence**: Sequence diagrams from scenarios

## Design Principles

1. **Formal but Readable**: Specifications should be precise enough for verification yet readable as documentation
2. **Rust-like Syntax**: Familiar syntax for developers coming from Rust
3. **Incremental Verification**: Start with lightweight checks, architect for full model checking
4. **Multi-paradigm**: Combine structural (Alloy), behavioral (TLA+), and scenario-based (Cucumber) specifications
5. **Self-documenting**: Generated documentation keeps specs and docs in sync

## License

MIT
