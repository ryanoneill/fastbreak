# Fastbreak Examples

Comprehensive examples demonstrating all features of the Fastbreak specification language, organized by feature with progressive complexity.

## What is Fastbreak?

Fastbreak is a formal methods-inspired specification language combining:
- **Alloy**: Entity relationships and structural constraints
- **TLA+**: State machines, transitions, temporal properties
- **Cucumber**: Given/When/Then executable scenarios
- **Design by Contract**: Preconditions, postconditions, invariants

## Learning Path

Work through the examples in order for the best learning experience:

| # | Section | Description | Files |
|---|---------|-------------|-------|
| 1 | [01-types](01-types/) | Type definitions, enums, aliases, generics, relations | 5 |
| 2 | [02-state-machines](02-state-machines/) | States, invariants, actions, contracts | 4 |
| 3 | [03-scenarios](03-scenarios/) | Given/When/Then scenarios, alternatives | 3 |
| 4 | [04-expressions](04-expressions/) | Operators, collections, quantifiers, patterns, lambdas | 5 |
| 5 | [05-properties](05-properties/) | Properties and temporal operators | 2 |
| 6 | [06-quality](06-quality/) | Non-functional requirements (NFRs) | 2 |
| 7 | [07-modules](07-modules/) | Modules and imports | 3 |
| 8 | [08-attributes](08-attributes/) | Metadata and traceability | 1 |

## Validating Examples

All examples can be validated using the Fastbreak CLI:

```bash
# Validate all examples
fastbreak check examples/

# Validate a specific file
fastbreak check examples/01-types/01-basic-types.fbrk

# Validate a specific section
fastbreak check examples/02-state-machines/
```

## Quick Reference

### Types
```fbrk
type User { id: Int, name: String }           // Struct type
enum Status { Active, Pending }               // Enum
type Email = String where self.contains("@")  // Refined alias
relation friends: User -> Set<User> { symmetric }  // Relation
```

### State Machines
```fbrk
state Counter {
    count: Int,
    invariant "positive" { count >= 0 }
}

action increment()
    requires { count < 100 }
    ensures { count' == count + 1 }
```

### Scenarios
```fbrk
scenario "Add user" {
    given { users = {} }
    when { result = add_user("Alice") }
    then { result is Ok }
}
```

### Properties
```fbrk
property "Unique emails" {
    always { forall u1, u2 in users where u1 != u2 => u1.email != u2.email }
}
```

### Quality Requirements
```fbrk
quality performance "Response time" {
    metric: latency,
    target: < 100ms,
}
```

## File Extension

Fastbreak files use the `.fbrk` extension.

## Further Reading

- [Fastbreak README](../README.md) - Full language documentation
- [Specification files](../spec/specs/) - Fastbreak's self-specification
