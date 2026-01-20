# Type System

Fastbreak's type system is designed for clarity and safety. This section covers all type-related features.

## Files

| File | Concepts |
|------|----------|
| [01-basic-types.fbrk](01-basic-types.fbrk) | `type` keyword, fields, built-in types |
| [02-enums.fbrk](02-enums.fbrk) | `enum` keyword, variants, associated data |
| [03-type-aliases.fbrk](03-type-aliases.fbrk) | Type aliases, `where` refinements, `self` |
| [04-generics.fbrk](04-generics.fbrk) | Type parameters, generic collections |
| [05-relations.fbrk](05-relations.fbrk) | Alloy-inspired relations, constraints |

## Built-in Types

| Type | Description |
|------|-------------|
| `Int` | 64-bit integer |
| `String` | Text string |
| `Bool` | Boolean (`true`/`false`) |
| `Set<T>` | Unordered collection, no duplicates |
| `List<T>` | Ordered sequence |
| `Map<K, V>` | Key-value mapping |
| `Option<T>` | Optional value (`Some`/`None`) |
| `Result<T, E>` | Success/failure (`Ok`/`Err`) |

## Key Concepts

### Type Definitions
Define structured data with named fields:
```fbrk
type User {
    id: Int,
    name: String,
}
```

### Enums
Define variants, optionally with associated data:
```fbrk
enum Result {
    Ok(value: Int),
    Err(message: String),
}
```

### Type Aliases
Create aliases with optional refinement predicates:
```fbrk
type PositiveInt = Int where self > 0
```

### Relations
Define relationships between types with constraints:
```fbrk
relation friends: User -> Set<User> { symmetric }
```
