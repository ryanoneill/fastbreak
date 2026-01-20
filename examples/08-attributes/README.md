# Attributes

Attributes provide metadata annotations for specification elements. They can be used for documentation, traceability, categorization, and tooling integration.

## Files

| File | Concepts |
|------|----------|
| [01-attributes.fbrk](01-attributes.fbrk) | `@id`, `@deprecated`, `@rationale`, and custom attributes |

## Key Concepts

### Attribute Syntax
Attributes are prefixed with `@` and placed before the element they annotate:
```fbrk
@id("REQ-001")
type User {
    id: Int,
}
```

### Argument Types
Attributes can have different argument types:

```fbrk
@id("string-value")        // String argument
@priority(1)               // Integer argument
@category(security)        // Identifier argument
@deprecated                // No argument (flag)
```

### Multiple Attributes
Elements can have multiple attributes:
```fbrk
@id("TYPE-001")
@deprecated
@rationale("Legacy type, use NewUser instead")
type OldUser {
    id: Int,
}
```

### Applicable Elements
Attributes can be applied to:
- Types and enums
- State definitions
- State fields
- Invariants
- Actions
- Requires/ensures clauses
- Scenarios
- Properties
- Quality requirements

## Common Attributes

| Attribute | Purpose | Example |
|-----------|---------|---------|
| `@id` | Unique identifier for traceability | `@id("REQ-001")` |
| `@deprecated` | Mark as obsolete | `@deprecated` |
| `@rationale` | Explain design decision | `@rationale("Security requirement")` |
| `@category` | Categorize the element | `@category(security)` |
| `@priority` | Set importance level | `@priority(1)` |
| `@severity` | Set severity level | `@severity(3)` |

## Use Cases

1. **Requirements Traceability** - Link specifications to requirements
2. **Documentation** - Add context and rationale
3. **Categorization** - Group related elements
4. **Tooling** - Support for code generation or validation tools
5. **Deprecation** - Mark elements for removal
