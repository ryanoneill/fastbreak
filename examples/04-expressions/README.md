# Expressions

Fastbreak has a rich expression language for specifying constraints, invariants, and behaviors. This section covers all expression types from basic literals to complex lambdas.

## Files

| File | Concepts |
|------|----------|
| [01-literals-operators.fbrk](01-literals-operators.fbrk) | Literals, arithmetic, comparison, logical operators |
| [02-collections.fbrk](02-collections.fbrk) | Sets, lists, maps, tuples, collection operations |
| [03-quantifiers.fbrk](03-quantifiers.fbrk) | `forall`, `exists`, filters, bindings |
| [04-pattern-matching.fbrk](04-pattern-matching.fbrk) | `match`, `is`, patterns, guards |
| [05-lambdas.fbrk](05-lambdas.fbrk) | Lambda syntax, closures, higher-order functions |

## Key Concepts

### Literals
```fbrk
42            // Integer
true          // Boolean
"hello"       // String
```

### Operators
```fbrk
// Arithmetic: +, -, *, /, %
// Comparison: ==, !=, <, <=, >, >=
// Logical: and, or, not, implies
```

### Collections
```fbrk
{1, 2, 3}           // Set literal
[1, 2, 3]           // List literal
{a: 1, b: 2}        // Map literal
(1, "hello", true)  // Tuple
```

### Quantifiers
```fbrk
forall x in collection => predicate
exists x in collection where condition
```

### Pattern Matching
```fbrk
match value {
    Pattern1 => result1,
    Pattern2 => result2,
    _ => default,
}
```

### Type Checking
```fbrk
value is SomeVariant
```

### Lambdas
```fbrk
x => x * 2
(a, b) => a + b
```
