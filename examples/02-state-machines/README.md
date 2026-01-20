# State Machines

Fastbreak's state machine features are inspired by TLA+ and Design by Contract. Define system states with invariants, and actions with pre/postconditions.

## Files

| File | Concepts |
|------|----------|
| [01-basic-state.fbrk](01-basic-state.fbrk) | `state` keyword, fields, initial values |
| [02-invariants.fbrk](02-invariants.fbrk) | `invariant` keyword, state constraints |
| [03-actions.fbrk](03-actions.fbrk) | `action` keyword, parameters, return types |
| [04-contracts.fbrk](04-contracts.fbrk) | `requires`, `ensures`, `state'`, `old()`, `result` |

## Key Concepts

### State Definition
Define system state with typed fields:
```fbrk
state Counter {
    count: Int,
    max: Int,
}
```

### Invariants
Boolean expressions that must always hold:
```fbrk
invariant "Count within bounds" {
    count >= 0 and count <= max
}
```

### Actions
Operations that can modify state:
```fbrk
action increment()
action create_user(name: String) -> Result<User, String>
```

### Contracts
Pre/postconditions for actions:
```fbrk
action add(n: Int)
    requires { n > 0 }          // Precondition
    ensures { count' == count + n }  // Postcondition
```

## Special Expressions

| Expression | Meaning | Context |
|------------|---------|---------|
| `state'` | Next state value | Postconditions |
| `old(expr)` | Value before action | Postconditions |
| `result` | Return value | Postconditions |
