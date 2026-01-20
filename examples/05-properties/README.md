# Properties

Properties define conditions that must hold across the system. Unlike invariants which are tied to specific states, properties can express system-wide constraints and temporal conditions.

## Files

| File | Concepts |
|------|----------|
| [01-basic-properties.fbrk](01-basic-properties.fbrk) | `property` keyword, simple assertions, quantified properties |
| [02-temporal-properties.fbrk](02-temporal-properties.fbrk) | `always`, `eventually`, temporal logic |

## Key Concepts

### Basic Properties
Simple boolean assertions about the system:
```fbrk
property "Name describes the property" {
    expression_that_must_be_true
}
```

### Quantified Properties
Properties over collections:
```fbrk
property "All users valid" {
    forall u in users => u.id > 0
}
```

### Temporal Properties

**Always** - Must hold at every point in time:
```fbrk
property "Safety" {
    always {
        count >= 0
    }
}
```

**Eventually** - Must hold at some point:
```fbrk
property "Liveness" {
    eventually {
        initialized == true
    }
}
```

### Combined Temporal
```fbrk
property "Once started, eventually completes" {
    always {
        started implies eventually {
            completed
        }
    }
}
```

## Safety vs Liveness

| Type | Keyword | Meaning |
|------|---------|---------|
| Safety | `always` | Bad things never happen |
| Liveness | `eventually` | Good things will happen |

Safety properties are easier to verify but liveness properties are important for progress guarantees.
