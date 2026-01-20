# Scenarios

Scenarios define executable test cases using a structured Given/When/Then format, inspired by BDD (Behavior-Driven Development) and specification by example.

## Files

| File | Concepts |
|------|----------|
| [01-basic-scenario.fbrk](01-basic-scenario.fbrk) | `scenario` keyword, `given`, `when`, `then` blocks |
| [02-assertions.fbrk](02-assertions.fbrk) | Pattern matching, Option/Result checking, quantifiers |
| [03-alternatives.fbrk](03-alternatives.fbrk) | `alt` keyword, conditional flows, error paths |

## Key Concepts

### Scenario Structure
A scenario has three parts:
```fbrk
scenario "Description" {
    given {
        // Initial state setup
    }
    when {
        // Actions to execute
    }
    then {
        // Assertions to verify
    }
}
```

### Given Block
Sets up the initial state:
```fbrk
given {
    items = {}
    count = 0
}
```

### When Block
Executes actions and captures results:
```fbrk
when {
    add_item(Item { id: 1, name: "Widget" })
    result = get_count()
}
```

### Then Block
Verifies postconditions:
```fbrk
then {
    items.len() == 1
    result == 0
}
```

### Alternative Flows
Handle different scenarios with conditions:
```fbrk
alt "Error case" when {
    some_condition
} {
    then {
        result is Err
    }
}
```
