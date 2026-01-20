# Modules and Imports

Fastbreak supports modular specifications through the module system. This allows you to organize large specifications into separate files, share common types, and manage dependencies between components.

## Files

| File | Concepts |
|------|----------|
| [common.fbrk](common.fbrk) | `module` declaration, shared types, reusable definitions |
| [users.fbrk](users.fbrk) | `use` imports, module dependencies |
| [01-imports.fbrk](01-imports.fbrk) | All import syntax patterns |

## Key Concepts

### Module Declaration
Every file can declare its module name:
```fbrk
module myproject.common
```

### Importing Items
Import specific items from other modules:
```fbrk
use common::UserId
use common::{Email, Status}
```

### Import with Alias
Rename imports to avoid conflicts:
```fbrk
use common::{Status as UserStatus}
```

### Module Hierarchy
Modules can be nested:
```fbrk
module myproject.auth.tokens
use myproject.common::UserId
```

## Import Patterns

| Pattern | Description |
|---------|-------------|
| `use mod::Item` | Import single item |
| `use mod::{A, B}` | Import multiple items |
| `use mod::{A as X}` | Import with alias |
| `use a.b.c::Item` | Import from nested module |

## Best Practices

1. **One module per file** - Each `.fbrk` file should declare exactly one module
2. **Common types in shared module** - Put reusable types like `UserId`, `Email` in a common module
3. **Explicit imports** - Import only what you need rather than entire modules
4. **Avoid circular dependencies** - Structure modules in a hierarchy to prevent cycles
