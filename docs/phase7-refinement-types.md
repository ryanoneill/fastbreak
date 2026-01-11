# Phase 7: Refinement Types

## Overview

Refinement types extend Fastbreak's type system with predicates that constrain valid values. This enables expressing constraints like "positive integers", "non-empty lists", or "valid email addresses" directly in the type system.

### Goals

1. Enable type-level constraints using predicates
2. Catch constraint violations during semantic analysis
3. Generate documentation for type constraints
4. Lay groundwork for future runtime/static verification

### Non-Goals (for this phase)

- Full dependent types (types parameterized by values)
- Theorem proving / SMT integration
- Runtime validation code generation

---

## Language Design

### Syntax

Two forms of refinement types:

#### 1. Type Aliases with Refinement

```fbs
// Simple refinement on built-in type
type PositiveInt = Int where self > 0

// String pattern constraint
type Email = String where matches(self, r"^[^@]+@[^@]+\.[^@]+$")

// Numeric range
type Percentage = Int where self >= 0 and self <= 100

// Collection constraint
type NonEmptyList<T> = List<T> where self.len() > 0

// Composite constraint
type ValidPassword = String where
    self.len() >= 8 and
    contains_uppercase(self) and
    contains_digit(self)
```

#### 2. Struct Types with Refinement

```fbs
// Struct with cross-field constraint
type DateRange {
    start: Date,
    end: Date,
} where self.start <= self.end

// Struct with field constraints
type User {
    id: Int,
    age: Int,
    email: String,
} where self.age >= 0 and self.age < 150

// Struct referencing other refined types
type Order {
    items: NonEmptyList<LineItem>,
    total: PositiveInt,
}
```

### The `self` Keyword

- `self` refers to the value being constrained
- For type aliases: `self` is the underlying value
- For structs: `self` is the struct instance, allowing field access via `self.field`

### Predicate Expressions

Refinement predicates can use:
- All existing expression operators (`and`, `or`, `not`, `implies`, comparisons)
- Field access on `self` for struct types
- Method calls (`self.len()`, `matches(self, pattern)`)
- Quantifiers over collection fields (`forall x in self.items => ...`)
- Literals and constants

---

## AST Changes

### New: TypeAlias

```rust
// src/ast/types.rs

/// Type alias with optional refinement: `type Email = String where ...`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    /// Alias name
    pub name: Ident,
    /// Type parameters (for generic aliases)
    pub type_params: Vec<Ident>,
    /// The underlying type
    pub base: TypeRef,
    /// Refinement predicate (optional)
    pub refinement: Option<Expr>,
    /// Span of the entire definition
    pub span: Span,
}
```

### Modified: TypeDef

```rust
// src/ast/types.rs

/// Type definition: `type User { id: UserId, email: Email } where ...`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDef {
    /// Type name
    pub name: Ident,
    /// Type parameters
    pub type_params: Vec<Ident>,
    /// Fields
    pub fields: Vec<Field>,
    /// Refinement predicate (optional)      // NEW
    pub refinement: Option<Expr>,
    /// Span of the entire definition
    pub span: Span,
}
```

### Modified: Specification

```rust
// src/ast/mod.rs

pub struct Specification {
    pub module: Option<Module>,
    pub imports: Vec<Import>,
    pub types: Vec<TypeDef>,
    pub type_aliases: Vec<TypeAlias>,  // NEW
    pub enums: Vec<EnumDef>,
    // ... rest unchanged
}
```

---

## Lexer Changes

### New Token: `self`

```rust
// src/lexer/token.rs

/// `self` keyword for refinement predicates
#[token("self")]
SelfKw,
```

The `where` keyword already exists (used in quantifier filters).

---

## Parser Changes

### Modified: parse_type_def

```rust
fn parse_type_def(&mut self) -> ParseResult<TypeDef> {
    let start = self.expect(&Token::Type)?;
    let name = self.parse_ident()?;

    let type_params = if self.check(&Token::LAngle) {
        self.parse_type_params()?
    } else {
        Vec::new()
    };

    // Check for '=' (type alias) vs '{' (struct definition)
    if self.check(&Token::Eq) {
        return self.parse_type_alias(name, type_params, start);
    }

    self.expect(&Token::LBrace)?;
    let fields = self.parse_comma_separated(Self::parse_field, &Token::RBrace)?;
    let end_brace = self.expect(&Token::RBrace)?;

    // NEW: Optional refinement clause
    let (refinement, end) = if self.check(&Token::Where) {
        self.advance(); // consume 'where'
        let pred = self.parse_expr()?;
        (Some(pred.clone()), pred.span)
    } else {
        (None, end_brace)
    };

    Ok(TypeDef {
        name,
        type_params,
        fields,
        refinement,
        span: start.merge(end),
    })
}
```

### New: parse_type_alias

```rust
fn parse_type_alias(
    &mut self,
    name: Ident,
    type_params: Vec<Ident>,
    start: Span,
) -> ParseResult<TypeAlias> {
    self.expect(&Token::Eq)?;
    let base = self.parse_type_ref()?;

    let (refinement, end) = if self.check(&Token::Where) {
        self.advance(); // consume 'where'
        let pred = self.parse_expr()?;
        (Some(pred.clone()), pred.span)
    } else {
        (None, base.span)
    };

    Ok(TypeAlias {
        name,
        type_params,
        base,
        refinement,
        span: start.merge(end),
    })
}
```

### Modified: parse_primary_expr

Add handling for `self`:

```rust
Token::SelfKw => {
    let span = self.advance().span;
    Ok(Expr::new(ExprKind::SelfRef, span))
}
```

### New: ExprKind::SelfRef

```rust
// src/ast/expr.rs

pub enum ExprKind {
    // ... existing variants

    /// Self reference in refinement context
    SelfRef,
}
```

---

## Semantic Analysis Changes

### New: Refinement Context

Track when we're inside a refinement predicate:

```rust
// src/semantic/mod.rs

struct AnalysisContext {
    // ... existing fields

    /// The type being refined (if in refinement context)
    refinement_subject: Option<RefinementSubject>,
}

enum RefinementSubject {
    /// Refining a type alias: self is the base type
    Alias { base_type: Type },
    /// Refining a struct: self is the struct type
    Struct { type_name: SmolStr, fields: Vec<(SmolStr, Type)> },
}
```

### New: analyze_type_alias

```rust
fn analyze_type_alias(&mut self, alias: &TypeAlias) {
    // Check for duplicate name
    if self.types.contains_key(&alias.name.name) {
        self.error(SemanticError::DuplicateType {
            name: alias.name.name.clone(),
            span: alias.name.span,
        });
        return;
    }

    // Resolve base type
    let base_type = self.resolve_type(&alias.base);

    // Register the type
    self.types.insert(alias.name.name.clone(), TypeInfo::Alias {
        base: base_type.clone(),
        refinement: alias.refinement.is_some(),
    });

    // Analyze refinement predicate if present
    if let Some(ref pred) = alias.refinement {
        self.with_refinement_context(
            RefinementSubject::Alias { base_type },
            |analyzer| {
                let pred_type = analyzer.analyze_expr(pred);
                if pred_type != Type::Bool {
                    analyzer.error(SemanticError::RefinementMustBeBool {
                        found: pred_type,
                        span: pred.span,
                    });
                }
            }
        );
    }
}
```

### Modified: analyze_expr for SelfRef

```rust
ExprKind::SelfRef => {
    match &self.context.refinement_subject {
        Some(RefinementSubject::Alias { base_type }) => {
            base_type.clone()
        }
        Some(RefinementSubject::Struct { type_name, .. }) => {
            Type::Struct(type_name.clone())
        }
        None => {
            self.error(SemanticError::SelfOutsideRefinement {
                span: expr.span,
            });
            Type::Error
        }
    }
}
```

### New Error Types

```rust
// src/semantic/error.rs

pub enum SemanticError {
    // ... existing variants

    /// `self` used outside refinement context
    SelfOutsideRefinement { span: Span },

    /// Refinement predicate must be boolean
    RefinementMustBeBool { found: Type, span: Span },

    /// Field access on non-struct self
    InvalidSelfFieldAccess { field: SmolStr, span: Span },
}
```

---

## Model Changes

### Modified: CompiledSpec

```rust
// src/model/spec.rs

pub struct CompiledSpec {
    pub module: Option<SmolStr>,
    pub types: IndexMap<SmolStr, CompiledType>,
    pub type_aliases: IndexMap<SmolStr, CompiledTypeAlias>,  // NEW
    // ... rest unchanged
}
```

### New: CompiledTypeAlias

```rust
pub struct CompiledTypeAlias {
    pub name: SmolStr,
    pub type_params: Vec<SmolStr>,
    pub base: Type,
    pub refinement: Option<CompiledExpr>,
}
```

### Modified: CompiledType

```rust
pub struct CompiledType {
    pub name: SmolStr,
    pub fields: IndexMap<SmolStr, Type>,
    pub refinement: Option<CompiledExpr>,  // NEW
}
```

---

## Codegen Changes

### Markdown Generator

Add documentation for refinements:

```rust
fn generate_type_def(&self, ty: &CompiledType) -> String {
    let mut output = format!("### `{}`\n\n", ty.name);

    // Fields table
    output.push_str("| Field | Type |\n|-------|------|\n");
    for (name, field_type) in &ty.fields {
        output.push_str(&format!("| `{}` | `{}` |\n", name, format_type(field_type)));
    }

    // NEW: Refinement constraint
    if let Some(ref refinement) = ty.refinement {
        output.push_str("\n**Constraint:**\n\n");
        output.push_str(&format!("```\nwhere {}\n```\n", format_expr(refinement)));
    }

    output
}

fn generate_type_alias(&self, alias: &CompiledTypeAlias) -> String {
    let mut output = format!("### `{}`\n\n", alias.name);
    output.push_str(&format!("**Base type:** `{}`\n\n", format_type(&alias.base)));

    if let Some(ref refinement) = alias.refinement {
        output.push_str("**Constraint:**\n\n");
        output.push_str(&format!("```\nwhere {}\n```\n", format_expr(refinement)));
    }

    output
}
```

### Mermaid Generator

Update ERD to show constraints:

```rust
fn generate_erd_type(&self, ty: &CompiledType) -> String {
    let mut output = format!("    {} {{\n", ty.name);

    for (name, field_type) in &ty.fields {
        output.push_str(&format!("        {} {}\n", format_mermaid_type(field_type), name));
    }

    // NEW: Add constraint as a note
    if ty.refinement.is_some() {
        output.push_str("        constraint refinement\n");
    }

    output.push_str("    }\n");
    output
}
```

---

## Testing Strategy

### Parser Tests

```rust
#[test]
fn test_parse_type_alias_simple() {
    let spec = parse("type PositiveInt = Int where self > 0").unwrap();
    assert_eq!(spec.type_aliases.len(), 1);
    let alias = &spec.type_aliases[0];
    assert_eq!(alias.name.as_str(), "PositiveInt");
    assert!(alias.refinement.is_some());
}

#[test]
fn test_parse_type_alias_generic() {
    let spec = parse("type NonEmptyList<T> = List<T> where self.len() > 0").unwrap();
    let alias = &spec.type_aliases[0];
    assert_eq!(alias.type_params.len(), 1);
}

#[test]
fn test_parse_struct_with_refinement() {
    let spec = parse(r#"
        type DateRange {
            start: Date,
            end: Date,
        } where self.start <= self.end
    "#).unwrap();
    assert_eq!(spec.types.len(), 1);
    assert!(spec.types[0].refinement.is_some());
}

#[test]
fn test_parse_self_keyword() {
    let spec = parse("type X = Int where self > 0").unwrap();
    let pred = spec.type_aliases[0].refinement.as_ref().unwrap();
    // Verify the expression contains SelfRef
    matches!(find_self_ref(pred), Some(_));
}
```

### Semantic Tests

```rust
#[test]
fn test_refinement_must_be_bool() {
    let result = analyze("type Bad = Int where self + 1");
    assert!(result.has_errors());
    assert!(matches!(
        result.errors()[0],
        SemanticError::RefinementMustBeBool { .. }
    ));
}

#[test]
fn test_self_outside_refinement() {
    let result = analyze(r#"
        type User { id: Int }
        invariant "bad" { self > 0 }
    "#);
    assert!(result.has_errors());
    assert!(matches!(
        result.errors()[0],
        SemanticError::SelfOutsideRefinement { .. }
    ));
}

#[test]
fn test_self_field_access_in_struct() {
    let result = analyze(r#"
        type User {
            age: Int,
        } where self.age >= 0
    "#);
    assert!(!result.has_errors());
}

#[test]
fn test_self_field_access_invalid() {
    let result = analyze("type Age = Int where self.foo > 0");
    assert!(result.has_errors());
    // Int doesn't have field 'foo'
}
```

### Integration Tests

```rust
#[test]
fn test_full_refinement_example() {
    let spec = r#"
        module types

        type PositiveInt = Int where self > 0
        type Email = String where self.len() > 0
        type NonEmptyList<T> = List<T> where self.len() > 0

        type User {
            id: PositiveInt,
            email: Email,
            age: Int,
        } where self.age >= 0 and self.age < 150

        type Order {
            items: NonEmptyList<Item>,
            total: PositiveInt,
        }
    "#;

    let project = Loader::load_string(spec).unwrap();
    assert!(!project.has_errors());
    assert_eq!(project.spec.type_aliases.len(), 3);
    assert_eq!(project.spec.types.len(), 2);
}
```

---

## Implementation Order

### Step 1: AST and Lexer (1 unit)
- Add `SelfKw` token
- Add `ExprKind::SelfRef`
- Add `TypeAlias` struct
- Add `refinement` field to `TypeDef`
- Update `Specification` to include `type_aliases`

### Step 2: Parser (2 units)
- Modify `parse_type_def` to detect `=` vs `{`
- Implement `parse_type_alias`
- Parse `where` clause for both forms
- Handle `self` in expression parsing
- Write parser tests

### Step 3: Semantic Analysis (2 units)
- Add refinement context tracking
- Implement `analyze_type_alias`
- Handle `SelfRef` expression
- Validate refinement predicates return Bool
- Validate field access on struct self
- Add new error types
- Write semantic tests

### Step 4: Model Compilation (1 unit)
- Add `CompiledTypeAlias`
- Add refinement to `CompiledType`
- Update compilation logic
- Write model tests

### Step 5: Code Generation (1 unit)
- Update Markdown generator for type aliases and refinements
- Update Mermaid ERD for constraints
- Write codegen tests

### Step 6: Integration and Documentation (1 unit)
- Update CLI output to show type aliases
- Add refinement examples to project template
- Update README
- Full integration tests

---

## Future Work (not in this phase)

1. **Runtime Validation**: Generate validation functions from refinements
2. **Static Verification**: Integrate with SMT solver to prove refinements at compile time
3. **Subtyping**: `PositiveInt` should be usable where `Int` is expected
4. **Inference**: Propagate refinements through expressions
5. **Dependent Types**: Types parameterized by values (`Vec<T, N>` where length = N)

---

## Example Output

After implementation, running `fb build` on a project with refinement types:

```
Checking myproject...
  5 definition(s) (2 type aliases, 3 types)
  2 scenario(s)
  0 property(ies)

No errors found.

Generating documentation for myproject...
  Generated: docs/myproject.md
```

Generated markdown includes:

```markdown
## Type Aliases

### `PositiveInt`

**Base type:** `Int`

**Constraint:**

```
where self > 0
```

### `Email`

**Base type:** `String`

**Constraint:**

```
where matches(self, r"^[^@]+@[^@]+\.[^@]+$")
```

## Types

### `User`

| Field | Type |
|-------|------|
| `id` | `PositiveInt` |
| `email` | `Email` |
| `age` | `Int` |

**Constraint:**

```
where self.age >= 0 and self.age < 150
```
```
