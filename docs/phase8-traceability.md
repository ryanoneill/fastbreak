# Phase 8: Traceability and Metadata

## Overview

Add attributes/annotations to Fastbreak constructs to capture metadata like rationale, stakeholders, priority, and traceability links. This enables requirements management and connects specifications to external systems.

### Goals

1. Attach metadata to any specification construct
2. Support standard attributes (rationale, priority, stakeholder)
3. Enable traceability links between constructs
4. Generate traceability reports

### Non-Goals (for this phase)

- External requirements management tool integration
- Bidirectional sync with issue trackers
- Automated traceability validation

---

## Language Design

### Syntax

Rust-style attributes using `@` prefix:

```fbs
@rationale("Prevents duplicate accounts and enables password recovery")
@stakeholder(security_team)
@priority(critical)
@id(REQ-AUTH-001)
invariant "users have unique emails" {
    forall u1, u2 in users where u1 != u2 =>
        u1.email != u2.email
}

@derives_from(REQ-AUTH-001)
@verified_by(test_duplicate_email_rejected)
scenario "reject duplicate registration" {
    given { ... }
    when { ... }
    then { ... }
}

@deprecated("Use EmailV2 instead")
@since("0.2.0")
type Email = String where self.len() > 0
```

### Standard Attributes

| Attribute | Applies To | Value Type | Description |
|-----------|------------|------------|-------------|
| `@id` | Any | String | Unique identifier for cross-referencing |
| `@rationale` | Any | String | Why this construct exists |
| `@stakeholder` | Any | Identifier | Who cares about this |
| `@priority` | Any | critical/high/medium/low | Importance level |
| `@since` | Any | String | Version when introduced |
| `@deprecated` | Any | String (optional) | Mark as deprecated with reason |
| `@derives_from` | Any | Identifier(s) | Parent requirement(s) |
| `@verified_by` | Any | Identifier(s) | Test/scenario that verifies this |
| `@traces_to` | Any | Identifier(s) | Implementation artifact |
| `@tag` | Any | Identifier(s) | Arbitrary categorization |

### Custom Attributes

Users can define any attribute:

```fbs
@custom_field("any value")
@risk_level(high)
@compliance(GDPR, SOC2)
type UserData { ... }
```

### Attribute Value Types

```fbs
// String literal
@rationale("explanation here")

// Identifier (no quotes)
@stakeholder(security_team)

// Enum-like value
@priority(critical)

// Multiple values (list)
@tags(auth, security, user_management)
@derives_from(REQ-001, REQ-002)

// Nested/structured (future consideration)
@source(document: "SRS.docx", section: "3.2.1")
```

---

## AST Changes

### New: Attribute

```rust
// src/ast/mod.rs

/// An attribute annotation: `@name(args)`
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    /// Attribute name
    pub name: Ident,
    /// Arguments (if any)
    pub args: Vec<AttributeArg>,
    /// Span of the entire attribute
    pub span: Span,
}

/// An attribute argument
#[derive(Debug, Clone, PartialEq)]
pub enum AttributeArg {
    /// String literal: `"explanation"`
    String(SmolStr),
    /// Identifier: `security_team`
    Ident(Ident),
    /// Nested attribute: `source(doc: "x")`
    Nested {
        name: Ident,
        value: Box<AttributeArg>,
    },
}
```

### Modified: All Major Constructs

Add `attributes: Vec<Attribute>` to:

```rust
pub struct TypeDef {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}

pub struct TypeAlias {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}

pub struct EnumDef {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}

pub struct StateBlock {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}

pub struct Action {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}

pub struct Invariant {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: SmolStr,
    // ...
}

pub struct Scenario {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: SmolStr,
    // ...
}

pub struct Property {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: SmolStr,
    // ...
}

// Also fields and enum variants:
pub struct Field {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}

pub struct EnumVariant {
    pub attributes: Vec<Attribute>,  // NEW
    pub name: Ident,
    // ...
}
```

---

## Lexer Changes

### New Token: `@`

```rust
// src/lexer/token.rs

/// `@` for attribute prefix
#[token("@")]
At,
```

---

## Parser Changes

### New: parse_attributes

```rust
fn parse_attributes(&mut self) -> ParseResult<Vec<Attribute>> {
    let mut attributes = Vec::new();

    while self.check(&Token::At) {
        attributes.push(self.parse_attribute()?);
    }

    Ok(attributes)
}

fn parse_attribute(&mut self) -> ParseResult<Attribute> {
    let start = self.expect(&Token::At)?;
    let name = self.parse_ident()?;

    let args = if self.check(&Token::LParen) {
        self.advance(); // consume '('
        let args = self.parse_comma_separated(Self::parse_attribute_arg, &Token::RParen)?;
        self.expect(&Token::RParen)?;
        args
    } else {
        Vec::new()
    };

    let end = if args.is_empty() { name.span } else { self.previous_span() };

    Ok(Attribute {
        name,
        args,
        span: start.merge(end),
    })
}

fn parse_attribute_arg(&mut self) -> ParseResult<AttributeArg> {
    if self.check(&Token::StringLit) {
        let s = self.parse_string()?;
        Ok(AttributeArg::String(s))
    } else {
        let ident = self.parse_ident()?;

        // Check for nested: `name: value`
        if self.check(&Token::Colon) {
            self.advance();
            let value = self.parse_attribute_arg()?;
            Ok(AttributeArg::Nested {
                name: ident,
                value: Box::new(value),
            })
        } else {
            Ok(AttributeArg::Ident(ident))
        }
    }
}
```

### Modified: All parse_* functions

Each parsing function now starts with attributes:

```rust
fn parse_type_def(&mut self) -> ParseResult<TypeDef> {
    let attributes = self.parse_attributes()?;  // NEW
    let start = self.expect(&Token::Type)?;
    // ... rest unchanged

    Ok(TypeDef {
        attributes,  // NEW
        name,
        type_params,
        fields,
        refinement,
        span: start.merge(end),
    })
}
```

---

## Semantic Analysis Changes

### Attribute Validation

```rust
fn analyze_attributes(&mut self, attrs: &[Attribute], context: AttributeContext) {
    let mut seen_ids = HashSet::new();

    for attr in attrs {
        // Check for duplicate @id
        if attr.name.as_str() == "id" {
            if let Some(AttributeArg::String(id)) = attr.args.first() {
                if !seen_ids.insert(id.clone()) {
                    self.error(SemanticError::DuplicateId {
                        id: id.clone(),
                        span: attr.span,
                    });
                }
                // Register for cross-reference resolution
                self.register_id(id.clone(), context.clone());
            }
        }

        // Validate @derives_from, @verified_by, @traces_to references
        if matches!(attr.name.as_str(), "derives_from" | "verified_by" | "traces_to") {
            for arg in &attr.args {
                if let AttributeArg::Ident(ref_id) = arg {
                    self.pending_references.push(PendingRef {
                        from: context.clone(),
                        to: ref_id.name.clone(),
                        kind: attr.name.name.clone(),
                        span: attr.span,
                    });
                }
            }
        }

        // Validate @priority values
        if attr.name.as_str() == "priority" {
            if let Some(AttributeArg::Ident(level)) = attr.args.first() {
                if !matches!(level.as_str(), "critical" | "high" | "medium" | "low") {
                    self.error(SemanticError::InvalidPriority {
                        value: level.name.clone(),
                        span: level.span,
                    });
                }
            }
        }
    }
}
```

### Cross-Reference Resolution

After analyzing all constructs:

```rust
fn resolve_references(&mut self) {
    for pending in &self.pending_references {
        if !self.known_ids.contains(&pending.to) {
            self.error(SemanticError::UnknownReference {
                kind: pending.kind.clone(),
                reference: pending.to.clone(),
                span: pending.span,
            });
        }
    }
}
```

---

## Model Changes

### New: CompiledAttribute

```rust
pub struct CompiledAttribute {
    pub name: SmolStr,
    pub args: Vec<CompiledAttributeArg>,
}

pub enum CompiledAttributeArg {
    String(SmolStr),
    Ident(SmolStr),
    Nested { name: SmolStr, value: Box<CompiledAttributeArg> },
}
```

### Add to All Compiled Types

```rust
pub struct CompiledType {
    pub name: SmolStr,
    pub attributes: Vec<CompiledAttribute>,  // NEW
    pub fields: IndexMap<SmolStr, Type>,
    pub refinement: Option<CompiledExpr>,
}

// Similarly for CompiledAction, CompiledScenario, etc.
```

---

## Codegen Changes

### Markdown Generator

```rust
fn generate_attributes(&self, attrs: &[CompiledAttribute]) -> String {
    let mut output = String::new();

    // Group by category
    let rationale = attrs.iter().find(|a| a.name == "rationale");
    let priority = attrs.iter().find(|a| a.name == "priority");
    let stakeholders: Vec<_> = attrs.iter()
        .filter(|a| a.name == "stakeholder")
        .collect();
    let derives: Vec<_> = attrs.iter()
        .filter(|a| a.name == "derives_from")
        .collect();

    if let Some(r) = rationale {
        output.push_str(&format!("**Rationale:** {}\n\n", format_arg(&r.args[0])));
    }

    if let Some(p) = priority {
        output.push_str(&format!("**Priority:** {}\n\n", format_arg(&p.args[0])));
    }

    if !stakeholders.is_empty() {
        output.push_str("**Stakeholders:** ");
        // ... format list
    }

    if !derives.is_empty() {
        output.push_str("**Derives from:** ");
        // ... format as links
    }

    output
}
```

### New: Traceability Matrix Generator

```rust
// src/codegen/traceability.rs

pub struct TraceabilityGenerator<'a> {
    spec: &'a CompiledSpec,
}

impl<'a> TraceabilityGenerator<'a> {
    pub fn generate_matrix(&self) -> String {
        let mut output = String::from("# Traceability Matrix\n\n");
        output.push_str("| Requirement | Derives From | Verified By | Traces To |\n");
        output.push_str("|-------------|--------------|-------------|------------|\n");

        for item in self.all_items_with_id() {
            output.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                item.id,
                item.derives_from.join(", "),
                item.verified_by.join(", "),
                item.traces_to.join(", "),
            ));
        }

        output
    }

    pub fn generate_coverage_report(&self) -> String {
        // Report on which requirements have scenarios
        // Report on orphan scenarios (no requirement link)
        // ...
    }
}
```

---

## CLI Changes

### New Command: `fb trace`

```rust
/// Generate traceability reports
Trace {
    /// Path to manifest or source file
    #[arg(default_value = ".")]
    path: PathBuf,
    /// Output format (matrix, coverage, graph)
    #[arg(short, long, default_value = "matrix")]
    format: String,
    /// Output file (defaults to stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,
}
```

---

## Testing Strategy

### Parser Tests

```rust
#[test]
fn test_parse_single_attribute() {
    let spec = parse(r#"
        @rationale("test reason")
        type User { id: Int }
    "#).unwrap();

    assert_eq!(spec.types[0].attributes.len(), 1);
    assert_eq!(spec.types[0].attributes[0].name.as_str(), "rationale");
}

#[test]
fn test_parse_multiple_attributes() {
    let spec = parse(r#"
        @id(REQ-001)
        @priority(critical)
        @stakeholder(security)
        invariant "test" { true }
    "#).unwrap();

    // Verify all three attributes parsed
}

#[test]
fn test_parse_attribute_list_args() {
    let spec = parse(r#"
        @tags(auth, security, v2)
        type Token { ... }
    "#).unwrap();

    assert_eq!(spec.types[0].attributes[0].args.len(), 3);
}
```

### Semantic Tests

```rust
#[test]
fn test_invalid_priority() {
    let result = analyze(r#"
        @priority(urgent)
        type X { id: Int }
    "#);
    assert!(result.has_errors());
    // "urgent" is not a valid priority
}

#[test]
fn test_unknown_reference() {
    let result = analyze(r#"
        @derives_from(REQ-NONEXISTENT)
        scenario "test" { ... }
    "#);
    assert!(result.has_errors());
}

#[test]
fn test_valid_cross_reference() {
    let result = analyze(r#"
        @id(REQ-001)
        invariant "unique emails" { ... }

        @derives_from(REQ-001)
        scenario "test unique emails" { ... }
    "#);
    assert!(!result.has_errors());
}
```

---

## Implementation Order

### Step 1: AST and Lexer (1 unit)
- Add `@` token
- Define `Attribute` and `AttributeArg` types
- Add `attributes` field to all construct types

### Step 2: Parser (1 unit)
- Implement `parse_attributes`
- Implement `parse_attribute`
- Implement `parse_attribute_arg`
- Modify all `parse_*` functions to parse leading attributes
- Write parser tests

### Step 3: Semantic Analysis (1 unit)
- Validate standard attribute values
- Track `@id` declarations
- Resolve cross-references
- Report unknown references
- Write semantic tests

### Step 4: Model Compilation (0.5 unit)
- Add `CompiledAttribute`
- Include attributes in all compiled types

### Step 5: Code Generation (1 unit)
- Update Markdown to show attributes
- Create traceability matrix generator
- Write codegen tests

### Step 6: CLI (0.5 unit)
- Add `fb trace` command
- Integration tests

---

## Example Output

### Markdown Documentation

```markdown
## Invariants

### users have unique emails

**ID:** REQ-AUTH-001

**Rationale:** Prevents duplicate accounts and enables password recovery

**Priority:** critical

**Stakeholders:** security_team, product

**Verified by:** test_duplicate_email_rejected

```fbs
forall u1, u2 in users where u1 != u2 =>
    u1.email != u2.email
```
```

### Traceability Matrix

```
fb trace --format matrix

| ID           | Type      | Derives From | Verified By                    |
|--------------|-----------|--------------|--------------------------------|
| REQ-AUTH-001 | invariant | REQ-SEC-001  | test_duplicate_email_rejected  |
| REQ-AUTH-002 | action    | REQ-AUTH-001 | scenario_user_registration     |
| ...          | ...       | ...          | ...                            |

Coverage: 12/15 requirements have verification (80%)
Orphans: 2 scenarios have no requirement link
```
