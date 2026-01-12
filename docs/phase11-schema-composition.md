# Phase 11: Schema Composition

## Overview

Add schema calculus-inspired composition operators for building complex specifications from simpler components. This enables modular specification design, reuse, and information hiding.

### Goals

1. Extend existing states with additional fields/invariants
2. Compose multiple states into unified specifications
3. Hide implementation details for public interfaces
4. Rename fields for integration with external systems

### Non-Goals (for this phase)

- Full Z-style schema calculus
- Schema piping/sequencing
- Schema negation

---

## Language Design

### Syntax Overview

Four composition mechanisms:

```fbs
// 1. Extension: add fields and invariants to base state
state SessionAuth extends BaseAuth {
    sessions: Map<Token, UserId>,
    invariant "valid sessions" { ... }
}

// 2. Composition: combine multiple states
state FullSystem = UserModule and AuthModule and LoggingModule

// 3. Hiding: create view with hidden fields
state PublicUser = User hiding { password_hash, internal_id }

// 4. Renaming: adapt field names
state LegacyAdapter = ModernUser renaming { userId -> id, emailAddr -> email }
```

### 1. State Extension

Extend a base state with additional fields and invariants:

```fbs
state BaseAuth {
    users: Set<User>,

    invariant "unique emails" {
        forall u1, u2 in users where u1 != u2 =>
            u1.email != u2.email
    }
}

state SessionAuth extends BaseAuth {
    sessions: Map<Token, UserId>,
    active_count: Int,

    // New invariant (added to base invariants)
    invariant "sessions reference valid users" {
        forall token in sessions.keys() =>
            exists u in users where u.id == sessions[token]
    }

    // Can reference base fields
    invariant "active count consistent" {
        active_count == sessions.len()
    }
}

// SessionAuth has:
// - users (from BaseAuth)
// - sessions, active_count (its own)
// - All three invariants
```

### 2. State Composition

Combine multiple states with `and`:

```fbs
state UserModule {
    users: Set<User>,
    invariant "unique users" { ... }
}

state AuthModule {
    sessions: Map<Token, Session>,
    invariant "valid sessions" { ... }
}

state LoggingModule {
    audit_log: List<LogEntry>,
    invariant "log ordered" { ... }
}

// Compose all three
state FullSystem = UserModule and AuthModule and LoggingModule

// FullSystem has all fields and all invariants from all three
```

#### Composition Rules

- **Fields**: Union of all fields; error if same name with different types
- **Invariants**: Conjunction (all must hold)
- **Name conflicts**: Must have same type or composition fails

```fbs
// OK: same field name, same type
state A { count: Int }
state B { count: Int }
state C = A and B  // C has one `count: Int`

// ERROR: same field name, different types
state X { value: Int }
state Y { value: String }
state Z = X and Y  // Error: conflicting types for `value`
```

### 3. Hiding

Create a view that hides internal fields:

```fbs
state InternalUser {
    id: UserId,
    email: Email,
    password_hash: String,
    salt: String,
    internal_notes: String,
    created_at: Timestamp,
}

// Public view hides sensitive fields
state PublicUser = InternalUser hiding { password_hash, salt, internal_notes }

// PublicUser has: id, email, created_at
```

#### Hiding Rules

- Hidden fields are removed from the resulting state
- Invariants referencing hidden fields are also removed (with warning)
- Can hide multiple fields: `hiding { a, b, c }`

```fbs
state Full {
    public_data: String,
    secret: String,

    invariant "data not empty" { public_data.len() > 0 }
    invariant "secret format" { secret.starts_with("sk_") }
}

state Public = Full hiding { secret }
// Public has:
// - public_data
// - invariant "data not empty"
// Warning: invariant "secret format" removed (references hidden field)
```

### 4. Renaming

Adapt field names for compatibility:

```fbs
state ModernUser {
    userId: UserId,
    emailAddress: Email,
    fullName: String,
}

// Rename to match legacy API
state LegacyUser = ModernUser renaming {
    userId -> id,
    emailAddress -> email,
    fullName -> name,
}

// LegacyUser has: id, email, name (same types)
```

#### Renaming Rules

- Original field is replaced by renamed field
- Type remains the same
- Invariants are updated to use new names

### Combining Operations

Operations can be chained:

```fbs
// Extend, then hide
state DetailedAuth extends BaseAuth {
    internal_metrics: Metrics,
}
state PublicAuth = DetailedAuth hiding { internal_metrics }

// Compose, then rename
state Combined = ModuleA and ModuleB
state Adapted = Combined renaming { fieldA -> field_a }

// Complex composition
state FullPublicAPI = (
    (UserModule and AuthModule) hiding { internal_state }
) renaming { userId -> user_id }
```

---

## AST Changes

### Modified: StateBlock

```rust
// src/ast/state.rs

/// A state definition - either a block or a composition
#[derive(Debug, Clone, PartialEq)]
pub struct StateBlock {
    /// State name
    pub name: Ident,
    /// How this state is defined
    pub kind: StateKind,
    /// Attributes
    pub attributes: Vec<Attribute>,
    /// Span
    pub span: Span,
}

/// The kind of state definition
#[derive(Debug, Clone, PartialEq)]
pub enum StateKind {
    /// Direct definition with fields and invariants
    Block {
        /// Optional base state to extend
        extends: Option<Path>,
        /// State fields
        fields: Vec<StateField>,
        /// State invariants
        invariants: Vec<Invariant>,
    },

    /// Composition of multiple states
    Compose {
        /// States being composed
        operands: Vec<Path>,
    },

    /// Hiding fields from a base state
    Hide {
        /// Base state
        base: Path,
        /// Fields to hide
        hidden: Vec<Ident>,
    },

    /// Renaming fields in a base state
    Rename {
        /// Base state
        base: Path,
        /// Rename mappings (old -> new)
        renames: Vec<(Ident, Ident)>,
    },
}
```

---

## Lexer Changes

### New Tokens

```rust
// src/lexer/token.rs

/// `extends` keyword for state extension
#[token("extends")]
Extends,

/// `hiding` keyword for field hiding
#[token("hiding")]
Hiding,

/// `renaming` keyword for field renaming
#[token("renaming")]
Renaming,
```

The `and` keyword already exists.

---

## Parser Changes

### Modified: parse_state_block

```rust
fn parse_state_block(&mut self) -> ParseResult<StateBlock> {
    let attributes = self.parse_attributes()?;
    let start = self.expect(&Token::State)?;
    let name = self.parse_ident()?;

    // Determine the kind of state definition
    let kind = if self.check(&Token::Eq) {
        // Composition: state X = A and B
        // Or hiding: state X = Y hiding { ... }
        // Or renaming: state X = Y renaming { ... }
        self.advance(); // consume '='
        self.parse_state_composition()?
    } else if self.check(&Token::Extends) {
        // Extension: state X extends Y { ... }
        self.advance(); // consume 'extends'
        let base = self.parse_path()?;
        self.expect(&Token::LBrace)?;
        let (fields, invariants) = self.parse_state_body()?;
        self.expect(&Token::RBrace)?;
        StateKind::Block {
            extends: Some(base),
            fields,
            invariants,
        }
    } else {
        // Simple block: state X { ... }
        self.expect(&Token::LBrace)?;
        let (fields, invariants) = self.parse_state_body()?;
        self.expect(&Token::RBrace)?;
        StateKind::Block {
            extends: None,
            fields,
            invariants,
        }
    };

    Ok(StateBlock {
        name,
        kind,
        attributes,
        span: start.merge(self.previous_span()),
    })
}

fn parse_state_composition(&mut self) -> ParseResult<StateKind> {
    let first = self.parse_path()?;

    // Check what follows
    if self.check(&Token::And) {
        // Composition: A and B and C
        let mut operands = vec![first];
        while self.check(&Token::And) {
            self.advance();
            operands.push(self.parse_path()?);
        }
        Ok(StateKind::Compose { operands })
    } else if self.check(&Token::Hiding) {
        // Hiding: X hiding { a, b }
        self.advance();
        self.expect(&Token::LBrace)?;
        let hidden = self.parse_comma_separated(Self::parse_ident, &Token::RBrace)?;
        self.expect(&Token::RBrace)?;
        Ok(StateKind::Hide { base: first, hidden })
    } else if self.check(&Token::Renaming) {
        // Renaming: X renaming { a -> b, c -> d }
        self.advance();
        self.expect(&Token::LBrace)?;
        let renames = self.parse_comma_separated(Self::parse_rename_pair, &Token::RBrace)?;
        self.expect(&Token::RBrace)?;
        Ok(StateKind::Rename { base: first, renames })
    } else {
        // Just an alias: state X = Y
        Ok(StateKind::Compose { operands: vec![first] })
    }
}

fn parse_rename_pair(&mut self) -> ParseResult<(Ident, Ident)> {
    let old = self.parse_ident()?;
    self.expect(&Token::Arrow)?;  // ->
    let new = self.parse_ident()?;
    Ok((old, new))
}
```

---

## Semantic Analysis Changes

### State Resolution

Resolve composed states to their effective fields and invariants:

```rust
struct ResolvedState {
    name: SmolStr,
    fields: IndexMap<SmolStr, Type>,
    invariants: Vec<ResolvedInvariant>,
    source: StateSource,  // for error reporting
}

enum StateSource {
    Direct,
    Extended(SmolStr),
    Composed(Vec<SmolStr>),
    Hidden(SmolStr, Vec<SmolStr>),
    Renamed(SmolStr, Vec<(SmolStr, SmolStr)>),
}

fn resolve_state(&mut self, state: &StateBlock) -> Result<ResolvedState, SemanticError> {
    match &state.kind {
        StateKind::Block { extends, fields, invariants } => {
            self.resolve_block_state(state, extends.as_ref(), fields, invariants)
        }
        StateKind::Compose { operands } => {
            self.resolve_composed_state(state, operands)
        }
        StateKind::Hide { base, hidden } => {
            self.resolve_hidden_state(state, base, hidden)
        }
        StateKind::Rename { base, renames } => {
            self.resolve_renamed_state(state, base, renames)
        }
    }
}
```

### Extension Resolution

```rust
fn resolve_block_state(
    &mut self,
    state: &StateBlock,
    extends: Option<&Path>,
    fields: &[StateField],
    invariants: &[Invariant],
) -> Result<ResolvedState, SemanticError> {
    let mut result_fields = IndexMap::new();
    let mut result_invariants = Vec::new();

    // If extending, get base state fields and invariants
    if let Some(base_path) = extends {
        let base_name = base_path.to_string();
        let base = self.resolve_state_by_name(&base_name)?;

        result_fields = base.fields.clone();
        result_invariants = base.invariants.clone();
    }

    // Add own fields (check for conflicts)
    for field in fields {
        let field_name = field.name.name.clone();
        let field_type = self.resolve_type(&field.ty);

        if let Some(existing) = result_fields.get(&field_name) {
            if existing != &field_type {
                return Err(SemanticError::ConflictingFieldType {
                    field: field_name,
                    existing: existing.clone(),
                    new: field_type,
                    span: field.span,
                });
            }
        } else {
            result_fields.insert(field_name, field_type);
        }
    }

    // Add own invariants
    for inv in invariants {
        result_invariants.push(self.resolve_invariant(inv, &result_fields)?);
    }

    Ok(ResolvedState {
        name: state.name.name.clone(),
        fields: result_fields,
        invariants: result_invariants,
        source: if extends.is_some() {
            StateSource::Extended(extends.unwrap().to_string().into())
        } else {
            StateSource::Direct
        },
    })
}
```

### Composition Resolution

```rust
fn resolve_composed_state(
    &mut self,
    state: &StateBlock,
    operands: &[Path],
) -> Result<ResolvedState, SemanticError> {
    let mut result_fields = IndexMap::new();
    let mut result_invariants = Vec::new();
    let mut sources = Vec::new();

    for operand in operands {
        let operand_name = operand.to_string();
        let resolved = self.resolve_state_by_name(&operand_name)?;
        sources.push(operand_name.into());

        // Merge fields
        for (name, ty) in &resolved.fields {
            if let Some(existing) = result_fields.get(name) {
                if existing != ty {
                    return Err(SemanticError::ConflictingFieldInComposition {
                        field: name.clone(),
                        state1: sources[0].clone(),
                        type1: existing.clone(),
                        state2: operand_name.into(),
                        type2: ty.clone(),
                        span: state.span,
                    });
                }
                // Same type - OK, just keep one
            } else {
                result_fields.insert(name.clone(), ty.clone());
            }
        }

        // Merge invariants
        result_invariants.extend(resolved.invariants.clone());
    }

    Ok(ResolvedState {
        name: state.name.name.clone(),
        fields: result_fields,
        invariants: result_invariants,
        source: StateSource::Composed(sources),
    })
}
```

### Hiding Resolution

```rust
fn resolve_hidden_state(
    &mut self,
    state: &StateBlock,
    base: &Path,
    hidden: &[Ident],
) -> Result<ResolvedState, SemanticError> {
    let base_name = base.to_string();
    let base_resolved = self.resolve_state_by_name(&base_name)?;

    let hidden_names: HashSet<_> = hidden.iter()
        .map(|i| i.name.clone())
        .collect();

    // Validate hidden fields exist
    for h in hidden {
        if !base_resolved.fields.contains_key(&h.name) {
            return Err(SemanticError::UnknownFieldToHide {
                field: h.name.clone(),
                state: base_name.into(),
                span: h.span,
            });
        }
    }

    // Filter fields
    let result_fields: IndexMap<_, _> = base_resolved.fields.iter()
        .filter(|(name, _)| !hidden_names.contains(*name))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    // Filter invariants (remove those referencing hidden fields)
    let mut result_invariants = Vec::new();
    for inv in &base_resolved.invariants {
        let refs_hidden = inv.referenced_fields.iter()
            .any(|f| hidden_names.contains(f));

        if refs_hidden {
            self.warning(SemanticWarning::InvariantRemovedDueToHiding {
                invariant: inv.name.clone(),
                state: state.name.name.clone(),
                span: state.span,
            });
        } else {
            result_invariants.push(inv.clone());
        }
    }

    Ok(ResolvedState {
        name: state.name.name.clone(),
        fields: result_fields,
        invariants: result_invariants,
        source: StateSource::Hidden(
            base_name.into(),
            hidden_names.into_iter().collect(),
        ),
    })
}
```

### Renaming Resolution

```rust
fn resolve_renamed_state(
    &mut self,
    state: &StateBlock,
    base: &Path,
    renames: &[(Ident, Ident)],
) -> Result<ResolvedState, SemanticError> {
    let base_name = base.to_string();
    let base_resolved = self.resolve_state_by_name(&base_name)?;

    let rename_map: HashMap<_, _> = renames.iter()
        .map(|(old, new)| (old.name.clone(), new.name.clone()))
        .collect();

    // Validate renamed fields exist
    for (old, _) in renames {
        if !base_resolved.fields.contains_key(&old.name) {
            return Err(SemanticError::UnknownFieldToRename {
                field: old.name.clone(),
                state: base_name.into(),
                span: old.span,
            });
        }
    }

    // Apply renames to fields
    let result_fields: IndexMap<_, _> = base_resolved.fields.iter()
        .map(|(name, ty)| {
            let new_name = rename_map.get(name).cloned().unwrap_or_else(|| name.clone());
            (new_name, ty.clone())
        })
        .collect();

    // Apply renames to invariants
    let result_invariants: Vec<_> = base_resolved.invariants.iter()
        .map(|inv| self.apply_renames_to_invariant(inv, &rename_map))
        .collect();

    Ok(ResolvedState {
        name: state.name.name.clone(),
        fields: result_fields,
        invariants: result_invariants,
        source: StateSource::Renamed(
            base_name.into(),
            renames.iter().map(|(o, n)| (o.name.clone(), n.name.clone())).collect(),
        ),
    })
}
```

---

## Model Changes

### Modified: CompiledState

```rust
pub struct CompiledState {
    pub name: SmolStr,
    pub fields: IndexMap<SmolStr, Type>,
    pub invariants: Vec<CompiledInvariant>,
    pub attributes: Vec<CompiledAttribute>,
    /// How this state was defined (for documentation)
    pub definition: StateDefinition,
}

pub enum StateDefinition {
    Direct,
    Extended { base: SmolStr },
    Composed { operands: Vec<SmolStr> },
    Hidden { base: SmolStr, hidden: Vec<SmolStr> },
    Renamed { base: SmolStr, renames: Vec<(SmolStr, SmolStr)> },
}
```

---

## Codegen Changes

### Markdown Generator

```rust
fn generate_state(&self, state: &CompiledState) -> String {
    let mut output = format!("### `{}`\n\n", state.name);

    // Show how it was defined
    match &state.definition {
        StateDefinition::Direct => {}
        StateDefinition::Extended { base } => {
            output.push_str(&format!("*Extends* `{}`\n\n", base));
        }
        StateDefinition::Composed { operands } => {
            output.push_str(&format!("*Composed from* {}\n\n",
                operands.iter().map(|s| format!("`{}`", s)).collect::<Vec<_>>().join(" and ")
            ));
        }
        StateDefinition::Hidden { base, hidden } => {
            output.push_str(&format!("*Based on* `{}` *hiding* {}\n\n",
                base,
                hidden.iter().map(|s| format!("`{}`", s)).collect::<Vec<_>>().join(", ")
            ));
        }
        StateDefinition::Renamed { base, renames } => {
            output.push_str(&format!("*Based on* `{}` *with renames*\n\n", base));
        }
    }

    // Fields table
    output.push_str("**State Variables:**\n\n");
    output.push_str("| Variable | Type |\n|----------|------|\n");
    for (name, ty) in &state.fields {
        output.push_str(&format!("| `{}` | `{}` |\n", name, format_type(ty)));
    }

    // Invariants
    if !state.invariants.is_empty() {
        output.push_str("\n**Invariants:**\n\n");
        for inv in &state.invariants {
            output.push_str(&format!("- {}\n", inv.name));
        }
    }

    output
}
```

---

## Testing Strategy

### Parser Tests

```rust
#[test]
fn test_parse_state_extends() {
    let spec = parse(r#"
        state Child extends Parent {
            extra: Int,
        }
    "#).unwrap();

    match &spec.states[0].kind {
        StateKind::Block { extends, .. } => {
            assert!(extends.is_some());
        }
        _ => panic!("expected Block"),
    }
}

#[test]
fn test_parse_state_compose() {
    let spec = parse("state All = A and B and C").unwrap();

    match &spec.states[0].kind {
        StateKind::Compose { operands } => {
            assert_eq!(operands.len(), 3);
        }
        _ => panic!("expected Compose"),
    }
}

#[test]
fn test_parse_state_hiding() {
    let spec = parse("state Public = Private hiding { secret, internal }").unwrap();

    match &spec.states[0].kind {
        StateKind::Hide { hidden, .. } => {
            assert_eq!(hidden.len(), 2);
        }
        _ => panic!("expected Hide"),
    }
}

#[test]
fn test_parse_state_renaming() {
    let spec = parse("state Legacy = Modern renaming { newName -> old_name }").unwrap();

    match &spec.states[0].kind {
        StateKind::Rename { renames, .. } => {
            assert_eq!(renames.len(), 1);
        }
        _ => panic!("expected Rename"),
    }
}
```

### Semantic Tests

```rust
#[test]
fn test_compose_conflicting_types() {
    let result = analyze(r#"
        state A { x: Int }
        state B { x: String }
        state C = A and B
    "#);
    assert!(result.has_errors());
}

#[test]
fn test_hide_unknown_field() {
    let result = analyze(r#"
        state A { x: Int }
        state B = A hiding { nonexistent }
    "#);
    assert!(result.has_errors());
}

#[test]
fn test_invariant_removed_on_hide() {
    let result = analyze(r#"
        state A {
            public: Int,
            secret: Int,
            invariant "uses secret" { secret > 0 }
        }
        state B = A hiding { secret }
    "#);
    // Should have warning about removed invariant
    assert!(result.has_warnings());
}
```

---

## Implementation Order

### Step 1: AST and Lexer (1 unit)
- Modify `StateBlock` to use `StateKind`
- Add `extends`, `hiding`, `renaming` tokens
- Define all StateKind variants

### Step 2: Parser (1.5 units)
- Modify `parse_state_block`
- Implement `parse_state_composition`
- Handle all composition forms
- Write parser tests

### Step 3: Semantic Analysis (2 units)
- Implement state resolution for all kinds
- Handle field merging and conflicts
- Handle invariant filtering/renaming
- Cycle detection for composed states
- Write semantic tests

### Step 4: Model Compilation (1 unit)
- Update `CompiledState`
- Generate effective fields/invariants

### Step 5: Code Generation (0.5 unit)
- Update Markdown to show composition info
- Update ERD for composed states

---

## Future Work

1. **Schema Piping**: `state X = Y then Z` (sequential composition)
2. **Schema Disjunction**: `state X = Y or Z` (either satisfies)
3. **Parameterized States**: `state Container<T> { items: Set<T> }`
4. **State Refinement**: Proving one state refines another
