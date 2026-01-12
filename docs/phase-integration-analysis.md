# Phase Integration Analysis

## Overview

This document analyzes how Phases 7-11 interact with each other and the existing Fastbreak implementation to ensure they integrate cleanly without conflicts.

---

## Summary of All Phases

| Phase | Feature | Primary AST Changes | Lexer Changes |
|-------|---------|---------------------|---------------|
| 7 | Refinement Types | `TypeAlias`, `refinement` on `TypeDef`, `ExprKind::SelfRef` | `self` token |
| 8 | Traceability | `Attribute` on all constructs | `@` token |
| 9 | Alternative Flows | `Alternative` in `Scenario` | `alt` token |
| 10 | NFRs | `QualityRequirement` | `quality`, category keywords, unit values |
| 11 | Schema Composition | `StateKind` enum | `extends`, `hiding`, `renaming` tokens |

---

## Interaction Matrix

|  | Phase 7 (Refinement) | Phase 8 (Traceability) | Phase 9 (Alternatives) | Phase 10 (NFRs) | Phase 11 (Composition) |
|--|---------------------|----------------------|----------------------|-----------------|----------------------|
| **Phase 7** | - | ✅ Compatible | ✅ Compatible | ✅ Compatible | ⚠️ Needs attention |
| **Phase 8** | ✅ | - | ✅ Compatible | ✅ Compatible | ✅ Compatible |
| **Phase 9** | ✅ | ✅ | - | ✅ Compatible | ✅ Compatible |
| **Phase 10** | ✅ | ✅ | ✅ | - | ⚠️ Needs attention |
| **Phase 11** | ⚠️ | ✅ | ✅ | ⚠️ | - |

---

## Detailed Interaction Analysis

### Phase 7 (Refinement Types) ↔ Phase 8 (Traceability)

**Status: ✅ Fully Compatible**

- Attributes can be applied to type aliases and refined types
- No conflicts in syntax or semantics

```fbs
@id(TYPE-001)
@rationale("Ensures positive quantities")
type PositiveInt = Int where self > 0
```

**Implementation Notes:**
- Phase 8's `attributes` field simply gets added to `TypeAlias` and `TypeDef`
- No ordering dependency

---

### Phase 7 (Refinement Types) ↔ Phase 9 (Alternative Flows)

**Status: ✅ Fully Compatible**

- No interaction between these features
- Refinement types are about types; alternatives are about scenarios
- Both use the same expression language (already shared)

---

### Phase 7 (Refinement Types) ↔ Phase 10 (NFRs)

**Status: ✅ Compatible**

- NFRs can reference types: `applies_to: type PositiveInt`
- No syntax conflicts

```fbs
type Password = String where self.len() >= 8

quality security "Password strength" {
    metric: entropy,
    target: >= 60bits,
    applies_to: type Password,
}
```

**Implementation Notes:**
- NFR `applies_to` resolution must handle type aliases

---

### Phase 7 (Refinement Types) ↔ Phase 11 (Schema Composition)

**Status: ⚠️ Needs Attention**

**Issue: Refinement on Composed States**

When states are composed, what happens to refinements?

```fbs
type User {
    age: Int,
} where self.age >= 0

state UserModule {
    users: Set<User>,
}

// Question: If we compose, do type refinements still apply?
state Combined = UserModule and OtherModule
```

**Resolution:**
- Type refinements are part of the type system, not state composition
- When a composed state has a field `users: Set<User>`, the `User` refinement still applies
- **No conflict** - refinements are at type level, composition is at state level

**Issue: Self in State Refinement**

If we ever add state-level refinements:

```fbs
// Future possibility - NOT in Phase 7
state System {
    count: Int,
} where self.count >= 0

state Extended extends System { ... }
// Does Extended inherit the refinement?
```

**Resolution:**
- Phase 7 only adds refinements to types, not states
- State invariants already serve this purpose
- **No conflict** for Phase 7 scope

---

### Phase 8 (Traceability) ↔ All Other Phases

**Status: ✅ Fully Compatible**

Traceability is purely additive - it adds `attributes: Vec<Attribute>` to every construct. This has no semantic impact on other features.

**Implementation Order Consideration:**
- Implementing Phase 8 first means all subsequent phases automatically get attribute support
- Alternatively, each phase can add attributes to its new constructs
- **Recommendation:** Implement Phase 8 early (it's low effort) so all new constructs inherit attribute capability

---

### Phase 9 (Alternative Flows) ↔ Phase 10 (NFRs)

**Status: ✅ Compatible**

- NFRs can reference scenarios (for verification)
- Alternatives don't change this relationship

```fbs
quality performance "Login response" {
    metric: latency,
    target: < 100ms,
    verified_by: scenario_login_success,  // references main scenario path
}
```

**Consideration:**
- Should NFRs be able to reference specific alternatives?
- Proposal: Use path syntax `verified_by: scenario_login.alt_wrong_password`
- Can be deferred to future work

---

### Phase 9 (Alternative Flows) ↔ Phase 11 (Schema Composition)

**Status: ✅ Compatible**

- No interaction - alternatives are for scenarios, composition is for states
- Different domains

---

### Phase 10 (NFRs) ↔ Phase 11 (Schema Composition)

**Status: ⚠️ Needs Attention**

**Issue: NFRs on Composed States**

```fbs
state BaseSystem {
    users: Set<User>,
}

quality reliability "Base uptime" {
    metric: uptime,
    target: >= 99%,
    applies_to: state BaseSystem,
}

state ExtendedSystem extends BaseSystem {
    sessions: Map<Token, User>,
}

// Question: Does ExtendedSystem inherit the NFR?
```

**Options:**
1. **No inheritance** - NFRs are explicit bindings only
2. **Inheritance** - Extended/composed states inherit NFRs
3. **Explicit choice** - New syntax to specify inheritance

**Resolution: Option 1 (No inheritance)**
- Keeps semantics simple and explicit
- Users can apply NFRs to composed states explicitly
- Matches how invariants work (extended states have their own)

```fbs
// Explicit is better:
quality reliability "Extended uptime" {
    metric: uptime,
    target: >= 99.5%,  // stricter requirement
    applies_to: state ExtendedSystem,
}
```

---

## Shared Infrastructure

### Expression Language

All phases use the same expression language:

| Phase | Expression Usage |
|-------|-----------------|
| 7 (Refinement) | `where self > 0` - refinement predicates |
| 8 (Traceability) | `@priority(critical)` - attribute values (limited) |
| 9 (Alternatives) | `alt when { condition }` - alternative conditions |
| 10 (NFRs) | `target: < 200ms` - quality expressions |
| 11 (Composition) | Invariants in composed states |

**This is a strength, not a conflict.** One expression parser serves all needs.

### Type System

| Phase | Type System Interaction |
|-------|------------------------|
| 7 (Refinement) | Extends types with predicates |
| 8 (Traceability) | No impact |
| 9 (Alternatives) | Uses types in given/when/then |
| 10 (NFRs) | References types via `applies_to` |
| 11 (Composition) | Types in state fields are composed |

**No conflicts.** Refinement types are a strict extension.

### Semantic Analysis

| Phase | Semantic Analysis Needs |
|-------|------------------------|
| 7 (Refinement) | New context for `self`; validate predicates return Bool |
| 8 (Traceability) | Validate references; track IDs |
| 9 (Alternatives) | Validate conditions; inherit environments |
| 10 (NFRs) | Validate applies_to references |
| 11 (Composition) | Resolve composed states; detect cycles |

**Consideration:** Order of analysis matters
- Phase 11 (composition) must resolve before Phase 10 (NFRs) validates references
- Phase 7 (refinement) must complete before type references are validated

---

## Recommended Implementation Order

Based on the analysis:

```
Phase 8 (Traceability)     ──┐
                             ├──> Foundation: All constructs get attributes
Phase 7 (Refinement Types) ──┘

Phase 9 (Alternative Flows) ──> Independent: Extends scenarios only

Phase 11 (Schema Composition) ──> Must precede NFRs for resolution

Phase 10 (NFRs) ──> Last: References types, states, actions from all above
```

**Revised Order:**

| Order | Phase | Rationale |
|-------|-------|-----------|
| 1 | Phase 8 (Traceability) | Low effort, enables attributes on all future constructs |
| 2 | Phase 7 (Refinement) | Extends type system, no dependencies |
| 3 | Phase 9 (Alternatives) | Independent, extends scenarios |
| 4 | Phase 11 (Composition) | State resolution needed before NFRs |
| 5 | Phase 10 (NFRs) | References all other constructs |

---

## Token/Keyword Conflicts

### Checking for Conflicts

| Token | Phase | Potential Conflict |
|-------|-------|-------------------|
| `self` | 7 | None - new keyword |
| `@` | 8 | None - new symbol |
| `alt` | 9 | None - new keyword |
| `quality` | 10 | None - new keyword |
| `performance` | 10 | None - new keyword (context-specific) |
| `reliability` | 10 | None - new keyword (context-specific) |
| `security` | 10 | None - new keyword (context-specific) |
| `extends` | 11 | None - new keyword |
| `hiding` | 11 | None - new keyword |
| `renaming` | 11 | None - new keyword |

**Existing keywords reused:**
- `where` - Phase 7 uses for refinement (already exists for quantifiers) ✅
- `and` - Phase 11 uses for composition (already exists for logical) ✅

**No conflicts.** All new tokens are distinct.

---

## AST Structure Compatibility

### Specification Struct Evolution

```rust
// After all phases:
pub struct Specification {
    // Existing
    pub module: Option<Module>,
    pub imports: Vec<Import>,
    pub types: Vec<TypeDef>,        // + refinement field (Phase 7)
    pub enums: Vec<EnumDef>,
    pub relations: Vec<Relation>,
    pub states: Vec<StateBlock>,    // + StateKind (Phase 11)
    pub actions: Vec<Action>,
    pub scenarios: Vec<Scenario>,   // + alternatives (Phase 9)
    pub properties: Vec<Property>,

    // New in phases
    pub type_aliases: Vec<TypeAlias>,           // Phase 7
    pub qualities: Vec<QualityRequirement>,     // Phase 10
}

// All constructs get (Phase 8):
pub attributes: Vec<Attribute>
```

**No structural conflicts.** Each phase adds to different parts.

---

## Semantic Analysis Order

The analyzer must process constructs in the right order:

```
1. Register all type names (TypeDef, TypeAlias, EnumDef)
2. Resolve type aliases and refinements (Phase 7)
3. Resolve state compositions (Phase 11)
4. Validate attributes and cross-references (Phase 8)
5. Validate scenarios and alternatives (Phase 9)
6. Validate NFR applies_to references (Phase 10)
```

**Current analyzer can be extended sequentially.** Each phase adds a validation pass.

---

## Testing Strategy

### Cross-Phase Integration Tests

```rust
#[test]
fn test_refined_type_with_attributes() {
    // Phase 7 + Phase 8
    let spec = parse(r#"
        @id(TYPE-001)
        @rationale("Ensures valid ages")
        type Age = Int where self >= 0 and self < 150
    "#).unwrap();

    assert!(spec.type_aliases[0].attributes.len() == 2);
    assert!(spec.type_aliases[0].refinement.is_some());
}

#[test]
fn test_scenario_with_alternatives_and_attributes() {
    // Phase 8 + Phase 9
    let spec = parse(r#"
        @id(SCENARIO-001)
        scenario "login" {
            given { ... }
            when { ... }
            then { ... }

            @id(SCENARIO-001-ALT1)
            alt "wrong password" { ... }
        }
    "#).unwrap();
}

#[test]
fn test_nfr_on_composed_state() {
    // Phase 10 + Phase 11
    let spec = parse(r#"
        state A { x: Int }
        state B { y: Int }
        state C = A and B

        quality performance "combined perf" {
            metric: latency,
            target: < 100ms,
            applies_to: state C,
        }
    "#).unwrap();

    // Verify NFR correctly references composed state
}

#[test]
fn test_full_integration() {
    // All phases together
    let spec = parse(r#"
        @id(TYPE-001)
        type UserId = Int where self > 0

        @id(STATE-001)
        state BaseAuth {
            users: Set<User>,
            invariant "unique" { ... }
        }

        @id(STATE-002)
        state FullAuth extends BaseAuth {
            sessions: Map<Token, UserId>,
        }

        @id(SCENARIO-001)
        scenario "login" {
            given { ... }
            when { ... }
            then { ... }

            @id(SCENARIO-001-ALT1)
            alt "invalid credentials" { ... }
        }

        @derives_from(STATE-002)
        quality reliability "auth uptime" {
            metric: uptime,
            target: >= 99.9%,
            applies_to: state FullAuth,
        }
    "#).unwrap();

    // Full end-to-end validation
}
```

---

## Conclusion

### Compatibility Assessment

| Assessment | Status |
|------------|--------|
| Lexer conflicts | ✅ None |
| Parser conflicts | ✅ None |
| AST conflicts | ✅ None |
| Semantic conflicts | ✅ Manageable with ordering |
| Expression language | ✅ Fully shared |
| Type system | ✅ Extended, not conflicting |

### Risk Areas (Mitigated)

1. **State composition + NFR inheritance**: Resolved by explicit-only NFR binding
2. **Semantic analysis ordering**: Resolved by defined pass order
3. **Self keyword scope**: Resolved by context tracking

### Final Recommendation

**These phases are designed to integrate cleanly.** The key success factors:

1. **Shared expression language** - One parser, many uses
2. **Orthogonal concerns** - Each phase addresses different specification aspects
3. **Additive changes** - New constructs, not modified semantics
4. **Clear layering** - Types → States → Behavior → Quality → Metadata

**Proceed with implementation in the revised order:**
1. Phase 8 (Traceability) - Foundation
2. Phase 7 (Refinement Types) - Type system
3. Phase 9 (Alternative Flows) - Scenarios
4. Phase 11 (Schema Composition) - States
5. Phase 10 (NFRs) - Quality layer
