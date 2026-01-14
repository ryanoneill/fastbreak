---
name: verify-spec
description: Verify the Fastbreak specification files match the Rust implementation. Use when checking spec compliance, reviewing spec changes, or validating that types/enums in spec match code.
allowed-tools: Read, Grep, Glob, Task
---

# Verify Spec

Verify that the Fastbreak specification files match the Rust implementation.

## Procedure

You MUST perform an exhaustive field-by-field comparison. Partial verification is unacceptable.

### Step 1: Read All Source Files

Read these files in parallel:

**Spec files:**
- `spec/specs/main.fbrk`
- `spec/specs/compiler.fbrk`

**Rust AST files:**
- `src/lib.rs` (Span)
- `src/ast/mod.rs` (Ident, Path, Specification)
- `src/ast/attribute.rs` (Attribute, AttributeArg)
- `src/ast/types.rs` (TypeRef, TypeRefKind, BuiltInType, GenericArg, Field, TypeDef, EnumVariant, EnumDef, TypeAlias, Module, Import, ImportItem, Relation, RelationConstraint)
- `src/ast/state.rs` (StateBlock, StateField, Invariant, Action, ActionParam, Contract, ContractKind)
- `src/ast/scenario.rs` (Scenario, Alternative, GivenClause, WhenClause, ThenClause, Binding, Assertion)
- `src/ast/property.rs` (Property, TemporalOp)
- `src/ast/expr.rs` (Expr, ExprKind, BinaryOp, UnaryOp, Literal, QuantBinding, QuantBindingKind, FieldInit, LambdaParam, MatchArm, Pattern, PatternKind, FieldPattern)
- `src/ast/quality.rs` (Quality, QualityCategory, Scale, Constraint, QualityTarget, QualityOp, QualityValue, AppliesTo, AppliesToKind, MeasurementPeriod, LoadConditions, DurationUnit, SizeUnit, RateUnit, VerificationMethod, VerificationKind, QualityProperty, QualityPropertyValue)
- `src/semantic/modules.rs` (ModuleRegistry, ModuleInfo, ResolvedImport, ImportedItem)
- `src/semantic/types.rs` (Type, TypeId, StructInfo, EnumInfo, VariantInfo, StateInfo, ActionInfo, RelationInfo)
- `src/lexer/token.rs` (Token)

### Step 2: Compare Every Type

For EACH type in the spec, verify against Rust:

1. Find the corresponding Rust struct
2. Check EVERY field:
   - Name matches
   - Type matches (accounting for conventions below)
3. Check for missing fields in spec
4. Check for extra fields in code

### Step 3: Compare Every Enum

For EACH enum in the spec, verify against Rust:

1. Find the corresponding Rust enum
2. Check EVERY variant:
   - Name matches
   - Associated data matches (if any)
3. Check for missing variants in spec
4. Check for extra variants in code

### Step 4: Report Findings

Create a structured report:

```
## Verification Report

### Matches (X types, Y enums verified)
[List verified items]

### Discrepancies Found
[List each discrepancy with:
- Spec location (file:line)
- Rust location (file:line)
- Description of mismatch]

### Conceptual Types (spec-only, documented)
[List spec types marked as conceptual]
```

## Known Conventions (NOT discrepancies)

These differences are intentional and documented:

| Spec | Rust | Reason |
|------|------|--------|
| `String` | `SmolStr` | Interned string optimization |
| `Int` | `i64`, `usize`, `u64` | Generic integer |
| `List<T>` | `Vec<T>` | Dynamic array |
| `Expression` | `Expr` | Shortened name |
| `ExpressionKind` | `ExprKind` | Shortened name |

## Conceptual Types (spec-only)

These are documented in the spec with comments and don't require Rust counterparts:

- `PrimitiveType` - Conceptual subset of BuiltInType
- `QuantifierKind` - ForAll/Exists are ExprKind variants
- `IntLiteral`, `FloatLiteral`, `StringLiteral`, `BoolLiteral` - Values in Literal enum
- `BlockExpression` - ExprKind::Block(Vec<Expr>)

## Required Actions

If discrepancies are found:

1. **Spec is source of truth**: Update spec FIRST, then code
2. **Fix immediately**: Do not leave discrepancies unfixed
3. **Add documentation**: If intentional, add comment explaining why

## Verification Checklist

Use this to track progress:

### Core Types
- [ ] Ident: name, span
- [ ] Span: start, end
- [ ] Path: segments, span

### Attributes
- [ ] Attribute: name, args, span
- [ ] AttributeArg: String, Ident, Int variants

### Type System
- [ ] TypeRef: kind, span
- [ ] TypeRefKind: Named, BuiltIn, Generic, Function, Tuple, Unit
- [ ] BuiltInType: Int, String, Bool, Set, Map, List, Option, Result
- [ ] GenericArg: ty
- [ ] Field: name, ty, span
- [ ] TypeDef: attributes, name, type_params, fields, refinement, span
- [ ] EnumVariant: name, fields, span
- [ ] EnumDef: attributes, name, type_params, variants, span
- [ ] TypeAlias: attributes, name, type_params, base, refinement, span
- [ ] Module: path, span
- [ ] Import: path, items, span
- [ ] ImportItem: name, alias
- [ ] Relation: attributes, name, source, target, constraints, span
- [ ] RelationConstraint: Symmetric, Reflexive, Irreflexive, Transitive, Antisymmetric

### State and Actions
- [ ] StateBlock: attributes, name, fields, invariants, span
- [ ] StateField: name, ty, init, span
- [ ] Invariant: attributes, description, expr, span
- [ ] Action: attributes, name, params, return_type, contracts, body, span
- [ ] ActionParam: name, ty, span
- [ ] Contract: kind, expr, span
- [ ] ContractKind: Requires, Ensures

### Scenarios
- [ ] Scenario: attributes, description, given, when, then, alternatives, span
- [ ] Alternative: attributes, name, condition, given, when, then, span
- [ ] GivenClause: bindings, span
- [ ] WhenClause: bindings, span
- [ ] ThenClause: assertions, span
- [ ] Binding: name, value, span
- [ ] Assertion: expr, description, span

### Properties
- [ ] Property: attributes, description, temporal_op, expr, span
- [ ] TemporalOp: Always, Eventually

### Expressions
- [ ] Expression/Expr: kind, span
- [ ] ExpressionKind/ExprKind: all 26 variants
- [ ] BinaryOp: Add, Sub, Mul, Div, Mod, Eq, NotEq, Lt, LtEq, Gt, GtEq, And, Or, In, Union, Intersect, Difference
- [ ] UnaryOp: Not, Neg
- [ ] Literal: Int, Float, String, Bool, Unit
- [ ] QuantBinding: name, kind, span
- [ ] QuantBindingKind: InCollection, Typed
- [ ] FieldInit: name, value, span
- [ ] LambdaParam: name, ty
- [ ] MatchArm: pattern, guard, body, span
- [ ] Pattern: kind, span
- [ ] PatternKind: Wildcard, Binding, Literal, Tuple, Struct, Variant, Or
- [ ] FieldPattern: name, pattern, span

### Quality Requirements
- [ ] Quality: attributes, category, description, metric, scale, target, constraint, applies_to, measurement, under_load, verified_by, properties, span
- [ ] QualityCategory: Performance, Reliability, Security, Usability, Scalability, Maintainability
- [ ] Scale: Mean, Median, P50, P90, P95, P99, P999, Max, Min
- [ ] Constraint: Hard, Soft
- [ ] QualityTarget: op, value, span
- [ ] QualityOp: Lt, LtEq, Gt, GtEq, Eq
- [ ] QualityValue: Int, Percentage, Duration, Size, Rate, Expr
- [ ] AppliesTo: kind, name, span
- [ ] AppliesToKind: Action, Type, State
- [ ] MeasurementPeriod: PerRequest, PerSecond, PerMinute, Hourly, Daily, Weekly, Monthly
- [ ] LoadConditions: concurrent_users, concurrent_connections, requests_per_second, payload_size, duration, span
- [ ] DurationUnit: Ms, S, M, H
- [ ] SizeUnit: Kb, Mb, Gb, Tb
- [ ] RateUnit: PerSecond, PerMinute, PerHour
- [ ] VerificationMethod: kind, name, span
- [ ] VerificationKind: Test, Monitor, Benchmark, Audit
- [ ] QualityProperty: name, value, span
- [ ] QualityPropertyValue: Ident, Target, AppliesTo

### Modules
- [ ] ModuleRegistry: modules, current_module
- [ ] ModuleInfo: name, types, enums, states, actions, relations, imports
- [ ] ResolvedImport: source_module, items
- [ ] ImportedItem: original_name, local_name

### Specification
- [ ] Specification: module, imports, types, type_aliases, enums, relations, states, actions, scenarios, properties, qualities
