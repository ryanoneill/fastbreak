---
description: Verify that spec files match Rust implementation (exhaustive field-by-field check)
---

Perform an exhaustive verification that the Fastbreak specification files match the Rust implementation.

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
- `src/lexer/token.rs` (Token)

### Step 2: Compare Every Type

For EACH type in the spec, verify against Rust:
1. Find the corresponding Rust struct
2. Check EVERY field: name matches, type matches
3. Check for missing fields in spec
4. Check for extra fields in code

### Step 3: Compare Every Enum

For EACH enum in the spec, verify against Rust:
1. Find the corresponding Rust enum
2. Check EVERY variant: name matches, associated data matches
3. Check for missing/extra variants

### Step 4: Report and Fix

Report all discrepancies found. Fix any issues (spec first, then code).

## Known Conventions (NOT discrepancies)

| Spec | Rust | Reason |
|------|------|--------|
| String | SmolStr | Interned string optimization |
| Int | i64, usize | Generic integer |
| List<T> | Vec<T> | Dynamic array |
| Expression | Expr | Shortened name |
| ExpressionKind | ExprKind | Shortened name |

## Conceptual Types (spec-only, documented)

- PrimitiveType, QuantifierKind, IntLiteral, FloatLiteral, StringLiteral, BoolLiteral, BlockExpression

Do NOT consider verification complete until every type and enum has been checked.
