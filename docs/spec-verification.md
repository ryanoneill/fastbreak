# Specification Verification Instructions

This document provides exhaustive instructions for verifying that the Fastbreak specification files (`spec/specs/*.fbrk`) match the Rust implementation.

## Purpose

The spec files serve as the source of truth for the Fastbreak language. Any discrepancy between spec and code is a potential bug. Verification must be exhaustive - partial verification is worse than no verification because it creates false confidence.

## Source File Mappings

### main.fbrk Type Locations

| Spec Section | Rust File | Types/Enums |
|--------------|-----------|-------------|
| Core Types | `src/lib.rs` | Span |
| Core Types | `src/ast/mod.rs` | Ident, Path, Specification |
| Attributes | `src/ast/attribute.rs` | Attribute, AttributeArg |
| Type System | `src/ast/types.rs` | TypeRef, TypeRefKind, BuiltInType, GenericArg, Field, TypeDef, EnumVariant, EnumDef, TypeAlias, Module, Import, ImportItem, Relation, RelationConstraint |
| State/Action | `src/ast/state.rs` | StateBlock, StateField, Invariant, Action, ActionParam, Contract, ContractKind |
| Scenarios | `src/ast/scenario.rs` | Scenario, Alternative, GivenClause, WhenClause, ThenClause, Binding, Assertion |
| Properties | `src/ast/property.rs` | Property, TemporalOp |
| Expressions | `src/ast/expr.rs` | Expr (spec: Expression), ExprKind (spec: ExpressionKind), BinaryOp, UnaryOp, Literal, QuantBinding, QuantBindingKind, FieldInit, LambdaParam, MatchArm, Pattern, PatternKind, FieldPattern |
| Quality | `src/ast/quality.rs` | Quality, QualityCategory, Scale, Constraint, QualityTarget, QualityOp, QualityValue, AppliesTo, AppliesToKind, MeasurementPeriod, LoadConditions, DurationUnit, SizeUnit, RateUnit, VerificationMethod, VerificationKind, QualityProperty, QualityPropertyValue |
| Modules | `src/semantic/modules.rs` | ModuleRegistry, ModuleInfo, ResolvedImport, ImportedItem |
| Semantic Types | `src/semantic/types.rs` | Type (semantic), TypeId, TypeVarId, StructInfo, EnumInfo, VariantInfo, StateInfo, ActionInfo, RelationInfo |

### compiler.fbrk Type Locations

| Spec Section | Rust File | Types/Enums |
|--------------|-----------|-------------|
| Tokens | `src/lexer/token.rs` | Token |
| Semantic Types | `src/semantic/types.rs` | ResolvedType (semantic), etc. |

## Verification Procedure

### Step 1: Enumerate All Spec Types

For each spec file, list every `type` and `enum` definition:

```bash
grep -E "^(type|enum) " spec/specs/main.fbrk | wc -l
```

Expected for main.fbrk: ~76 types, ~26 enums

### Step 2: Field-by-Field Comparison

For EACH type in the spec, verify against the Rust code:

1. **Open both files side-by-side**
2. **Check every field**:
   - Field name matches
   - Field type matches (accounting for known conventions below)
   - No missing fields in spec
   - No extra fields in code

### Step 3: Variant-by-Variant Comparison

For EACH enum in the spec, verify against the Rust code:

1. **Check every variant**:
   - Variant name matches
   - Associated data matches (if any)
   - No missing variants in spec
   - No extra variants in code

### Step 4: Document Findings

Create a checklist file with every type/enum and its verification status.

## Known Conventions (Acceptable Differences)

These differences are intentional and should NOT be flagged as issues:

### Type Conventions

| Spec | Rust | Reason |
|------|------|--------|
| `String` | `SmolStr` | SmolStr is an optimization for interned strings |
| `Int` | `i64` or `usize` | Int represents any integer type |
| `List<T>` | `Vec<T>` | List is the spec name for dynamic arrays |
| `Expression` | `Expr` | Shortened name in implementation |
| `ExpressionKind` | `ExprKind` | Shortened name in implementation |

### Spec-Only Conceptual Types

These types exist in the spec for documentation but don't have direct code counterparts:

- `PrimitiveType` - Conceptual subset of BuiltInType
- `QuantifierKind` - ForAll/Exists are ExprKind variants directly
- `IntLiteral`, `FloatLiteral`, `StringLiteral`, `BoolLiteral` - Values are embedded in Literal enum variants
- `BlockExpression` - Block is ExprKind::Block(Vec<Expr>)

## Checklist Template

Use this template when performing verification:

```markdown
## main.fbrk Verification - [DATE]

### Core Types
- [ ] Ident - fields: name (String/SmolStr), span
- [ ] Span - fields: start (Int/usize), end (Int/usize)
- [ ] Path - fields: segments, span

### Attributes
- [ ] Attribute - fields: name, args, span
- [ ] AttributeArg - variants: String, Ident, Int

### Type System
- [ ] TypeRef - fields: kind, span
- [ ] TypeRefKind - variants: Named, BuiltIn, Generic, Function, Tuple, Unit
- [ ] BuiltInType - variants: Int, String, Bool, Set, Map, List, Option, Result
- [ ] GenericArg - fields: ty
- [ ] Field - fields: name, ty, span
- [ ] TypeDef - fields: attributes, name, type_params, fields, refinement, span
- [ ] EnumVariant - fields: name, fields, span
- [ ] EnumDef - fields: attributes, name, type_params, variants, span
- [ ] TypeAlias - fields: attributes, name, type_params, base, refinement, span
- [ ] Module - fields: path, span
- [ ] Import - fields: path, items, span
- [ ] ImportItem - fields: name, alias
- [ ] Relation - fields: attributes, name, source, target, constraints, span
- [ ] RelationConstraint - variants: Symmetric, Reflexive, Irreflexive, Transitive, Antisymmetric

### State and Actions
- [ ] StateBlock - fields: attributes, name, fields, invariants, span
- [ ] StateField - fields: name, ty, init, span
- [ ] Invariant - fields: attributes, description, expr, span
- [ ] Action - fields: attributes, name, params, return_type, contracts, body, span
- [ ] ActionParam - fields: name, ty, span
- [ ] Contract - fields: kind, expr, span
- [ ] ContractKind - variants: Requires, Ensures

### Scenarios
- [ ] Scenario - fields: attributes, description, given, when, then, alternatives, span
- [ ] Alternative - fields: attributes, name, condition, given, when, then, span
- [ ] GivenClause - fields: bindings, span
- [ ] WhenClause - fields: bindings, span
- [ ] ThenClause - fields: assertions, span
- [ ] Binding - fields: name, value, span
- [ ] Assertion - fields: expr, description, span

### Properties
- [ ] Property - fields: attributes, description, temporal_op, expr, span
- [ ] TemporalOp - variants: Always, Eventually

### Expressions
- [ ] Expression/Expr - fields: kind, span
- [ ] ExpressionKind/ExprKind - all 26 variants
- [ ] BinaryOp - variants: Add, Sub, Mul, Div, Mod, Eq, NotEq, Lt, LtEq, Gt, GtEq, And, Or, In, Union, Intersect, Difference
- [ ] UnaryOp - variants: Not, Neg
- [ ] Literal - variants: Int, Float, String, Bool, Unit
- [ ] QuantBinding - fields: name, kind, span
- [ ] QuantBindingKind - variants: InCollection, Typed
- [ ] FieldInit - fields: name, value, span
- [ ] LambdaParam - fields: name, ty
- [ ] MatchArm - fields: pattern, guard, body, span
- [ ] Pattern - fields: kind, span
- [ ] PatternKind - variants: Wildcard, Binding, Literal, Tuple, Struct, Variant, Or
- [ ] FieldPattern - fields: name, pattern, span

### Quality Requirements
- [ ] Quality - all 13 fields
- [ ] QualityCategory - 6 variants
- [ ] Scale - 9 variants
- [ ] Constraint - variants: Hard, Soft
- [ ] QualityTarget - fields: op, value, span
- [ ] QualityOp - 5 variants
- [ ] QualityValue - 6 variants
- [ ] AppliesTo - fields: kind, name, span
- [ ] AppliesToKind - variants: Action, Type, State
- [ ] MeasurementPeriod - 7 variants
- [ ] LoadConditions - fields: concurrent_users, concurrent_connections, requests_per_second, payload_size, duration, span
- [ ] DurationUnit - variants: Ms, S, M, H
- [ ] SizeUnit - variants: Kb, Mb, Gb, Tb
- [ ] RateUnit - variants: PerSecond, PerMinute, PerHour
- [ ] VerificationMethod - fields: kind, name, span
- [ ] VerificationKind - variants: Test, Monitor, Benchmark, Audit
- [ ] QualityProperty - fields: name, value, span
- [ ] QualityPropertyValue - variants: Ident, Target, AppliesTo

### Modules
- [ ] ModuleRegistry - fields: modules, current_module
- [ ] ModuleInfo - fields: name, types, enums, states, actions, relations, imports
- [ ] ResolvedImport - fields: source_module, items
- [ ] ImportedItem - fields: original_name, local_name

### Specification
- [ ] Specification - fields: module, imports, types, type_aliases, enums, relations, states, actions, scenarios, properties, qualities
```

## When to Perform Verification

1. After any change to AST types or enums
2. After any change to spec files
3. Before major releases
4. When requested by user

## Fixing Discrepancies

When a discrepancy is found:

1. **Spec is source of truth**: If the spec is correct, update the code
2. **Code is correct**: If the code is correct, update the spec
3. **Document intentional differences**: Add comments explaining any intentional divergence

Always modify the spec FIRST before making code changes (spec-first development).
