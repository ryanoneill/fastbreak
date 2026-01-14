---
description: Verify that spec files match Rust implementation (exhaustive field-by-field check)
---

Perform an exhaustive verification that the Fastbreak specification files (`spec/specs/main.fbrk` and `spec/specs/compiler.fbrk`) match the Rust implementation.

You MUST:
1. Read ALL spec files and ALL corresponding Rust source files
2. Compare EVERY type field-by-field
3. Compare EVERY enum variant-by-variant
4. Report ALL discrepancies found
5. Fix any discrepancies (spec first, then code)

Follow the detailed procedure in `.claude/skills/verify-spec/SKILL.md`.

Known conventions that are NOT discrepancies:
- String ↔ SmolStr (interned string optimization)
- Int ↔ i64/usize (generic integer)
- List<T> ↔ Vec<T> (dynamic array)
- Expression ↔ Expr (shortened name)
- ExpressionKind ↔ ExprKind (shortened name)

Conceptual spec-only types (documented with comments):
- PrimitiveType, QuantifierKind, IntLiteral, FloatLiteral, StringLiteral, BoolLiteral, BlockExpression

Do NOT consider the verification complete until you have checked every single type and enum in the checklist.
