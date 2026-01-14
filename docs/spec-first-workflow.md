# Spec-First TDD Workflow

This document describes the required workflow for making changes to Fastbreak. All changes follow a **spec-first, test-driven** approach.

## The Golden Rule

**Never write implementation code before updating the spec.**

The spec files (`spec/specs/*.fbrk`) are the source of truth. Code implements the spec, not the other way around.

## Workflow Steps

### 1. Update the Specification First

Before writing any code, update the appropriate spec file:

- `spec/specs/main.fbrk` - Core language types, enums, state, actions
- `spec/specs/compiler.fbrk` - Compiler internals, tokens, semantic types

**What to add:**
- New `type` definitions with all fields
- New `enum` definitions with all variants
- New `action` definitions with contracts (requires/ensures)
- New `scenario` definitions showing expected behavior
- New `property` definitions for invariants

**Example - Adding a new AST node:**
```fbrk
// In main.fbrk
type NewFeature {
    name: Ident,
    value: Expression,
    span: Span,
}
```

### 2. Validate the Specification

Run the spec checker to ensure your additions are syntactically valid:

```bash
cargo run -- check spec/specs/
```

Fix any parse or validation errors before proceeding.

### 3. Write a Failing Test (TDD Red Phase)

Write a test that exercises the new functionality. The test MUST fail initially.

**For parser changes:**
```rust
#[test]
fn test_parse_new_feature() {
    let spec = parse("new_feature foo { value = 42 }").unwrap();
    assert_eq!(spec.new_features.len(), 1);
    // This will fail - NewFeature doesn't exist yet
}
```

**For semantic analysis changes:**
```rust
#[test]
fn test_analyze_new_feature() {
    let result = analyze("new_feature foo { value = bar }");
    // Should report undefined 'bar'
    assert!(result.errors.iter().any(|e| e.contains("undefined")));
}
```

**For behavior changes (from scenarios):**
```rust
#[test]
fn test_new_feature_scenario() {
    // Test based on scenario in spec
}
```

### 4. Implement the Code (TDD Green Phase)

Now implement the minimum code needed to make the test pass:

1. Add the Rust type/enum matching the spec
2. Update the parser if needed
3. Update semantic analysis if needed
4. Update any affected codegen

**Keep implementations minimal** - only what's needed to pass the test.

### 5. Refactor (TDD Refactor Phase)

Clean up the implementation:
- Remove duplication
- Improve naming
- Ensure code follows project conventions

All tests must still pass after refactoring.

### 6. Verify Spec-Code Alignment

Run the verification to ensure spec and code match:

```bash
/verify-spec
```

Or manually verify using `docs/spec-verification.md`.

### 7. Run All Tests

Ensure nothing is broken:

```bash
cargo test --quiet
```

All 155+ tests must pass.

### 8. Commit

Commit with a descriptive message:

```bash
git add -A
git commit -S -m "Add NewFeature support

- Update spec with NewFeature type definition
- Add parser support for new_feature blocks
- Add semantic analysis for NewFeature
- Add tests for parsing and analysis

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

## Quick Reference

| Step | Command | Purpose |
|------|---------|---------|
| 1 | Edit `spec/specs/*.fbrk` | Define the change |
| 2 | `cargo run -- check spec/specs/` | Validate spec |
| 3 | Write test | TDD Red |
| 4 | Implement | TDD Green |
| 5 | Refactor | TDD Refactor |
| 6 | `/verify-spec` | Ensure alignment |
| 7 | `cargo test --quiet` | All tests pass |
| 8 | `git commit -S` | Commit changes |

## Anti-Patterns to Avoid

1. **Writing code before spec** - Always spec first
2. **Skipping tests** - Every change needs tests
3. **Large commits** - Small, focused changes
4. **Ignoring spec validation** - Specs must parse cleanly
5. **Skipping verification** - Always verify spec matches code

## Example: Adding Action Calls in Invariants

This feature was added following this workflow:

1. **Spec**: Added comment to `Invariant` type noting actions can be called
2. **Test**: Added `test_analyze_invariant_with_action_call`
3. **Implement**: Added `pre_register_action()` to register actions before states
4. **Verify**: Ran `/verify-spec` to confirm alignment
5. **Commit**: Single focused commit with the change

## See Also

- [Spec Verification Instructions](spec-verification.md)
- [CLAUDE.md](../CLAUDE.md) - Project guidelines
