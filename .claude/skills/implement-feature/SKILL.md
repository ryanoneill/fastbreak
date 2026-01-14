---
name: implement-feature
description: Guide spec-first TDD implementation of new features. Use when adding new types, enums, actions, or capabilities to Fastbreak.
allowed-tools: Read, Write, Edit, Grep, Glob, Bash, Task
---

# Implement Feature (Spec-First TDD)

Guide the implementation of a new feature following the spec-first TDD workflow.

## CRITICAL: Order of Operations

You MUST follow this exact order. Violations break the spec-first principle.

### Phase 1: Specification (DO THIS FIRST)

1. **Understand the feature** - What types/enums/actions are needed?

2. **Update the spec file** - Edit `spec/specs/main.fbrk` or `spec/specs/compiler.fbrk`:
   - Add new `type` definitions with ALL fields
   - Add new `enum` definitions with ALL variants
   - Add new `action` definitions with contracts
   - Add new `scenario` definitions showing behavior
   - Add comments explaining the feature

3. **Validate the spec**:
   ```bash
   cargo run -- check spec/specs/
   ```
   Fix any errors before proceeding.

### Phase 2: Test (TDD Red)

4. **Write a failing test** - Create test in appropriate file:
   - Parser tests: `src/parser/tests.rs` or inline
   - Semantic tests: `src/semantic/mod.rs` (test module)
   - Integration tests: `tests/` directory

5. **Run test to confirm it fails**:
   ```bash
   cargo test <test_name> -- --nocapture
   ```
   The test MUST fail. If it passes, the test is wrong.

### Phase 3: Implementation (TDD Green)

6. **Implement minimum code** to make the test pass:
   - Add Rust struct/enum matching spec exactly
   - Update parser if syntax changes
   - Update semantic analysis if needed
   - Keep implementation minimal

7. **Run test to confirm it passes**:
   ```bash
   cargo test <test_name>
   ```

### Phase 4: Verification

8. **Run all tests**:
   ```bash
   cargo test --quiet
   ```

9. **Verify spec alignment**:
   ```bash
   # Use /verify-spec command or manually check
   ```

10. **Commit the change**:
    ```bash
    git add -A
    git commit -S -m "Description

    Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
    ```

## File Locations

| Change Type | Spec File | Rust File(s) |
|-------------|-----------|--------------|
| New AST type | `main.fbrk` | `src/ast/*.rs` |
| New AST enum | `main.fbrk` | `src/ast/*.rs` |
| Parser change | `main.fbrk` | `src/parser/mod.rs` |
| Semantic change | `main.fbrk` | `src/semantic/*.rs` |
| Token change | `compiler.fbrk` | `src/lexer/token.rs` |
| New action | `main.fbrk` | `src/ast/state.rs` |
| New scenario | `main.fbrk` or `compiler.fbrk` | Tests |

## Checklist for Each Feature

- [ ] Spec updated FIRST
- [ ] Spec validates cleanly
- [ ] Failing test written
- [ ] Test actually fails
- [ ] Implementation added
- [ ] Test passes
- [ ] All tests pass
- [ ] Spec verification passes
- [ ] Committed with sign-off

## Common Mistakes to Avoid

1. **Writing Rust code before spec** - NEVER do this
2. **Writing test after implementation** - Test must fail first
3. **Skipping spec validation** - Always run `cargo run -- check`
4. **Large changes** - Break into small, testable pieces
5. **Forgetting span fields** - Most AST types need `span: Span`

## Example Interaction

User: "Add support for type aliases with constraints"

Correct approach:
1. First, update `main.fbrk` with `TypeAlias` type
2. Add a scenario showing type alias behavior
3. Validate spec
4. Write test: `test_parse_type_alias`
5. Run test (fails)
6. Add `TypeAlias` struct to `src/ast/types.rs`
7. Add parser support
8. Run test (passes)
9. Run all tests
10. Verify spec
11. Commit
