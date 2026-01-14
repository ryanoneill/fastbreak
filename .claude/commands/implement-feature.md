---
description: Implement a new feature following spec-first TDD workflow
---

Implement the requested feature following the **spec-first TDD workflow**:

## Required Order (DO NOT SKIP STEPS)

1. **SPEC FIRST** - Update `spec/specs/main.fbrk` or `compiler.fbrk` with:
   - New type definitions (all fields)
   - New enum definitions (all variants)
   - New action definitions (with contracts)
   - Scenarios showing expected behavior

2. **VALIDATE SPEC** - Run `cargo run -- check spec/specs/`

3. **WRITE FAILING TEST** - Test must fail initially (TDD Red)

4. **IMPLEMENT** - Minimum code to pass test (TDD Green)

5. **ALL TESTS PASS** - Run `cargo test --quiet`

6. **VERIFY SPEC** - Use `/verify-spec` to ensure alignment

7. **COMMIT** - With signed commit

## CRITICAL RULES

- **NEVER write Rust code before updating the spec**
- **NEVER write implementation before the failing test**
- **ALWAYS validate the spec before writing tests**
- **ALWAYS verify spec-code alignment before committing**

See `docs/spec-first-workflow.md` for complete details.

What feature would you like to implement?
