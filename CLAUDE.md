# Fastbreak Project Guidelines

## Specification Integrity

When verifying that the fastbreak specification matches the code implementation:

- **Any difference between the spec and code that is not explicitly discussed and approved is considered a breakage**
- The spec files in `spec/specs/` must accurately reflect the actual implementation
- If a difference is found, either the spec or the code must be updated to match
- Document intentional differences with comments explaining the divergence
- **Follow the exhaustive verification procedure in [`docs/spec-verification.md`](docs/spec-verification.md)**
- Verification must be field-by-field and variant-by-variant - partial verification creates false confidence

## Testing

- Use Test-Driven Development (TDD) when adding new features
- All 154+ tests must pass before committing changes
- Run `cargo test --quiet` to verify

## Specification Files

- `spec/specs/main.fbrk` - Core language types and definitions
- `spec/specs/compiler.fbrk` - Compiler-specific types and scenarios
- Run `cargo run -- check spec/specs/` to validate specifications
