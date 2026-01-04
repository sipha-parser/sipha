# Contributing

Thank you for your interest in contributing to Sipha! This document provides guidelines and instructions for contributing.

## Code of Conduct

This project adheres to the Rust Code of Conduct. By participating, you are expected to uphold this code.

## Getting Started

### Prerequisites

- Rust toolchain (stable, beta, or nightly)
- `cargo` (comes with Rust)
- `rustfmt` and `clippy` (install with `rustup component add rustfmt clippy`)

### Development Setup

1. Fork the repository and clone your fork:
   ```bash
   git clone https://github.com/yourusername/sipha.git
   cd sipha
   ```

2. Build the project:
   ```bash
   cargo build
   ```

3. Run the test suite:
   ```bash
   cargo test
   ```

4. Run clippy:
   ```bash
   cargo clippy --all-targets --all-features --workspace
   ```

5. Check formatting:
   ```bash
   cargo fmt --all -- --check
   ```

## Development Workflow

1. **Create a branch** for your changes:
   ```bash
   git checkout -b feature/your-feature-name
   # or
   git checkout -b fix/your-bug-fix
   ```

2. **Make your changes** following the coding standards below.

3. **Write or update tests** for your changes.

4. **Ensure all tests pass**:
   ```bash
   cargo test --all-features
   ```

5. **Run clippy** and fix any warnings:
   ```bash
   cargo clippy --all-targets --all-features --workspace
   ```

6. **Format your code**:
   ```bash
   cargo fmt --all
   ```

7. **Commit your changes** with a clear, descriptive commit message:
   ```bash
   git commit -m "Add feature: description of what you added"
   ```

8. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

9. **Open a Pull Request** on GitHub with a clear description of your changes.

## Coding Standards

### Code Style

- Follow Rust's standard formatting conventions. Run `cargo fmt` before committing.
- Use `cargo clippy` to catch common issues and follow Rust best practices.
- We use clippy with workspace-level configuration in `Cargo.toml` - some pedantic lints are allowed, but try to follow them when possible.

### Documentation

- Document all public APIs with doc comments.
- Include examples in doc comments where helpful.
- Update the README if you add new features or change existing behavior.

### Testing

- Write tests for new features and bug fixes.
- Aim for good test coverage, especially for critical paths.
- Include both unit tests and integration tests where appropriate.
- Test with different feature combinations when applicable.

### Error Handling

- Prefer `Result` types over panicking in library code.
- Provide clear, actionable error messages.
- Use appropriate error types from the `error` module.

### Commit Messages

- Use clear, descriptive commit messages.
- Start with a verb in imperative mood (e.g., "Add", "Fix", "Update").
- Reference issue numbers when applicable (e.g., "Fix #123: description").

Example:
```text
Add incremental parsing cache invalidation

Implements cache invalidation for incremental parsing when text edits
occur. This improves performance by only re-parsing affected regions.

Fixes #42
```

## Pull Request Process

1. **Ensure your PR is up to date** with the target branch (usually `trunk`).

2. **Write a clear description** of what your PR does and why.

3. **Reference related issues** if applicable.

4. **Ensure CI passes** - all tests, clippy checks, and formatting checks must pass.

5. **Request review** from maintainers.

6. **Address feedback** promptly and update your PR as needed.

## Feature Requests

If you have an idea for a new feature:

1. Check existing issues to see if it's already been discussed.
2. Open an issue describing the feature, its use case, and potential implementation approach.
3. Wait for maintainer feedback before implementing.

## Bug Reports

When reporting bugs, please include:

- A clear description of the bug
- Steps to reproduce
- Expected behavior
- Actual behavior
- Your environment (Rust version, OS, etc.)
- Minimal code example if possible

## Fuzz Testing

Sipha includes fuzz testing infrastructure to ensure robustness and correctness. Fuzz tests help find edge cases and potential panics in the parser.

### Prerequisites

Install `cargo-fuzz`:

```bash
cargo install cargo-fuzz
```

### Running Fuzz Tests

Navigate to the `fuzz` directory and run the fuzz targets:

```bash
cd fuzz
cargo fuzz run parser_fuzz
cargo fuzz run incremental_fuzz
```

### Fuzz Targets

- **`parser_fuzz`**: Tests the parser with random byte streams to ensure no panics occur.
- **`incremental_fuzz`**: Differential fuzzing that compares incremental parsing results with full re-parsing to ensure correctness.

### Adding New Fuzz Targets

To add a new fuzz target:

1. Create a new file in `fuzz/fuzz_targets/` with your fuzz target code
2. Add a `[[bin]]` entry in `fuzz/Cargo.toml` pointing to your target
3. Run `cargo fuzz list` to verify the target is registered

## Questions?

Feel free to open an issue for questions or discussions. We're happy to help!

## License

By contributing to Sipha, you agree that your contributions will be licensed under the MIT License.

