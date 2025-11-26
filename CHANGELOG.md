# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- GitHub Actions CI/CD workflow for automated testing and linting
- Comprehensive clippy configuration with pedantic lints
- MIT License file
- `.gitignore` file with standard Rust patterns
- Repository metadata in `Cargo.toml`

### Changed
- Improved error handling: replaced panic calls in production code with proper error types
- `GreenNodeBuilder::token()` now returns `Result<(), BuilderError>` instead of panicking
- Enhanced error messages for configuration errors in parser backends
- Added `Grammar::try_get_fallback_kind()` helper method for better error recovery

### Fixed
- Fixed test code to handle new `Result` return type from `token()` method
- Improved error messages when syntax kind cannot be determined

## [0.5.0] - 2024-XX-XX

### Added
- Initial release of Sipha parsing library
- LL(k) parser backend with configurable lookahead
- LR parser backend (basic implementation)
- Incremental parsing infrastructure
- Green/red syntax tree representation
- Lexer with pattern matching
- Grammar builder API
- Error types and diagnostics support
- Syntax tree traversal and query APIs
- Visitor pattern support
- Unicode support (optional feature)
- Rich diagnostics with miette integration (optional feature)

[Unreleased]: https://github.com/nyaleph/sipha/compare/v0.5.0...HEAD
[0.5.0]: https://github.com/nyaleph/sipha/releases/tag/v0.5.0

