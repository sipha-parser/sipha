# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Grammar Analysis Module**: New `grammar::analysis` module with tools for analyzing and optimizing grammars
  - `GrammarMetrics` struct for computing complexity metrics (rule count, depth, nullable rules, left recursion)
  - `analyze_grammar()` function for optimization suggestions
  - `grammar_summary()` function for grammar characteristics
- **Advanced Lexer with DFA**: Complete rewrite of lexer with DFA-based tokenization
  - NFA-to-DFA conversion using subset construction algorithm
  - Maximal Munch algorithm for longest match tokenization
  - Binary search optimization for character range lookups (O(log n) instead of O(n))
  - Keyword trie for efficient keyword matching
  - Incremental lexing support with `LexerState` for resumable tokenization
  - Support for literal patterns, regex fallback, and custom matchers
  - Fast-path ASCII character handling with Unicode fallback
- **Enhanced Incremental Parsing**: Major improvements to incremental parsing infrastructure
  - Parse cache with version-based invalidation
  - Node reuse from previous parses
  - Affected range computation for minimal re-parsing
  - `parse_incremental_with_grammar()` method for cache population
  - Cache eviction strategies
- **Expanded Error Handling**: Significantly expanded error types and diagnostics
  - New error kinds: `InvalidEscape`, `InvalidNumber`, `UnexpectedToken`, `UnexpectedEof`, `Ambiguity`
  - Enhanced error messages with better context
  - Improved error recovery strategies
- **Benchmarking Suite**: New `parsing_bench` benchmark suite
  - Full parse benchmarks
  - Incremental parse benchmarks
  - Grammar analysis benchmarks
- **Test Coverage**: New comprehensive test suites
  - `incremental_tests.rs` for incremental parsing functionality
  - Enhanced lexer tests with DFA coverage
  - Improved integration tests
- GitHub Actions CI/CD workflow for automated testing and linting
- Comprehensive clippy configuration with pedantic lints
- MIT License file
- `.gitignore` file with standard Rust patterns
- Repository metadata in `Cargo.toml`

### Changed
- **Lexer API**: Complete redesign of lexer builder and compilation
  - Pattern-based rule definition with `Pattern` enum (Literal, CharClass, Repeat, Regex, Any)
  - `CharSet` type for efficient character class matching
  - Priority-based rule ordering for Maximal Munch
  - Support for trivia tokens and keyword post-processing
- **LL Parser Backend**: Major refactoring and improvements (1073 lines changed)
  - Enhanced error recovery mechanisms
  - Improved parsing table construction
  - Better incremental parsing support
- **LR Parser Backend**: Significant improvements to LR table construction (932 lines changed)
  - Enhanced state machine generation
  - Improved conflict resolution
  - Better error recovery
- **Incremental Parser**: Complete rewrite with cache management
  - Version-based cache invalidation
  - Efficient node reuse algorithms
  - Smart affected range computation
- **Syntax Tree**: Improvements to green/red tree implementation
  - Enhanced tree traversal APIs
  - Better memory efficiency
  - Improved query APIs
- **Grammar Builder**: Enhanced grammar definition API
  - Improved rule construction
  - Better validation
  - Enhanced expression building
- Improved error handling: replaced panic calls in production code with proper error types
- `GreenNodeBuilder::token()` now returns `Result<(), BuilderError>` instead of panicking
- Enhanced error messages for configuration errors in parser backends
- Added `Grammar::try_get_fallback_kind()` helper method for better error recovery
- Updated examples to use new lexer API

### Fixed
- Fixed test code to handle new `Result` return type from `token()` method
- Improved error messages when syntax kind cannot be determined
- Fixed all clippy warnings and errors:
  - Fixed empty line after outer attribute
  - Added missing backticks in documentation
  - Added missing `# Panics` and `# Errors` sections to documentation
  - Fixed unnecessary boolean `not` operation
  - Removed unnecessary `Result<(), LexerError>` wrapper from `nfa_to_dfa` function
  - Added `#[must_use]` attributes and `Default` implementation for `LexerState`
  - Fixed unused `self` arguments by converting methods to associated functions
  - Fixed unnecessary literal bound in `analysis.rs`
  - Fixed private interfaces warning by making `NfaState` `pub(crate)`
  - Fixed explicit counter loop, redundant closure, and `while_let_on_iterator` warnings
  - Collapsed multiple nested if statements
  - Fixed various other style and documentation issues

## [0.5.0] - 2025-XX-XX

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

