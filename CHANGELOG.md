# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.5.0] - 2025-XX-XX

### Added

#### Core Parsing Infrastructure

- **LL(k) Parser Backend** (`backend-ll` feature): Top-down predictive parsing with configurable lookahead
  - Supports left-recursion elimination
  - Configurable error recovery mechanisms
  - Incremental parsing support
  - Enhanced parsing table construction
  - Improved error recovery strategies

- **LR Parser Backend** (`backend-lr` feature): Bottom-up shift-reduce parsing
  - Supports LALR and canonical LR table construction
  - Enhanced state machine generation
  - Improved conflict resolution
  - Better error recovery
  - Table-based parsing

- **GLR Parser Backend** (`backend-glr` feature): Generalized LR parsing for ambiguous grammars
  - Tomita-style GLR parsing implementation
  - Parse forest representation (`ParseForest`, `ForestNode`) with ambiguity tracking
  - Stack management (`GlrStack`) with configurable pruning and parallel merge strategies
  - Disambiguation helpers for precedence/associativity plus custom strategies
  - Incremental-aware `parse_with_session()` support and dedicated `GlrConfig`
  - Comprehensive GLR regression tests and libFuzzer targets

- **Parser Backend Utilities**: Shared helpers in `backend::common` for grammar validation reused across LL/LR/GLR implementations

- **Unified Parser Backend Trait**: `ParserBackend` trait providing a common interface for all parser backends
  - Backend capabilities reporting
  - Grammar validation hooks
  - Incremental parsing support

#### Lexer

- **Advanced DFA-Based Lexer**: Complete lexer implementation with DFA-based tokenization
  - NFA-to-DFA conversion using subset construction algorithm
  - Maximal Munch algorithm for longest match tokenization
  - Binary search optimization for character range lookups (O(log n) instead of O(n))
  - Keyword trie for efficient keyword matching
  - Incremental lexing support with `LexerState` for resumable tokenization
  - Support for literal patterns, regex fallback, and custom matchers
  - Fast-path ASCII character handling with Unicode fallback

- **Pattern-Based Rule Definition**: Flexible lexer builder API
  - `Pattern` enum supporting Literal, CharClass, Repeat, Regex, and Any patterns
  - `CharSet` type for efficient character class matching
  - Priority-based rule ordering for Maximal Munch
  - Support for trivia tokens and keyword post-processing

#### Incremental Parsing

- **Incremental Parsing Infrastructure**: Complete incremental parsing system
  - Parse cache with version-based invalidation
  - Node reuse from previous parses with efficient algorithms
  - Affected range computation for minimal re-parsing
  - `parse_incremental_with_grammar()` method for cache population
  - Cache eviction strategies
  - `IncrementalParser` wrapper for high-level incremental parsing
  - `IncrementalSession` for managing parse sessions
  - `TextEdit` API for representing source code changes
  - Reuse budget management for controlling cache behavior

#### Syntax Trees

- **Green/Red Tree Representation**: Immutable syntax tree implementation
  - Green tree for efficient storage and sharing
  - Red tree for convenient traversal and querying
  - Enhanced tree traversal APIs
  - Better memory efficiency through shared subtrees
  - `GreenNodeBuilder` for constructing syntax trees

- **Tree Traversal APIs** (`traversal` feature): Comprehensive tree navigation
  - Parent/child relationship queries
  - Sibling navigation
  - Ancestor/descendant traversal
  - Tree depth and position utilities

- **Visitor Pattern Support** (`visitor` feature): Type-safe tree visiting
  - `SyntaxVisitor` trait for custom tree visitors
  - `SyntaxWalker` for driving tree traversal
  - Pre-order and post-order visiting support

- **Query APIs** (`query` feature): XPath-like querying for syntax trees
  - `QueryBuilder` for building complex queries
  - `XPathQuery` for XPath-style node selection
  - Predicate-based node matching
  - Efficient query execution

- **Tree Utilities** (`tree-utils` feature): Additional tree manipulation tools
  - `TreeDiff` for comparing syntax trees
  - `TreeStats` for analyzing tree structure
  - `ValidationResult` for tree validation
  - `GreenNodeSpan` for tracking node positions

- **Typed AST Infrastructure**: Foundation for strongly-typed AST wrappers
  - `syntax::ast` module with `AstNode` and `FromSyntaxNode` traits
  - `sipha_derive` crate with `AstNode` derive macro
  - Scaffolding for generating strongly-typed node APIs from grammars
  - Typed wrappers over red tree nodes

- **Text Position Utilities**: Source position and range tracking
  - `TextSize` for representing byte offsets in UTF-8 text
  - `TextRange` for representing spans of text (start and end positions)
  - Arithmetic operations on text positions
  - `LineCol` struct for line/column positions
  - `LineIndex` for efficient byte offset to line/column conversion
  - O(log n) binary search lookups for line starts
  - Essential for LSP integration and error reporting

#### Grammar System

- **Grammar Builder API**: Declarative grammar definition
  - `GrammarBuilder` for building context-free grammars
  - Expression combinators (Sequence, Choice, Repeat, etc.)
  - Rule construction and validation
  - Enhanced expression building capabilities
  - Backend hints system (`BackendHint` trait) for providing parser-specific metadata
  - `PrecedenceHint` for operator precedence and associativity specification

- **Grammar Validation**: Comprehensive grammar analysis
  - Left recursion detection
  - Conflict detection
  - Nullable rule analysis
  - Grammar error reporting

- **Grammar Analysis Module** (`grammar::analysis`): Tools for analyzing and optimizing grammars
  - `GrammarMetrics` struct for computing complexity metrics
    - Rule count, depth, nullable rules, left recursion analysis
  - `analyze_grammar()` function for optimization suggestions
  - `grammar_summary()` function for grammar characteristics
  - FIRST/FOLLOW set analysis

- **Grammar Documentation Generation** (`grammar-docs` feature): Comprehensive markdown documentation generator
  - EBNF representation of grammar rules
  - Statistics table with rule counts, token counts, and recursion analysis
  - Rule dependency graphs (Mermaid format)
  - Token reference tables with literal examples and usage context
  - Grammar design notes (error recovery, nullable rules, recursion patterns)
  - Configurable documentation output via `MarkdownConfig`
  - Support for rule and token descriptions
  - Enhanced token usage analysis (by rule and by token)

#### Error Handling

- **Comprehensive Error Types**: Rich error representation
  - `ParseError` enum with variants:
    - `UnexpectedToken` with expected tokens list
    - `InvalidSyntax` with detailed messages
    - `UnexpectedEof` with expected tokens
    - `Ambiguity` with alternative parse trees
  - `LexerError` with span information and error kinds:
    - `UnexpectedChar` for invalid characters
    - `UnterminatedString` for string literal errors
    - `InvalidEscape` for escape sequence errors
    - `InvalidNumber` for number format errors
    - `UnexpectedEof` for premature end of input
  - Enhanced error messages with better context
  - `ParseWarning` for non-fatal parsing issues
  - `ParseMetrics` for parsing performance statistics

- **Rich Diagnostics** (`diagnostics` feature): Beautiful error messages
  - Integration with `miette` for rich diagnostic formatting
  - Source code spans and labels
  - Error codes and suggestions
  - Multi-line error context

- **Error Recovery Strategies**: Configurable error recovery
  - Synchronization tokens for skipping to recovery points
  - Delimited recovery for matching delimiters
  - Best-effort parsing for continuing despite errors
  - `Grammar::try_get_fallback_kind()` helper method

#### Language Server Protocol Integration

- **LSP Integration Crate** (`sipha_lsp`): Conversion traits for LSP integration
  - `ToDiagnostic` for converting `ParseError` to `lsp_types::Diagnostic`
  - `ToRange` and `ToRangeWithSource` for converting `TextRange` to `lsp_types::Range`
  - `ToDocumentSymbol` for converting `SyntaxNode` to `lsp_types::DocumentSymbol`
  - `SyntaxKindToSymbolKind` for mapping syntax kinds to LSP symbol kinds
  - Helper functions for common LSP operations

#### Development Tools

- **Fuzzing Support**: libFuzzer targets for parser testing
  - Fuzzing targets for LL parser backend
  - Fuzzing targets for incremental parsing
  - Fuzzing targets for GLR parser backend
  - Located in `fuzz/` workspace package

- **Benchmarking Suite**: Comprehensive performance benchmarks
  - `parsing_bench` benchmark suite using Criterion
  - Full parse benchmarks
  - Incremental parse benchmarks
  - Grammar analysis benchmarks

- **Comprehensive Test Coverage**: Extensive test suites
  - `incremental_tests.rs` for incremental parsing functionality
  - `glr_parser_tests.rs` for GLR parsing
  - `lr_parser_tests.rs` for LR parsing
  - `lexer_tests.rs` with DFA coverage
  - `grammar_tests.rs` for grammar validation
  - `integration_tests.rs` for end-to-end scenarios
  - `error_helpers.rs` for error handling utilities

- **Example Programs**: Demonstrative examples
  - `basic_arithmetic.rs` - Basic arithmetic expression parsing
  - `simple_calculator.rs` - Calculator example
  - `glr_parsing.rs` - GLR parsing with ambiguity handling
  - `incremental_parsing.rs` - Incremental parsing demonstration
  - `json_grammar_docs.rs` - Grammar documentation generation

#### Build and Infrastructure

- **Feature Flags**: Modular feature system
  - `backend-ll`, `backend-lr`, `backend-glr` for parser backends
  - `diagnostics` for miette integration
  - `unicode` for Unicode support
  - `query`, `visitor`, `traversal`, `tree-utils` for syntax tree features
  - `grammar-docs` for documentation generation
  - `parallel` for parallel processing (rayon)
  - `profiling` for performance profiling (puffin)
  - `serialize` for serialization support
  - `arena` for arena allocation (bumpalo)

- **Unicode Support** (`unicode` feature): Full Unicode handling
  - Unicode identifier recognition
  - Unicode normalization
  - Unicode-aware character class matching

- **CI/CD**: GitHub Actions workflow
  - Automated testing and linting
  - Comprehensive clippy configuration with pedantic lints

- **Project Infrastructure**:
  - MIT License
  - `.gitignore` with standard Rust patterns
  - Repository metadata in `Cargo.toml`
  - Workspace structure with multiple crates

### Changed

- **Lexer API**: Complete redesign of lexer builder and compilation
  - Pattern-based rule definition with `Pattern` enum (Literal, CharClass, Repeat, Regex, Any)
  - `CharSet` type for efficient character class matching
  - Priority-based rule ordering for Maximal Munch
  - Support for trivia tokens and keyword post-processing
  - Migration from old lexer API to new pattern-based API

- **LL Parser Backend**: Major refactoring and improvements
  - Enhanced error recovery mechanisms
  - Improved parsing table construction
  - Better incremental parsing support
  - More robust state management

- **LR Parser Backend**: Significant improvements to LR table construction
  - Enhanced state machine generation
  - Improved conflict resolution
  - Better error recovery
  - More efficient table representation

- **Incremental Parser**: Complete rewrite with cache management
  - Version-based cache invalidation
  - Efficient node reuse algorithms
  - Smart affected range computation
  - Improved cache management APIs

- **Syntax Tree**: Improvements to green/red tree implementation
  - Enhanced tree traversal APIs
  - Better memory efficiency through improved sharing
  - Improved query APIs with better performance
  - More efficient tree construction

- **Grammar Builder**: Enhanced grammar definition API
  - Improved rule construction with better validation
  - Enhanced expression building with more combinators
  - Better error messages for invalid grammars

- **Error Handling**: Replaced panic calls in production code with proper error types
  - `GreenNodeBuilder::token()` now returns `Result<(), BuilderError>` instead of panicking
  - Enhanced error messages for configuration errors in parser backends
  - More graceful error handling throughout the codebase

- **Examples**: Updated all examples to use new lexer API
  - Migration to pattern-based lexer definitions
  - Updated error handling patterns
  - Improved code clarity and documentation

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

[0.5.0]: https://github.com/nyaleph/sipha/releases/tag/v0.5.0
