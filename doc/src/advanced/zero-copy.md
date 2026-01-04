# Zero-Copy Parsing

This document describes Sipha's zero-copy capabilities, current limitations, and opportunities for improvement.

## What is Zero-Copy?

Zero-copy parsing means avoiding unnecessary copying of input data during parsing. In an ideal zero-copy parser:

- Tokens reference the original source text directly (via string slices)
- No intermediate string allocations for token text
- Minimal memory overhead for token storage

## Current Status: Partial Zero-Copy

Sipha currently provides **partial** zero-copy support:

### What Works (Zero-Copy)

1. **Token Text Storage**: Tokens use `CompactString` which can store small strings inline without heap allocation
2. **Source Text References**: The lexer can work with string slices from the original source
3. **Tree Sharing**: Green trees use `Arc` for sharing, avoiding copies of tree structure

### Current Limitations

1. **Token Text Copying**: While `CompactString` is efficient, it still copies token text from the source. For tokens longer than the inline capacity (~24 bytes), heap allocation occurs.

2. **Token Storage**: The `Token` struct stores a `CompactString` rather than a reference to the original source. This means:
   - Token text is copied during tokenization
   - Tokens can outlive the source text (which is good for some use cases)
   - Memory overhead for storing token text

3. **Grammar Matching**: Grammar rules match tokens by kind, not by text content, which is good for zero-copy in principle, but token creation still requires text copying.

4. **Incremental Parsing**: When re-tokenizing after edits, unchanged tokens may be recreated rather than reused, leading to additional copies.

## Why Not Full Zero-Copy?

Sipha prioritizes:

1. **API Ergonomics**: Tokens that own their text are easier to work with and don't require lifetime management
2. **Incremental Parsing**: Tokens need to persist across parse sessions, which requires owned data
3. **Flexibility**: Users can modify source text without invalidating tokens

## Opportunities for Improvement

### 1. Token Text Interning

**Current**: Each token stores its own text copy.

**Improvement**: Use string interning to share token text across tokens with the same content.

**Benefits**:
- Reduced memory usage for repeated tokens (keywords, operators)
- Faster token comparison
- Better cache locality

**Implementation**: Use `lasso` or similar interning library (already used for rule names).

### 2. Lazy Token Text

**Current**: Token text is always copied during tokenization.

**Improvement**: Store token text as a reference when possible, only copying when needed.

**Benefits**:
- Zero-copy for the common case
- Copy only when token outlives source or is modified

**Challenges**:
- Lifetime management complexity
- API changes needed

### 3. Token Reuse in Incremental Lexing

**Current**: Incremental lexing reuses token positions but may recreate token objects.

**Improvement**: Reuse token objects entirely when text hasn't changed.

**Benefits**:
- Fewer allocations during incremental updates
- Better performance for small edits

**Status**: Partially implemented - positions are reused, but token objects may be recreated.

### 4. Slice-Based Token API (Optional)

**Current**: All tokens own their text.

**Improvement**: Provide an optional `TokenRef<'a>` type that borrows from source.

**Benefits**:
- True zero-copy for batch parsing scenarios
- Lower memory usage for one-shot parsing

**Trade-offs**:
- More complex API with lifetimes
- Not suitable for incremental parsing

## Recommendations

### For Maximum Zero-Copy

If you need maximum zero-copy performance:

1. **Use string interning**: Enable the `intern` feature (if available) to share token text
2. **Batch parsing**: For one-shot parsing, consider using arena allocation
3. **Minimize token text access**: Access token kinds rather than text when possible
4. **Use compact strings**: `CompactString` already provides efficient storage for small tokens

### For Incremental Parsing

For incremental parsing scenarios (IDEs, language servers):

1. **Current approach is optimal**: Token ownership is necessary for cross-session reuse
2. **Focus on node reuse**: Zero-copy is less important than efficient node reuse
3. **Cache management**: Proper cache configuration reduces the need for zero-copy

## Future Work

Potential improvements for future releases:

1. **Hybrid token storage**: Use references when possible, owned strings when needed
2. **Token text interning**: Share token text across tokens (especially for keywords)
3. **Enhanced incremental lexing**: Better token object reuse
4. **Optional zero-copy API**: Provide lifetime-based API for batch parsing

## Comparison with Other Parsers

| Parser | Zero-Copy Support | Notes |
|--------|------------------|-------|
| **Sipha** | Partial | Tokens own text, but use `CompactString` for efficiency |
| **pest** | Full | Uses string slices directly |
| **nom** | Full | Works with byte slices |
| **lalrpop** | None | Tokens copy text |

Sipha's partial zero-copy is a deliberate design choice to support incremental parsing and API ergonomics while still providing reasonable memory efficiency through `CompactString`.

