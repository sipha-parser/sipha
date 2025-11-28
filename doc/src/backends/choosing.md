# Choosing a Backend

This guide helps you choose the right parsing backend for your use case.

## Quick Comparison

| Feature | LL(k) | LR | GLR |
|---------|-------|----|-----|
| Grammar compatibility | LL(k) | LR | Any |
| Ambiguity handling | No | No | Yes |
| Left recursion | With elimination | Yes | Yes |
| Error recovery | Good | Excellent | Good |
| Incremental parsing | Yes | Yes | Yes |
| Performance | Fast | Fast | Slower (ambiguous) |
| Table size | Small | Medium | Medium |
| Complexity | Low | Medium | High |

## Decision Tree

### 1. Is your grammar ambiguous?

- **Yes** → Use **GLR**
- **No** → Continue to step 2

### 2. Does your grammar have left recursion?

- **Yes, and you want to keep it** → Use **LR** or **GLR**
- **Yes, but you can eliminate it** → Use **LL(k)**
- **No** → Continue to step 3

### 3. What's your primary use case?

- **Interactive tools (IDEs, editors)** → Use **LL(k)** (simpler, faster)
- **Batch parsing** → Use **LR** (efficient, good error recovery)
- **Complex language (C++, etc.)** → Use **GLR** (handles ambiguities)

## Detailed Scenarios

### Scenario 1: Simple Expression Parser

**Grammar**: Arithmetic expressions with precedence

**Recommendation**: **LL(k)** with left-recursion elimination

**Reasoning**:
- Grammar is unambiguous
- Left recursion can be eliminated
- Simple and fast
- Good for interactive tools

### Scenario 2: Language Server

**Grammar**: Full programming language

**Recommendation**: **LL(k)** or **LR**

**Reasoning**:
- Need incremental parsing (both support it)
- LL(k) is simpler for error messages
- LR has better error recovery
- Choose based on grammar characteristics

### Scenario 3: C++ Parser

**Grammar**: C++ with template syntax ambiguities

**Recommendation**: **GLR**

**Reasoning**:
- Grammar has inherent ambiguities
- Need to handle multiple parse trees
- Can disambiguate using precedence/associativity

### Scenario 4: Batch Parser

**Grammar**: Any unambiguous grammar

**Recommendation**: **LR**

**Reasoning**:
- Efficient for batch processing
- Good error recovery
- Handles left recursion naturally

## Performance Considerations

### LL(k)

- **Fast**: O(n) parsing time
- **Small tables**: Efficient memory usage
- **Good for**: Interactive tools, small to medium files

### LR

- **Fast**: O(n) parsing time
- **Medium tables**: Reasonable memory usage
- **Good for**: Batch processing, large files

### GLR

- **Variable**: O(n) to O(n³) depending on ambiguity
- **Medium to large tables**: More memory for forests
- **Good for**: Ambiguous grammars, complex languages

## Grammar Compatibility

### LL(k) Compatible

- No left recursion (or can be eliminated)
- No ambiguity (or k > 1)
- Can compute FIRST/FOLLOW sets

### LR Compatible

- No ambiguity
- No shift/reduce conflicts
- No reduce/reduce conflicts

### GLR Compatible

- Any grammar
- Handles all conflicts
- Handles all ambiguities

## Migration Path

### From LL to LR

If you need better error recovery or left recursion:

1. Update grammar (if needed)
2. Change parser type
3. Update configuration
4. Test thoroughly

### From LR to GLR

If you encounter ambiguities:

1. Keep same grammar
2. Change parser type
3. Add disambiguation logic
4. Handle parse forests

## Recommendations by Use Case

### Interactive Tools (IDEs, Editors)

**Primary**: **LL(k)**
**Alternative**: **LR** if grammar requires it

### Batch Processing

**Primary**: **LR**
**Alternative**: **LL(k)** if grammar is simpler

### Complex Languages

**Primary**: **GLR**
**Alternative**: Transform grammar to be unambiguous

### Learning/Prototyping

**Primary**: **LL(k)**
**Reasoning**: Simplest to understand and use

## Next Steps

- Read backend-specific documentation:
  - [LL Parser](ll-parser.md)
  - [LR Parser](lr-parser.md)
  - [GLR Parser](glr-parser.md)
- Check [Examples](../examples/) for real-world usage

