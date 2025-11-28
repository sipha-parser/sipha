# Glossary

This glossary defines key terms used throughout the Sipha documentation.

## A

**Affected Region**  
The minimal range of text that needs to be re-parsed after an edit. Sipha computes this automatically by expanding the edit range to include necessary context.

**Ambiguous Grammar**  
A grammar that can produce multiple valid parse trees for the same input. Sipha's GLR backend can handle ambiguous grammars by producing a parse forest.

**Arena Allocation**  
A memory allocation strategy where objects are allocated in a contiguous region (arena) for better cache locality and faster deallocation. Green trees use arena allocation.

## B

**Backend**  
A parsing algorithm implementation. Sipha supports multiple backends: LL(k), LR, and GLR, each with different characteristics.

## C

**Cache (Parse Cache)**  
A persistent storage for parse results that enables cross-session node reuse. The cache stores parse nodes keyed by rule name, position, and version.

**Choice Expression**  
A grammar expression that matches one of several alternatives. Represented as `Expr::choice(vec![...])`.

## D

**DFA (Deterministic Finite Automaton)**  
A state machine used by Sipha's lexer for efficient tokenization. DFAs provide O(n) tokenization time.

**Disambiguation**  
The process of selecting a single parse tree from multiple valid alternatives in an ambiguous grammar. GLR parsers support various disambiguation strategies.

## E

**Entry Point**  
The starting non-terminal for parsing. Defined when building a grammar using `GrammarBuilder::entry_point()`.

**Error Recovery**  
The ability of a parser to continue parsing after encountering an error, rather than stopping immediately. Sipha supports configurable recovery strategies.

**Expression (Grammar Expression)**  
A component of a grammar rule's right-hand side. Can be a token, non-terminal, sequence, choice, optional, repeat, or empty.

## F

**First Set**  
The set of terminals that can appear as the first token in a derivation from a non-terminal. Used by LL parsers for prediction.

**Follow Set**  
The set of terminals that can appear immediately after a non-terminal in a derivation. Used by LL parsers for error recovery.

**Forest (Parse Forest)**  
A data structure representing multiple parse trees for ambiguous grammars. GLR parsers produce parse forests when ambiguity is detected.

## G

**GLR (Generalized LR)**  
A parsing algorithm that extends LR parsing to handle ambiguous and non-deterministic grammars by maintaining multiple parser stacks.

**Grammar**  
A formal description of a language's syntax, consisting of non-terminals, production rules, and an entry point.

**Green Tree**  
The storage layer of Sipha's syntax tree representation. Green trees are compact, immutable, and can be shared across multiple red trees.

## I

**Incremental Parsing**  
The ability to efficiently re-parse only changed portions of code, reusing unchanged subtrees from previous parses. Sipha's core feature.

**Immutable**  
Data structures that cannot be modified after creation. Sipha's syntax trees are immutable, enabling safe sharing and efficient updates.

## L

**Left Recursion**  
A grammar rule where a non-terminal appears as the first symbol in its own production. Some backends require left-recursion elimination.

**Lexer**  
The component that converts raw text into tokens. Also called a tokenizer or scanner.

**LL(k) Parser**  
A top-down predictive parser that uses k tokens of lookahead to make parsing decisions.

**LR Parser**  
A bottom-up shift-reduce parser that builds parse trees from the leaves up. Supports LALR and canonical LR variants.

## N

**Non-Terminal**  
A grammar symbol that can be expanded into other symbols. Represented as enum variants implementing the `NonTerminal` trait.

**Node Reuse**  
The process of identifying and reusing unchanged subtrees from previous parses during incremental parsing.

## P

**Parse Cache**  
See [Cache (Parse Cache)](#c).

**Parser Backend**  
See [Backend](#b).

**Production Rule**  
A grammar rule that defines how a non-terminal can be expanded. Written as `NonTerminal -> Expression`.

## R

**Red Tree**  
The API layer of Sipha's syntax tree representation. Red trees provide convenient navigation and query APIs, created on-demand from green trees.

**Reuse Budget**  
A limit on how many nodes to consider for reuse during incremental parsing. Can be fixed or heuristic-based.

## S

**Sequence Expression**  
A grammar expression that matches a sequence of expressions in order. Represented as `Expr::seq(vec![...])`.

**Syntax Kind**  
A unified type representing both terminals (tokens) and non-terminals. All syntax kinds implement the `SyntaxKind` trait.

**Syntax Tree**  
A tree representation of the syntactic structure of parsed code. Sipha uses green/red trees for efficient representation.

## T

**Terminal**  
A grammar symbol that cannot be expanded further. In Sipha, terminals are tokens produced by the lexer.

**Token**  
A unit of lexical analysis, representing a sequence of characters with a specific meaning (e.g., identifier, keyword, operator).

**Trivia**  
Tokens that are syntactically insignificant, such as whitespace and comments. Trivia is automatically skipped during parsing.

## U

**Unambiguous Grammar**  
A grammar that produces exactly one parse tree for each valid input. Most parsers require unambiguous grammars, except GLR.

## V

**Version (Cache Version)**  
A number used to invalidate old cache entries. When the grammar or lexer changes, the version increments, invalidating old cache entries.

