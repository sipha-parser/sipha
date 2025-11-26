/// Trait for syntax kind identifiers.
/// 
/// This represents a union of:
/// - Terminal kinds - produced by the lexer (tokens like `Number`, `Plus`, `Ident`)
/// - Non-terminal kinds - produced by the parser (rules like `Expr`, `Statement`)
/// 
/// Users must implement this trait for their language-specific syntax kinds.
/// The type should typically be an enum that includes both terminal and non-terminal variants.
/// 
/// ## Example
/// 
/// ```rust
/// use sipha::syntax::SyntaxKind;
/// 
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// enum MySyntaxKind {
///     // Terminals (produced by lexer)
///     Number,
///     Plus,
///     Whitespace,
///     Eof,
///     // Non-terminals (produced by parser)
///     Expr,
///     Term,
/// }
/// 
/// impl SyntaxKind for MySyntaxKind {
///     fn is_terminal(self) -> bool {
///         !matches!(self, MySyntaxKind::Expr | MySyntaxKind::Term)
///     }
///     
///     fn is_trivia(self) -> bool {
///         matches!(self, MySyntaxKind::Whitespace)
///     }
/// }
/// ```
pub trait SyntaxKind: Copy + PartialEq + Eq + std::hash::Hash + std::fmt::Debug + Send + Sync + 'static {
    /// Check if this kind represents a terminal (lexer token).
    /// 
    /// Terminals are the "leaf" nodes in the syntax tree - they represent
    /// actual source text tokens produced by the lexer (e.g., `Number`, `Plus`, `Ident`).
    /// 
    /// Non-terminals represent grammar rules and are internal nodes in the tree
    /// (e.g., `Expr`, `Statement`, `Block`).
    fn is_terminal(self) -> bool;
    
    /// Check if this kind represents trivia (whitespace, comments, etc.)
    /// 
    /// Trivia tokens are typically skipped during parsing but preserved
    /// in the syntax tree for formatting and IDE features.
    /// 
    /// This should return `false` for non-terminal kinds.
    fn is_trivia(self) -> bool;
    
    /// Check if this kind represents a keyword.
    /// 
    /// Keywords are reserved words in the language (e.g., `if`, `while`, `fn`).
    /// This is useful for syntax highlighting and IDE features.
    fn is_keyword(self) -> bool {
        false
    }
    
    /// Check if this kind represents a literal.
    /// 
    /// Literals are constant values like numbers, strings, and booleans.
    fn is_literal(self) -> bool {
        false
    }
}
