use crate::grammar::Token as GrammarToken;
use crate::syntax::{SyntaxKind, TextRange, TextSize};
use compact_str::CompactString;

/// A token produced by the lexer.
///
/// Tokens represent the smallest meaningful units of source code, such as
/// identifiers, keywords, operators, and literals. Each token has a kind,
/// the source text it represents, its position in the source, and optionally
/// a parsed value.
///
/// # Example
///
/// ```rust,no_run
/// use sipha::lexer::Token;
/// use sipha::syntax::{SyntaxKind, TextRange, TextSize};
///
/// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// # enum MyKind { Number, Eof }
/// # impl SyntaxKind for MyKind {
/// #     fn is_terminal(self) -> bool { true }
/// #     fn is_trivia(self) -> bool { false }
/// # }
/// let token = Token::new(
///     MyKind::Number,
///     "42",
///     TextRange::at(TextSize::from(0), TextSize::from(2)),
/// );
/// ```
#[derive(Debug, Clone)]
pub struct Token<K: SyntaxKind> {
    /// The kind of this token (e.g., `Number`, `Identifier`, `Plus`)
    pub kind: K,
    /// The source text that this token represents
    pub text: CompactString,
    /// The byte range in the source text where this token appears
    pub range: TextRange,
    /// The parsed value of this token (if applicable)
    pub value: TokenValue,
}

// Implement PartialEq, Eq, and Hash for grammar::Token trait requirements
// We compare tokens by their kind only, which is what matters for grammar matching
impl<K: SyntaxKind> PartialEq for Token<K> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl<K: SyntaxKind> Eq for Token<K> {}

impl<K: SyntaxKind> std::hash::Hash for Token<K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

/// The parsed value of a token, if applicable.
///
/// Some tokens (like numbers, strings, etc.) have semantic values beyond
/// just their text. This enum represents those parsed values.
///
/// # Example
///
/// ```rust,no_run
/// use sipha::lexer::TokenValue;
///
/// // Integer token
/// let int_value = TokenValue::Integer(42);
///
/// // String token
/// let str_value = TokenValue::String("hello".into());
///
/// // Token without a value
/// let none_value = TokenValue::None;
/// ```
#[derive(Debug, Clone)]
pub enum TokenValue {
    /// No parsed value (e.g., keywords, operators)
    None,
    /// Parsed integer value
    Integer(i64),
    /// Parsed floating-point value
    Float(f64),
    /// Parsed string value
    String(CompactString),
    /// Parsed character value
    Char(char),
    /// Parsed boolean value
    Bool(bool),
}

impl<K: SyntaxKind> Token<K> {
    /// Create a new token with the given kind, text, and range.
    ///
    /// The value is initialized to `TokenValue::None` and can be set later
    /// if the token has a parsed value.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use sipha::lexer::Token;
    /// use sipha::syntax::{SyntaxKind, TextRange, TextSize};
    ///
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MyKind { Plus, Eof }
    /// # impl SyntaxKind for MyKind {
    /// #     fn is_terminal(self) -> bool { true }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// let token = Token::new(
    ///     MyKind::Plus,
    ///     "+",
    ///     TextRange::at(TextSize::from(0), TextSize::from(1)),
    /// );
    /// ```
    #[must_use]
    pub fn new(kind: K, text: impl Into<CompactString>, range: TextRange) -> Self {
        Self {
            kind,
            text: text.into(),
            range,
            value: TokenValue::None,
        }
    }

    /// Check if this token is trivia (whitespace, comments, etc.).
    ///
    /// Trivia tokens are typically ignored during parsing but preserved
    /// in the syntax tree for formatting and error reporting.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use sipha::lexer::Token;
    /// use sipha::syntax::{SyntaxKind, TextRange, TextSize};
    ///
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MyKind { Whitespace, Number, Eof }
    /// # impl SyntaxKind for MyKind {
    /// #     fn is_terminal(self) -> bool { true }
    /// #     fn is_trivia(self) -> bool { matches!(self, MyKind::Whitespace) }
    /// # }
    /// let whitespace = Token::new(MyKind::Whitespace, " ", TextRange::at(TextSize::from(0), TextSize::from(1)));
    /// assert!(whitespace.is_trivia());
    /// ```
    #[inline]
    #[must_use]
    pub fn is_trivia(&self) -> bool {
        self.kind.is_trivia()
    }

    /// Get the parsed value of this token.
    ///
    /// Returns a reference to the `TokenValue` enum, which may contain
    /// a parsed integer, float, string, etc., or `None` if the token
    /// doesn't have a semantic value.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use sipha::lexer::{Token, TokenValue};
    /// use sipha::syntax::{SyntaxKind, TextRange, TextSize};
    ///
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MyKind { Number, Eof }
    /// # impl SyntaxKind for MyKind {
    /// #     fn is_terminal(self) -> bool { true }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// let mut token = Token::new(MyKind::Number, "42", TextRange::at(TextSize::from(0), TextSize::from(2)));
    /// token.value = TokenValue::Integer(42);
    /// assert!(matches!(token.value(), TokenValue::Integer(42)));
    /// ```
    #[must_use]
    pub const fn value(&self) -> &TokenValue {
        &self.value
    }
}

// Implement grammar::Token trait so lexer tokens can be used directly in grammars
impl<K: SyntaxKind> GrammarToken for Token<K> {
    type Kind = K;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        self.range.len()
    }

    fn text(&self) -> CompactString {
        self.text.clone()
    }
}
