use crate::grammar::Token as GrammarToken;
use crate::syntax::{SyntaxKind, TextRange, TextSize};
use compact_str::CompactString;

#[derive(Debug, Clone)]
pub struct Token<K: SyntaxKind> {
    pub kind: K,
    pub text: CompactString,
    pub range: TextRange,
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

#[derive(Debug, Clone)]
pub enum TokenValue {
    None,
    Integer(i64),
    Float(f64),
    String(CompactString),
    Char(char),
    Bool(bool),
}

impl<K: SyntaxKind> Token<K> {
    #[must_use]
    pub fn new(kind: K, text: impl Into<CompactString>, range: TextRange) -> Self {
        Self {
            kind,
            text: text.into(),
            range,
            value: TokenValue::None,
        }
    }

    #[inline]
    #[must_use]
    pub fn is_trivia(&self) -> bool {
        self.kind.is_trivia()
    }

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
