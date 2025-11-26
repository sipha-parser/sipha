use smallvec::SmallVec;
use hashbrown::{HashMap, HashSet};
use crate::error::LexerError;
use crate::lexer::dfa::CompiledLexer;
use crate::syntax::SyntaxKind;

// Rule counts are extremely unlikely to exceed u32::MAX
#[allow(clippy::cast_possible_truncation)]
pub struct LexerBuilder<K: SyntaxKind> {
    rules: SmallVec<[LexRule<K>; 16]>,
    keywords: HashMap<compact_str::CompactString, K, ahash::RandomState>,
    trivia_kinds: HashSet<K, ahash::RandomState>,
}

pub struct LexRule<K: SyntaxKind> {
    pub pattern: Pattern,
    pub kind: K,
    pub priority: u32,
    pub custom: Option<CustomMatcher>,
}

pub enum Pattern {
    Literal(compact_str::CompactString),
    CharClass(CharSet),
    Repeat {
        pattern: Box<Pattern>,
        min: usize,
        max: Option<usize>,
    },
    Regex(compact_str::CompactString), // For complex patterns
    Any,
}

pub struct CharSet {
    /// Character ranges for character class patterns (e.g., `[a-z]`, `[0-9]`).
    chars: Vec<(char, char)>, // Ranges
}

impl CharSet {
    /// Create a new character set with the given ranges
    #[must_use]
    pub const fn new(ranges: Vec<(char, char)>) -> Self {
        Self { chars: ranges }
    }
    
    /// Create a character set for digits [0-9]
    #[must_use]
    pub fn digits() -> Self {
        Self::new(vec![('0', '9')])
    }
    
    /// Create a character set for whitespace characters
    #[must_use]
    pub fn whitespace() -> Self {
        Self::new(vec![(' ', ' '), ('\t', '\t'), ('\r', '\r'), ('\n', '\n')])
    }
    
    /// Check if a character matches this character set
    #[must_use]
    pub fn matches(&self, c: char) -> bool {
        self.chars.iter().any(|(start, end)| {
            c >= *start && c <= *end
        })
    }
}

pub type CustomMatcher = std::sync::Arc<dyn Fn(&str, usize) -> Option<(usize, crate::lexer::TokenValue)> + Send + Sync>;

impl<K: SyntaxKind> Default for LexerBuilder<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: SyntaxKind> LexerBuilder<K> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            rules: SmallVec::new(),
            keywords: HashMap::with_hasher(ahash::RandomState::new()),
            trivia_kinds: HashSet::with_hasher(ahash::RandomState::new()),
        }
    }
    
    #[must_use]
    pub fn token(mut self, kind: K, pattern: Pattern) -> Self {
        self.rules.push(LexRule {
            pattern,
            kind,
            priority: u32::try_from(self.rules.len()).unwrap_or(0),
            custom: None,
        });
        self
    }
    
    #[must_use]
    pub fn keyword(mut self, text: &str, kind: K) -> Self {
        self.keywords.insert(compact_str::CompactString::new(text), kind);
        self
    }
    
    #[must_use]
    pub fn trivia(mut self, kind: K) -> Self {
        self.trivia_kinds.insert(kind);
        self
    }
    
    #[must_use]
    pub fn custom_token(
        mut self,
        kind: K,
        matcher: impl Fn(&str, usize) -> Option<(usize, crate::lexer::TokenValue)> + Send + Sync + 'static,
    ) -> Self {
        self.rules.push(LexRule {
            pattern: Pattern::Any,
            kind,
            priority: u32::try_from(self.rules.len()).unwrap_or(0),
            custom: Some(std::sync::Arc::new(matcher)),
        });
        self
    }
    
    /// Build the lexer from the configured rules.
    ///
    /// # Errors
    ///
    /// Returns an error if lexer compilation fails.
    pub fn build(self, eof_kind: K, ident_kind: K) -> Result<CompiledLexer<K>, LexerError> {
        // For now, return a simple lexer
        // In a full implementation, this would compile patterns into a DFA
        Ok(CompiledLexer {
            rules: self.rules,
            keywords: self.keywords,
            trivia_kinds: self.trivia_kinds,
            eof_kind,
            ident_kind,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::SyntaxKind;
    use crate::lexer::TokenValue;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestKind {
        Ident,
        Number,
        Plus,
        Whitespace,
        Eof,
        Keyword,
    }

    impl SyntaxKind for TestKind {
        fn is_terminal(self) -> bool {
            true // All variants are terminals in this test
        }
        
        fn is_trivia(self) -> bool {
            matches!(self, Self::Whitespace)
        }
    }

    #[test]
    fn test_lexer_builder_new() {
        let builder = LexerBuilder::<TestKind>::new();
        assert!(builder.rules.is_empty());
        assert!(builder.keywords.is_empty());
        assert!(builder.trivia_kinds.is_empty());
    }

    #[test]
    fn test_lexer_builder_token() {
        let builder = LexerBuilder::new()
            .token(TestKind::Ident, Pattern::Any);
        
        assert_eq!(builder.rules.len(), 1);
        assert_eq!(builder.rules[0].kind, TestKind::Ident);
        assert_eq!(builder.rules[0].priority, 0);
    }

    #[test]
    fn test_lexer_builder_token_priority() {
        let builder = LexerBuilder::new()
            .token(TestKind::Ident, Pattern::Any)
            .token(TestKind::Number, Pattern::Any);
        
        assert_eq!(builder.rules.len(), 2);
        assert_eq!(builder.rules[0].priority, 0);
        assert_eq!(builder.rules[1].priority, 1);
    }

    #[test]
    fn test_lexer_builder_keyword() {
        let builder = LexerBuilder::new()
            .keyword("if", TestKind::Keyword);
        
        assert_eq!(builder.keywords.len(), 1);
        assert!(builder.keywords.contains_key(&compact_str::CompactString::new("if")));
    }

    #[test]
    fn test_lexer_builder_multiple_keywords() {
        let builder = LexerBuilder::new()
            .keyword("if", TestKind::Keyword)
            .keyword("else", TestKind::Keyword)
            .keyword("while", TestKind::Keyword);
        
        assert_eq!(builder.keywords.len(), 3);
    }

    #[test]
    fn test_lexer_builder_trivia() {
        let builder = LexerBuilder::new()
            .trivia(TestKind::Whitespace);
        
        assert!(builder.trivia_kinds.contains(&TestKind::Whitespace));
    }

    #[test]
    fn test_lexer_builder_multiple_trivia() {
        let builder = LexerBuilder::new()
            .trivia(TestKind::Whitespace);
        
        assert_eq!(builder.trivia_kinds.len(), 1);
    }

    #[test]
    fn test_lexer_builder_custom_token() {
        let builder = LexerBuilder::new()
            .custom_token(TestKind::Number, |_text, _pos| {
                Some((5, TokenValue::Integer(42)))
            });
        
        assert_eq!(builder.rules.len(), 1);
        assert!(builder.rules[0].custom.is_some());
    }

    #[test]
    fn test_lexer_builder_build() {
        let lexer = LexerBuilder::new()
            .token(TestKind::Ident, Pattern::Any)
            .keyword("if", TestKind::Keyword)
            .trivia(TestKind::Whitespace)
            .build(TestKind::Eof, TestKind::Ident)
            .unwrap();
        
        assert_eq!(lexer.rules.len(), 1);
        assert_eq!(lexer.keywords.len(), 1);
        assert_eq!(lexer.trivia_kinds.len(), 1);
        assert_eq!(lexer.eof_kind, TestKind::Eof);
        assert_eq!(lexer.ident_kind, TestKind::Ident);
    }

    #[test]
    fn test_lexer_builder_combined() {
        let lexer = LexerBuilder::new()
            .token(TestKind::Ident, Pattern::Literal("x".into()))
            .token(TestKind::Number, Pattern::Any)
            .keyword("if", TestKind::Keyword)
            .keyword("else", TestKind::Keyword)
            .trivia(TestKind::Whitespace)
            .custom_token(TestKind::Number, |_text, _pos| {
                Some((3, TokenValue::Integer(123)))
            })
            .build(TestKind::Eof, TestKind::Ident)
            .unwrap();
        
        assert_eq!(lexer.rules.len(), 3); // 2 tokens + 1 custom
        assert_eq!(lexer.keywords.len(), 2);
        assert_eq!(lexer.trivia_kinds.len(), 1);
    }

    #[test]
    fn test_pattern_literal() {
        let pattern = Pattern::Literal("hello".into());
        match pattern {
            Pattern::Literal(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected Literal pattern"),
        }
    }

    #[test]
    fn test_pattern_any() {
        let pattern = Pattern::Any;
        match pattern {
            Pattern::Any => {}
            _ => panic!("Expected Any pattern"),
        }
    }

    #[test]
    fn test_lex_rule() {
        let rule = LexRule {
            pattern: Pattern::Any,
            kind: TestKind::Ident,
            priority: 42,
            custom: None,
        };
        
        assert_eq!(rule.kind, TestKind::Ident);
        assert_eq!(rule.priority, 42);
        assert!(rule.custom.is_none());
    }
}
