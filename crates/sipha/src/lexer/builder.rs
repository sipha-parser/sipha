use crate::error::LexerError;
use crate::lexer::dfa::CompiledLexer;
use crate::syntax::SyntaxKind;
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;
use std::fmt::Write;

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

#[derive(Clone)]
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

#[derive(Clone)]
pub struct CharSet {
    /// Character ranges for character class patterns (e.g., `[a-z]`, `[0-9]`).
    /// Uses Rust's `RangeInclusive` for idiomatic character ranges.
    ranges: Vec<std::ops::RangeInclusive<char>>,
    /// Whether ranges are sorted (for efficient binary search)
    sorted: bool,
}

impl CharSet {
    /// Create a new character set with the given ranges
    #[must_use]
    pub fn new(ranges: Vec<std::ops::RangeInclusive<char>>) -> Self {
        let mut set = Self {
            ranges,
            sorted: false,
        };
        set.sort_ranges();
        set
    }

    /// Create a character set for digits [0-9]
    #[must_use]
    pub fn digits() -> Self {
        Self::new(vec!['0'..='9'])
    }

    /// Create a character set for whitespace characters
    #[must_use]
    pub fn whitespace() -> Self {
        Self::new(vec![' '..=' ', '\t'..='\t', '\r'..='\r', '\n'..='\n'])
    }

    /// Check if a character matches this character set
    /// Uses binary search when ranges are sorted for O(log n) performance
    #[must_use]
    pub fn matches(&self, c: char) -> bool {
        if self.sorted {
            // Binary search for sorted ranges
            self.ranges
                .binary_search_by(|range| {
                    if c < *range.start() {
                        std::cmp::Ordering::Greater
                    } else if c > *range.end() {
                        std::cmp::Ordering::Less
                    } else {
                        std::cmp::Ordering::Equal
                    }
                })
                .is_ok()
        } else {
            // Linear search for unsorted ranges
            self.ranges.iter().any(|range| range.contains(&c))
        }
    }

    /// Sort ranges for efficient binary search
    /// This is called automatically in `new()`, but can be called manually
    /// if ranges are modified after construction.
    pub fn sort_ranges(&mut self) {
        self.ranges.sort_by_key(|range| *range.start());
        self.sorted = true;
    }

    /// Get the character ranges in this set
    #[must_use]
    pub fn ranges(&self) -> &[std::ops::RangeInclusive<char>] {
        &self.ranges
    }

    /// Add a range to this character set
    pub fn add_range(&mut self, range: std::ops::RangeInclusive<char>) {
        self.ranges.push(range);
        self.sorted = false; // Mark as unsorted after modification
    }
}

pub type CustomMatcher =
    std::sync::Arc<dyn Fn(&str, usize) -> Option<(usize, crate::lexer::TokenValue)> + Send + Sync>;

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
        self.keywords
            .insert(compact_str::CompactString::new(text), kind);
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
        matcher: impl Fn(&str, usize) -> Option<(usize, crate::lexer::TokenValue)>
        + Send
        + Sync
        + 'static,
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
    ///
    /// # Panics
    ///
    /// Panics if a custom matcher rule has a `None` custom matcher.
    pub fn build(self, eof_kind: K, ident_kind: K) -> Result<CompiledLexer<K>, LexerError> {
        use crate::lexer::dfa::{Dfa, KeywordTrie, Nfa, nfa_to_dfa, pattern_to_nfa};

        let mut compiled_rules = Vec::new();
        let rules = self.rules;
        let keywords = self.keywords.clone();
        let trivia_kinds = self.trivia_kinds;

        let mut dfa_patterns = Vec::new();
        let mut dfa_kind_to_rule = Vec::new();
        let mut keyword_trie: Option<KeywordTrie<K>> = None;

        // Separate rules into DFA-compatible and others
        for (rule_idx, rule) in rules.iter().enumerate() {
            if let Some(_custom) = &rule.custom {
                // Keep custom matchers separate
                compiled_rules.push(crate::lexer::dfa::CompiledRule::Custom {
                    kind: rule.kind,
                    matcher: rule.custom.as_ref().unwrap().clone(),
                    priority: rule.priority,
                });
            } else {
                match &rule.pattern {
                    Pattern::Literal(s) => {
                        // Fast-path literal matching
                        compiled_rules.push(crate::lexer::dfa::CompiledRule::Literal {
                            kind: rule.kind,
                            literal: s.clone(),
                            priority: rule.priority,
                        });
                    }
                    Pattern::Regex(_) => {
                        // Complex regex - use regex fallback
                        let regex_str = Self::pattern_to_regex(&rule.pattern)?;
                        let anchored_regex = format!("^{regex_str}");
                        let regex = regex::Regex::new(&anchored_regex).map_err(|e| LexerError {
                            span: crate::syntax::TextRange::new(
                                crate::syntax::TextSize::zero(),
                                crate::syntax::TextSize::zero(),
                            ),
                            kind: crate::error::LexerErrorKind::InvalidNumber {
                                reason: format!("Invalid Regex: {e}"),
                            },
                        })?;
                        compiled_rules.push(crate::lexer::dfa::CompiledRule::Regex {
                            kind: rule.kind,
                            regex,
                            priority: rule.priority,
                        });
                    }
                    Pattern::Any | Pattern::CharClass(_) | Pattern::Repeat { .. } => {
                        // DFA-compatible patterns
                        dfa_patterns.push((rule.pattern.clone(), rule.kind, rule.priority));
                        dfa_kind_to_rule.push(rule_idx);
                        compiled_rules.push(crate::lexer::dfa::CompiledRule::Dfa {
                            kind: rule.kind,
                            priority: rule.priority,
                        });
                    }
                }
            }
        }

        // Build DFA from DFA-compatible patterns
        let dfa = if dfa_patterns.is_empty() {
            None
        } else {
            let mut nfa = Nfa::new();
            let nfa_start = nfa.start();

            // Build each pattern as a separate sub-NFA, all connected to the main start
            for (kind_index, (pattern, _kind, priority)) in dfa_patterns.iter().enumerate() {
                let pattern_start = nfa.add_state();
                let pattern_end = nfa.add_state();

                // Connect main start to this pattern's start via epsilon
                nfa.add_epsilon_transition(nfa_start, pattern_start);

                // Build the pattern NFA
                pattern_to_nfa(
                    pattern,
                    &mut nfa,
                    pattern_start,
                    pattern_end,
                    u32::try_from(kind_index).unwrap_or(0),
                    *priority,
                );
            }

            let mut dfa = Dfa::new();
            nfa_to_dfa(&nfa, &mut dfa);
            Some(dfa)
        };

        // Build keyword trie
        if !keywords.is_empty() {
            let mut trie = KeywordTrie::new();
            for (word, kind) in &keywords {
                // Use a high priority for keywords (lower number = higher priority)
                trie.insert(word.as_str(), *kind, 0);
            }
            keyword_trie = Some(trie);
        }

        Ok(CompiledLexer {
            rules: compiled_rules,
            dfa,
            dfa_kind_to_rule,
            keywords,
            trivia_kinds,
            eof_kind,
            ident_kind,
            keyword_trie,
        })
    }

    fn pattern_to_regex(pattern: &Pattern) -> Result<String, LexerError> {
        match pattern {
            Pattern::Literal(s) => Ok(regex::escape(s)),
            Pattern::Regex(s) => Ok(s.to_string()),
            Pattern::Any => Ok(".".to_string()),
            Pattern::CharClass(chars) => {
                let mut s = String::from("[");
                for range in chars.ranges() {
                    // Escape special characters in character classes: ], \, ^, -
                    let start_escaped = Self::escape_char_for_class(*range.start());
                    let end_escaped = Self::escape_char_for_class(*range.end());
                    write!(s, "{start_escaped}-{end_escaped}")
                        .map_err(|_| Self::regex_err("Failed to write char class"))?;
                }
                s.push(']');
                Ok(s)
            }
            Pattern::Repeat { pattern, min, max } => {
                let inner = Self::pattern_to_regex(pattern)?;
                let suffix = max.as_ref().map_or_else(
                    || {
                        if *min == 0 {
                            "*".to_string()
                        } else if *min == 1 {
                            "+".to_string()
                        } else {
                            format!("{{{min},}}")
                        }
                    },
                    |max| format!("{{{min},{max}}}"),
                );
                Ok(format!("(?:{inner}){suffix}"))
            }
        }
    }

    fn escape_char_for_class(c: char) -> String {
        match c {
            ']' | '\\' | '^' | '-' => format!("\\{c}"),
            _ => c.to_string(),
        }
    }

    fn regex_err(msg: &str) -> LexerError {
        LexerError {
            span: crate::syntax::TextRange::new(
                crate::syntax::TextSize::zero(),
                crate::syntax::TextSize::zero(),
            ),
            kind: crate::error::LexerErrorKind::InvalidNumber { reason: msg.into() },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenValue;
    use crate::syntax::SyntaxKind;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
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
        let builder = LexerBuilder::new().token(TestKind::Ident, Pattern::Any);

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
        let builder = LexerBuilder::new().keyword("if", TestKind::Keyword);

        assert_eq!(builder.keywords.len(), 1);
        assert!(
            builder
                .keywords
                .contains_key(&compact_str::CompactString::new("if"))
        );
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
        let builder = LexerBuilder::new().trivia(TestKind::Whitespace);

        assert!(builder.trivia_kinds.contains(&TestKind::Whitespace));
    }

    #[test]
    fn test_lexer_builder_multiple_trivia() {
        let builder = LexerBuilder::new().trivia(TestKind::Whitespace);

        assert_eq!(builder.trivia_kinds.len(), 1);
    }

    #[test]
    fn test_lexer_builder_custom_token() {
        let builder = LexerBuilder::new().custom_token(TestKind::Number, |_text, _pos| {
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
