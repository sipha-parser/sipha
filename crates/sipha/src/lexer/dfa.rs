use crate::syntax::{TextRange, TextSize, SyntaxKind};
use crate::error::LexerError;
use crate::lexer::Token;
use super::builder::{LexRule, Pattern};
// Text positions are extremely unlikely to exceed u32::MAX (4GB)
#[allow(clippy::cast_possible_truncation)]
pub struct CompiledLexer<K: SyntaxKind> {
    pub(crate) rules: smallvec::SmallVec<[LexRule<K>; 16]>,
    pub(crate) keywords: hashbrown::HashMap<compact_str::CompactString, K, ahash::RandomState>,
    pub(crate) trivia_kinds: hashbrown::HashSet<K, ahash::RandomState>,
    pub(crate) eof_kind: K,
    pub(crate) ident_kind: K,
}

impl<K: SyntaxKind> CompiledLexer<K> {
    /// Tokenize the input string.
    ///
    /// # Errors
    ///
    /// Returns a list of lexer errors if tokenization fails.
    pub fn tokenize(&self, input: &str) -> Result<Vec<Token<K>>, Vec<LexerError>> {
        let bytes = input.as_bytes();
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut pos = 0;
        
        while pos < bytes.len() {
            // Skip basic whitespace and comments (for performance)
            pos = Self::skip_basic_trivia(input, pos);
            
            if pos >= bytes.len() {
                break;
            }
            
            match self.next_token(input, pos) {
                Ok((token, new_pos)) => {
                    // Skip tokens whose kind is marked as trivia
                    // This allows custom trivia kinds to work
                    if !self.trivia_kinds.contains(&token.kind) {
                        tokens.push(token);
                    }
                    pos = new_pos;
                }
                Err(e) => {
                    errors.push(e);
                    // Try to recover
                    pos = Self::recover(input, pos);
                }
            }
        }
        
        // Add EOF token
        tokens.push(Token::new(
            self.eof_kind,
            "",
            TextRange::at(TextSize::from(u32::try_from(pos).unwrap_or(0)), TextSize::zero()),
        ));
        
        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }
    
    fn next_token(&self, input: &str, pos: usize) -> Result<(Token<K>, usize), LexerError> {
        // Try custom matchers first
        for rule in &self.rules {
            if let Some(custom) = &rule.custom
                && let Some((len, value)) = custom(input, pos) {
                let range = TextRange::at(
                    TextSize::from(u32::try_from(pos).unwrap_or(0)),
                    TextSize::from(u32::try_from(len).unwrap_or(0)),
                );
                return Ok((
                    Token {
                        kind: rule.kind,
                        text: compact_str::CompactString::from(&input[pos..pos + len]),
                        range,
                        value,
                    },
                    pos + len,
                ));
            }
        }
        
        // Try Repeat patterns (including repeating character classes)
        for rule in &self.rules {
            if let Pattern::Repeat { pattern, min, max } = &rule.pattern
                && let Some(matched_len) = Self::match_repeat_pattern(input, pos, pattern, *min, *max) {
                    let range = TextRange::at(
                        TextSize::from(u32::try_from(pos).unwrap_or(0)),
                        TextSize::from(u32::try_from(matched_len).unwrap_or(0)),
                    );
                    let text = &input[pos..pos + matched_len];
                    
                    return Ok((
                        Token::new(rule.kind, text, range),
                        pos + matched_len,
                    ));
                }
        }
        
        // Try character class patterns (single character)
        for rule in &self.rules {
            if let Pattern::CharClass(char_set) = &rule.pattern
                && let Some(c) = input[pos..].chars().next()
                && char_set.matches(c) {
                let len = c.len_utf8();
                let range = TextRange::at(
                    TextSize::from(u32::try_from(pos).unwrap_or(0)),
                    TextSize::from(u32::try_from(len).unwrap_or(0)),
                );
                let text = &input[pos..pos + len];
                
                return Ok((
                    Token::new(rule.kind, text, range),
                    pos + len,
                ));
            }
        }
        
        // Simple literal matching for now
        // In a full implementation, this would use a compiled DFA
        for rule in &self.rules {
            if let Pattern::Literal(lit) = &rule.pattern
                && input[pos..].starts_with(lit.as_str()) {
                let len = lit.len();
                let range = TextRange::at(
                    TextSize::from(u32::try_from(pos).unwrap_or(0)),
                    TextSize::from(u32::try_from(len).unwrap_or(0)),
                );
                let mut kind = rule.kind;
                
                // Check for keywords
                if kind == self.ident_kind
                    && let Some(kw_kind) = self.keywords.get(lit) {
                    kind = *kw_kind;
                }
                
                return Ok((
                    Token::new(kind, lit.as_str(), range),
                    pos + len,
                ));
            }
        }
        
        // Try to match identifier pattern (simple heuristic)
        let bytes = input.as_bytes();
        if pos < bytes.len() && (bytes[pos] as char).is_alphabetic() {
            let mut len = 1;
            while pos + len < bytes.len() && (bytes[pos + len] as char).is_alphanumeric() {
                len += 1;
            }
            let text = &input[pos..pos + len];
            let range = TextRange::at(
                TextSize::from(u32::try_from(pos).unwrap_or(0)),
                TextSize::from(u32::try_from(len).unwrap_or(0)),
            );
            let mut kind = self.ident_kind;
            
            if let Some(kw_kind) = self.keywords.get(text) {
                kind = *kw_kind;
            }
            
            return Ok((
                Token::new(kind, text, range),
                pos + len,
            ));
        }
        
        Err(LexerError {
            span: TextRange::at(TextSize::from(u32::try_from(pos).unwrap_or(0)), TextSize::from(1)),
            kind: crate::error::LexerErrorKind::UnexpectedChar {
                char: input[pos..].chars().next().unwrap_or('\0'),
            },
        })
    }
    
    /// Skip basic whitespace and comments (for performance optimization).
    /// Custom trivia kinds are handled by checking `trivia_kinds` after tokenization.
    /// Match a repeating pattern (e.g., one or more digits)
    fn match_repeat_pattern(input: &str, pos: usize, pattern: &Pattern, min: usize, max: Option<usize>) -> Option<usize> {
        let mut total_len = 0;
        let mut count = 0;
        let mut current_pos = pos;
        
        loop {
            // Check if we've reached max repetitions
            if let Some(max_count) = max
                && count >= max_count {
                break;
            }
            
            // Try to match one instance of the pattern
            let matched = match pattern {
                Pattern::CharClass(char_set) => {
                    input[current_pos..].chars().next().and_then(|c| {
                        if char_set.matches(c) {
                            Some(c.len_utf8())
                        } else {
                            None
                        }
                    })
                }
                Pattern::Literal(lit) => {
                    if input[current_pos..].starts_with(lit.as_str()) {
                        Some(lit.len())
                    } else {
                        None
                    }
                }
                _ => None, // Other patterns not supported in Repeat yet
            };
            
            if let Some(len) = matched {
                total_len += len;
                current_pos += len;
                count += 1;
            } else {
                break;
            }
        }
        
        // Check if we matched at least min repetitions
        if count >= min {
            Some(total_len)
        } else {
            None
        }
    }
    
    fn skip_basic_trivia(input: &str, mut pos: usize) -> usize {
        let bytes = input.as_bytes();
        
        loop {
            let start = pos;
            
            // Skip whitespace
            while pos < bytes.len() && matches!(bytes[pos], b' ' | b'\t' | b'\r' | b'\n') {
                pos += 1;
            }
            
            if pos >= bytes.len() {
                return input.len();
            }
            
            // Check for comment
            if input[pos..].starts_with("//") {
                pos = memchr::memchr(b'\n', &bytes[pos..])
                    .map_or(input.len(), |p| pos + p + 1);
            } else if input[pos..].starts_with("/*") {
                // Skip block comment
                while pos + 1 < input.len() {
                    if input[pos..].starts_with("*/") {
                        pos += 2;
                        break;
                    }
                    pos += 1;
                }
            } else {
                break;
            }
            
            if pos == start {
                break;
            }
        }
        
        pos
    }
    
    fn recover(input: &str, pos: usize) -> usize {
        // Skip one character and try again
        pos + input[pos..].chars().next().map_or(1, char::len_utf8)
    }
}
