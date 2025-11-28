use super::builder::{CustomMatcher, Pattern};
use crate::error::LexerError;
use crate::lexer::{Token, TokenValue};
use crate::syntax::{SyntaxKind, TextRange, TextSize};
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;
/// State ID in the DFA
///
/// Uses u32 which is sufficient for all practical DFA sizes.
/// Conversions to usize for indexing are safe on all platforms (usize >= 32 bits).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StateId(pub u32);

/// Character range for transitions
/// Uses Rust's `RangeInclusive` for idiomatic character ranges
type CharRange = std::ops::RangeInclusive<char>;

/// Helper functions for character ranges
mod char_range {
    use super::CharRange;

    pub const fn single(c: char) -> CharRange {
        c..=c
    }
}

/// Accepting state information for Maximal Munch
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AcceptingState {
    kind: u32, // Index into rule kinds
    priority: u32,
}

/// DFA state with transitions and accepting information
#[derive(Debug, Clone)]
struct DfaState {
    /// Transitions: (character range, target state)
    /// Sorted by range start for binary search optimization
    transitions: Vec<(CharRange, StateId)>,
    /// Accepting state info: (kind index, priority)
    /// Multiple accepting states possible for Maximal Munch
    /// Pre-sorted by priority for O(1) best match lookup
    accepting: Vec<AcceptingState>,
    /// Cached best accepting state (lowest priority = highest precedence)
    best_accepting: Option<AcceptingState>,
}

impl Default for DfaState {
    fn default() -> Self {
        Self::new()
    }
}

impl DfaState {
    #[must_use]
    const fn new() -> Self {
        Self {
            transitions: Vec::new(),
            accepting: Vec::new(),
            best_accepting: None,
        }
    }

    fn add_transition(&mut self, range: CharRange, target: StateId) {
        self.transitions.push((range, target));
    }

    fn add_accepting(&mut self, kind: u32, priority: u32) {
        let accepting = AcceptingState { kind, priority };
        self.accepting.push(accepting);
        // Update best accepting state (lower priority = higher precedence)
        self.best_accepting = match self.best_accepting {
            Some(best) if best.priority <= priority => Some(best),
            _ => Some(accepting),
        };
    }

    /// Finalize state after all transitions/accepting states are added
    /// Sorts transitions for efficient binary search lookup
    fn finalize(&mut self) {
        // Sort transitions by range start for binary search
        self.transitions.sort_by_key(|(range, _)| *range.start());
    }

    /// Find transition using binary search on sorted ranges
    /// O(log n) instead of O(n) linear search
    fn find_transition(&self, c: char) -> Option<StateId> {
        // Binary search for the range containing c
        // Since ranges are sorted by start, we can use binary search
        self.transitions
            .binary_search_by(|(range, _)| {
                if c < *range.start() {
                    std::cmp::Ordering::Greater
                } else if c > *range.end() {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .ok()
            .map(|idx| self.transitions[idx].1)
    }

    /// Get the best accepting state (O(1) after finalize)
    const fn best_accepting(&self) -> Option<AcceptingState> {
        self.best_accepting
    }
}

/// NFA state for construction
#[derive(Debug, Clone)]
pub(crate) struct NfaState {
    transitions: Vec<(CharRange, u32)>, // (range, target state id)
    epsilon_transitions: Vec<u32>,      // Target state ids via epsilon
    accepting: Option<(u32, u32)>,      // (kind index, priority)
}

impl Default for NfaState {
    fn default() -> Self {
        Self::new()
    }
}

impl NfaState {
    #[must_use]
    const fn new() -> Self {
        Self {
            transitions: Vec::new(),
            epsilon_transitions: Vec::new(),
            accepting: None,
        }
    }
}

/// Compiled rule - can be DFA, regex fallback, or custom matcher
pub enum CompiledRule<K: SyntaxKind> {
    Dfa {
        kind: K,
        priority: u32,
    },
    Regex {
        kind: K,
        regex: regex::Regex,
        priority: u32,
    },
    Custom {
        kind: K,
        matcher: CustomMatcher,
        priority: u32,
    },
    Literal {
        kind: K,
        literal: compact_str::CompactString,
        priority: u32,
    },
}

/// Compiled DFA for fast tokenization
pub(crate) struct Dfa {
    states: Vec<DfaState>,
    start_state: StateId,
    /// Map from kind index to actual `SyntaxKind`
    #[allow(dead_code)]
    kind_map: Vec<u32>, // Will store indices, actual kinds stored separately
}

impl Default for Dfa {
    fn default() -> Self {
        Self::new()
    }
}

impl Dfa {
    #[must_use]
    pub(crate) fn new() -> Self {
        let mut dfa = Self {
            states: Vec::new(),
            start_state: StateId(0),
            kind_map: Vec::new(),
        };
        // Create initial state
        dfa.states.push(DfaState::default());
        dfa
    }

    fn add_state(&mut self) -> StateId {
        let id = StateId(u32::try_from(self.states.len()).unwrap_or(0));
        self.states.push(DfaState::new());
        id
    }

    fn state_mut(&mut self, id: StateId) -> &mut DfaState {
        &mut self.states[id.0 as usize]
    }

    fn state(&self, id: StateId) -> &DfaState {
        &self.states[id.0 as usize]
    }
}

/// NFA for pattern compilation
pub(crate) struct Nfa {
    states: Vec<NfaState>,
    start: u32,
    end: u32,
}

impl Default for Nfa {
    fn default() -> Self {
        Self::new()
    }
}

impl Nfa {
    #[must_use]
    pub(crate) fn new() -> Self {
        let mut nfa = Self {
            states: Vec::new(),
            start: 0,
            end: 0,
        };
        nfa.states.push(NfaState::default());
        nfa.states.push(NfaState::default());
        nfa.start = 0;
        nfa.end = 1;
        nfa
    }

    pub(crate) fn add_state(&mut self) -> u32 {
        let id = u32::try_from(self.states.len()).unwrap_or(0);
        self.states.push(NfaState::default());
        id
    }

    pub(crate) fn state_mut(&mut self, id: u32) -> &mut NfaState {
        &mut self.states[id as usize]
    }

    pub(crate) const fn start(&self) -> u32 {
        self.start
    }

    pub(crate) fn add_epsilon_transition(&mut self, from: u32, to: u32) {
        self.state_mut(from).epsilon_transitions.push(to);
    }
}

/// Build NFA from pattern
pub(crate) fn pattern_to_nfa(
    pattern: &Pattern,
    nfa: &mut Nfa,
    start: u32,
    end: u32,
    kind: u32,
    priority: u32,
) {
    match pattern {
        Pattern::Literal(s) => {
            // Create chain of states for each character
            let mut current = start;
            let chars: Vec<char> = s.chars().collect();
            for (i, &c) in chars.iter().enumerate() {
                let next = if i == chars.len() - 1 {
                    end
                } else {
                    nfa.add_state()
                };
                nfa.state_mut(current)
                    .transitions
                    .push((char_range::single(c), next));
                current = next;
            }
            // Mark end as accepting
            nfa.state_mut(end).accepting = Some((kind, priority));
        }
        Pattern::CharClass(chars) => {
            // Create transitions for each range
            for range in chars.ranges() {
                nfa.state_mut(start).transitions.push((range.clone(), end));
            }
            nfa.state_mut(end).accepting = Some((kind, priority));
        }
        Pattern::Any => {
            // Match any single character (full Unicode range)
            nfa.state_mut(start)
                .transitions
                .push(('\u{0000}'..='\u{10FFFF}', end));
            nfa.state_mut(end).accepting = Some((kind, priority));
        }
        Pattern::Repeat { pattern, min, max } => {
            let inner_start = nfa.add_state();
            let inner_end = nfa.add_state();

            // Build inner pattern
            pattern_to_nfa(pattern, nfa, inner_start, inner_end, kind, priority);

            // Connect start to inner
            nfa.state_mut(start).epsilon_transitions.push(inner_start);

            // Handle repetition
            if *min == 0 {
                // Optional: epsilon from start to end
                nfa.state_mut(start).epsilon_transitions.push(end);
            }

            // Loop back for repetition
            nfa.state_mut(inner_end)
                .epsilon_transitions
                .push(inner_start);

            // Connect to end
            nfa.state_mut(inner_end).epsilon_transitions.push(end);
            if let Some(max_val) = max {
                if *max_val == 1 {
                    // No loop, just connect (already done above)
                } else {
                    // Limited repetition - would need counter, simplified for now
                }
            } else {
                // Unlimited: loop back (already done above)
            }

            nfa.state_mut(end).accepting = Some((kind, priority));
        }
        Pattern::Regex(_) => {
            // Regex patterns are handled separately, not converted to DFA
        }
    }
}

/// Compute epsilon closure of NFA states
fn epsilon_closure(nfa: &Nfa, states: &[u32]) -> HashSet<u32> {
    let mut closure: HashSet<u32> = states.iter().copied().collect();
    let mut stack: Vec<u32> = states.to_vec();

    while let Some(state) = stack.pop() {
        for &next in &nfa.states[state as usize].epsilon_transitions {
            if closure.insert(next) {
                stack.push(next);
            }
        }
    }

    closure
}

/// Convert NFA to DFA using subset construction
pub(crate) fn nfa_to_dfa(nfa: &Nfa, dfa: &mut Dfa) {
    // Map from DFA state (set of NFA states) to DFA state ID
    let mut dfa_state_map: HashMap<SmallVec<[u32; 8]>, StateId> = HashMap::new();
    let mut worklist: Vec<SmallVec<[u32; 8]>> = Vec::new();

    // Start with epsilon closure of NFA start state
    let start_closure: SmallVec<[u32; 8]> =
        epsilon_closure(nfa, &[nfa.start]).into_iter().collect();
    let start_dfa_state = dfa.add_state();
    dfa.start_state = start_dfa_state;
    dfa_state_map.insert(start_closure.clone(), start_dfa_state);
    worklist.push(start_closure);

    while let Some(nfa_states) = worklist.pop() {
        let dfa_state_id = *dfa_state_map.get(&nfa_states).unwrap();

        // Collect all transitions from this set of NFA states
        let mut char_transitions: HashMap<CharRange, HashSet<u32>> = HashMap::new();

        for &nfa_state_id in &nfa_states {
            let nfa_state = &nfa.states[nfa_state_id as usize];
            for (range, target) in &nfa_state.transitions {
                let targets = char_transitions
                    .entry(range.clone())
                    .or_insert_with(HashSet::new);
                targets.insert(*target);
            }
        }

        // Merge overlapping ranges (simplified - full implementation would be more complex)
        for (range, targets) in char_transitions {
            let target_closure: SmallVec<[u32; 8]> =
                epsilon_closure(nfa, &targets.iter().copied().collect::<Vec<_>>())
                    .into_iter()
                    .collect();

            let target_dfa_state =
                *dfa_state_map
                    .entry(target_closure.clone())
                    .or_insert_with(|| {
                        let new_state = dfa.add_state();
                        worklist.push(target_closure);
                        new_state
                    });

            dfa.state_mut(dfa_state_id)
                .add_transition(range.clone(), target_dfa_state);
        }

        // Mark accepting states
        for &nfa_state_id in &nfa_states {
            if let Some((kind, priority)) = nfa.states[nfa_state_id as usize].accepting {
                dfa.state_mut(dfa_state_id).add_accepting(kind, priority);
            }
        }

        // Finalize state: sort transitions for binary search
        dfa.state_mut(dfa_state_id).finalize();
    }
}

/// Incremental lexing state cache
#[derive(Debug, Clone)]
pub struct LexerState {
    /// Current position in input
    pos: usize,
    /// Current DFA state (if using DFA) - cached at token boundaries for fast resumption
    dfa_state: Option<StateId>,
    /// Last token boundary position
    last_boundary: usize,
}

impl Default for LexerState {
    fn default() -> Self {
        Self::new()
    }
}

impl LexerState {
    /// Create a new lexer state at the start of input
    #[must_use]
    pub const fn new() -> Self {
        Self {
            pos: 0,
            dfa_state: None,
            last_boundary: 0,
        }
    }

    /// Create a lexer state at a specific position
    #[must_use]
    pub const fn at_position(pos: usize) -> Self {
        Self {
            pos,
            dfa_state: None,
            last_boundary: pos,
        }
    }

    /// Create a lexer state at a specific position with a cached DFA state
    #[must_use]
    pub const fn at_position_with_state(pos: usize, dfa_state: Option<StateId>) -> Self {
        Self {
            pos,
            dfa_state,
            last_boundary: pos,
        }
    }

    /// Get the cached DFA state
    #[must_use]
    pub const fn dfa_state(&self) -> Option<StateId> {
        self.dfa_state
    }

    /// Set the cached DFA state
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    pub fn set_dfa_state(&mut self, state: Option<StateId>) {
        self.dfa_state = state;
    }
}

pub struct CompiledLexer<K: SyntaxKind> {
    /// Rules with their compiled forms
    pub(crate) rules: Vec<CompiledRule<K>>,
    /// DFA for simple patterns (if any)
    pub(crate) dfa: Option<Dfa>,
    /// Map from DFA kind indices to actual rule indices
    pub(crate) dfa_kind_to_rule: Vec<usize>,
    /// Keywords for post-processing
    pub(crate) keywords: hashbrown::HashMap<compact_str::CompactString, K, ahash::RandomState>,
    /// Trivia kinds to skip
    pub(crate) trivia_kinds: hashbrown::HashSet<K, ahash::RandomState>,
    /// EOF token kind
    pub(crate) eof_kind: K,
    /// Identifier token kind
    pub(crate) ident_kind: K,
    /// Keyword trie for integrated keyword matching
    pub(crate) keyword_trie: Option<KeywordTrie<K>>,
}

/// Simple trie for keyword matching
pub(crate) struct KeywordTrie<K: SyntaxKind> {
    children: HashMap<char, Box<KeywordTrie<K>>>,
    keyword: Option<(K, u32)>, // (kind, priority)
}

impl<K: SyntaxKind> Default for KeywordTrie<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: SyntaxKind> KeywordTrie<K> {
    #[must_use]
    pub(crate) fn new() -> Self {
        Self {
            children: HashMap::new(),
            keyword: None,
        }
    }

    pub(crate) fn insert(&mut self, word: &str, kind: K, priority: u32) {
        let mut node = self;
        for c in word.chars() {
            node = node.children.entry(c).or_insert_with(Box::default);
        }
        node.keyword = Some((kind, priority));
    }

    fn find_longest(&self, input: &str, pos: usize) -> Option<(usize, K, u32)> {
        let mut node = self;
        let mut best: Option<(usize, K, u32)> = None;

        for (byte_offset, c) in input[pos..].char_indices() {
            if let Some(child) = node.children.get(&c) {
                node = child;
                let len = byte_offset + c.len_utf8();
                if let Some((kind, priority)) = node.keyword {
                    best = Some((len, kind, priority));
                }
            } else {
                break;
            }
        }

        best
    }
}

impl<K: SyntaxKind> CompiledLexer<K> {
    /// Tokenize the input string using Maximal Munch.
    ///
    /// # Errors
    ///
    /// Returns a vector of lexer errors if tokenization fails at any point.
    pub fn tokenize(&self, input: &str) -> Result<Vec<Token<K>>, Vec<LexerError>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut pos = 0;
        let len = input.len();

        while pos < len {
            // 1. Skip basic whitespace and comments (for performance)
            pos = Self::skip_basic_trivia(input, pos);
            if pos >= len {
                break;
            }

            // 2. Maximal Munch: Find the longest matching rule
            match self.next_token(input, pos) {
                Ok((token, new_pos)) => {
                    // Check if identifier is actually a keyword
                    let mut final_token = token;
                    if final_token.kind == self.ident_kind
                        && let Some(kw_kind) = self.keywords.get(&final_token.text)
                    {
                        final_token.kind = *kw_kind;
                    }

                    if !self.trivia_kinds.contains(&final_token.kind) {
                        tokens.push(final_token);
                    }
                    pos = new_pos;
                }
                Err(e) => {
                    errors.push(e);
                    pos = Self::recover(input, pos);
                }
            }
        }

        // Add EOF
        tokens.push(Token::new(
            self.eof_kind,
            "",
            TextRange::at(
                TextSize::from(u32::try_from(pos).unwrap_or(0)),
                TextSize::zero(),
            ),
        ));

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    /// Tokenize incrementally, allowing resumption from a saved state
    /// This is useful for incremental parsing where only parts of the input change
    ///
    /// # Errors
    ///
    /// Returns a vector of lexer errors if tokenization fails at any point.
    pub fn tokenize_incremental(
        &self,
        input: &str,
        start_state: Option<LexerState>,
    ) -> Result<(Vec<Token<K>>, LexerState), Vec<LexerError>> {
        let mut state = start_state.unwrap_or_default();
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let len = input.len();

        while state.pos < len {
            // Skip trivia
            state.pos = Self::skip_basic_trivia(input, state.pos);
            if state.pos >= len {
                break;
            }

            // Tokenize from current position, using cached DFA state if available
            let start_dfa_state = state.dfa_state;
            match self.next_token_from_state(input, state.pos, start_dfa_state) {
                Ok((token, new_pos, end_dfa_state)) => {
                    let mut final_token = token;
                    if final_token.kind == self.ident_kind
                        && let Some(kw_kind) = self.keywords.get(&final_token.text)
                    {
                        final_token.kind = *kw_kind;
                    }

                    if !self.trivia_kinds.contains(&final_token.kind) {
                        tokens.push(final_token);
                    }

                    // Update state with new position and cached DFA state
                    state.last_boundary = state.pos;
                    state.pos = new_pos;
                    // Cache DFA state at token boundary for fast resumption
                    state.dfa_state = end_dfa_state;
                }
                Err(e) => {
                    errors.push(e);
                    state.pos = Self::recover(input, state.pos);
                }
            }
        }

        // Add EOF
        tokens.push(Token::new(
            self.eof_kind,
            "",
            TextRange::at(
                TextSize::from(u32::try_from(state.pos).unwrap_or(0)),
                TextSize::zero(),
            ),
        ));

        if errors.is_empty() {
            Ok((tokens, state))
        } else {
            Err(errors)
        }
    }

    fn next_token(&self, input: &str, pos: usize) -> Result<(Token<K>, usize), LexerError> {
        self.next_token_from_state(input, pos, None)
            .map(|(token, new_pos, _)| (token, new_pos))
    }

    /// Tokenize from a specific position, optionally starting from a cached DFA state.
    /// Returns the token, new position, and the DFA state at the end of the token (for caching).
    #[allow(clippy::too_many_lines)]
    fn next_token_from_state(
        &self,
        input: &str,
        pos: usize,
        start_dfa_state: Option<StateId>,
    ) -> Result<(Token<K>, usize, Option<StateId>), LexerError> {
        let slice = &input[pos..];
        let mut best_match: Option<(usize, u32, K, TokenValue)> = None; // (len, priority, kind, value)
        let mut end_dfa_state: Option<StateId> = None;

        // Try DFA first if available
        if let Some(dfa) = &self.dfa {
            let dfa_result = start_dfa_state.map_or_else(
                || Self::run_dfa(dfa, input, pos),
                |start_state| Self::run_dfa_from_state(dfa, input, pos, start_state),
            );

            if let Some((len, rule_idx)) = dfa_result {
                // Track the final DFA state for caching by running DFA to the end of the match
                let final_state = Self::run_dfa_to_state(dfa, input, pos, len, start_dfa_state);
                end_dfa_state = Some(final_state);

                let rule = &self.rules[self.dfa_kind_to_rule[rule_idx]];
                match rule {
                    CompiledRule::Dfa { kind, priority } => {
                        Self::update_best(&mut best_match, len, *priority, *kind, TokenValue::None);
                    }
                    _ => unreachable!(),
                }
            }
        }

        // Try keyword trie if available
        if let Some(trie) = &self.keyword_trie
            && let Some((len, kind, priority)) = trie.find_longest(input, pos)
        {
            Self::update_best(&mut best_match, len, priority, kind, TokenValue::None);
        }

        // Try literal patterns (fast path)
        // Collect matching literals and sort by length descending for Maximal Munch
        let mut literal_matches: SmallVec<[(usize, u32, K); 8]> = SmallVec::new();
        for rule in &self.rules {
            if let CompiledRule::Literal {
                kind,
                literal,
                priority,
            } = rule
                && slice.starts_with(literal.as_str())
            {
                literal_matches.push((literal.len(), *priority, *kind));
            }
        }
        // Sort by length descending, then priority ascending
        literal_matches.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
        if let Some((len, priority, kind)) = literal_matches.first() {
            Self::update_best(&mut best_match, *len, *priority, *kind, TokenValue::None);
        }

        // Try regex fallback
        for rule in &self.rules {
            match rule {
                CompiledRule::Regex {
                    kind,
                    regex,
                    priority,
                } => {
                    if let Some(m) = regex.find(slice) {
                        let len = m.end();
                        Self::update_best(&mut best_match, len, *priority, *kind, TokenValue::None);
                    }
                }
                CompiledRule::Custom {
                    kind,
                    matcher,
                    priority,
                } => {
                    if let Some((len, value)) = matcher(input, pos) {
                        Self::update_best(&mut best_match, len, *priority, *kind, value);
                    }
                }
                _ => {} // DFA, Literal already handled
            }
        }

        // If no candidates found, try identifier pattern as fallback
        if best_match.is_none()
            && let Some((len, kind)) =
                Self::match_identifier_fallback(input, pos, self.ident_kind, &self.keywords)
        {
            let range = TextRange::at(
                TextSize::from(u32::try_from(pos).unwrap_or(0)),
                TextSize::from(u32::try_from(len).unwrap_or(0)),
            );
            return Ok((
                Token::new(kind, &input[pos..pos + len], range),
                pos + len,
                end_dfa_state,
            ));
        }

        if let Some((len, _, kind, value)) = best_match {
            let range = TextRange::at(
                TextSize::from(u32::try_from(pos).unwrap_or(0)),
                TextSize::from(u32::try_from(len).unwrap_or(0)),
            );
            Ok((
                Token {
                    kind,
                    text: compact_str::CompactString::from(&input[pos..pos + len]),
                    range,
                    value,
                },
                pos + len,
                end_dfa_state,
            ))
        } else {
            Err(LexerError {
                span: TextRange::at(
                    TextSize::from(u32::try_from(pos).unwrap_or(0)),
                    TextSize::from(1),
                ),
                kind: crate::error::LexerErrorKind::UnexpectedChar {
                    char: input[pos..].chars().next().unwrap_or('\0'),
                },
            })
        }
    }

    /// Run DFA on input starting at pos, return (length, `rule_index`) of longest match
    /// Optimized with cached state lookups and O(1) accepting state access
    fn run_dfa(dfa: &Dfa, input: &str, pos: usize) -> Option<(usize, usize)> {
        Self::run_dfa_from_state(dfa, input, pos, dfa.start_state)
    }

    /// Run DFA on input starting at pos with a specific starting state
    /// Returns (length, `rule_index`) of longest match
    fn run_dfa_from_state(
        dfa: &Dfa,
        input: &str,
        pos: usize,
        start_state: StateId,
    ) -> Option<(usize, usize)> {
        let mut state = start_state;
        let mut best_match: Option<(usize, usize)> = None; // (length, rule_index)
        let mut current_len = 0;

        // Check initial state (cached O(1) lookup)
        if let Some(best_accepting) = dfa.state(state).best_accepting() {
            best_match = Some((0, best_accepting.kind as usize));
        }

        // Run DFA with Maximal Munch
        // Use byte iteration for ASCII characters (faster), fall back to char iteration for Unicode
        let bytes = input.as_bytes();
        let mut byte_pos = pos;

        while byte_pos < bytes.len() {
            // Fast path: ASCII character (single byte)
            if bytes[byte_pos].is_ascii() {
                let c = bytes[byte_pos] as char;
                if let Some(next_state) = dfa.state(state).find_transition(c) {
                    state = next_state;
                    current_len += 1;
                    byte_pos += 1;

                    // Check if this state is accepting (O(1) cached lookup)
                    if let Some(best_accepting) = dfa.state(state).best_accepting() {
                        best_match = Some((current_len, best_accepting.kind as usize));
                    }
                } else {
                    break;
                }
            } else {
                // Unicode character: use char iteration
                let remaining = &input[byte_pos..];
                let Some(c) = remaining.chars().next() else {
                    break;
                };
                let char_len = c.len_utf8();

                if let Some(next_state) = dfa.state(state).find_transition(c) {
                    state = next_state;
                    current_len += char_len;
                    byte_pos += char_len;

                    // Check if this state is accepting (O(1) cached lookup)
                    if let Some(best_accepting) = dfa.state(state).best_accepting() {
                        best_match = Some((current_len, best_accepting.kind as usize));
                    }
                } else {
                    break;
                }
            }
        }

        best_match
    }

    /// Run DFA to a specific length and return the final state
    /// This is used to cache the DFA state at token boundaries
    fn run_dfa_to_state(
        dfa: &Dfa,
        input: &str,
        pos: usize,
        len: usize,
        start_state: Option<StateId>,
    ) -> StateId {
        let mut state = start_state.unwrap_or(dfa.start_state);
        let bytes = input.as_bytes();
        let mut byte_pos = pos;
        let mut current_len = 0;

        while byte_pos < bytes.len() && current_len < len {
            // Fast path: ASCII character (single byte)
            if bytes[byte_pos].is_ascii() {
                let c = bytes[byte_pos] as char;
                if let Some(next_state) = dfa.state(state).find_transition(c) {
                    state = next_state;
                    current_len += 1;
                    byte_pos += 1;
                } else {
                    break;
                }
            } else {
                // Unicode character: use char iteration
                let remaining = &input[byte_pos..];
                let Some(c) = remaining.chars().next() else {
                    break;
                };
                let char_len = c.len_utf8();

                if let Some(next_state) = dfa.state(state).find_transition(c) {
                    state = next_state;
                    current_len += char_len;
                    byte_pos += char_len;
                } else {
                    break;
                }
            }
        }

        state
    }

    fn update_best(
        best: &mut Option<(usize, u32, K, TokenValue)>,
        len: usize,
        prio: u32,
        kind: K,
        val: TokenValue,
    ) {
        match best {
            Some((best_len, best_prio, _, _)) => {
                // Rule: Longest match wins. Ties broken by priority (lower priority ID usually means defined earlier/higher precedence).
                if len > *best_len || (len == *best_len && prio < *best_prio) {
                    *best = Some((len, prio, kind, val));
                }
            }
            None => *best = Some((len, prio, kind, val)),
        }
    }

    fn skip_basic_trivia(input: &str, mut pos: usize) -> usize {
        // Fast path for skipping whitespace
        let bytes = input.as_bytes();
        while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
            pos += 1;
        }

        // Check for comments
        let remaining = &input[pos..];
        if remaining.starts_with("//") {
            pos = memchr::memchr(b'\n', &bytes[pos..]).map_or(input.len(), |p| pos + p + 1);
        } else if remaining.starts_with("/*") {
            // Skip block comment
            let mut comment_pos = pos + 2;
            while comment_pos + 1 < input.len() {
                if input[comment_pos..].starts_with("*/") {
                    pos = comment_pos + 2;
                    break;
                }
                comment_pos += 1;
            }
            if comment_pos + 1 >= input.len() {
                pos = input.len();
            }
        }

        pos
    }

    fn recover(input: &str, pos: usize) -> usize {
        pos + input[pos..].chars().next().map_or(1, char::len_utf8)
    }

    /// Fallback identifier matching when no patterns match
    /// This handles identifiers that start with alphabetic characters
    /// Optimized to avoid creating `CompactString` until necessary
    fn match_identifier_fallback(
        input: &str,
        pos: usize,
        ident_kind: K,
        keywords: &hashbrown::HashMap<compact_str::CompactString, K, ahash::RandomState>,
    ) -> Option<(usize, K)> {
        let remaining = &input[pos..];
        if remaining.is_empty() {
            return None;
        }

        // Check if first character is a valid identifier start
        let first_char = remaining.chars().next()?;

        #[cfg(feature = "unicode")]
        let is_ident_start = unicode_ident::is_xid_start(first_char);

        #[cfg(not(feature = "unicode"))]
        let is_ident_start = first_char.is_alphabetic() || first_char == '_';

        if !is_ident_start {
            return None;
        }

        // Match identifier: start character followed by identifier continue characters
        let mut len = first_char.len_utf8();
        for c in remaining.chars().skip(1) {
            #[cfg(feature = "unicode")]
            let is_ident_continue = unicode_ident::is_xid_continue(c);

            #[cfg(not(feature = "unicode"))]
            let is_ident_continue = c.is_alphanumeric() || c == '_';

            if is_ident_continue {
                len += c.len_utf8();
            } else {
                break;
            }
        }

        let text = &remaining[..len];
        let mut kind = ident_kind;

        // Check for keywords - only create CompactString if we have keywords to check
        if !keywords.is_empty() {
            // Use temporary CompactString for lookup
            let text_compact = compact_str::CompactString::from(text);
            if let Some(kw_kind) = keywords.get(&text_compact) {
                kind = *kw_kind;
            }
        }

        Some((len, kind))
    }
}
