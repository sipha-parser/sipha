use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use hashbrown::{HashMap, HashSet};
use std::hash::Hash;

/// LR(1) parsing table with action and goto tables
pub struct LrParsingTable<T, N> {
    /// Action table: (state, token) -> Action
    action_table: ActionTable<T>,

    /// Goto table: (state, non-terminal) -> state
    goto_table: GotoTable<N>,

    /// Number of states
    num_states: usize,

    /// Productions indexed by number
    productions: Vec<Production<T, N>>,

    /// Entry production (augmented start symbol)
    _entry_production: usize,
}

/// Key for action table lookup
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ActionKey<T> {
    Token(T),
    Eof,
}

impl<T> ActionKey<T> {
    /// Create an action key from a token
    #[must_use]
    const fn token(token: T) -> Self {
        Self::Token(token)
    }

    /// Create an EOF action key
    #[must_use]
    const fn eof() -> Self {
        Self::Eof
    }
}

/// LR parsing action
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    /// Shift to state
    Shift(usize),
    /// Reduce using production
    Reduce(usize),
    /// Accept (successful parse)
    Accept,
    /// Error (no action)
    Error,
}

impl Action {
    /// Create a shift action to the given state
    #[must_use]
    pub const fn shift(state: usize) -> Self {
        Self::Shift(state)
    }

    /// Create a reduce action using the given production index
    #[must_use]
    pub const fn reduce(production: usize) -> Self {
        Self::Reduce(production)
    }

    /// Create an accept action
    #[must_use]
    pub const fn accept() -> Self {
        Self::Accept
    }

    /// Create an error action
    #[must_use]
    pub const fn error() -> Self {
        Self::Error
    }
}

/// Type alias for action table: (state, token) -> Action
type ActionTable<T> = HashMap<(usize, ActionKey<T>), Action, ahash::RandomState>;

/// Type alias for goto table: (state, non-terminal) -> state
type GotoTable<N> = HashMap<(usize, N), usize, ahash::RandomState>;

/// Type alias for LR state transitions: symbol -> set of items
type Transition<T, N> = (Symbol<T, N>, HashSet<LrItem<T, N>>);

/// Production rule for LR parsing
#[derive(Debug, Clone)]
pub struct Production<T, N> {
    pub lhs: N,
    pub rhs: Vec<ProductionItem<T, N>>,
    /// Production ID for internal tracking and debugging
    ///
    /// This field is reserved for future use in:
    /// - Production statistics and profiling
    /// - Debug output and error messages
    /// - Performance analysis tools
    #[allow(dead_code)] // Reserved for future use in debugging/statistics
    pub production_id: usize,
}

impl<T, N> Production<T, N> {
    /// Create a new production rule
    #[must_use]
    pub const fn new(lhs: N, rhs: Vec<ProductionItem<T, N>>, production_id: usize) -> Self {
        Self {
            lhs,
            rhs,
            production_id,
        }
    }

    /// Create a new production with a single token on the right-hand side
    #[must_use]
    pub fn with_token(lhs: N, token: T, production_id: usize) -> Self {
        Self::new(lhs, vec![ProductionItem::Token(token)], production_id)
    }

    /// Create a new production with a single non-terminal on the right-hand side
    #[must_use]
    pub fn with_non_terminal(lhs: N, nt: N, production_id: usize) -> Self {
        Self::new(lhs, vec![ProductionItem::NonTerminal(nt)], production_id)
    }

    /// Create an empty production (epsilon production)
    #[must_use]
    pub const fn empty(lhs: N, production_id: usize) -> Self {
        Self::new(lhs, vec![], production_id)
    }
}

/// Item in a production (terminal or non-terminal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProductionItem<T, N> {
    Token(T),
    NonTerminal(N),
}

impl<T, N> ProductionItem<T, N> {
    /// Create a production item from a token
    #[must_use]
    pub const fn token(token: T) -> Self {
        Self::Token(token)
    }

    /// Create a production item from a non-terminal
    #[must_use]
    pub const fn non_terminal(nt: N) -> Self {
        Self::NonTerminal(nt)
    }
}

impl<T, N> LrParsingTable<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Create a new LR parsing table from a grammar.
    ///
    /// # Errors
    ///
    /// Returns an error string if:
    /// - The grammar cannot be converted to LR productions
    /// - The LR automaton construction fails
    /// - Conflicts are detected that cannot be resolved
    pub fn new(grammar: &Grammar<T, N>, use_lalr: bool) -> Result<Self, String> {
        // Convert grammar to productions
        let mut productions = Vec::new();

        // Add augmented start production: S' -> S
        let entry_nt = grammar.entry_point();
        let entry_production = productions.len();
        productions.push(Production::with_non_terminal(
            entry_nt.clone(),
            entry_nt.clone(),
            0,
        ));

        // Convert grammar rules to productions
        for (lhs, rule) in grammar.rules() {
            Self::rule_to_productions(lhs, &rule.rhs, &mut productions)?;
        }

        // Pre-compute FOLLOW sets for use in closure and reduce actions
        let follow_sets = grammar.compute_follow_sets();

        // Build LR(1) automaton
        let (states, num_states) = if use_lalr {
            Self::build_lalr1_automaton(grammar, &productions, &follow_sets)
        } else {
            Self::build_lr1_automaton(grammar, &productions)
        };

        // Build action and goto tables
        let (action_table, goto_table) = Self::build_tables(
            grammar,
            &productions,
            &states,
            num_states,
            entry_production,
            &follow_sets,
        );

        Ok(Self {
            action_table,
            goto_table,
            num_states,
            productions,
            _entry_production: entry_production,
        })
    }

    /// Convert a grammar rule expression to a list of productions
    fn rule_to_productions(
        lhs: &N,
        expr: &Expr<T, N>,
        productions: &mut Vec<Production<T, N>>,
    ) -> Result<(), String> {
        match expr {
            Expr::Choice(alternatives) => {
                for alt in alternatives {
                    Self::expr_to_production_items(alt, productions, lhs)?;
                }
            }
            _ => {
                Self::expr_to_production_items(expr, productions, lhs)?;
            }
        }
        Ok(())
    }

    /// Convert an expression to production items
    #[allow(clippy::too_many_lines)] // Complex function that handles many expression types
    fn expr_to_production_items(
        expr: &Expr<T, N>,
        productions: &mut Vec<Production<T, N>>,
        lhs: &N,
    ) -> Result<(), String> {
        match expr {
            Expr::Choice(alternatives) => {
                // Expand choice into separate productions for each alternative
                for alt in alternatives {
                    Self::expr_to_production_items(alt, productions, lhs)?;
                }
            }
            Expr::Seq(exprs) => {
                let mut items = Vec::new();
                for e in exprs {
                    Self::flatten_expr_to_items(e, &mut items)?;
                }
                let prod_id = productions.len();
                productions.push(Production::new(lhs.clone(), items, prod_id));
            }
            Expr::Token(t) => {
                let prod_id = productions.len();
                productions.push(Production::with_token(lhs.clone(), t.clone(), prod_id));
            }
            Expr::Rule(n) => {
                let prod_id = productions.len();
                productions.push(Production::with_non_terminal(
                    lhs.clone(),
                    n.clone(),
                    prod_id,
                ));
            }
            Expr::Empty => {
                let prod_id = productions.len();
                productions.push(Production::empty(lhs.clone(), prod_id));
            }
            Expr::Opt(inner) => {
                // Expand Opt(expr) to two productions: one with expr, one empty
                // Production 1: lhs -> expr
                Self::expr_to_production_items(inner, productions, lhs)?;
                // Production 2: lhs -> ε (empty)
                let prod_id = productions.len();
                productions.push(Production::empty(lhs.clone(), prod_id));
            }
            Expr::Repeat {
                expr,
                min,
                max,
                greedy: _,
            } => {
                // For bounded repeats with small max, expand to multiple productions
                if let Some(max_val) = max {
                    if *max_val <= 10 {
                        // Expand to productions for each count from min to max
                        for count in *min..=*max_val {
                            let mut items = Vec::new();
                            for _ in 0..count {
                                Self::flatten_expr_to_items(expr, &mut items)?;
                            }
                            let prod_id = productions.len();
                            productions.push(Production::new(lhs.clone(), items, prod_id));
                        }
                    } else {
                        // High-count bounded repeat: use recursive productions
                        // First, create productions for the minimum count
                        if *min > 0 {
                            let mut items = Vec::new();
                            for _ in 0..*min {
                                Self::flatten_expr_to_items(expr, &mut items)?;
                            }
                            let prod_id = productions.len();
                            productions.push(Production::new(lhs.clone(), items, prod_id));
                        }

                        // Then create recursive productions for additional items
                        // Pattern: lhs -> expr lhs (for 1 more item)
                        let mut recursive_items = Vec::new();
                        Self::flatten_expr_to_items(expr, &mut recursive_items)?;
                        recursive_items.push(ProductionItem::NonTerminal(lhs.clone()));
                        let prod_id = productions.len();
                        productions.push(Production::new(lhs.clone(), recursive_items, prod_id));

                        // Also add empty production if min is 0
                        if *min == 0 {
                            let prod_id = productions.len();
                            productions.push(Production::empty(lhs.clone(), prod_id));
                        }

                        // Note: The bounded nature (max) is not enforced by the grammar
                        // but by the parser's runtime behavior. This is acceptable for LR parsers.
                    }
                } else {
                    // Unbounded repeat: use recursive productions
                    if *min == 0 {
                        // Pattern: lhs -> ε | expr lhs
                        // Empty production
                        let prod_id = productions.len();
                        productions.push(Production::empty(lhs.clone(), prod_id));

                        // Recursive production: lhs -> expr lhs
                        let mut recursive_items = Vec::new();
                        Self::flatten_expr_to_items(expr, &mut recursive_items)?;
                        recursive_items.push(ProductionItem::NonTerminal(lhs.clone()));
                        let prod_id = productions.len();
                        productions.push(Production::new(lhs.clone(), recursive_items, prod_id));
                    } else if *min == 1 {
                        // Pattern: lhs -> expr | expr lhs
                        // Single item production
                        let mut items = Vec::new();
                        Self::flatten_expr_to_items(expr, &mut items)?;
                        let prod_id = productions.len();
                        productions.push(Production::new(lhs.clone(), items, prod_id));
                    } else {
                        // min > 1: create productions for min count, then recursive
                        // Productions for exact min count
                        let mut items = Vec::new();
                        for _ in 0..*min {
                            Self::flatten_expr_to_items(expr, &mut items)?;
                        }
                        let prod_id = productions.len();
                        productions.push(Production::new(lhs.clone(), items, prod_id));
                    }
                    // Recursive production: lhs -> expr lhs (for additional items)
                    // This is common to both min == 1 and min > 1 cases
                    let mut recursive_items = Vec::new();
                    Self::flatten_expr_to_items(expr, &mut recursive_items)?;
                    recursive_items.push(ProductionItem::NonTerminal(lhs.clone()));
                    let prod_id = productions.len();
                    productions.push(Production::new(lhs.clone(), recursive_items, prod_id));
                }
            }
            Expr::Delimited {
                open,
                content,
                close,
                ..
            } => {
                // Flatten delimited expression: open content close
                let mut items = Vec::new();
                Self::flatten_expr_to_items(open, &mut items)?;
                Self::flatten_expr_to_items(content, &mut items)?;
                Self::flatten_expr_to_items(close, &mut items)?;
                let prod_id = productions.len();
                productions.push(Production::new(lhs.clone(), items, prod_id));
            }
            Expr::Separated {
                item,
                separator,
                min,
                ..
            } => {
                // For separated lists, we create productions for the minimum count
                // Additional items are handled by repetition
                if *min > 0 {
                    let mut items = Vec::new();
                    Self::flatten_expr_to_items(item, &mut items)?;
                    // Add separator-item pairs for min > 1
                    for _ in 1..*min {
                        Self::flatten_expr_to_items(separator, &mut items)?;
                        Self::flatten_expr_to_items(item, &mut items)?;
                    }
                    let prod_id = productions.len();
                    productions.push(Production::new(lhs.clone(), items, prod_id));
                }
                // Empty production if min is 0
                if *min == 0 {
                    let prod_id = productions.len();
                    productions.push(Production::empty(lhs.clone(), prod_id));
                }
            }
            Expr::Label { expr: inner, .. } => {
                // Labels are metadata - process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Node { expr: inner, .. } => {
                // Node kind is metadata - process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Flatten(inner) => {
                // Flatten removes nesting - process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Prune(inner) => {
                // Prune removes nodes - process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Lookahead(inner) | Expr::NotLookahead(inner) => {
                // Lookahead predicates don't consume input - process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::RecoveryPoint { expr: inner, .. } => {
                // Recovery points are error handling - process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Cut(inner) => {
                // Cut operator: LR parsers don't backtrack, so this is a no-op
                // Process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::TokenClass { .. } => {
                // Token classes need to be expanded to specific tokens
                // For now, return an error suggesting to use specific tokens
                return Err(
                    "TokenClass expressions are not directly supported in LR parser. \
                     Consider using specific Token expressions or expanding the token class \
                     into a Choice of tokens."
                        .to_string(),
                );
            }
            Expr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                // Conditional: expand to productions for both branches
                // First, create a production for the condition + then_expr
                let mut then_items = Vec::new();
                Self::flatten_expr_to_items(condition, &mut then_items)?;
                Self::flatten_expr_to_items(then_expr, &mut then_items)?;
                let prod_id = productions.len();
                productions.push(Production::new(lhs.clone(), then_items, prod_id));
                // If else_expr is provided, create a production for it
                if let Some(else_expr) = else_expr {
                    let mut else_items = Vec::new();
                    Self::flatten_expr_to_items(else_expr, &mut else_items)?;
                    let prod_id = productions.len();
                    productions.push(Production::new(lhs.clone(), else_items, prod_id));
                }
            }
            Expr::SemanticPredicate { expr: inner, .. } => {
                // Semantic predicates are checked during reduce actions
                // For table construction, process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Capture { expr: inner, .. } => {
                // Capture is not fully supported in LR parsers (no backreferences)
                // Just process the inner expression
                Self::expr_to_production_items(inner, productions, lhs)?;
            }
            Expr::Backreference { .. } => {
                // Backreferences are not well-supported in LR parsers
                return Err("Backreference expressions are not supported in LR parser. \
                     Consider rewriting the grammar to avoid backreferences."
                    .to_string());
            }
            Expr::Any | Expr::Eof => {
                // These need special handling
                return Err(format!(
                    "Expression type {expr:?} is not directly supported in LR parser. \
                     Consider rewriting using Token, Rule, Seq, Choice, Opt, or Repeat."
                ));
            }
        }
        Ok(())
    }

    /// Flatten an expression into production items
    #[allow(clippy::too_many_lines)]
    fn flatten_expr_to_items(
        expr: &Expr<T, N>,
        items: &mut Vec<ProductionItem<T, N>>,
    ) -> Result<(), String> {
        match expr {
            Expr::Token(t) => {
                items.push(ProductionItem::token(t.clone()));
            }
            Expr::Rule(n) => {
                items.push(ProductionItem::non_terminal(n.clone()));
            }
            Expr::Seq(exprs) => {
                for e in exprs {
                    Self::flatten_expr_to_items(e, items)?;
                }
            }
            Expr::Empty => {
                // Empty production - no items
            }
            Expr::Opt(inner) => {
                // For flattening, Opt is treated as the inner expression
                // The optionality is handled at the production level
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Repeat {
                expr: inner,
                greedy: _,
                ..
            } => {
                // For flattening, Repeat is treated as the inner expression
                // The repetition is handled at the production level
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Delimited {
                open,
                content,
                close,
                ..
            } => {
                // Flatten delimited expression: open content close
                Self::flatten_expr_to_items(open, items)?;
                Self::flatten_expr_to_items(content, items)?;
                Self::flatten_expr_to_items(close, items)?;
            }
            Expr::Separated {
                item,
                separator,
                min,
                ..
            } => {
                // For LR parsing, we flatten separated lists into a sequence
                // The minimum count determines how many item-separator pairs we need
                // For simplicity, we flatten as: item (separator item)*
                // Note: This is a simplified representation - actual parsing handles repetition
                if *min > 0 {
                    Self::flatten_expr_to_items(item, items)?;
                    // For min > 1, we need additional item-separator pairs
                    for _ in 1..*min {
                        Self::flatten_expr_to_items(separator, items)?;
                        Self::flatten_expr_to_items(item, items)?;
                    }
                }
                // Additional repetitions are handled by the parser's repeat logic
            }
            Expr::Label { expr: inner, .. } => {
                // Labels are metadata - flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Node { expr: inner, .. } => {
                // Node kind is metadata - flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Flatten(inner) => {
                // Flatten removes one level of nesting - flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Prune(inner) => {
                // Prune removes nodes from tree - flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Lookahead(inner) | Expr::NotLookahead(inner) => {
                // Lookahead predicates don't consume input - flatten the inner expression
                // Note: LR parsers handle lookahead differently, but we can still flatten
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::RecoveryPoint { expr: inner, .. } => {
                // Recovery points are error handling metadata - flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Cut(inner) => {
                // Cut operator: LR parsers don't backtrack, so flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::TokenClass { .. } => {
                // Token classes need specific tokens - can't flatten directly
                return Err("TokenClass expressions cannot be flattened in LR parser. \
                     Use specific Token expressions instead."
                    .to_string());
            }
            Expr::Conditional { .. } => {
                // Conditionals need to be expanded at the production level
                return Err(
                    "Conditional expressions must be expanded at the production level \
                     for LR parsing."
                        .to_string(),
                );
            }
            Expr::SemanticPredicate { expr: inner, .. } => {
                // Semantic predicates are checked during reduce - flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Capture { expr: inner, .. } => {
                // Capture is not fully supported in LR parsers (no backreferences)
                // Just flatten the inner expression
                Self::flatten_expr_to_items(inner, items)?;
            }
            Expr::Backreference { .. } => {
                // Backreferences are not supported
                return Err("Backreference expressions are not supported in LR parser.".to_string());
            }
            Expr::Any | Expr::Eof => {
                // These are special cases that need special handling
                // For now, we'll return an error with guidance
                return Err(format!(
                    "Expression type {expr:?} is not directly supported in LR parser. \
                     Consider rewriting using Token, Rule, Seq, Choice, Opt, or Repeat."
                ));
            }
            Expr::Choice(_) => {
                // Choices need to be expanded into separate productions
                // This is handled at a higher level when building productions
                return Err(
                    "Choice expressions must be expanded into separate productions for LR parsing. \
                     The grammar builder should handle this automatically.".to_string()
                );
            }
        }
        Ok(())
    }

    /// Build LR(1) automaton (canonical LR)
    ///
    /// Canonical LR(1) construction builds states distinguished by both core items
    /// (production and dot position) AND lookahead sets. Unlike LALR(1), states with
    /// the same core but different lookahead sets are kept separate.
    ///
    /// This produces larger parsing tables than LALR(1) but can handle more complex
    /// grammars that LALR(1) cannot parse correctly.
    fn build_lr1_automaton(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
    ) -> (Vec<LrState<T, N>>, usize) {
        // Pre-compute FOLLOW sets for use in closure
        let follow_sets = grammar.compute_follow_sets();

        // Build LR(1) items with full lookahead sets (not merged like LALR)
        let mut states = Vec::new();
        let mut state_map: HashMap<LrState<T, N>, usize> = HashMap::new();

        // Create initial state with item [S' -> .S, $]
        // For LR(1), we use a special lookahead token if available, or None
        // The reduce action for entry production will handle EOF explicitly
        let mut initial_items: HashSet<LrItem<T, N>> = HashSet::new();
        // Start with item without lookahead - closure will propagate lookahead properly
        initial_items.insert(LrItem::without_lookahead(0, 0));

        let initial_state = LrState::new(initial_items);
        let initial_state_id = states.len();
        states.push(initial_state.clone());
        state_map.insert(initial_state, initial_state_id);

        // Build closure and transitions
        let mut worklist = vec![initial_state_id];
        let mut visited = HashSet::new();
        visited.insert(initial_state_id);

        while let Some(state_id) = worklist.pop() {
            let state = &states[state_id];

            // Compute closure with full lookahead propagation
            let closure = Self::closure(grammar, productions, state, &follow_sets);

            // Find transitions
            let transitions = Self::compute_transitions(grammar, productions, &closure);

            for (_symbol, next_items) in transitions {
                // Compute closure of the next state
                let next_state_base = LrState::new(next_items);
                let next_closure =
                    Self::closure(grammar, productions, &next_state_base, &follow_sets);
                let next_state = LrState::new(next_closure);

                // For canonical LR(1), states are distinguished by BOTH core AND lookahead
                // No merging happens - each distinct set of items (including lookahead) is a separate state
                if let Some(&id) = state_map.get(&next_state) {
                    // State already exists (same core AND lookahead) - reuse it
                    let _ = id; // Suppress unused warning
                } else {
                    // New state - add it
                    let id = states.len();
                    states.push(next_state.clone());
                    state_map.insert(next_state, id);
                    if visited.insert(id) {
                        worklist.push(id);
                    }
                }
            }
        }

        let num_states = states.len();
        (states, num_states)
    }

    /// Build LALR(1) automaton (more practical, smaller tables)
    fn build_lalr1_automaton(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        follow_sets: &hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
    ) -> (Vec<LrState<T, N>>, usize) {
        // Build LR(0) items first
        let mut states = Vec::new();
        let mut state_map: HashMap<LrState<T, N>, usize> = HashMap::new();
        let mut closure_cache: Vec<Option<HashSet<LrItem<T, N>>>> = Vec::new();

        // Create initial state with item [S' -> .S, $]
        let mut initial_items: HashSet<LrItem<T, N>> = HashSet::new();
        initial_items.insert(LrItem::without_lookahead(0, 0));

        let initial_state = LrState::new(initial_items);
        let initial_state_id = states.len();
        states.push(initial_state.clone());
        state_map.insert(initial_state, initial_state_id);
        closure_cache.push(None);

        // Build closure and transitions
        let mut worklist = vec![initial_state_id];
        let mut visited = HashSet::new();
        visited.insert(initial_state_id);

        while let Some(state_id) = worklist.pop() {
            let state = &states[state_id];

            // Compute closure
            let closure = if let Some(Some(cached)) = closure_cache.get(state_id) {
                cached.clone()
            } else {
                let computed = Self::closure(grammar, productions, state, follow_sets);
                if let Some(slot) = closure_cache.get_mut(state_id) {
                    *slot = Some(computed.clone());
                }
                computed
            };

            // Find transitions
            let transitions = Self::compute_transitions(grammar, productions, &closure);

            for (_symbol, next_items) in transitions {
                // Compute closure of the next state
                let next_state_base = LrState::new(next_items);
                let next_closure =
                    Self::closure(grammar, productions, &next_state_base, follow_sets);
                let next_state = LrState::new(next_closure.clone());

                if let Some(&id) = state_map.get(&next_state) {
                    // State already exists, no need to add it again
                    let _ = id; // Suppress unused warning
                } else {
                    let id = states.len();
                    states.push(next_state.clone());
                    state_map.insert(next_state, id);
                    closure_cache.push(Some(next_closure));
                    if visited.insert(id) {
                        worklist.push(id);
                    }
                }

                // Transition is tracked implicitly through state_map
                // The goto table will be built from these states
            }
        }

        let num_states = states.len();
        (states, num_states)
    }

    /// Compute closure of LR items
    ///
    /// For LALR(1), we propagate lookahead from parent items to child items.
    /// If an item has [A -> α.Bβ, a] and B -> γ is a production,
    /// we add [B -> .γ, b] where b is in FIRST(βa).
    /// If β is nullable and a is not available, we use FOLLOW(A).
    fn closure(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        state: &LrState<T, N>,
        follow_sets: &hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
    ) -> HashSet<LrItem<T, N>> {
        let mut closure = state.items.clone();
        let mut changed = true;

        while changed {
            changed = false;
            let mut items_to_add = Vec::new();

            // Collect items to add (avoid borrowing closure mutably while iterating)
            for item in &closure {
                let prod = &productions[item.production];
                if item.dot < prod.rhs.len()
                    && let ProductionItem::NonTerminal(nt) = &prod.rhs[item.dot]
                {
                    // Compute FIRST(βa) where β is the suffix after the dot and a is the lookahead
                    let first_beta =
                        Self::first_of_suffix(grammar, productions, &prod.rhs, item.dot + 1);

                    // Determine lookahead tokens
                    // For [A -> α.Bβ, a]:
                    // - If FIRST(β) is not empty, use FIRST(β)
                    // - If β is nullable and we have lookahead a, use {a}
                    // - If β is nullable and no lookahead, use FOLLOW(A)
                    let lookahead_tokens: Vec<T> = if !first_beta.is_empty() {
                        // Use FIRST(β) as lookahead
                        first_beta.into_iter().collect()
                    } else if let Some(lookahead) = &item.lookahead {
                        // If β is nullable, use the parent's lookahead
                        vec![lookahead.clone()]
                    } else {
                        // β is nullable and no lookahead - use FOLLOW(A) where A is the LHS
                        let lhs = &productions[item.production].lhs;
                        follow_sets.get(lhs).map_or_else(
                            || {
                                // Fallback: if FOLLOW set not available, use all tokens
                                // This shouldn't happen in a well-formed grammar
                                grammar
                                    .rules()
                                    .flat_map(|(_, rule)| rule.rhs.first_set(grammar))
                                    .collect()
                            },
                            |follow| follow.iter().cloned().collect(),
                        )
                    };

                    // Add items for all productions of this non-terminal
                    for (i, p) in productions.iter().enumerate() {
                        if p.lhs == *nt {
                            for token in &lookahead_tokens {
                                let new_item = LrItem::with_lookahead(i, 0, token.clone());
                                if !closure.contains(&new_item) {
                                    items_to_add.push(new_item);
                                }
                            }
                        }
                    }
                }
            }

            // Add new items
            for item in items_to_add {
                if closure.insert(item) {
                    changed = true;
                }
            }
        }

        closure
    }

    /// Compute FIRST set of suffix after dot
    fn first_of_suffix(
        grammar: &Grammar<T, N>,
        _productions: &[Production<T, N>],
        rhs: &[ProductionItem<T, N>],
        start: usize,
    ) -> HashSet<T> {
        let mut result = HashSet::new();

        for item in rhs.iter().skip(start) {
            match item {
                ProductionItem::Token(t) => {
                    result.insert(t.clone());
                    return result;
                }
                ProductionItem::NonTerminal(nt) => {
                    if let Some(rule) = grammar.get_rule(nt) {
                        let first = rule.rhs.first_set(grammar);
                        result.extend(first);
                        if !rule.rhs.is_nullable(grammar) {
                            return result;
                        }
                    }
                }
            }
        }

        result
    }

    /// Compute transitions (GOTO sets) from a closure
    ///
    /// For each symbol (terminal or non-terminal) that appears after the dot
    /// in some item in the closure, compute the set of items reachable by
    /// advancing the dot over that symbol.
    fn compute_transitions(
        _grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        closure: &HashSet<LrItem<T, N>>,
    ) -> Vec<Transition<T, N>> {
        let mut transitions: HashMap<Symbol<T, N>, HashSet<LrItem<T, N>>> = HashMap::new();

        // For each item in the closure, if the dot is not at the end,
        // add a transition on the symbol after the dot
        for item in closure {
            let prod = &productions[item.production];
            if item.dot < prod.rhs.len() {
                let symbol = match &prod.rhs[item.dot] {
                    ProductionItem::Token(t) => Symbol::token(t.clone()),
                    ProductionItem::NonTerminal(nt) => Symbol::non_terminal(nt.clone()),
                };

                // Create new item with dot advanced
                let new_item = LrItem::new(item.production, item.dot + 1, item.lookahead.clone());

                transitions
                    .entry(symbol)
                    .or_insert_with(HashSet::new)
                    .insert(new_item);
            }
        }

        transitions.into_iter().collect()
    }

    /// Build state transitions map from LR automaton states.
    ///
    /// Returns a map from (`state_id`, symbol) to `next_state_id`.
    fn build_state_transitions(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        states: &[LrState<T, N>],
        follow_sets: &hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
    ) -> HashMap<(usize, Symbol<T, N>), usize> {
        let mut state_transitions: HashMap<(usize, Symbol<T, N>), usize> = HashMap::new();
        let mut worklist = vec![0];
        let mut visited = HashSet::new();
        visited.insert(0);

        while let Some(state_id) = worklist.pop() {
            let state = &states[state_id];
            let closure = Self::closure(grammar, productions, state, follow_sets);
            let transitions = Self::compute_transitions(grammar, productions, &closure);

            for (symbol, next_items) in transitions {
                // Compute closure of next state
                let next_state_base = LrState::new(next_items);
                let next_closure =
                    Self::closure(grammar, productions, &next_state_base, follow_sets);
                let next_state = LrState::new(next_closure);

                // Find the state ID for this closure
                // Compare states by their core items (ignoring lookahead for matching)
                // For LALR(1), states with same core are merged
                if let Some(next_state_id) = states.iter().position(|s| {
                    let s_core: HashSet<_> =
                        s.items.iter().map(|i| (i.production, i.dot)).collect();
                    let next_core: HashSet<_> = next_state
                        .items
                        .iter()
                        .map(|i| (i.production, i.dot))
                        .collect();
                    s_core == next_core
                }) {
                    state_transitions.insert((state_id, symbol.clone()), next_state_id);

                    if visited.insert(next_state_id) {
                        worklist.push(next_state_id);
                    }
                }
                // If state not found, skip this transition (shouldn't happen if automaton was built correctly)
            }
        }

        state_transitions
    }

    /// Check if an expression contains a specific token.
    fn expr_contains_token(expr: &Expr<T, N>, token: &T) -> bool
    where
        T: PartialEq,
    {
        match expr {
            Expr::Token(t) => t == token,
            Expr::Seq(exprs) | Expr::Choice(exprs) => {
                exprs.iter().any(|e| Self::expr_contains_token(e, token))
            }
            Expr::Opt(inner)
            | Expr::Repeat {
                expr: inner,
                greedy: _,
                ..
            }
            | Expr::Label { expr: inner, .. }
            | Expr::Node { expr: inner, .. }
            | Expr::Flatten(inner)
            | Expr::Prune(inner)
            | Expr::Lookahead(inner)
            | Expr::NotLookahead(inner)
            | Expr::Cut(inner)
            | Expr::RecoveryPoint { expr: inner, .. }
            | Expr::SemanticPredicate { expr: inner, .. } => {
                Self::expr_contains_token(inner, token)
            }
            Expr::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                Self::expr_contains_token(condition, token)
                    || Self::expr_contains_token(then_expr, token)
                    || else_expr
                        .as_ref()
                        .is_some_and(|e| Self::expr_contains_token(e, token))
            }
            Expr::Separated {
                item, separator, ..
            } => {
                Self::expr_contains_token(item, token)
                    || Self::expr_contains_token(separator, token)
            }
            Expr::Delimited {
                open,
                content,
                close,
                ..
            } => {
                Self::expr_contains_token(open, token)
                    || Self::expr_contains_token(content, token)
                    || Self::expr_contains_token(close, token)
            }
            _ => false,
        }
    }

    /// Get precedence hint for a token by searching rules that use it.
    ///
    /// Returns the precedence and associativity if found in any rule that uses this token.
    fn get_token_precedence(
        grammar: &Grammar<T, N>,
        token: &T,
    ) -> Option<(u32, crate::grammar::Associativity)>
    where
        T: PartialEq,
    {
        // Search through all rules to find one that uses this token and has precedence
        for (_lhs, rule) in grammar.rules() {
            if let Some(hint) = rule.metadata.get_hint::<crate::grammar::PrecedenceHint>() {
                // Check if this rule's expression uses the token
                if Self::expr_contains_token(&rule.rhs, token) {
                    return Some((hint.precedence, hint.associativity));
                }
            }
        }
        None
    }

    /// Get precedence hint for a production by looking up its LHS rule.
    fn get_production_precedence(
        grammar: &Grammar<T, N>,
        production: &Production<T, N>,
    ) -> Option<(u32, crate::grammar::Associativity)> {
        grammar.get_rule(&production.lhs).and_then(|rule| {
            rule.metadata
                .get_hint::<crate::grammar::PrecedenceHint>()
                .map(|hint| (hint.precedence, hint.associativity))
        })
    }

    /// Resolve a shift/reduce conflict using precedence and associativity.
    ///
    /// Returns true if reduce should be preferred, false if shift should be preferred.
    fn resolve_shift_reduce_conflict(
        grammar: &Grammar<T, N>,
        token: &T,
        production: &Production<T, N>,
    ) -> bool
    where
        T: PartialEq,
    {
        let token_prec = Self::get_token_precedence(grammar, token);
        let prod_prec = Self::get_production_precedence(grammar, production);

        match (token_prec, prod_prec) {
            (Some((tok_prec, tok_assoc)), Some((prod_prec, prod_assoc))) => {
                // Both have precedence - compare
                match tok_prec.cmp(&prod_prec) {
                    std::cmp::Ordering::Greater => {
                        // Token has higher precedence - prefer shift
                        false
                    }
                    std::cmp::Ordering::Less => {
                        // Production has higher precedence - prefer reduce
                        true
                    }
                    std::cmp::Ordering::Equal => {
                        // Equal precedence - use associativity
                        match (tok_assoc, prod_assoc) {
                            (crate::grammar::Associativity::Left, _) => {
                                // Left associative - prefer reduce
                                true
                            }
                            (
                                crate::grammar::Associativity::Right
                                | crate::grammar::Associativity::None,
                                _,
                            ) => {
                                // Right associative or non-associative - prefer shift
                                false
                            }
                        }
                    }
                }
            }
            (Some(_) | None, None) => {
                // Token has precedence (production doesn't) or neither has precedence
                // Heuristic: prefer reducing short productions (helps with postfix operators)
                production.rhs.len() <= 1
            }
            (None, Some(_)) => {
                // Production has precedence, token doesn't - prefer reduce
                true
            }
        }
    }

    /// Handle shift action insertion with conflict resolution
    fn handle_shift_action(
        grammar: &Grammar<T, N>,
        state_id: usize,
        token: &T,
        next_state: usize,
        _state_transitions: &HashMap<(usize, Symbol<T, N>), usize>,
        productions: &[Production<T, N>],
        action_table: &mut ActionTable<T>,
    ) {
        let key = (state_id, ActionKey::token(token.clone()));
        match action_table.get(&key) {
            Some(Action::Shift(_)) => {
                // Shift-shift conflict - keep existing (shouldn't happen in valid grammar)
            }
            Some(Action::Reduce(reduce_prod)) => {
                // Shift-reduce conflict - resolve using precedence
                let reduce_production = &productions[*reduce_prod];
                if Self::resolve_shift_reduce_conflict(grammar, token, reduce_production) {
                    // Precedence says reduce - keep the reduce action
                } else {
                    // Precedence says shift - use shift
                    action_table.insert(key, Action::shift(next_state));
                }
            }
            _ => {
                action_table.insert(key, Action::shift(next_state));
            }
        }
    }

    /// Handle reduce action insertion with conflict resolution
    fn handle_reduce_action(
        grammar: &Grammar<T, N>,
        state_id: usize,
        item: &LrItem<T, N>,
        production: &Production<T, N>,
        productions: &[Production<T, N>],
        lookahead_tokens: Vec<T>,
        action_table: &mut ActionTable<T>,
    ) {
        for token in lookahead_tokens {
            let key = (state_id, ActionKey::token(token.clone()));
            match action_table.get(&key) {
                Some(Action::Shift(_)) => {
                    // Shift-reduce conflict - resolve using precedence
                    if Self::resolve_shift_reduce_conflict(grammar, &token, production) {
                        // Precedence says reduce - replace shift with reduce
                        action_table.insert(key, Action::reduce(item.production));
                    }
                    // Otherwise keep shift
                }
                Some(Action::Reduce(other_prod)) => {
                    // Reduce-reduce conflict - resolve using precedence
                    if *other_prod != item.production {
                        Self::resolve_reduce_reduce_conflict(
                            grammar,
                            state_id,
                            item.production,
                            *other_prod,
                            productions,
                            &token,
                            action_table,
                        );
                    }
                }
                _ => {
                    action_table.insert(key, Action::reduce(item.production));
                }
            }
        }
    }

    /// Resolve reduce-reduce conflict using precedence
    fn resolve_reduce_reduce_conflict(
        grammar: &Grammar<T, N>,
        state_id: usize,
        current_prod: usize,
        other_prod: usize,
        productions: &[Production<T, N>],
        token: &T,
        action_table: &mut ActionTable<T>,
    ) {
        let current_production = &productions[current_prod];
        let other_production = &productions[other_prod];

        let current_prec = Self::get_production_precedence(grammar, current_production);
        let other_prec = Self::get_production_precedence(grammar, other_production);

        let key = (state_id, ActionKey::token(token.clone()));
        match (current_prec, other_prec) {
            (Some((curr_prec, _)), Some((other_prec, _))) => {
                // Both have precedence - prefer higher
                match curr_prec.cmp(&other_prec) {
                    std::cmp::Ordering::Greater => {
                        action_table.insert(key, Action::reduce(current_prod));
                    }
                    std::cmp::Ordering::Less => {
                        // Keep other production (already in table)
                    }
                    std::cmp::Ordering::Equal => {
                        // Equal precedence - prefer earlier production (lower index)
                        if current_prod < other_prod {
                            action_table.insert(key, Action::reduce(current_prod));
                        }
                    }
                }
            }
            (Some(_), None) => {
                // Current has precedence, other doesn't - prefer current
                action_table.insert(key, Action::reduce(current_prod));
            }
            (None, Some(_)) => {
                // Other has precedence, current doesn't - keep other
            }
            (None, None) => {
                // Neither has precedence - prefer earlier production
                if current_prod < other_prod {
                    action_table.insert(key, Action::reduce(current_prod));
                }
            }
        }
    }

    /// Build action table entries for a single state.
    #[allow(clippy::too_many_arguments)]
    fn build_action_entries_for_state(
        state_id: usize,
        state: &LrState<T, N>,
        productions: &[Production<T, N>],
        state_transitions: &HashMap<(usize, Symbol<T, N>), usize>,
        entry_production: usize,
        grammar: &Grammar<T, N>,
        follow_sets: &hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
        action_table: &mut ActionTable<T>,
    ) {
        for item in &state.items {
            let prod = &productions[item.production];

            if item.dot < prod.rhs.len() {
                // Dot not at end - check if we can shift
                if let ProductionItem::Token(token) = &prod.rhs[item.dot] {
                    // Shift action
                    if let Some(&next_state) =
                        state_transitions.get(&(state_id, Symbol::token(token.clone())))
                    {
                        Self::handle_shift_action(
                            grammar,
                            state_id,
                            token,
                            next_state,
                            state_transitions,
                            productions,
                            action_table,
                        );
                    }
                }
            } else {
                // Dot at end - reduce action
                // For entry production, this is accept
                if item.production == entry_production {
                    // Accept action on EOF
                    let key = (state_id, ActionKey::eof());
                    action_table.insert(key, Action::accept());
                } else {
                    // Reduce action
                    // For LALR(1), we use the lookahead from the item
                    // If no lookahead, we use FOLLOW set of the LHS
                    let lhs = &productions[item.production].lhs;
                    let lookahead_tokens: Vec<T> = item.lookahead.as_ref().map_or_else(
                        || {
                            // Use FOLLOW set for the LHS of this production
                            follow_sets.get(lhs).map_or_else(
                                || {
                                    // Fallback: if FOLLOW set not available, use all tokens
                                    // This shouldn't happen in a well-formed grammar
                                    grammar
                                        .rules()
                                        .flat_map(|(_, rule)| rule.rhs.first_set(grammar))
                                        .collect()
                                },
                                |follow| follow.iter().cloned().collect(),
                            )
                        },
                        |lookahead| vec![lookahead.clone()],
                    );

                    Self::handle_reduce_action(
                        grammar,
                        state_id,
                        item,
                        &productions[item.production],
                        productions,
                        lookahead_tokens,
                        action_table,
                    );
                }
            }
        }
    }

    /// Build goto table entries for a single state.
    fn build_goto_entries_for_state(
        state_id: usize,
        state: &LrState<T, N>,
        productions: &[Production<T, N>],
        state_transitions: &HashMap<(usize, Symbol<T, N>), usize>,
        goto_table: &mut GotoTable<N>,
    ) {
        for item in &state.items {
            let prod = &productions[item.production];

            if item.dot < prod.rhs.len() {
                // Dot not at end - check for goto on non-terminal
                if let ProductionItem::NonTerminal(nt) = &prod.rhs[item.dot] {
                    // Goto action
                    if let Some(&next_state) =
                        state_transitions.get(&(state_id, Symbol::non_terminal(nt.clone())))
                    {
                        goto_table.insert((state_id, nt.clone()), next_state);
                    }
                }
            }
        }
    }

    /// Build action and goto tables
    ///
    /// Constructs the action table (shift/reduce/accept/error) and goto table
    /// from the LR automaton states.
    fn build_tables(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        states: &[LrState<T, N>],
        _num_states: usize,
        entry_production: usize,
        follow_sets: &hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
    ) -> (ActionTable<T>, GotoTable<N>) {
        let mut action_table: ActionTable<T> = HashMap::with_hasher(ahash::RandomState::new());
        let mut goto_table: GotoTable<N> = HashMap::with_hasher(ahash::RandomState::new());

        // Build state transitions map
        let state_transitions =
            Self::build_state_transitions(grammar, productions, states, follow_sets);

        // Build action and goto tables from states
        for (state_id, state) in states.iter().enumerate() {
            Self::build_action_entries_for_state(
                state_id,
                state,
                productions,
                &state_transitions,
                entry_production,
                grammar,
                follow_sets,
                &mut action_table,
            );
            Self::build_goto_entries_for_state(
                state_id,
                state,
                productions,
                &state_transitions,
                &mut goto_table,
            );
        }

        (action_table, goto_table)
    }

    /// Get action for state and token
    #[must_use]
    pub fn get_action(&self, state: usize, token: Option<&T>) -> Action {
        let key = token.map_or(ActionKey::eof(), |t| ActionKey::token(t.clone()));
        self.action_table
            .get(&(state, key))
            .cloned()
            .unwrap_or_else(Action::error)
    }

    /// Get goto state for state and non-terminal
    #[must_use]
    pub fn get_goto(&self, state: usize, nt: &N) -> Option<usize> {
        self.goto_table.get(&(state, nt.clone())).copied()
    }

    /// Get production by index
    #[must_use]
    pub fn get_production(&self, idx: usize) -> Option<&Production<T, N>> {
        self.productions.get(idx)
    }

    /// Get number of states in the LR automaton
    ///
    /// This is useful for debugging, statistics, or understanding the complexity
    /// of the generated parsing table.
    /// Get the number of states in the parsing table
    #[must_use]
    #[allow(dead_code)] // Public API for debugging and statistics
    pub const fn num_states(&self) -> usize {
        self.num_states
    }

    /// Get expected tokens for a given state (tokens that have valid actions)
    ///
    /// This is useful for error recovery - it returns all tokens that would
    /// result in a valid action (shift, reduce, or accept) in the given state.
    #[must_use]
    pub fn get_expected_tokens(&self, state: usize) -> Vec<T>
    where
        T: Clone,
    {
        let mut expected = Vec::new();
        for ((s, key), action) in &self.action_table {
            if *s == state
                && !matches!(action, Action::Error)
                && let ActionKey::Token(token) = key
            {
                expected.push(token.clone());
            }
        }
        expected
    }
}

/// LR state (set of LR items)
#[derive(Debug, Clone)]
struct LrState<T, N> {
    items: HashSet<LrItem<T, N>>,
    _phantom: std::marker::PhantomData<N>,
}

impl<T: Hash + Eq, N: Hash + Eq> LrState<T, N> {
    /// Create a new LR state with the given items
    #[must_use]
    const fn new(items: HashSet<LrItem<T, N>>) -> Self {
        Self {
            items,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Create an empty LR state
    #[must_use]
    #[allow(dead_code)] // Reserved for future use
    fn empty() -> Self {
        Self::new(HashSet::new())
    }

    /// Create an LR state with a single item
    #[must_use]
    #[allow(dead_code)] // Reserved for future use
    fn with_item(item: LrItem<T, N>) -> Self {
        let mut items = HashSet::new();
        items.insert(item);
        Self::new(items)
    }
}

impl<T: Hash + Eq, N: Hash + Eq> PartialEq for LrState<T, N> {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}

impl<T: Hash + Eq, N: Hash + Eq> Eq for LrState<T, N> {}

impl<T: Hash + Eq, N: Hash + Eq> std::hash::Hash for LrState<T, N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash items in a deterministic order
        let mut items: Vec<_> = self.items.iter().collect();
        items.sort_by(|a, b| {
            a.production
                .cmp(&b.production)
                .then_with(|| a.dot.cmp(&b.dot))
        });
        for item in items {
            item.hash(state);
        }
    }
}

/// LR item: production with dot position and lookahead
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct LrItem<T, N> {
    production: usize,
    dot: usize,
    lookahead: Option<T>,
    _phantom: std::marker::PhantomData<N>,
}

impl<T, N> LrItem<T, N> {
    /// Create a new LR item
    #[must_use]
    const fn new(production: usize, dot: usize, lookahead: Option<T>) -> Self {
        Self {
            production,
            dot,
            lookahead,
            _phantom: std::marker::PhantomData,
        }
    }

    /// Create an LR item without lookahead
    #[must_use]
    const fn without_lookahead(production: usize, dot: usize) -> Self {
        Self::new(production, dot, None)
    }

    /// Create an LR item with lookahead
    #[must_use]
    const fn with_lookahead(production: usize, dot: usize, lookahead: T) -> Self {
        Self::new(production, dot, Some(lookahead))
    }
}

/// Symbol (terminal or non-terminal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol<T, N> {
    Token(T),
    NonTerminal(N),
}

impl<T, N> Symbol<T, N> {
    /// Create a symbol from a token
    #[must_use]
    const fn token(token: T) -> Self {
        Self::Token(token)
    }

    /// Create a symbol from a non-terminal
    #[must_use]
    const fn non_terminal(nt: N) -> Self {
        Self::NonTerminal(nt)
    }
}
