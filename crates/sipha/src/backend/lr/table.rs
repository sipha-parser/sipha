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

/// Production rule for LR parsing
#[derive(Debug, Clone)]
pub struct Production<T, N> {
    pub lhs: N,
    pub rhs: Vec<ProductionItem<T, N>>,
    pub _production_id: usize,
}

impl<T, N> Production<T, N> {
    /// Create a new production rule
    #[must_use]
    pub const fn new(lhs: N, rhs: Vec<ProductionItem<T, N>>, production_id: usize) -> Self {
        Self {
            lhs,
            rhs,
            _production_id: production_id,
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
            Self::build_lalr1_automaton(grammar, &productions, &follow_sets)?
        } else {
            Self::build_lr1_automaton(grammar, &productions)?
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
            Expr::Repeat { expr, min, max } => {
                // For bounded repeats (max is Some and reasonable), expand to multiple productions
                // For unbounded repeats, we'd need helper non-terminals which we can't create dynamically
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
                        return Err(format!(
                            "Repeat expression with max={max_val} is too large to expand. \
                             Consider using a helper non-terminal for unbounded repeats."
                        ));
                    }
                } else {
                    // Unbounded repeat - would need helper non-terminal
                    // For now, expand to a reasonable number of productions (0 to 10)
                    // This is a limitation - proper support would require helper non-terminals
                    let max_count = 10.min(*min + 10);
                    for count in *min..=max_count {
                        let mut items = Vec::new();
                        for _ in 0..count {
                            Self::flatten_expr_to_items(expr, &mut items)?;
                        }
                        let prod_id = productions.len();
                        productions.push(Production {
                            lhs: lhs.clone(),
                            rhs: items,
                            _production_id: prod_id,
                        });
                    }
                    // Also add empty production if min is 0
                    if *min == 0 {
                        let prod_id = productions.len();
                        productions.push(Production::empty(lhs.clone(), prod_id));
                    }
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
            Expr::Repeat { expr: inner, .. } => {
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
    /// # Implementation Status
    ///
    /// **Intentionally not implemented**: This function returns an error directing users
    /// to use LALR(1) instead, which is more practical for most grammars.
    ///
    /// LALR(1) has the same parsing power as LR(1) for most practical grammars but with
    /// significantly smaller table sizes. Canonical LR(1) construction is more complex
    /// and produces larger tables without practical benefits for most use cases.
    ///
    /// If canonical LR(1) is truly needed, it can be implemented by:
    /// 1. Building LR(1) items with full lookahead sets (not merged like LALR)
    /// 2. Computing the full canonical collection of LR(1) item sets
    /// 3. Building the automaton without state merging
    fn build_lr1_automaton(
        _grammar: &Grammar<T, N>,
        _productions: &[Production<T, N>],
    ) -> Result<(Vec<LrState<T, N>>, usize), String> {
        Err(
            "Full LR(1) automaton construction not yet implemented. Use LALR(1) instead."
                .to_string(),
        )
    }

    /// Build LALR(1) automaton (more practical, smaller tables)
    // Note: Returns Result for API consistency with build_lr1_automaton, even though this implementation never fails
    #[allow(clippy::unnecessary_wraps)] // Result wrapper is kept for consistency with other build functions
    fn build_lalr1_automaton(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        follow_sets: &hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
    ) -> Result<(Vec<LrState<T, N>>, usize), String> {
        // Build LR(0) items first
        let mut states = Vec::new();
        let mut state_map: HashMap<LrState<T, N>, usize> = HashMap::new();

        // Create initial state with item [S' -> .S, $]
        let mut initial_items: HashSet<LrItem<T, N>> = HashSet::new();
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

            // Compute closure
            let closure = Self::closure(grammar, productions, state, follow_sets);

            // Find transitions
            let transitions = Self::compute_transitions(grammar, productions, &closure);

            for (_symbol, next_items) in transitions {
                // Compute closure of the next state
                let next_state_base = LrState::new(next_items);
                let next_closure =
                    Self::closure(grammar, productions, &next_state_base, follow_sets);
                let next_state = LrState::new(next_closure);

                if let Some(&id) = state_map.get(&next_state) {
                    // State already exists, no need to add it again
                    let _ = id; // Suppress unused warning
                } else {
                    let id = states.len();
                    states.push(next_state.clone());
                    state_map.insert(next_state, id);
                    if visited.insert(id) {
                        worklist.push(id);
                    }
                }

                // Transition is tracked implicitly through state_map
                // The goto table will be built from these states
            }
        }

        let num_states = states.len();
        Ok((states, num_states))
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
    #[allow(clippy::type_complexity)] // The tuple structure is important and clear in context
    fn compute_transitions(
        _grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        closure: &HashSet<LrItem<T, N>>,
    ) -> Vec<(Symbol<T, N>, HashSet<LrItem<T, N>>)> {
        // Type alias would reduce clarity here - the tuple structure is important
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

    /// Build action table entries for a single state.
    #[allow(clippy::too_many_arguments)] // Complex function that needs all these parameters
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
                        let key = (state_id, ActionKey::token(token.clone()));
                        // Check for conflicts
                        match action_table.get(&key) {
                            Some(Action::Shift(_)) => {
                                // Shift-shift conflict - keep existing (shouldn't happen in valid grammar)
                            }
                            Some(Action::Reduce(_)) => {
                                // Shift-reduce conflict - prefer shift (default resolution)
                                // In a full implementation, we'd use precedence/associativity
                                action_table.insert(key, Action::shift(next_state));
                            }
                            _ => {
                                action_table.insert(key, Action::shift(next_state));
                            }
                        }
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

                    for token in lookahead_tokens {
                        let key = (state_id, ActionKey::token(token));
                        // Check for conflicts
                        match action_table.get(&key) {
                            Some(Action::Shift(_)) => {
                                // Shift-reduce conflict - prefer shift (default)
                                // In a full implementation, we'd use precedence
                            }
                            Some(Action::Reduce(other_prod)) => {
                                // Reduce-reduce conflict
                                if *other_prod != item.production {
                                    // Prefer the production with lower index (default)
                                    if item.production < *other_prod {
                                        action_table.insert(key, Action::reduce(item.production));
                                    }
                                }
                            }
                            _ => {
                                action_table.insert(key, Action::reduce(item.production));
                            }
                        }
                    }
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
