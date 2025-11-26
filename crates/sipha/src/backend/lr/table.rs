use hashbrown::{HashMap, HashSet};
use std::hash::Hash;
use crate::grammar::{Grammar, Token, NonTerminal, Expr};

/// LR(1) parsing table with action and goto tables
pub struct LrParsingTable<T, N> {
    /// Action table: (state, token) -> Action
    action_table: HashMap<(usize, ActionKey<T>), Action, ahash::RandomState>,
    
    /// Goto table: (state, non-terminal) -> state
    goto_table: HashMap<(usize, N), usize, ahash::RandomState>,
    
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

/// Production rule for LR parsing
#[derive(Debug, Clone)]
pub struct Production<T, N> {
    pub lhs: N,
    pub rhs: Vec<ProductionItem<T, N>>,
    pub _production_id: usize,
}

/// Item in a production (terminal or non-terminal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ProductionItem<T, N> {
    Token(T),
    NonTerminal(N),
}

impl<T, N> LrParsingTable<T, N>
where
    T: Token,
    N: NonTerminal,
{
    pub fn new(grammar: &Grammar<T, N>, use_lalr: bool) -> Result<Self, String> {
        // Convert grammar to productions
        let mut productions = Vec::new();
        let mut production_map: HashMap<(N, usize), usize> = HashMap::new();
        
        // Add augmented start production: S' -> S
        let entry_nt = grammar.entry_point();
        let entry_production = productions.len();
        productions.push(Production {
            lhs: entry_nt.clone(),
            rhs: vec![ProductionItem::NonTerminal(entry_nt.clone())],
            _production_id: 0,
        });
        
        // Convert grammar rules to productions
        for (lhs, rule) in grammar.rules() {
            Self::rule_to_productions(
                lhs,
                &rule.rhs,
                &mut productions,
                &mut production_map,
            )?;
        }
        
        // Build LR(1) automaton
        let (states, num_states) = if use_lalr {
            Self::build_lalr1_automaton(grammar, &productions)?
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
        )?;
        
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
        production_map: &mut HashMap<(N, usize), usize>,
    ) -> Result<(), String> {
        match expr {
            Expr::Choice(alternatives) => {
                for alt in alternatives {
                    Self::expr_to_production_items(alt, productions, production_map, lhs)?;
                }
            }
            _ => {
                Self::expr_to_production_items(expr, productions, production_map, lhs)?;
            }
        }
        Ok(())
    }
    
    /// Convert an expression to production items
    fn expr_to_production_items(
        expr: &Expr<T, N>,
        productions: &mut Vec<Production<T, N>>,
        _production_map: &mut HashMap<(N, usize), usize>,
        lhs: &N,
    ) -> Result<(), String> {
        match expr {
            Expr::Seq(exprs) => {
                let mut items = Vec::new();
                for e in exprs {
                    Self::flatten_expr_to_items(e, &mut items)?;
                }
                let prod_id = productions.len();
                productions.push(Production {
                    lhs: lhs.clone(),
                    rhs: items,
                    _production_id: prod_id,
                });
            }
            Expr::Token(t) => {
                let prod_id = productions.len();
                productions.push(Production {
                    lhs: lhs.clone(),
                    rhs: vec![ProductionItem::Token(t.clone())],
                    _production_id: prod_id,
                });
            }
            Expr::Rule(n) => {
                let prod_id = productions.len();
                productions.push(Production {
                    lhs: lhs.clone(),
                    rhs: vec![ProductionItem::NonTerminal(n.clone())],
                    _production_id: prod_id,
                });
            }
            Expr::Empty => {
                let prod_id = productions.len();
                productions.push(Production {
                    lhs: lhs.clone(),
                    rhs: vec![],
                    _production_id: prod_id,
                });
            }
            _ => {
                // For complex expressions (Opt, Repeat, etc.), we need to expand them
                // For now, we'll handle them as best we can
                return Err(format!("Complex expression not yet supported in LR parser: {expr:?}"));
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
                items.push(ProductionItem::Token(t.clone()));
            }
            Expr::Rule(n) => {
                items.push(ProductionItem::NonTerminal(n.clone()));
            }
            Expr::Seq(exprs) => {
                for e in exprs {
                    Self::flatten_expr_to_items(e, items)?;
                }
            }
            Expr::Empty => {
                // Empty production - no items
            }
            _ => {
                return Err(format!("Cannot flatten expression to items: {expr:?}"));
            }
        }
        Ok(())
    }
    
    /// Build LR(1) automaton (canonical LR)
    fn build_lr1_automaton(
        _grammar: &Grammar<T, N>,
        _productions: &[Production<T, N>],
    ) -> Result<(Vec<LrState<T, N>>, usize), String> {
        // This is a simplified implementation
        // A full LR(1) implementation would build the full canonical LR(1) automaton
        // For now, we'll use a simpler approach that works for many grammars
        Err("Full LR(1) automaton construction not yet implemented. Use LALR(1) instead.".to_string())
    }
    
    /// Build LALR(1) automaton (more practical, smaller tables)
    #[allow(clippy::unnecessary_wraps)]
    fn build_lalr1_automaton(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
    ) -> Result<(Vec<LrState<T, N>>, usize), String> {
        // Build LR(0) items first
        let mut states = Vec::new();
        let mut state_map: HashMap<LrState<T, N>, usize> = HashMap::new();
        
        // Create initial state with item [S' -> .S, $]
        let mut initial_items: HashSet<LrItem<T, N>> = HashSet::new();
        initial_items.insert(LrItem {
            production: 0,
            dot: 0,
            lookahead: None, // Will be computed later
            _phantom: std::marker::PhantomData,
        });
        
        let initial_state = LrState {
            items: initial_items,
            _phantom: std::marker::PhantomData,
        };
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
            let closure = Self::closure(grammar, productions, state);
            
            // Find transitions
            let transitions = Self::compute_transitions(grammar, productions, &closure);
            
            for (_symbol, next_items) in transitions {
                let next_state = LrState {
                    items: next_items,
                    _phantom: std::marker::PhantomData,
                };
                
                let _next_state_id = if let Some(&id) = state_map.get(&next_state) {
                    id
                } else {
                    let id = states.len();
                    states.push(next_state.clone());
                    state_map.insert(next_state, id);
                    if visited.insert(id) {
                        worklist.push(id);
                    }
                    id
                };
                
                // Add transition (will be used in goto table)
                // Note: This is simplified - full implementation would track transitions
            }
        }
        
        let num_states = states.len();
        Ok((states, num_states))
    }
    
    /// Compute closure of LR items
    fn closure(
        grammar: &Grammar<T, N>,
        productions: &[Production<T, N>],
        state: &LrState<T, N>,
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
                    && let ProductionItem::NonTerminal(nt) = &prod.rhs[item.dot] {
                    // Add items for all productions of this non-terminal
                    for (i, p) in productions.iter().enumerate() {
                        if p.lhs == *nt {
                            let first = Self::first_of_suffix(grammar, productions, &prod.rhs, item.dot + 1);
                            for token in first {
                                let new_item = LrItem {
                                    production: i,
                                    dot: 0,
                                    lookahead: Some(token),
                                    _phantom: std::marker::PhantomData,
                                };
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
    
    /// Compute transitions from a state
    #[allow(clippy::unused_self, clippy::missing_const_for_fn, clippy::type_complexity)]
    fn compute_transitions(
        _grammar: &Grammar<T, N>,
        _productions: &[Production<T, N>],
        _closure: &HashSet<LrItem<T, N>>,
    ) -> Vec<(Symbol<T, N>, HashSet<LrItem<T, N>>)> {
        // Simplified - would compute GOTO sets
        Vec::new()
    }
    
    /// Build action and goto tables
    #[allow(clippy::unnecessary_wraps, clippy::type_complexity)]
    fn build_tables(
        _grammar: &Grammar<T, N>,
        _productions: &[Production<T, N>],
        _states: &[LrState<T, N>],
        _num_states: usize,
        _entry_production: usize,
    ) -> Result<(
        HashMap<(usize, ActionKey<T>), Action, ahash::RandomState>,
        HashMap<(usize, N), usize, ahash::RandomState>,
    ), String> {
        // This is a placeholder - full implementation would build the tables
        // For now, return empty tables
        Ok((
            HashMap::with_hasher(ahash::RandomState::new()),
            HashMap::with_hasher(ahash::RandomState::new()),
        ))
    }
    
    /// Get action for state and token
    #[must_use]
    pub fn get_action(&self, state: usize, token: Option<&T>) -> Action {
        let key = token
            .map_or(ActionKey::Eof, |t| ActionKey::Token(t.clone()));
        self.action_table
            .get(&(state, key))
            .cloned()
            .unwrap_or(Action::Error)
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
    
    /// Get number of states
    #[allow(dead_code)] // Reserved for future use
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
        items.sort_by(|a, b| a.production.cmp(&b.production).then_with(|| a.dot.cmp(&b.dot)));
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

/// Symbol (terminal or non-terminal)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Symbol<T, N> {
    Token(T),
    NonTerminal(N),
}

