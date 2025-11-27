use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use hashbrown::{HashMap, HashSet};
use smallvec::SmallVec;

/// Predictive parsing table for LL(k) parsing
pub struct ParsingTable<T, N> {
    /// Table mapping (non-terminal, lookahead) -> production index
    /// For LL(1), lookahead is a single token
    /// For LL(k), lookahead is a sequence of k tokens (represented as a key)
    table: HashMap<(N, LookaheadKey<T>), usize, ahash::RandomState>,

    /// Cached FIRST sets for all non-terminals
    first_sets: HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,

    /// Cached FOLLOW sets for all non-terminals
    #[allow(dead_code)] // Used by follow_set() method
    follow_sets: HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,

    /// Lookahead depth
    k: usize,
}

/// Key for lookahead in parsing table
/// For LL(1), this is just a single token
/// For LL(k), this is a sequence of up to k tokens
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LookaheadKey<T> {
    /// Single token (LL(1))
    Token(T),
    /// Sequence of tokens (LL(k) with k > 1)
    Sequence(smallvec::SmallVec<[T; 4]>),
    /// End of file
    Eof,
}

impl<T, N> ParsingTable<T, N>
where
    T: Token,
    N: NonTerminal,
{
    pub fn new(grammar: &Grammar<T, N>, k: usize) -> Result<Self, String> {
        if k == 0 {
            return Err("Lookahead depth must be at least 1".to_string());
        }

        // Compute FIRST and FOLLOW sets
        let mut first_sets = HashMap::with_hasher(ahash::RandomState::new());
        let follow_sets = grammar.compute_follow_sets();

        for (nt, rule) in grammar.rules() {
            first_sets.insert(nt.clone(), rule.rhs.first_set(grammar));
        }

        // Build parsing table
        let mut table = HashMap::with_hasher(ahash::RandomState::new());

        for (lhs, rule) in grammar.rules() {
            if k == 1 {
                Self::build_table_entry_ll1(
                    grammar,
                    lhs,
                    &rule.rhs,
                    &first_sets,
                    &follow_sets,
                    &mut table,
                )?;
            } else {
                Self::build_table_entry_llk(
                    grammar,
                    lhs,
                    &rule.rhs,
                    &first_sets,
                    &follow_sets,
                    &mut table,
                    k,
                )?;
            }
        }

        Ok(Self {
            table,
            first_sets,
            follow_sets,
            k,
        })
    }

    fn build_table_entry_ll1(
        grammar: &Grammar<T, N>,
        lhs: &N,
        expr: &Expr<T, N>,
        _first_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        follow_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        table: &mut HashMap<(N, LookaheadKey<T>), usize, ahash::RandomState>,
    ) -> Result<(), String> {
        if let Expr::Choice(alternatives) = expr {
            for (alt_idx, alt) in alternatives.iter().enumerate() {
                let first = alt.first_set(grammar);
                let nullable = alt.is_nullable(grammar);

                // For each token in FIRST(alt), add entry
                for token in &first {
                    let key = (lhs.clone(), LookaheadKey::Token(token.clone()));
                    if table.insert(key, alt_idx).is_some() {
                        return Err(format!("Conflict in parsing table for rule {lhs:?}"));
                    }
                }

                // If nullable, add FOLLOW(lhs) entries
                if nullable && let Some(follow) = follow_sets.get(lhs) {
                    for token in follow {
                        let key = (lhs.clone(), LookaheadKey::Token(token.clone()));
                        if table.insert(key.clone(), alt_idx).is_some() {
                            return Err(format!("Conflict in parsing table for rule {lhs:?}"));
                        }
                    }
                }
            }
        } else {
            // Single production (not a choice)
            let first = expr.first_set(grammar);
            let nullable = expr.is_nullable(grammar);

            for token in &first {
                let key = (lhs.clone(), LookaheadKey::Token(token.clone()));
                table.insert(key, 0);
            }

            if nullable && let Some(follow) = follow_sets.get(lhs) {
                for token in follow {
                    let key = (lhs.clone(), LookaheadKey::Token(token.clone()));
                    table.insert(key, 0);
                }
            }
        }

        Ok(())
    }

    fn build_table_entry_llk(
        grammar: &Grammar<T, N>,
        lhs: &N,
        expr: &Expr<T, N>,
        first_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        follow_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        table: &mut HashMap<(N, LookaheadKey<T>), usize, ahash::RandomState>,
        k: usize,
    ) -> Result<(), String> {
        // For LL(k) with k > 1, we need to compute FIRST_k sets
        // This is more complex - we need sequences of k tokens

        if let Expr::Choice(alternatives) = expr {
            for (alt_idx, alt) in alternatives.iter().enumerate() {
                // Compute FIRST_k for this alternative
                let first_k = Self::compute_first_k(grammar, alt, first_sets, k);
                let nullable = alt.is_nullable(grammar);

                // For each sequence in FIRST_k, add entry
                for seq in first_k {
                    let key = (lhs.clone(), LookaheadKey::Sequence(seq));
                    if table.insert(key, alt_idx).is_some() {
                        return Err(format!(
                            "Conflict in parsing table for rule {lhs:?} (LL({k}))"
                        ));
                    }
                }

                // If nullable, add FOLLOW_k entries
                if nullable {
                    let follow_k = Self::compute_follow_k(grammar, lhs, follow_sets, first_sets, k);
                    for seq in follow_k {
                        let key = (lhs.clone(), LookaheadKey::Sequence(seq));
                        if table.insert(key, alt_idx).is_some() {
                            return Err(format!(
                                "Conflict in parsing table for rule {lhs:?} (LL({k}))"
                            ));
                        }
                    }
                }
            }
        } else {
            // Single production (not a choice)
            let first_k = Self::compute_first_k(grammar, expr, first_sets, k);
            let nullable = expr.is_nullable(grammar);

            for seq in first_k {
                let key = (lhs.clone(), LookaheadKey::Sequence(seq));
                table.insert(key, 0);
            }

            if nullable {
                let follow_k = Self::compute_follow_k(grammar, lhs, follow_sets, first_sets, k);
                for seq in follow_k {
                    let key = (lhs.clone(), LookaheadKey::Sequence(seq));
                    table.insert(key, 0);
                }
            }
        }

        Ok(())
    }

    /// Compute `FIRST_k` set for an expression
    fn compute_first_k(
        grammar: &Grammar<T, N>,
        expr: &Expr<T, N>,
        first_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        k: usize,
    ) -> HashSet<SmallVec<[T; 4]>, ahash::RandomState> {
        let mut result = HashSet::with_hasher(ahash::RandomState::new());
        let mut visited = HashSet::with_hasher(ahash::RandomState::new());
        Self::compute_first_k_impl(
            grammar,
            expr,
            first_sets,
            k,
            &mut result,
            &mut visited,
            &mut SmallVec::new(),
        );
        result
    }

    /// Internal recursive helper for computing `FIRST_k` sets.
    ///
    /// This function is intentionally recursive to traverse expression trees.
    /// The `only_used_in_recursion` warning is expected for this algorithm.
    #[allow(clippy::only_used_in_recursion)]
    fn compute_first_k_impl(
        grammar: &Grammar<T, N>,
        expr: &Expr<T, N>,
        first_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        k: usize,
        result: &mut HashSet<SmallVec<[T; 4]>, ahash::RandomState>,
        visited: &mut HashSet<N, ahash::RandomState>,
        current: &mut SmallVec<[T; 4]>,
    ) {
        if current.len() >= k {
            return;
        }

        match expr {
            Expr::Token(t) => {
                let mut seq = current.clone();
                seq.push(t.clone());
                result.insert(seq);
            }
            Expr::Rule(n) => {
                if visited.insert(n.clone())
                    && let Some(rule) = grammar.get_rule(n)
                {
                    Self::compute_first_k_impl(
                        grammar, &rule.rhs, first_sets, k, result, visited, current,
                    );
                }
            }
            Expr::Seq(exprs) => {
                for e in exprs {
                    Self::compute_first_k_impl(grammar, e, first_sets, k, result, visited, current);
                    if !e.is_nullable(grammar) {
                        break;
                    }
                }
            }
            Expr::Choice(exprs) => {
                for e in exprs {
                    Self::compute_first_k_impl(grammar, e, first_sets, k, result, visited, current);
                }
            }
            Expr::Opt(e) | Expr::Repeat { expr: e, .. } => {
                Self::compute_first_k_impl(grammar, e, first_sets, k, result, visited, current);
            }
            _ => {}
        }
    }

    /// Compute `FOLLOW_k` set for a non-terminal
    fn compute_follow_k(
        _grammar: &Grammar<T, N>,
        nt: &N,
        follow_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        _first_sets: &HashMap<N, HashSet<T, ahash::RandomState>, ahash::RandomState>,
        k: usize,
    ) -> HashSet<SmallVec<[T; 4]>, ahash::RandomState> {
        // For FOLLOW_k, we take sequences from FOLLOW and pad/truncate to k
        let mut result = HashSet::with_hasher(ahash::RandomState::new());

        if let Some(follow) = follow_sets.get(nt) {
            for token in follow {
                let mut seq = SmallVec::new();
                seq.push(token.clone());
                // Pad with EOF or truncate to k
                // For now, just use the single token
                // In a full implementation, we'd need to consider what comes after
                // Note: seq.len() is already 1, so we don't need to pad
                if seq.len() <= k {
                    result.insert(seq);
                }
            }
        }

        result
    }

    /// Get the production index for a non-terminal and lookahead
    ///
    /// This is optimized for LL(1) with a fast path, and supports LL(k) for k > 1.
    #[must_use]
    pub fn get(&self, nt: &N, lookahead: &[T]) -> Option<usize> {
        if self.k == 1 {
            // Fast path for LL(1) - most common case
            let key = lookahead
                .first()
                .map_or((nt.clone(), LookaheadKey::Eof), |t| {
                    (nt.clone(), LookaheadKey::Token(t.clone()))
                });
            self.table.get(&key).copied()
        } else {
            // LL(k) path - build sequence of up to k tokens
            let seq: SmallVec<[T; 4]> = lookahead.iter().take(self.k).cloned().collect();
            let key = if seq.is_empty() {
                (nt.clone(), LookaheadKey::Eof)
            } else if seq.len() == 1 {
                // Optimize single-token sequences
                (nt.clone(), LookaheadKey::Token(seq[0].clone()))
            } else {
                (nt.clone(), LookaheadKey::Sequence(seq))
            };
            self.table.get(&key).copied()
        }
    }

    /// Get lookahead depth
    pub const fn k(&self) -> usize {
        self.k
    }

    /// Get FIRST set for a non-terminal
    #[must_use]
    pub fn first_set(&self, nt: &N) -> Option<&HashSet<T, ahash::RandomState>> {
        self.first_sets.get(nt)
    }

    /// Get FOLLOW set for a non-terminal
    ///
    /// # Note
    ///
    /// This is a convenience method for accessing FOLLOW sets. The sets are computed
    /// during table construction and used internally. This public method is provided
    /// for debugging and advanced use cases.
    #[allow(dead_code)] // Public API method for debugging
    pub fn follow_set(&self, nt: &N) -> Option<&HashSet<T, ahash::RandomState>> {
        self.follow_sets.get(nt)
    }
}
