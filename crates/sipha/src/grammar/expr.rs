use smallvec::SmallVec;
use crate::grammar::{Token, NonTerminal};

/// Grammar expression representing production rules
#[derive(Debug, Clone)]
pub enum Expr<T, N> {
    // Primitives
    Token(T),
    Rule(N),
    Any,
    Eof,
    Empty,
    
    // Combinators
    Seq(Vec<Expr<T, N>>),
    Choice(Vec<Expr<T, N>>),
    Opt(Box<Expr<T, N>>),
    Repeat {
        expr: Box<Expr<T, N>>,
        min: usize,
        max: Option<usize>,
    },
    
    // Advanced
    Separated {
        item: Box<Expr<T, N>>,
        separator: Box<Expr<T, N>>,
        min: usize,
        trailing: TrailingSeparator,
    },
    Delimited {
        open: Box<Expr<T, N>>,
        content: Box<Expr<T, N>>,
        close: Box<Expr<T, N>>,
        recover: bool,
    },
    
    // Predicates
    Lookahead(Box<Expr<T, N>>),
    NotLookahead(Box<Expr<T, N>>),
    
    // Tree construction
    Label {
        name: String,
        expr: Box<Expr<T, N>>,
    },
    Node {
        kind: N,
        expr: Box<Expr<T, N>>,
    },
    Flatten(Box<Expr<T, N>>),
    Prune(Box<Expr<T, N>>),
    
    // Error recovery
    RecoveryPoint {
        expr: Box<Expr<T, N>>,
        sync_tokens: SmallVec<[T; 4]>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrailingSeparator {
    Forbid,
    Allow,
    Require,
}

// Builder methods
impl<T, N> Expr<T, N> {
    // Primitives
    #[must_use]
    pub const fn token(t: T) -> Self { Self::Token(t) }
    #[must_use]
    pub const fn rule(n: N) -> Self { Self::Rule(n) }
    #[must_use]
    pub const fn any() -> Self { Self::Any }
    #[must_use]
    pub const fn eof() -> Self { Self::Eof }
    #[must_use]
    pub const fn empty() -> Self { Self::Empty }
    
    // Combinators
    /// Create a sequence expression.
    ///
    /// # Panics
    ///
    /// Panics if the iterator reports a length of 1 but `next()` returns `None`.
    #[must_use]
    pub fn seq<I>(exprs: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        let vec: Vec<_> = exprs.into_iter().collect();
        if vec.len() == 1 {
            vec.into_iter().next().unwrap()
        } else {
            Self::Seq(vec)
        }
    }
    
    /// Create a choice expression.
    ///
    /// # Panics
    ///
    /// Panics if the iterator reports a length of 1 but `next()` returns `None`.
    #[must_use]
    pub fn choice<I>(exprs: I) -> Self
    where
        I: IntoIterator<Item = Self>,
    {
        let vec: Vec<_> = exprs.into_iter().collect();
        if vec.len() == 1 {
            vec.into_iter().next().unwrap()
        } else {
            Self::Choice(vec)
        }
    }
    
    #[must_use]
    pub fn opt(expr: Self) -> Self {
        Self::Opt(Box::new(expr))
    }
    
    #[must_use]
    pub fn star(expr: Self) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min: 0,
            max: None,
        }
    }
    
    #[must_use]
    pub fn plus(expr: Self) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min: 1,
            max: None,
        }
    }
    
    // Advanced
    #[must_use]
    pub fn separated(item: Self, sep: Self) -> Self {
        Self::Separated {
            item: Box::new(item),
            separator: Box::new(sep),
            min: 0,
            trailing: TrailingSeparator::Allow,
        }
    }
    
    #[must_use]
    pub fn delimited(open: Self, content: Self, close: Self) -> Self {
        Self::Delimited {
            open: Box::new(open),
            content: Box::new(content),
            close: Box::new(close),
            recover: true,
        }
    }
    
    // Predicates
    #[must_use]
    pub fn lookahead(expr: Self) -> Self {
        Self::Lookahead(Box::new(expr))
    }
    
    #[must_use]
    pub fn not_lookahead(expr: Self) -> Self {
        Self::NotLookahead(Box::new(expr))
    }
}

// Semantic analysis
impl<T, N> Expr<T, N>
where
    T: Token,
    N: NonTerminal,
{
    /// Check if nullable (can match empty input)
    #[must_use]
    pub fn is_nullable(&self, grammar: &crate::grammar::Grammar<T, N>) -> bool {
        match self {
            Self::Empty | Self::Opt(_) | Self::Lookahead(_) | Self::NotLookahead(_) | Self::Prune(_) => true,
            
            Self::Rule(n) => grammar.get_rule(n)
                .is_some_and(|r| r.rhs.is_nullable(grammar)),
            
            Self::Seq(exprs) => exprs.iter().all(|e| e.is_nullable(grammar)),
            Self::Choice(exprs) => exprs.iter().any(|e| e.is_nullable(grammar)),
            
            Self::Repeat { min, .. } | Self::Separated { min, .. } => *min == 0,
            
            Self::Node { expr, .. }
            | Self::Label { expr, .. }
            | Self::Flatten(expr) => expr.is_nullable(grammar),
            
            _ => false,
        }
    }
    
    /// Compute FIRST set
    #[must_use]
    pub fn first_set(&self, grammar: &crate::grammar::Grammar<T, N>) -> hashbrown::HashSet<T, ahash::RandomState> {
        let mut result = hashbrown::HashSet::with_hasher(ahash::RandomState::new());
        let mut visited = hashbrown::HashSet::with_hasher(ahash::RandomState::new());
        self.first_set_impl(grammar, &mut result, &mut visited);
        result
    }
    
    fn first_set_impl(
        &self,
        grammar: &crate::grammar::Grammar<T, N>,
        result: &mut hashbrown::HashSet<T, ahash::RandomState>,
        visited: &mut hashbrown::HashSet<N, ahash::RandomState>,
    ) {
        match self {
            Self::Token(t) => {
                result.insert(t.clone());
            }
            
            Self::Rule(n) => {
                if visited.insert(n.clone())
                    && let Some(rule) = grammar.get_rule(n) {
                    rule.rhs.first_set_impl(grammar, result, visited);
                }
            }
            
            Self::Seq(exprs) => {
                for expr in exprs {
                    expr.first_set_impl(grammar, result, visited);
                    if !expr.is_nullable(grammar) {
                        break;
                    }
                }
            }
            
            Self::Choice(exprs) => {
                for expr in exprs {
                    expr.first_set_impl(grammar, result, visited);
                }
            }
            
            Self::Opt(expr) | Self::Repeat { expr, .. } => {
                expr.first_set_impl(grammar, result, visited);
            }
            
            _ => {}
        }
    }
    
    /// Extract non-terminals that appear in this expression (for FOLLOW set computation)
    /// Used internally by `Grammar::compute_follow_sets()`.
    #[allow(dead_code)] // False positive: used internally via trait method
    fn extract_nonterminals(&self, result: &mut Vec<(N, usize)>, depth: usize) {
        match self {
            Self::Rule(n) => {
                result.push((n.clone(), depth));
            }
            Self::Seq(exprs) | Self::Choice(exprs) => {
                for expr in exprs {
                    expr.extract_nonterminals(result, depth);
                }
            }
            Self::Opt(expr)
            | Self::Repeat { expr, .. }
            | Self::Label { expr, .. }
            | Self::Node { expr, .. }
            | Self::Flatten(expr)
            | Self::Prune(expr)
            | Self::Lookahead(expr)
            | Self::NotLookahead(expr)
            | Self::RecoveryPoint { expr, .. } => {
                expr.extract_nonterminals(result, depth);
            }
            Self::Separated { item, separator, .. } => {
                item.extract_nonterminals(result, depth);
                separator.extract_nonterminals(result, depth);
            }
            Self::Delimited { open, content, close, .. } => {
                open.extract_nonterminals(result, depth);
                content.extract_nonterminals(result, depth);
                close.extract_nonterminals(result, depth);
            }
            _ => {}
        }
    }
}

