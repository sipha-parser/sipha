//! Core grammar expressions that are universal across all backends
//!
//! This module defines `CoreExpr`, which contains only expression variants
//! that are supported by all parser backends. Extended features that may
//! require transformation or be backend-specific are in `ExtendedExpr`.

use crate::grammar::NonTerminal;

/// Controls whether a trailing separator is allowed in separated lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrailingSeparator {
    /// Trailing separator is not allowed (e.g., `[a, b, c]` is valid, `[a, b, c,]` is invalid)
    Forbid,
    /// Trailing separator is optional (e.g., both `[a, b, c]` and `[a, b, c,]` are valid)
    Allow,
    /// Trailing separator is required (e.g., `[a, b, c,]` is valid, `[a, b, c]` is invalid)
    Require,
}

/// Core grammar expression representing production rules
///
/// This enum contains only expression variants that are universally supported
/// across all parser backends. These are the fundamental building blocks
/// that every backend can handle without transformation.
#[derive(Debug, Clone)]
pub enum CoreExpr<T, N> {
    // Primitives
    /// Match a specific token
    Token(T),
    /// Reference a non-terminal rule
    Rule(N),
    /// Match any single token
    Any,
    /// Match end of file
    Eof,
    /// Match empty string (always succeeds)
    Empty,

    // Combinators
    /// Sequence: match all expressions in order
    Seq(Vec<CoreExpr<T, N>>),
    /// Choice: match any of the expressions
    Choice(Vec<CoreExpr<T, N>>),
    /// Optional: match zero or one occurrence
    Opt(Box<CoreExpr<T, N>>),
    /// Repeat: match expression multiple times
    Repeat {
        /// Expression to repeat
        expr: Box<CoreExpr<T, N>>,
        /// Minimum number of repetitions
        min: usize,
        /// Maximum number of repetitions (None for unlimited)
        max: Option<usize>,
    },

    // Advanced
    /// Separated list: match items separated by a separator
    Separated {
        /// Item expression
        item: Box<CoreExpr<T, N>>,
        /// Separator expression
        separator: Box<CoreExpr<T, N>>,
        /// Minimum number of items
        min: usize,
        /// Trailing separator policy
        trailing: TrailingSeparator,
    },
    /// Delimited: match content between open and close delimiters
    Delimited {
        /// Opening delimiter
        open: Box<CoreExpr<T, N>>,
        /// Content expression
        content: Box<CoreExpr<T, N>>,
        /// Closing delimiter
        close: Box<CoreExpr<T, N>>,
    },

    // Tree construction
    /// Label: attach a name to an expression for tree construction
    Label {
        /// Label name
        name: String,
        /// Expression to label
        expr: Box<CoreExpr<T, N>>,
    },
    /// Node: create a syntax tree node
    Node {
        /// Node kind (non-terminal)
        kind: N,
        /// Expression to parse
        expr: Box<CoreExpr<T, N>>,
    },
    /// Flatten: remove intermediate nodes in tree
    Flatten(Box<CoreExpr<T, N>>),
    /// Prune: remove this node from tree (keep children)
    Prune(Box<CoreExpr<T, N>>),
}

// Builder methods
impl<T, N> CoreExpr<T, N> {
    /// Create a token expression
    #[must_use]
    pub const fn token(t: T) -> Self {
        Self::Token(t)
    }

    /// Create a rule expression
    #[must_use]
    pub const fn rule(n: N) -> Self {
        Self::Rule(n)
    }

    /// Create an any expression
    #[must_use]
    pub const fn any() -> Self {
        Self::Any
    }

    /// Create an EOF expression
    #[must_use]
    pub const fn eof() -> Self {
        Self::Eof
    }

    /// Create an empty expression
    #[must_use]
    pub const fn empty() -> Self {
        Self::Empty
    }

    /// Create a sequence expression
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

    /// Create a choice expression
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

    /// Create an optional expression
    #[must_use]
    pub fn opt(expr: Self) -> Self {
        Self::Opt(Box::new(expr))
    }

    /// Create a Kleene star expression (zero or more)
    #[must_use]
    pub fn star(expr: Self) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min: 0,
            max: None,
        }
    }

    /// Create a Kleene plus expression (one or more)
    #[must_use]
    pub fn plus(expr: Self) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min: 1,
            max: None,
        }
    }

    /// Create a repeat expression
    #[must_use]
    pub fn repeat(expr: Self, min: usize, max: Option<usize>) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min,
            max,
        }
    }

    /// Create a separated list expression
    #[must_use]
    pub fn separated(item: Self, sep: Self) -> Self {
        Self::Separated {
            item: Box::new(item),
            separator: Box::new(sep),
            min: 0,
            trailing: TrailingSeparator::Allow,
        }
    }

    /// Create a delimited expression
    #[must_use]
    pub fn delimited(open: Self, content: Self, close: Self) -> Self {
        Self::Delimited {
            open: Box::new(open),
            content: Box::new(content),
            close: Box::new(close),
        }
    }

    /// Create a label expression
    #[must_use]
    pub fn label(name: String, expr: Self) -> Self {
        Self::Label {
            name,
            expr: Box::new(expr),
        }
    }

    /// Create a node expression
    #[must_use]
    pub fn node(kind: N, expr: Self) -> Self {
        Self::Node {
            kind,
            expr: Box::new(expr),
        }
    }

    /// Create a flatten expression
    #[must_use]
    pub fn flatten(expr: Self) -> Self {
        Self::Flatten(Box::new(expr))
    }

    /// Create a prune expression
    #[must_use]
    pub fn prune(expr: Self) -> Self {
        Self::Prune(Box::new(expr))
    }
}

// Semantic analysis
impl<T, N> CoreExpr<T, N>
where
    T: crate::grammar::Token,
    N: NonTerminal,
{
    /// Check if nullable (can match empty input)
    #[must_use]
    pub fn is_nullable(&self, grammar: &crate::grammar::Grammar<T, N>) -> bool {
        match self {
            Self::Empty | Self::Opt(_) | Self::Prune(_) => true,
            Self::Rule(n) => grammar
                .get_rule(n)
                .is_some_and(|r| r.rhs.is_nullable(grammar)),
            Self::Seq(exprs) => exprs.iter().all(|e| e.is_nullable(grammar)),
            Self::Choice(exprs) => exprs.iter().any(|e| e.is_nullable(grammar)),
            Self::Repeat { min, .. } | Self::Separated { min, .. } => *min == 0,
            Self::Node { expr, .. } | Self::Label { expr, .. } | Self::Flatten(expr) => {
                expr.is_nullable(grammar)
            }
            _ => false,
        }
    }

    /// Compute FIRST set
    #[must_use]
    pub fn first_set(
        &self,
        grammar: &crate::grammar::Grammar<T, N>,
    ) -> hashbrown::HashSet<T, ahash::RandomState> {
        let mut result = hashbrown::HashSet::with_hasher(ahash::RandomState::new());
        let mut visited = hashbrown::HashSet::with_hasher(ahash::RandomState::new());
        self.first_set_impl(grammar, &mut result, &mut visited);
        result
    }

    pub(crate) fn first_set_impl(
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
                    && let Some(rule) = grammar.get_rule(n)
                {
                    // rule.rhs is ExtendedExpr, use its first_set_impl
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
            Self::Node { expr, .. }
            | Self::Label { expr, .. }
            | Self::Flatten(expr)
            | Self::Prune(expr) => {
                expr.first_set_impl(grammar, result, visited);
            }
            Self::Separated { item, .. } => {
                item.first_set_impl(grammar, result, visited);
            }
            Self::Delimited { open, .. } => {
                open.first_set_impl(grammar, result, visited);
            }
            _ => {}
        }
    }

    /// Extract non-terminals that appear in this expression
    pub(crate) fn extract_nonterminals(&self, result: &mut Vec<(N, usize)>, depth: usize) {
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
            | Self::Prune(expr) => {
                expr.extract_nonterminals(result, depth);
            }
            Self::Separated {
                item, separator, ..
            } => {
                item.extract_nonterminals(result, depth);
                separator.extract_nonterminals(result, depth);
            }
            Self::Delimited {
                open,
                content,
                close,
                ..
            } => {
                open.extract_nonterminals(result, depth);
                content.extract_nonterminals(result, depth);
                close.extract_nonterminals(result, depth);
            }
            _ => {}
        }
    }
}
