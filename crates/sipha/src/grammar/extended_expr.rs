//! Extended grammar expressions with backend-specific features
//!
//! This module defines `ExtendedExpr`, which wraps `CoreExpr` and adds
//! extended features that may require transformation for some backends or
//! be backend-specific.

use crate::grammar::{
    NonTerminal, capture::CaptureId, predicate::SemanticPredicate, token_class::TokenClass,
};
use smallvec::SmallVec;

use super::core_expr::{CoreExpr, TrailingSeparator};

/// Extended grammar expression with additional features
///
/// This enum wraps `CoreExpr` and adds extended features that may not be
/// supported by all backends. Backends that don't support certain features
/// will need to transform them or fail at compile time.
#[derive(Debug, Clone)]
pub enum ExtendedExpr<T, N> {
    /// Core expression (universally supported)
    Core(CoreExpr<T, N>),

    // Extended predicates
    /// Positive lookahead: match if expression would match, but don't consume
    Lookahead(Box<ExtendedExpr<T, N>>),
    /// Negative lookahead: match if expression would not match, but don't consume
    NotLookahead(Box<ExtendedExpr<T, N>>),

    /// Token class matching: match tokens by class rather than specific value
    TokenClass {
        /// Token class to match
        class: TokenClass,
    },

    /// Conditional expression: parse based on a condition
    Conditional {
        /// Condition expression
        condition: Box<ExtendedExpr<T, N>>,
        /// Expression to parse if condition succeeds
        then_expr: Box<ExtendedExpr<T, N>>,
        /// Expression to parse if condition fails (optional)
        else_expr: Option<Box<ExtendedExpr<T, N>>>,
    },

    /// Semantic predicate: check semantic conditions during parsing
    SemanticPredicate {
        /// Expression to parse
        expr: Box<ExtendedExpr<T, N>>,
        /// Predicate to check
        predicate: std::sync::Arc<dyn SemanticPredicate<T, N>>,
    },

    /// Backreference: match previously captured content
    Backreference {
        /// Capture ID to reference
        capture_id: CaptureId,
    },

    /// Error recovery point: mark a location for error recovery
    RecoveryPoint {
        /// Expression to parse
        expr: Box<ExtendedExpr<T, N>>,
        /// Synchronization tokens for recovery
        sync_tokens: SmallVec<[T; 4]>,
    },

    // Backend-specific features (feature-gated)
    /// Cut operator: prevents backtracking past this point (PEG-specific)
    #[cfg(feature = "backend-peg")]
    Cut(Box<ExtendedExpr<T, N>>),

    /// Pratt operator: operator precedence parsing (Pratt parser specific)
    #[cfg(feature = "backend-pratt")]
    PrattOperator {
        /// Expression to parse
        expr: Box<ExtendedExpr<T, N>>,
        /// Precedence level
        precedence: u32,
        /// Associativity
        associativity: crate::grammar::hint::Associativity,
    },
}

impl<T, N> ExtendedExpr<T, N> {
    /// Convert to core expression if possible
    ///
    /// Returns `Some` if this is a core expression or can be converted to one,
    /// `None` if it contains extended features that cannot be converted.
    #[must_use]
    pub fn to_core(&self) -> Option<CoreExpr<T, N>>
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match self {
            Self::Core(expr) => Some(expr.clone()),
            _ => None,
        }
    }

    /// Convert from core expression
    #[must_use]
    pub fn from_core(core: CoreExpr<T, N>) -> Self {
        Self::Core(core)
    }

    /// Check if this is a core expression
    #[must_use]
    pub fn is_core(&self) -> bool {
        matches!(self, Self::Core(_))
    }

    /// Unwrap core expression, panicking if not core
    #[must_use]
    pub fn unwrap_core(self) -> CoreExpr<T, N> {
        match self {
            Self::Core(expr) => expr,
            _ => panic!("ExtendedExpr::unwrap_core called on non-core expression"),
        }
    }

    /// Get core expression reference if this is core
    #[must_use]
    pub fn as_core(&self) -> Option<&CoreExpr<T, N>> {
        match self {
            Self::Core(expr) => Some(expr),
            _ => None,
        }
    }

    /// Check if this is a specific core variant (for pattern matching)
    #[must_use]
    pub fn is_core_variant<F>(&self, f: F) -> bool
    where
        F: FnOnce(&CoreExpr<T, N>) -> bool,
    {
        match self {
            Self::Core(expr) => f(expr),
            _ => false,
        }
    }

    /// Try to match a core variant
    pub fn match_core<F, R>(&self, f: F) -> Option<R>
    where
        F: FnOnce(&CoreExpr<T, N>) -> Option<R>,
    {
        match self {
            Self::Core(expr) => f(expr),
            _ => None,
        }
    }

    // Builder methods that delegate to CoreExpr
    /// Create a token expression
    #[must_use]
    pub const fn token(t: T) -> Self {
        Self::Core(CoreExpr::Token(t))
    }

    /// Create a rule expression
    #[must_use]
    pub const fn rule(n: N) -> Self {
        Self::Core(CoreExpr::Rule(n))
    }

    /// Create an any expression
    #[must_use]
    pub const fn any() -> Self {
        Self::Core(CoreExpr::Any)
    }

    /// Create an EOF expression
    #[must_use]
    pub const fn eof() -> Self {
        Self::Core(CoreExpr::Eof)
    }

    /// Create an empty expression
    #[must_use]
    pub const fn empty() -> Self {
        Self::Core(CoreExpr::Empty)
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
            // Convert to core expressions if all are core
            let all_core = vec.iter().all(|e| e.is_core());
            if all_core {
                let core_vec: Vec<CoreExpr<T, N>> =
                    vec.into_iter().map(|e| e.unwrap_core()).collect();
                Self::Core(CoreExpr::Seq(core_vec))
            } else {
                // Keep as extended if any are extended
                Self::Core(CoreExpr::Seq(
                    vec.into_iter()
                        .map(|e| match e {
                            Self::Core(c) => c,
                            _ => panic!("Cannot create Seq with extended expressions directly"),
                        })
                        .collect(),
                ))
            }
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
            // For now, keep as extended if any are extended
            // This will be handled by transformation
            Self::Core(CoreExpr::Choice(
                vec.into_iter()
                    .map(|e| match e {
                        Self::Core(c) => c,
                        _ => panic!("Cannot create Choice with extended expressions directly"),
                    })
                    .collect(),
            ))
        }
    }

    /// Create an optional expression
    #[must_use]
    pub fn opt(expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::Opt(Box::new(core))),
            _ => Self::Core(CoreExpr::Opt(Box::new(
                expr.to_core()
                    .expect("Cannot create Opt with extended expression"),
            ))),
        }
    }

    /// Create a Kleene star expression
    #[must_use]
    pub fn star(expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::star(core)),
            _ => Self::Core(CoreExpr::star(
                expr.to_core()
                    .expect("Cannot create star with extended expression"),
            )),
        }
    }

    /// Create a Kleene plus expression
    #[must_use]
    pub fn plus(expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::plus(core)),
            _ => Self::Core(CoreExpr::plus(
                expr.to_core()
                    .expect("Cannot create plus with extended expression"),
            )),
        }
    }

    /// Create a lookahead expression
    #[must_use]
    pub fn lookahead(expr: Self) -> Self {
        Self::Lookahead(Box::new(expr))
    }

    /// Create a negative lookahead expression
    #[must_use]
    pub fn not_lookahead(expr: Self) -> Self {
        Self::NotLookahead(Box::new(expr))
    }

    /// Create a token class expression
    #[must_use]
    pub const fn token_class(class: TokenClass) -> Self {
        Self::TokenClass { class }
    }

    /// Create a conditional expression
    #[must_use]
    pub fn conditional(condition: Self, then_expr: Self, else_expr: Option<Self>) -> Self {
        Self::Conditional {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: else_expr.map(Box::new),
        }
    }

    /// Create a semantic predicate expression
    #[must_use]
    pub fn semantic_predicate(expr: Self, predicate: Box<dyn SemanticPredicate<T, N>>) -> Self {
        Self::SemanticPredicate {
            expr: Box::new(expr),
            predicate: std::sync::Arc::from(predicate),
        }
    }

    /// Create a backreference expression
    #[must_use]
    pub fn backreference(capture_id: CaptureId) -> Self {
        Self::Backreference { capture_id }
    }

    /// Create a recovery point expression
    #[must_use]
    pub fn recovery_point(expr: Self, sync_tokens: SmallVec<[T; 4]>) -> Self {
        Self::RecoveryPoint {
            expr: Box::new(expr),
            sync_tokens,
        }
    }

    /// Create a cut expression (PEG-specific)
    #[cfg(feature = "backend-peg")]
    #[must_use]
    pub fn cut(expr: Self) -> Self {
        Self::Cut(Box::new(expr))
    }

    /// Create a repeat expression with explicit greedy control
    ///
    /// Note: The `greedy` parameter is currently ignored in CoreExpr but
    /// will be handled during transformation for backends that support it.
    #[must_use]
    pub fn repeat_with_greedy(expr: Self, min: usize, max: Option<usize>, _greedy: bool) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::repeat(core, min, max)),
            _ => Self::Core(CoreExpr::repeat(
                expr.to_core()
                    .expect("Cannot create repeat with extended expression"),
                min,
                max,
            )),
        }
    }

    /// Create a separated list with custom configuration
    #[must_use]
    pub fn separated_with_config(
        item: Self,
        sep: Self,
        min: usize,
        trailing: TrailingSeparator,
    ) -> Self {
        match (item, sep) {
            (Self::Core(item_core), Self::Core(sep_core)) => Self::Core(CoreExpr::Separated {
                item: Box::new(item_core),
                separator: Box::new(sep_core),
                min,
                trailing,
            }),
            _ => panic!("Cannot create Separated with extended expressions directly"),
        }
    }

    /// Create a delimited expression with recovery option
    ///
    /// Note: The `recover` parameter is currently ignored in CoreExpr but
    /// will be handled during transformation for backends that support it.
    #[must_use]
    pub fn delimited_with_recovery(open: Self, content: Self, close: Self, _recover: bool) -> Self {
        match (open, content, close) {
            (Self::Core(open_core), Self::Core(content_core), Self::Core(close_core)) => {
                Self::Core(CoreExpr::delimited(open_core, content_core, close_core))
            }
            _ => panic!("Cannot create Delimited with extended expressions directly"),
        }
    }

    /// Create a label expression
    #[must_use]
    pub fn label(name: String, expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::label(name, core)),
            _ => Self::Core(CoreExpr::label(
                name,
                expr.to_core()
                    .expect("Cannot create label with extended expression"),
            )),
        }
    }

    /// Create a node expression
    #[must_use]
    pub fn node(kind: N, expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::node(kind, core)),
            _ => Self::Core(CoreExpr::node(
                kind,
                expr.to_core()
                    .expect("Cannot create node with extended expression"),
            )),
        }
    }

    /// Create a flatten expression
    #[must_use]
    pub fn flatten(expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::flatten(core)),
            _ => Self::Core(CoreExpr::flatten(
                expr.to_core()
                    .expect("Cannot create flatten with extended expression"),
            )),
        }
    }

    /// Create a prune expression
    #[must_use]
    pub fn prune(expr: Self) -> Self
    where
        T: crate::grammar::Token,
        N: NonTerminal,
    {
        match expr {
            Self::Core(core) => Self::Core(CoreExpr::prune(core)),
            _ => Self::Core(CoreExpr::prune(
                expr.to_core()
                    .expect("Cannot create prune with extended expression"),
            )),
        }
    }
}

// Semantic analysis
impl<T, N> ExtendedExpr<T, N>
where
    T: crate::grammar::Token,
    N: NonTerminal,
{
    /// Check if nullable (can match empty input)
    #[must_use]
    pub fn is_nullable(&self, grammar: &crate::grammar::Grammar<T, N>) -> bool {
        match self {
            Self::Core(expr) => expr.is_nullable(grammar),
            Self::Lookahead(_) | Self::NotLookahead(_) => true,
            #[cfg(feature = "backend-peg")]
            Self::Cut(expr) => expr.is_nullable(grammar),
            Self::SemanticPredicate { expr, .. } => expr.is_nullable(grammar),
            Self::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                if condition.is_nullable(grammar) {
                    then_expr.is_nullable(grammar)
                } else if let Some(else_expr) = else_expr {
                    else_expr.is_nullable(grammar)
                } else {
                    false
                }
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

    /// Internal implementation of FIRST set computation
    pub(crate) fn first_set_impl(
        &self,
        grammar: &crate::grammar::Grammar<T, N>,
        result: &mut hashbrown::HashSet<T, ahash::RandomState>,
        visited: &mut hashbrown::HashSet<N, ahash::RandomState>,
    ) {
        match self {
            Self::Core(expr) => expr.first_set_impl(grammar, result, visited),
            Self::Lookahead(expr) | Self::NotLookahead(expr) => {
                expr.first_set_impl(grammar, result, visited);
            }
            #[cfg(feature = "backend-peg")]
            Self::Cut(expr) => {
                expr.first_set_impl(grammar, result, visited);
            }
            Self::SemanticPredicate { expr, .. } => {
                expr.first_set_impl(grammar, result, visited);
            }
            Self::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                condition.first_set_impl(grammar, result, visited);
                if condition.is_nullable(grammar) {
                    then_expr.first_set_impl(grammar, result, visited);
                }
                if let Some(else_expr) = else_expr {
                    else_expr.first_set_impl(grammar, result, visited);
                }
            }
            _ => {}
        }
    }

    /// Extract non-terminals that appear in this expression
    pub(crate) fn extract_nonterminals(&self, result: &mut Vec<(N, usize)>, depth: usize) {
        match self {
            Self::Core(expr) => expr.extract_nonterminals(result, depth),
            Self::Lookahead(expr)
            | Self::NotLookahead(expr)
            | Self::SemanticPredicate { expr, .. }
            | Self::RecoveryPoint { expr, .. } => {
                expr.extract_nonterminals(result, depth);
            }
            #[cfg(feature = "backend-peg")]
            Self::Cut(expr) => {
                expr.extract_nonterminals(result, depth);
            }
            Self::Conditional {
                condition,
                then_expr,
                else_expr,
            } => {
                condition.extract_nonterminals(result, depth);
                then_expr.extract_nonterminals(result, depth);
                if let Some(else_expr) = else_expr {
                    else_expr.extract_nonterminals(result, depth);
                }
            }
            _ => {}
        }
    }
}
