use crate::grammar::{
    NonTerminal, Token, capture::CaptureId, predicate::SemanticPredicate, token_class::TokenClass,
};
use smallvec::SmallVec;

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
        greedy: bool,
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
    /// Cut operator: prevents backtracking past this point (PEG-specific)
    ///
    /// Once a cut operator succeeds, backtracking is disabled for the
    /// remainder of the current choice alternative. This improves performance
    /// and error messages by committing to a parse path.
    Cut(Box<Expr<T, N>>),
    /// Token class matching: matches tokens by class rather than specific value
    TokenClass {
        class: TokenClass,
    },
    /// Conditional expression: parse based on a condition
    Conditional {
        condition: Box<Expr<T, N>>,
        then_expr: Box<Expr<T, N>>,
        else_expr: Option<Box<Expr<T, N>>>,
    },
    /// Semantic predicate: check semantic conditions during parsing
    SemanticPredicate {
        expr: Box<Expr<T, N>>,
        predicate: std::sync::Arc<dyn SemanticPredicate<T, N>>,
    },
    /// Capture: capture matched content for later use in backreferences
    Capture {
        id: CaptureId,
        expr: Box<Expr<T, N>>,
    },
    /// Backreference: match previously captured content
    Backreference {
        capture_id: CaptureId,
    },

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

/// Controls whether a trailing separator is allowed in separated lists.
///
/// This enum is used with [`Expr::Separated`] to specify whether the last
/// item in a separated list can have a trailing separator.
///
/// # Examples
///
/// ```rust,no_run
/// use sipha::grammar::expr::TrailingSeparator;
///
/// // Forbid trailing separator: `[a, b, c]` ✓, `[a, b, c,]` ✗
/// let forbid = TrailingSeparator::Forbid;
///
/// // Allow trailing separator: `[a, b, c]` ✓, `[a, b, c,]` ✓
/// let allow = TrailingSeparator::Allow;
///
/// // Require trailing separator: `[a, b, c]` ✗, `[a, b, c,]` ✓
/// let require = TrailingSeparator::Require;
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrailingSeparator {
    /// Trailing separator is not allowed (e.g., `[a, b, c]` is valid, `[a, b, c,]` is invalid)
    Forbid,
    /// Trailing separator is optional (e.g., both `[a, b, c]` and `[a, b, c,]` are valid)
    Allow,
    /// Trailing separator is required (e.g., `[a, b, c,]` is valid, `[a, b, c]` is invalid)
    Require,
}

// Builder methods
impl<T, N> Expr<T, N> {
    // Primitives
    /// Create a token expression that matches a specific token.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // Assuming T and N are defined
    /// # // let token_expr = Expr::token(my_token);
    /// ```
    #[must_use]
    pub const fn token(t: T) -> Self {
        Self::Token(t)
    }
    /// Create a rule expression that references a non-terminal.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // Assuming T and N are defined
    /// # // let rule_expr = Expr::rule(my_non_terminal);
    /// ```
    #[must_use]
    pub const fn rule(n: N) -> Self {
        Self::Rule(n)
    }
    /// Create an expression that matches any single token.
    ///
    /// This is useful for error recovery or wildcard matching.
    #[must_use]
    pub const fn any() -> Self {
        Self::Any
    }
    /// Create an expression that matches the end of file.
    ///
    /// This is typically used to ensure the entire input has been consumed.
    #[must_use]
    pub const fn eof() -> Self {
        Self::Eof
    }
    /// Create an expression that matches the empty string.
    ///
    /// This is useful for optional elements or as a base case in recursive rules.
    #[must_use]
    pub const fn empty() -> Self {
        Self::Empty
    }

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

    /// Create an optional expression (matches zero or one occurrence).
    ///
    /// Equivalent to `expr?` in regex notation.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // let optional = Expr::opt(some_expr);
    /// ```
    #[must_use]
    pub fn opt(expr: Self) -> Self {
        Self::Opt(Box::new(expr))
    }

    /// Create a Kleene star expression (matches zero or more occurrences).
    ///
    /// Equivalent to `expr*` in regex notation.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // let zero_or_more = Expr::star(some_expr);
    /// ```
    #[must_use]
    pub fn star(expr: Self) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min: 0,
            max: None,
            greedy: true,
        }
    }

    /// Create a Kleene plus expression (matches one or more occurrences).
    ///
    /// Equivalent to `expr+` in regex notation.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // let one_or_more = Expr::plus(some_expr);
    /// ```
    #[must_use]
    pub fn plus(expr: Self) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min: 1,
            max: None,
            greedy: true,
        }
    }

    /// Create a repeat expression with explicit greedy control.
    ///
    /// # Arguments
    ///
    /// * `expr` - The expression to repeat
    /// * `min` - Minimum number of repetitions (must match)
    /// * `max` - Maximum number of repetitions (None for unlimited)
    /// * `greedy` - Whether to match greedily (true) or non-greedily (false)
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // Greedy: match as many as possible
    /// # // let expr = Expr::token(/* your token */);
    /// # // let greedy = Expr::repeat_with_greedy(expr, 0, None, true);
    /// # // Non-greedy: match as few as possible
    /// # // let non_greedy = Expr::repeat_with_greedy(expr, 0, None, false);
    /// ```
    #[must_use]
    pub fn repeat_with_greedy(expr: Self, min: usize, max: Option<usize>, greedy: bool) -> Self {
        Self::Repeat {
            expr: Box::new(expr),
            min,
            max,
            greedy,
        }
    }

    // Advanced
    /// Create a separated list expression (e.g., `item, item, item`).
    ///
    /// Matches zero or more occurrences of `item` separated by `sep`.
    /// Trailing separators are allowed by default.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // let list = Expr::separated(item_expr, comma_expr);
    /// ```
    #[must_use]
    pub fn separated(item: Self, sep: Self) -> Self {
        Self::Separated {
            item: Box::new(item),
            separator: Box::new(sep),
            min: 0,
            trailing: TrailingSeparator::Allow,
        }
    }

    /// Create a delimited expression (e.g., `(content)` or `{content}`).
    ///
    /// Matches `open`, followed by `content`, followed by `close`.
    /// Error recovery is enabled by default to handle mismatched delimiters.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // let parens = Expr::delimited(lparen, content, rparen);
    /// ```
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
    /// Create a positive lookahead expression.
    ///
    /// Matches if `expr` would match at the current position, but doesn't consume
    /// any input. Useful for disambiguation without committing to a parse.
    #[must_use]
    pub fn lookahead(expr: Self) -> Self {
        Self::Lookahead(Box::new(expr))
    }

    /// Create a negative lookahead expression.
    ///
    /// Matches if `expr` would not match at the current position, but doesn't consume
    /// any input. Useful for ensuring certain patterns don't appear.
    #[must_use]
    pub fn not_lookahead(expr: Self) -> Self {
        Self::NotLookahead(Box::new(expr))
    }

    /// Create a cut operator expression (PEG-specific).
    ///
    /// The cut operator prevents backtracking past this point. Once a cut
    /// succeeds, the parser commits to the current alternative and cannot
    /// backtrack to try other alternatives in the same choice.
    ///
    /// This improves performance and provides better error messages by
    /// committing to a parse path early.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // After matching a keyword, commit to this alternative
    /// # // let keyword_token = /* your token */;
    /// # // let cut_expr = Expr::cut(Expr::token(keyword_token));
    /// ```
    #[must_use]
    pub fn cut(expr: Self) -> Self {
        Self::Cut(Box::new(expr))
    }

    /// Create a token class expression.
    ///
    /// Matches any token that belongs to the specified token class (e.g., digit,
    /// letter, whitespace). This is more flexible than matching specific tokens.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{Expr, token_class::TokenClass};
    /// # // Match any digit
    /// # // let digit: Expr<MyToken, MyNonTerminal> = Expr::token_class(TokenClass::Digit);
    /// ```
    #[must_use]
    pub const fn token_class(class: TokenClass) -> Self {
        Self::TokenClass { class }
    }

    /// Create a conditional expression.
    ///
    /// First parses the `condition` expression. If it succeeds, parses `then_expr`.
    /// If it fails and `else_expr` is provided, parses `else_expr`. If it fails
    /// and no `else_expr` is provided, the entire expression fails.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::Expr;
    /// # // Parse different expressions based on a condition
    /// # // let condition_expr = Expr::token(/* condition token */);
    /// # // let then_expr = Expr::token(/* then token */);
    /// # // let else_expr = Expr::token(/* else token */);
    /// # // let conditional = Expr::conditional(
    /// # //     condition_expr,
    /// # //     then_expr,
    /// # //     Some(else_expr),
    /// # // );
    /// ```
    #[must_use]
    pub fn conditional(condition: Self, then_expr: Self, else_expr: Option<Self>) -> Self {
        Self::Conditional {
            condition: Box::new(condition),
            then_expr: Box::new(then_expr),
            else_expr: else_expr.map(Box::new),
        }
    }

    /// Create a semantic predicate expression.
    ///
    /// Parses the expression and then checks the semantic predicate. If the
    /// predicate returns `true`, the parse succeeds. If it returns `false`,
    /// the parse fails even if the expression matched.
    ///
    /// This enables context-sensitive parsing based on semantic information
    /// (e.g., symbol tables, type information).
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{Expr, predicate::SemanticPredicate};
    /// # // Only parse if type is defined in symbol table
    /// # // #[derive(Debug)]
    /// # // struct IsTypeDefined;
    /// # // impl<T: Token, N: NonTerminal> SemanticPredicate<T, N> for IsTypeDefined {
    /// # //     fn check(&self, grammar: &Grammar<T, N>, input: &[T], pos: usize) -> bool { true }
    /// # // }
    /// # // let type_expr = Expr::token(/* type token */);
    /// # // let predicate_expr = Expr::semantic_predicate(
    /// # //     type_expr,
    /// # //     Box::new(IsTypeDefined),
    /// # // );
    /// ```
    #[must_use]
    pub fn semantic_predicate(expr: Self, predicate: Box<dyn SemanticPredicate<T, N>>) -> Self {
        Self::SemanticPredicate {
            expr: Box::new(expr),
            predicate: std::sync::Arc::from(predicate),
        }
    }

    /// Create a capture expression.
    ///
    /// Captures the matched content of the given expression for later use in
    /// backreferences. This enables patterns like matching paired delimiters
    /// or repeating previously matched content.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{Expr, capture::CaptureId};
    /// # // Capture a delimiter for later matching
    /// # // let capture: Expr<MyToken, MyNonTerminal> = Expr::capture(
    /// # //     CaptureId::Named("delimiter".to_string()),
    /// # //     Expr::token(MyToken::Quote)
    /// # // );
    /// ```
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // CaptureId::Named contains String, cannot be const
    pub fn capture(id: CaptureId, expr: Self) -> Self {
        Self::Capture {
            id,
            expr: Box::new(expr),
        }
    }

    /// Create a backreference expression.
    ///
    /// Matches the same content that was previously captured by a capture group
    /// with the given `capture_id`. This enables patterns like matching paired
    /// delimiters or repeating previously matched content.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{Expr, capture::CaptureId};
    /// # // Match the same delimiter that opened the group
    /// # // let backref: Expr<MyToken, MyNonTerminal> = Expr::backreference(
    /// # //     CaptureId::Named("delimiter".to_string())
    /// # // );
    /// ```
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // CaptureId::Named contains String, cannot be const
    pub fn backreference(capture_id: CaptureId) -> Self {
        Self::Backreference { capture_id }
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
            Self::Empty
            | Self::Opt(_)
            | Self::Lookahead(_)
            | Self::NotLookahead(_)
            | Self::Prune(_) => true,

            Self::Rule(n) => grammar
                .get_rule(n)
                .is_some_and(|r| r.rhs.is_nullable(grammar)),

            Self::Seq(exprs) => exprs.iter().all(|e| e.is_nullable(grammar)),
            Self::Choice(exprs) => exprs.iter().any(|e| e.is_nullable(grammar)),

            Self::Repeat { min, .. } | Self::Separated { min, .. } => *min == 0,

            Self::Node { expr, .. } | Self::Label { expr, .. } | Self::Flatten(expr) => {
                expr.is_nullable(grammar)
            }

            // Cut doesn't affect nullability - depends on inner expression
            // SemanticPredicate: nullable if inner expression is nullable
            Self::Cut(expr) | Self::SemanticPredicate { expr, .. } => expr.is_nullable(grammar),
            // Capture: nullable if inner expression is nullable
            Self::Capture { expr, .. } => expr.is_nullable(grammar),
            // TokenClass matches a token, so not nullable
            // Conditional: nullable if condition is nullable and then_expr is nullable,
            // or if condition fails and else_expr is Some and nullable
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
            // Backreference: not nullable (matches previously captured content)
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
                    && let Some(rule) = grammar.get_rule(n)
                {
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

            // Cut: first set is same as inner expression
            // SemanticPredicate: first set is same as inner expression
            // Capture: first set is same as inner expression
            Self::Opt(expr)
            | Self::Repeat { expr, .. }
            | Self::Cut(expr)
            | Self::SemanticPredicate { expr, .. }
            | Self::Capture { expr, .. } => {
                expr.first_set_impl(grammar, result, visited);
            }
            // TokenClass: can't determine specific tokens, skip
            // Conditional: first set is union of condition, then_expr, and else_expr
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
                    // If condition fails, else_expr is tried
                    // We can't easily determine if condition will fail, so include else_expr's FIRST
                    else_expr.first_set_impl(grammar, result, visited);
                }
            }
            // Backreference: can't determine FIRST set (depends on captured content)
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
            | Self::Cut(expr)
            | Self::RecoveryPoint { expr, .. }
            | Self::SemanticPredicate { expr, .. }
            | Self::Capture { expr, .. } => {
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
            // TokenClass and Backreference don't contain non-terminals
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
