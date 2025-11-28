use crate::grammar::{
    BackendHint, Expr, GrammarValidationOptions, NonTerminal, Token, validate_grammar_with_options,
};
use hashbrown::HashMap;
use lasso::Rodeo;
use smallvec::SmallVec;

/// Grammar definition for context-free grammars.
///
/// A `Grammar` represents a complete context-free grammar with production rules,
/// an entry point, and string interning for efficient rule name storage.
///
/// # Performance
///
/// The grammar uses string interning for rule names to reduce memory usage and
/// enable fast string comparisons. Rule names are automatically interned when
/// the grammar is built.
///
/// # Example
///
/// ```rust,no_run
/// use sipha::grammar::{GrammarBuilder, Expr, NonTerminal, Token};
/// use sipha::syntax::{SyntaxKind, TextSize};
///
/// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// # enum MyNonTerminal { Expr }
/// # impl NonTerminal for MyNonTerminal {
/// #     fn name(&self) -> &str { "Expr" }
/// # }
/// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// # enum MySyntaxKind { Number }
/// # impl SyntaxKind for MySyntaxKind {
/// #     fn is_terminal(self) -> bool { true }
/// #     fn is_trivia(self) -> bool { false }
/// # }
/// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// # struct MyToken;
/// # impl Token for MyToken {
/// #     type Kind = MySyntaxKind;
/// #     fn kind(&self) -> Self::Kind { MySyntaxKind::Number }
/// #     fn text_len(&self) -> TextSize { TextSize::from(1) }
/// #     fn text(&self) -> compact_str::CompactString { "1".into() }
/// # }
/// let grammar = GrammarBuilder::<MyToken, MyNonTerminal>::new()
///     .entry_point(MyNonTerminal::Expr)
///     .rule(MyNonTerminal::Expr, Expr::Empty)
///     .build()
///     .expect("Failed to build grammar");
/// ```
#[derive(Clone)]
pub struct Grammar<T, N>
where
    T: Token,
    N: NonTerminal,
{
    rules: HashMap<N, Rule<T, N>, ahash::RandomState>,
    entry_point: N,
    /// String interner for rule names and grammar strings.
    ///
    /// This is used to intern rule names during grammar construction, reducing memory usage
    /// and enabling fast string comparisons. Rule names are automatically interned when
    /// the grammar is built via `GrammarBuilder`.
    interner: Rodeo,
    #[cfg(feature = "grammar-docs")]
    token_descriptions: HashMap<T, String, ahash::RandomState>,
}

/// Production rule
pub struct Rule<T, N> {
    pub lhs: N,
    pub rhs: Expr<T, N>,
    pub metadata: RuleMetadata,
}

impl<T, N> Clone for Rule<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn clone(&self) -> Self {
        Self {
            lhs: self.lhs.clone(),
            rhs: self.rhs.clone(),
            metadata: RuleMetadata {
                hints: SmallVec::new(), // Hints don't need to be cloned
                #[cfg(feature = "grammar-docs")]
                description: self.metadata.description.clone(),
            },
        }
    }
}

/// Metadata for a rule
pub struct RuleMetadata {
    hints: SmallVec<[Box<dyn BackendHint>; 2]>,
    #[cfg(feature = "grammar-docs")]
    description: Option<String>,
}

impl Default for RuleMetadata {
    fn default() -> Self {
        Self::new()
    }
}

impl RuleMetadata {
    #[must_use]
    pub fn new() -> Self {
        Self {
            hints: SmallVec::new(),
            #[cfg(feature = "grammar-docs")]
            description: None,
        }
    }

    pub fn add_hint(&mut self, hint: impl BackendHint + 'static) {
        self.hints.push(Box::new(hint));
    }

    #[must_use]
    pub fn get_hint<H: BackendHint + 'static>(&self) -> Option<&H> {
        self.hints
            .iter()
            .find_map(|h| h.as_any().downcast_ref::<H>())
    }

    #[cfg(feature = "grammar-docs")]
    /// Set a description for this rule
    #[must_use]
    pub fn with_description(mut self, description: String) -> Self {
        self.description = Some(description);
        self
    }

    #[cfg(feature = "grammar-docs")]
    /// Get the description for this rule, if any
    #[must_use]
    pub fn description(&self) -> Option<&str> {
        self.description.as_deref()
    }
}

impl<T, N> Grammar<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn get_rule(&self, lhs: &N) -> Option<&Rule<T, N>> {
        self.rules.get(lhs)
    }

    #[cfg(feature = "grammar-docs")]
    /// Get the description for a token, if any
    #[must_use]
    pub fn token_description(&self, token: &T) -> Option<&str> {
        self.token_descriptions.get(token).map(String::as_str)
    }

    #[cfg(feature = "grammar-docs")]
    /// Generate markdown documentation for this grammar
    ///
    /// The `token_name` closure is used to format token names when no description is available.
    /// Typically, this would format the token using its Debug implementation or a custom mapping.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{GrammarBuilder, Expr, NonTerminal, Token};
    /// # use sipha::syntax::SyntaxKind;
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MySyntaxKind { Plus, Expr }
    /// # impl SyntaxKind for MySyntaxKind {
    /// #     fn is_terminal(self) -> bool { matches!(self, Self::Plus) }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MyToken { Plus }
    /// # impl Token for MyToken {
    /// #     type Kind = MySyntaxKind;
    /// #     fn kind(&self) -> Self::Kind { MySyntaxKind::Plus }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # enum MyNonTerminal { Expr }
    /// # impl NonTerminal for MyNonTerminal {
    /// #     fn name(&self) -> &str { "Expr" }
    /// # }
    /// # let grammar = GrammarBuilder::<MyToken, MyNonTerminal>::new()
    /// #     .entry_point(MyNonTerminal::Expr)
    /// #     .rule(MyNonTerminal::Expr, Expr::Empty)
    /// #     .build()
    /// #     .unwrap();
    /// let markdown = grammar.to_markdown(&|t| format!("{t:?}"));
    /// println!("{markdown}");
    /// ```
    #[cfg(feature = "grammar-docs")]
    pub fn to_markdown(&self, token_name: &impl Fn(&T) -> String) -> String {
        crate::grammar::docs::generate_markdown(self, token_name)
    }

    /// Generate markdown documentation with custom configuration
    ///
    /// This method allows you to customize what information is included in the generated documentation.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{GrammarBuilder, Expr, NonTerminal, Token, docs::MarkdownConfig};
    /// # use sipha::syntax::SyntaxKind;
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MySyntaxKind { Plus, Expr }
    /// # impl SyntaxKind for MySyntaxKind {
    /// #     fn is_terminal(self) -> bool { matches!(self, Self::Plus) }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # enum MyNonTerminal { Expr }
    /// # impl NonTerminal for MyNonTerminal {
    /// #     fn name(&self) -> &str { "Expr" }
    /// # }
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # struct MyToken;
    /// # impl Token for MyToken {
    /// #     type Kind = MySyntaxKind;
    /// #     fn kind(&self) -> Self::Kind { MySyntaxKind::Plus }
    /// # }
    /// # let grammar = GrammarBuilder::<MyToken, MyNonTerminal>::new()
    /// #     .entry_point(MyNonTerminal::Expr)
    /// #     .rule(MyNonTerminal::Expr, Expr::Empty)
    /// #     .build()
    /// #     .unwrap();
    /// let mut config = MarkdownConfig::default();
    /// config.include_dependencies = false;
    /// config.include_token_usage = false;
    /// config.max_line_length = 100;
    /// let markdown = grammar.to_markdown_with_config(&|t: &MyToken| format!("{t:?}"), &config);
    /// ```
    #[cfg(feature = "grammar-docs")]
    pub fn to_markdown_with_config(
        &self,
        token_name: &impl Fn(&T) -> String,
        config: &crate::grammar::docs::MarkdownConfig,
    ) -> String {
        crate::grammar::docs::generate_markdown_with_config(self, token_name, config)
    }

    #[must_use]
    pub const fn entry_point(&self) -> &N {
        &self.entry_point
    }

    pub fn rules(&self) -> impl Iterator<Item = (&N, &Rule<T, N>)> {
        self.rules.iter()
    }

    /// Get an interned rule name for a non-terminal (read-only access)
    ///
    /// This returns the interned reference if the name was already interned,
    /// or None if it hasn't been interned yet. For building grammars, use
    /// `GrammarBuilder` which can intern new strings.
    #[must_use]
    pub fn get_interned_rule_name(&self, name: &str) -> Option<lasso::Spur> {
        self.interner.get(name)
    }

    /// Get an interned rule name for a non-terminal (read-only access)
    ///
    /// This is a convenience method that gets the interned name of the given non-terminal.
    #[must_use]
    pub fn get_interned_non_terminal_name(&self, nt: &N) -> Option<lasso::Spur> {
        self.get_interned_rule_name(nt.name())
    }

    /// Resolve an interned string back to its original value
    ///
    /// This is useful when you have an interned reference and need the original string.
    #[must_use]
    pub fn resolve_interned(&self, spur: lasso::Spur) -> &str {
        self.interner.resolve(&spur)
    }

    /// Try to get a fallback syntax kind from any rule in the grammar.
    /// This is used as a last resort when no other kind can be determined.
    #[must_use]
    pub fn try_get_fallback_kind<K: crate::syntax::SyntaxKind>(&self) -> Option<K> {
        // Try to get a kind from any rule's non-terminal
        for (nt, _) in &self.rules {
            if let Some(kind) = nt.to_syntax_kind() {
                return Some(kind);
            }
            if let Some(kind) = nt.default_syntax_kind() {
                return Some(kind);
            }
        }
        None
    }

    /// Compute FOLLOW sets for all non-terminals in the grammar
    #[must_use]
    pub fn compute_follow_sets(
        &self,
    ) -> hashbrown::HashMap<N, hashbrown::HashSet<T, ahash::RandomState>, ahash::RandomState> {
        use hashbrown::HashSet;
        let mut follow_sets: hashbrown::HashMap<
            N,
            HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        > = hashbrown::HashMap::with_hasher(ahash::RandomState::new());

        // Initialize all non-terminals with empty sets
        for (nt, _) in &self.rules {
            follow_sets.insert(nt.clone(), HashSet::with_hasher(ahash::RandomState::new()));
        }

        // Add EOF to FOLLOW of entry point
        // Note: We can't create an EOF token here, so we'll handle this in the LL parser

        // Iterate until no changes (fixed point algorithm)
        let mut changed = true;
        while changed {
            changed = false;

            for (lhs, rule) in &self.rules {
                // For each production A -> αBβ:
                // - Add FIRST(β) - {ε} to FOLLOW(B)
                // - If ε ∈ FIRST(β), add FOLLOW(A) to FOLLOW(B)

                self.update_follow_sets_for_expr(&rule.rhs, lhs, &mut follow_sets, &mut changed);
            }
        }

        follow_sets
    }

    #[allow(clippy::too_many_lines)]
    fn update_follow_sets_for_expr(
        &self,
        expr: &Expr<T, N>,
        lhs: &N,
        follow_sets: &mut hashbrown::HashMap<
            N,
            hashbrown::HashSet<T, ahash::RandomState>,
            ahash::RandomState,
        >,
        changed: &mut bool,
    ) {
        // Helper to process a sequence and update FOLLOW sets
        fn process_sequence<T: Token, N: NonTerminal>(
            grammar: &Grammar<T, N>,
            exprs: &[Expr<T, N>],
            lhs: &N,
            follow_sets: &mut hashbrown::HashMap<
                N,
                hashbrown::HashSet<T, ahash::RandomState>,
                ahash::RandomState,
            >,
            changed: &mut bool,
        ) {
            for (i, expr) in exprs.iter().enumerate() {
                // Find all non-terminals in this expression
                let mut nts = Vec::new();
                collect_nonterminals(expr, &mut nts);

                for nt in nts {
                    // Compute FIRST of suffix after this position
                    let mut suffix_first =
                        hashbrown::HashSet::with_hasher(ahash::RandomState::new());
                    let mut nullable_suffix = true;

                    for suffix_expr in exprs.iter().skip(i + 1) {
                        let first = suffix_expr.first_set(grammar);
                        suffix_first.extend(first.iter().cloned());
                        if !suffix_expr.is_nullable(grammar) {
                            nullable_suffix = false;
                            break;
                        }
                    }

                    // Add FIRST(suffix) to FOLLOW(nt)
                    if let Some(follow_nt) = follow_sets.get_mut(&nt) {
                        for token in &suffix_first {
                            if follow_nt.insert(token.clone()) {
                                *changed = true;
                            }
                        }
                    }

                    // If suffix is nullable, add FOLLOW(lhs) to FOLLOW(nt)
                    if nullable_suffix {
                        let follow_lhs_tokens: Vec<T> = follow_sets
                            .get(lhs)
                            .map(|s| s.iter().cloned().collect())
                            .unwrap_or_default();

                        if let Some(follow_nt) = follow_sets.get_mut(&nt) {
                            for token in follow_lhs_tokens {
                                if follow_nt.insert(token) {
                                    *changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        fn collect_nonterminals<T: Token, N: NonTerminal>(expr: &Expr<T, N>, result: &mut Vec<N>) {
            match expr {
                Expr::Rule(n) => {
                    result.push(n.clone());
                }
                Expr::Seq(exprs) | Expr::Choice(exprs) => {
                    for e in exprs {
                        collect_nonterminals(e, result);
                    }
                }
                Expr::Opt(e) | Expr::Repeat { expr: e, .. } => {
                    collect_nonterminals(e, result);
                }
                Expr::Separated {
                    item, separator, ..
                } => {
                    collect_nonterminals(item, result);
                    collect_nonterminals(separator, result);
                }
                Expr::Delimited {
                    open,
                    content,
                    close,
                    ..
                } => {
                    collect_nonterminals(open, result);
                    collect_nonterminals(content, result);
                    collect_nonterminals(close, result);
                }
                Expr::Label { expr, .. }
                | Expr::Node { expr, .. }
                | Expr::Flatten(expr)
                | Expr::Prune(expr)
                | Expr::Lookahead(expr)
                | Expr::NotLookahead(expr)
                | Expr::RecoveryPoint { expr, .. } => {
                    collect_nonterminals(expr, result);
                }
                _ => {}
            }
        }

        match expr {
            Expr::Seq(exprs) => {
                process_sequence(self, exprs, lhs, follow_sets, changed);
            }
            Expr::Choice(exprs) => {
                for expr in exprs {
                    self.update_follow_sets_for_expr(expr, lhs, follow_sets, changed);
                }
            }
            Expr::Opt(expr)
            | Expr::Repeat { expr, .. }
            | Expr::Label { expr, .. }
            | Expr::Node { expr, .. }
            | Expr::Flatten(expr)
            | Expr::Prune(expr)
            | Expr::Lookahead(expr)
            | Expr::NotLookahead(expr)
            | Expr::RecoveryPoint { expr, .. } => {
                self.update_follow_sets_for_expr(expr, lhs, follow_sets, changed);
            }
            Expr::Separated {
                item, separator, ..
            } => {
                // Treat as sequence: item separator item separator ...
                // For simplicity, process item and separator separately
                self.update_follow_sets_for_expr(item, lhs, follow_sets, changed);
                self.update_follow_sets_for_expr(separator, lhs, follow_sets, changed);
            }
            Expr::Delimited {
                open,
                content,
                close,
                ..
            } => {
                // Treat as sequence: open content close
                process_sequence(
                    self,
                    &[
                        open.as_ref().clone(),
                        content.as_ref().clone(),
                        close.as_ref().clone(),
                    ],
                    lhs,
                    follow_sets,
                    changed,
                );
            }
            _ => {}
        }
    }

    /// Get FOLLOW set for a specific non-terminal
    #[must_use]
    pub fn follow_set(&self, nt: &N) -> hashbrown::HashSet<T, ahash::RandomState> {
        self.compute_follow_sets()
            .get(nt)
            .cloned()
            .unwrap_or_else(|| hashbrown::HashSet::with_hasher(ahash::RandomState::new()))
    }
}

/// Builder for constructing grammars
pub struct GrammarBuilder<T, N> {
    rules: Vec<Rule<T, N>>,
    entry_point: Option<N>,
    interner: Rodeo,
    validation_options: GrammarValidationOptions,
    #[cfg(feature = "grammar-docs")]
    token_descriptions: HashMap<T, String, ahash::RandomState>,
}

impl<T, N> Default for GrammarBuilder<T, N>
where
    T: Token,
    N: NonTerminal,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T, N> GrammarBuilder<T, N>
where
    T: Token,
    N: NonTerminal,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            entry_point: None,
            interner: Rodeo::new(),
            validation_options: GrammarValidationOptions::default(),
            #[cfg(feature = "grammar-docs")]
            token_descriptions: HashMap::with_hasher(ahash::RandomState::new()),
        }
    }

    /// Allow direct left recursion during validation.
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    pub fn allow_left_recursion(mut self) -> Self {
        self.validation_options.allow_left_recursion = true;
        self
    }

    #[must_use]
    pub fn entry_point(mut self, entry: N) -> Self {
        self.entry_point = Some(entry);
        self
    }

    #[must_use]
    pub fn rule(mut self, lhs: N, rhs: Expr<T, N>) -> Self {
        // Intern the rule name for efficient storage
        let _ = self.interner.get_or_intern(lhs.name());
        self.rules.push(Rule {
            lhs,
            rhs,
            metadata: RuleMetadata::new(),
        });
        self
    }

    #[must_use]
    pub fn rule_with(mut self, lhs: N, rhs: Expr<T, N>, f: impl FnOnce(&mut RuleMetadata)) -> Self {
        // Intern the rule name for efficient storage
        let _ = self.interner.get_or_intern(lhs.name());
        let mut metadata = RuleMetadata::new();
        f(&mut metadata);

        self.rules.push(Rule { lhs, rhs, metadata });
        self
    }

    #[cfg(feature = "grammar-docs")]
    /// Add a rule with a description
    ///
    /// This is a convenience method for adding a rule with documentation.
    /// It's equivalent to calling `rule_with` and setting the description in the closure.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{GrammarBuilder, Expr, NonTerminal, Token};
    /// # use sipha::syntax::SyntaxKind;
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MySyntaxKind { Plus, Expr }
    /// # impl SyntaxKind for MySyntaxKind {
    /// #     fn is_terminal(self) -> bool { matches!(self, Self::Plus) }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MyToken { Plus }
    /// # impl Token for MyToken {
    /// #     type Kind = MySyntaxKind;
    /// #     fn kind(&self) -> Self::Kind { MySyntaxKind::Plus }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # enum MyNonTerminal { Expr }
    /// # impl NonTerminal for MyNonTerminal {
    /// #     fn name(&self) -> &str { "Expr" }
    /// # }
    /// GrammarBuilder::<MyToken, MyNonTerminal>::new()
    ///     .rule_with_description(
    ///         MyNonTerminal::Expr,
    ///         Expr::Empty,
    ///         "An expression rule".to_string(),
    ///     );
    /// ```
    #[must_use]
    pub fn rule_with_description(mut self, lhs: N, rhs: Expr<T, N>, description: String) -> Self {
        // Intern the rule name for efficient storage
        let _ = self.interner.get_or_intern(lhs.name());
        let metadata = RuleMetadata::new().with_description(description);

        self.rules.push(Rule { lhs, rhs, metadata });
        self
    }

    #[cfg(feature = "grammar-docs")]
    /// Add a description for a token
    ///
    /// This allows you to document tokens that appear in the grammar.
    /// Token descriptions will be included in the generated markdown documentation.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// # use sipha::grammar::{GrammarBuilder, Expr, NonTerminal, Token};
    /// # use sipha::syntax::SyntaxKind;
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MySyntaxKind { Plus, Expr }
    /// # impl SyntaxKind for MySyntaxKind {
    /// #     fn is_terminal(self) -> bool { matches!(self, Self::Plus) }
    /// #     fn is_trivia(self) -> bool { false }
    /// # }
    /// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    /// # enum MyToken { Plus }
    /// # impl Token for MyToken {
    /// #     type Kind = MySyntaxKind;
    /// #     fn kind(&self) -> Self::Kind { MySyntaxKind::Plus }
    /// # }
    /// # #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    /// # enum MyNonTerminal { Expr }
    /// # impl NonTerminal for MyNonTerminal {
    /// #     fn name(&self) -> &str { "Expr" }
    /// # }
    /// GrammarBuilder::<MyToken, MyNonTerminal>::new()
    ///     .token_description(MyToken::Plus, "The plus operator (+)".to_string());
    /// ```
    #[must_use]
    pub fn token_description(mut self, token: T, description: String) -> Self {
        self.token_descriptions.insert(token, description);
        self
    }

    /// Build the grammar from the configured rules.
    ///
    /// # Errors
    ///
    /// Returns an error if the entry point is missing, or if grammar validation fails.
    pub fn build(mut self) -> Result<Grammar<T, N>, GrammarError<T, N>> {
        let entry_point = self.entry_point.ok_or(GrammarError::MissingEntryPoint)?;

        // Intern the entry point name
        let _ = self.interner.get_or_intern(entry_point.name());

        // Validate grammar
        validate_grammar_with_options(&self.rules, self.validation_options)?;

        Ok(Grammar {
            rules: self.rules.into_iter().map(|r| (r.lhs.clone(), r)).collect(),
            entry_point,
            interner: self.interner,
            #[cfg(feature = "grammar-docs")]
            token_descriptions: self.token_descriptions,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GrammarError<T, N> {
    #[error("Missing entry point")]
    MissingEntryPoint,

    #[error("Left recursion detected: {0:?}")]
    LeftRecursion(Vec<Vec<N>>),

    #[error("FIRST/FIRST conflict in rule {rule:?} between alternatives {alt1} and {alt2}")]
    FirstFirstConflict {
        rule: N,
        alt1: usize,
        alt2: usize,
        tokens: Vec<T>,
    },

    #[error(
        "FIRST/FOLLOW conflict in nullable rule {rule:?}: tokens {tokens:?} appear in both FIRST and FOLLOW sets"
    )]
    FirstFollowConflict { rule: N, tokens: Vec<T> },

    #[error("Undefined rule: {0:?}")]
    UndefinedRule(N),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::Expr;
    use crate::syntax::SyntaxKind;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestToken {
        Ident,
        Number,
        Plus,
        Eof,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum TestNonTerminal {
        Expr,
        Term,
    }

    impl crate::grammar::Token for TestToken {
        type Kind = TestSyntaxKind;

        fn kind(&self) -> Self::Kind {
            match self {
                Self::Ident => TestSyntaxKind::Ident,
                Self::Number => TestSyntaxKind::Number,
                Self::Plus => TestSyntaxKind::Plus,
                Self::Eof => TestSyntaxKind::Eof,
            }
        }

        fn text_len(&self) -> crate::syntax::TextSize {
            crate::syntax::TextSize::from(1)
        }

        fn text(&self) -> compact_str::CompactString {
            match self {
                Self::Ident => "ident".into(),
                Self::Number => "number".into(),
                Self::Plus => "+".into(),
                Self::Eof => "".into(),
            }
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestSyntaxKind {
        Ident,
        Number,
        Plus,
        Eof,
    }

    impl SyntaxKind for TestSyntaxKind {
        fn is_terminal(self) -> bool {
            true // All variants are terminals in this test
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    impl crate::grammar::NonTerminal for TestNonTerminal {
        fn name(&self) -> &str {
            match self {
                Self::Expr => "Expr",
                Self::Term => "Term",
            }
        }
    }

    #[test]
    fn test_grammar_builder_new() {
        let builder = GrammarBuilder::<TestToken, TestNonTerminal>::new();
        assert!(builder.entry_point.is_none());
        assert!(builder.rules.is_empty());
    }

    #[test]
    fn test_grammar_builder_entry_point() {
        let builder =
            GrammarBuilder::<TestToken, TestNonTerminal>::new().entry_point(TestNonTerminal::Expr);

        assert_eq!(builder.entry_point, Some(TestNonTerminal::Expr));
    }

    #[test]
    fn test_grammar_builder_rule() {
        let builder = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident));

        assert_eq!(builder.rules.len(), 1);
        assert_eq!(builder.rules[0].lhs, TestNonTerminal::Expr);
    }

    #[test]
    fn test_grammar_builder_rule_with() {
        let builder = GrammarBuilder::<TestToken, TestNonTerminal>::new().rule_with(
            TestNonTerminal::Expr,
            Expr::token(TestToken::Ident),
            |_| {},
        );

        assert_eq!(builder.rules.len(), 1);
    }

    #[test]
    fn test_grammar_builder_build_missing_entry_point() {
        let builder = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident));

        let result = builder.build();
        assert!(result.is_err());
        // Check error type without requiring Debug
        assert!(
            matches!(result, Err(GrammarError::MissingEntryPoint)),
            "Expected MissingEntryPoint error"
        );
    }

    #[test]
    fn test_grammar_builder_build_success() {
        let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident))
            .build()
            .unwrap();

        assert_eq!(grammar.entry_point(), &TestNonTerminal::Expr);
        // Grammar doesn't implement Debug, so we can't format it
        // but we can test its functionality
    }

    #[test]
    fn test_grammar_get_rule() {
        let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident))
            .build()
            .unwrap();

        let rule = grammar.get_rule(&TestNonTerminal::Expr);
        assert!(rule.is_some());
        assert_eq!(rule.unwrap().lhs, TestNonTerminal::Expr);

        let missing = grammar.get_rule(&TestNonTerminal::Term);
        assert!(missing.is_none());
    }

    #[test]
    fn test_grammar_rules_iter() {
        let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident))
            .rule(TestNonTerminal::Term, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        assert_eq!(grammar.rules().count(), 2);
    }

    #[test]
    fn test_rule_metadata_new() {
        let metadata = RuleMetadata::new();
        assert!(metadata.hints.is_empty());
    }

    #[test]
    fn test_rule_metadata_add_hint() {
        #[derive(Debug)]
        struct TestHint;
        impl BackendHint for TestHint {
            fn as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn description(&self) -> String {
                "Test hint".to_string()
            }
        }

        let mut metadata = RuleMetadata::new();
        metadata.add_hint(TestHint);
        assert_eq!(metadata.hints.len(), 1);
    }

    #[test]
    fn test_rule_metadata_get_hint() {
        #[derive(Debug)]
        struct TestHint;
        impl BackendHint for TestHint {
            fn as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn description(&self) -> String {
                "Test hint".to_string()
            }
        }

        let mut metadata = RuleMetadata::new();
        metadata.add_hint(TestHint);

        let hint = metadata.get_hint::<TestHint>();
        assert!(hint.is_some());
    }

    #[test]
    fn test_rule_clone() {
        let rule = Rule {
            lhs: TestNonTerminal::Expr,
            rhs: Expr::token(TestToken::Ident),
            metadata: RuleMetadata::new(),
        };

        let cloned = rule.clone();
        assert_eq!(rule.lhs, cloned.lhs);
        assert_eq!(format!("{:?}", rule.rhs), format!("{:?}", cloned.rhs));
    }

    #[test]
    fn test_grammar_follow_sets() {
        let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident))
            .build()
            .unwrap();

        let follow_sets = grammar.compute_follow_sets();
        assert!(!follow_sets.is_empty());
    }

    #[test]
    fn test_grammar_follow_set() {
        let grammar = GrammarBuilder::<TestToken, TestNonTerminal>::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Ident))
            .build()
            .unwrap();

        let _follow_set = grammar.follow_set(&TestNonTerminal::Expr);
        // Follow set should exist (even if empty)
        // Just check it doesn't panic
    }
}
