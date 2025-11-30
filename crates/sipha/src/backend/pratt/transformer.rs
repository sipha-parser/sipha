//! Pratt grammar transformer
//!
//! Transforms ExtendedExpr grammars into PrattGrammar format suitable for Pratt parsing.

use crate::backend::pratt::grammar::{OperatorInfo, PrattGrammar};
use crate::backend::traits::{GrammarTransformer, TransformConfig, TransformError};
use crate::grammar::{CoreExpr, Expr, ExtendedExpr, Grammar, NonTerminal, Token};

/// Pratt grammar transformer
pub struct PrattTransformer;

impl<T, N> GrammarTransformer<T, N> for PrattTransformer
where
    T: Token + Clone,
    N: NonTerminal + Clone,
{
    type BackendGrammar = PrattGrammar<T, N>;
    type Error = TransformError;

    fn transform(
        grammar: &Grammar<T, N>,
        _config: &TransformConfig,
    ) -> Result<Self::BackendGrammar, Self::Error> {
        let mut pratt_grammar = PrattGrammar::new(grammar.clone());

        // Extract operator information from PrattOperator expressions
        for (_, rule) in grammar.rules() {
            Self::extract_operators(&rule.rhs, &mut pratt_grammar)?;
        }

        Ok(pratt_grammar)
    }

    fn supports_expr(expr: &Expr<T, N>) -> bool {
        match expr {
            ExtendedExpr::Core(_) => true,
            ExtendedExpr::PrattOperator { .. } => true, // Pratt natively supports operators
            ExtendedExpr::Lookahead(_) | ExtendedExpr::NotLookahead(_) => false, // Not supported
            ExtendedExpr::TokenClass { .. } => false,   // Not supported
            ExtendedExpr::Conditional { .. } => false,  // Not supported
            ExtendedExpr::SemanticPredicate { .. } => false, // Not supported
            ExtendedExpr::Backreference { .. } => false, // Not supported
            ExtendedExpr::RecoveryPoint { .. } => true, // Can be handled during parsing
            #[cfg(feature = "backend-peg")]
            ExtendedExpr::Cut(_) => false, // Not a Pratt feature
        }
    }

    fn transform_expr(
        expr: &Expr<T, N>,
        _grammar: &Grammar<T, N>,
    ) -> Result<Option<CoreExpr<T, N>>, TransformError> {
        match expr {
            ExtendedExpr::Core(core) => Ok(Some(core.clone())),
            ExtendedExpr::PrattOperator { expr, .. } => {
                // Extract the inner expression
                Self::transform_expr(expr, _grammar)
            }
            ExtendedExpr::RecoveryPoint { expr, .. } => {
                // Strip recovery point wrapper, keep inner expression
                Self::transform_expr(expr, _grammar)
            }
            _ => Err(TransformError::UnsupportedFeature {
                feature: format!("{:?}", expr),
                backend: "Pratt".to_string(),
            }),
        }
    }
}

impl PrattTransformer {
    /// Extract operator information from expressions
    fn extract_operators<T, N>(
        expr: &Expr<T, N>,
        grammar: &mut PrattGrammar<T, N>,
    ) -> Result<(), TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        match expr {
            ExtendedExpr::Core(core_expr) => {
                Self::extract_operators_from_core(core_expr, grammar)?;
            }
            ExtendedExpr::PrattOperator {
                expr: inner_expr,
                precedence,
                associativity,
            } => {
                // Extract operator tokens from the inner expression
                // The inner expression typically contains the operator token
                // For example: PrattOperator { expr: Token(Plus), precedence: 10, ... }
                let operator_tokens = Self::extract_operator_tokens(inner_expr);
                for token in operator_tokens {
                    grammar.add_operator(
                        token,
                        OperatorInfo {
                            precedence: *precedence,
                            associativity: *associativity,
                            is_prefix: false, // Will be determined from context
                            is_infix: true,   // Default to infix
                            is_postfix: false,
                        },
                    );
                }
                // Also recurse into the inner expression
                Self::extract_operators(inner_expr, grammar)?;
            }
            ExtendedExpr::RecoveryPoint { expr, .. } => {
                Self::extract_operators(expr, grammar)?;
            }
            _ => {
                // Other extended expressions are not supported
            }
        }
        Ok(())
    }

    /// Extract operator tokens from an expression
    fn extract_operator_tokens<T, N>(expr: &Expr<T, N>) -> Vec<T>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        let mut tokens = Vec::new();
        match expr {
            ExtendedExpr::Core(CoreExpr::Token(t)) => {
                tokens.push(t.clone());
            }
            ExtendedExpr::Core(CoreExpr::Seq(exprs)) => {
                // In a sequence, operators are typically in the middle
                // For now, collect all tokens
                for e in exprs {
                    tokens.extend(Self::extract_operator_tokens(&ExtendedExpr::Core(
                        e.clone(),
                    )));
                }
            }
            ExtendedExpr::Core(CoreExpr::Choice(exprs)) => {
                // In a choice, any alternative could be an operator
                for e in exprs {
                    tokens.extend(Self::extract_operator_tokens(&ExtendedExpr::Core(
                        e.clone(),
                    )));
                }
            }
            ExtendedExpr::PrattOperator { expr: inner, .. } => {
                tokens.extend(Self::extract_operator_tokens(inner));
            }
            _ => {
                // Other expressions don't directly contain operator tokens
            }
        }
        tokens
    }

    /// Extract operators from core expressions
    fn extract_operators_from_core<T, N>(
        expr: &CoreExpr<T, N>,
        _grammar: &mut PrattGrammar<T, N>,
    ) -> Result<(), TransformError>
    where
        T: Token + Clone,
        N: NonTerminal + Clone,
    {
        match expr {
            CoreExpr::Seq(exprs) | CoreExpr::Choice(exprs) => {
                for e in exprs {
                    Self::extract_operators_from_core(e, _grammar)?;
                }
            }
            CoreExpr::Opt(e)
            | CoreExpr::Repeat { expr: e, .. }
            | CoreExpr::Label { expr: e, .. }
            | CoreExpr::Node { expr: e, .. }
            | CoreExpr::Flatten(e)
            | CoreExpr::Prune(e) => {
                Self::extract_operators_from_core(e, _grammar)?;
            }
            CoreExpr::Separated {
                item, separator, ..
            } => {
                Self::extract_operators_from_core(item, _grammar)?;
                Self::extract_operators_from_core(separator, _grammar)?;
            }
            CoreExpr::Delimited {
                open,
                content,
                close,
            } => {
                Self::extract_operators_from_core(open, _grammar)?;
                Self::extract_operators_from_core(content, _grammar)?;
                Self::extract_operators_from_core(close, _grammar)?;
            }
            CoreExpr::Token(_)
            | CoreExpr::Rule(_)
            | CoreExpr::Any
            | CoreExpr::Eof
            | CoreExpr::Empty => {
                // Leaf nodes, no operators to extract
            }
        }
        Ok(())
    }
}
