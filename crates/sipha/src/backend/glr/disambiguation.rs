//! Disambiguation strategies for GLR parse forests
//!
//! This module provides strategies for resolving ambiguity in parse forests,
//! including precedence-based and associativity-based disambiguation.

use crate::backend::glr::forest::ForestNode;
use crate::grammar::{Associativity, Expr, Grammar, NonTerminal, Token};
use crate::syntax::{GreenElement, GreenNode, SyntaxKind};
use hashbrown::HashMap;
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::sync::Arc;

type BestAlternative<K> = Option<(Arc<GreenNode<K>>, Option<OperatorInfo<K>>)>;

/// Trait for user-defined disambiguation functions
///
/// Users can implement this trait to provide custom disambiguation logic
/// for ambiguous parse forests. The disambiguator receives all alternative
/// parse trees and returns the preferred one.
pub trait Disambiguator<K: SyntaxKind> {
    /// Disambiguate an ambiguous forest node by selecting the preferred alternative
    ///
    /// # Arguments
    ///
    /// * `alternatives` - All alternative parse trees for the ambiguous node
    ///
    /// # Returns
    ///
    /// The index of the preferred alternative, or `None` if no preference
    fn disambiguate(&self, alternatives: &[Arc<GreenNode<K>>]) -> Option<usize>;
}

/// Disambiguation strategy for resolving ambiguous parse forests
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DisambiguationStrategy {
    /// No disambiguation - return all alternatives
    None,
    /// Use precedence and associativity rules from grammar
    #[default]
    Precedence,
    /// Use associativity rules only
    Associativity,
    /// Use custom disambiguation function
    Custom,
}

/// Apply disambiguation to a forest node using precedence and associativity
pub fn disambiguate_by_precedence<T, N, K>(
    grammar: &Grammar<T, N>,
    node: &ForestNode<K>,
) -> Option<Arc<GreenNode<K>>>
where
    T: Token<Kind = K>,
    N: NonTerminal,
    K: SyntaxKind,
{
    match node {
        ForestNode::Node(n) => Some(n.clone()),
        ForestNode::Ambiguous(alternatives) => {
            if alternatives.is_empty() {
                return None;
            }

            let precedence_map = build_token_precedence_map::<T, N>(grammar);
            if precedence_map.is_empty() {
                return alternatives.first().cloned();
            }

            let mut best: BestAlternative<K> = None;
            for alternative in alternatives {
                let candidate = find_root_operator(alternative, &precedence_map);
                match &best {
                    None => best = Some((alternative.clone(), candidate)),
                    Some((_node, current_info)) => {
                        if precedence_better(candidate.as_ref(), current_info.as_ref()) {
                            best = Some((alternative.clone(), candidate));
                        }
                    }
                }
            }

            best.map(|(node, _)| node)
                .or_else(|| alternatives.first().cloned())
        }
    }
}

/// Apply disambiguation using a custom disambiguator
pub fn disambiguate_with_custom<K: SyntaxKind>(
    node: &ForestNode<K>,
    disambiguator: &dyn Disambiguator<K>,
) -> Option<Arc<GreenNode<K>>> {
    match node {
        ForestNode::Node(n) => Some(n.clone()),
        ForestNode::Ambiguous(alternatives) => {
            if alternatives.is_empty() {
                return None;
            }
            if alternatives.len() == 1 {
                return Some(alternatives[0].clone());
            }

            // Use custom disambiguator to select preferred alternative
            disambiguator.disambiguate(alternatives).map_or_else(
                || Some(alternatives[0].clone()),
                |preferred_idx| alternatives.get(preferred_idx).cloned(),
            )
        }
    }
}

/// Apply disambiguation to a forest node using associativity only
pub fn disambiguate_by_associativity<T, N, K>(
    grammar: &Grammar<T, N>,
    node: &ForestNode<K>,
) -> Option<Arc<GreenNode<K>>>
where
    T: Token<Kind = K>,
    N: NonTerminal,
    K: SyntaxKind,
{
    match node {
        ForestNode::Node(n) => Some(n.clone()),
        ForestNode::Ambiguous(alternatives) => {
            if alternatives.is_empty() {
                return None;
            }

            let precedence_map = build_token_precedence_map::<T, N>(grammar);
            if precedence_map.is_empty() {
                return alternatives.first().cloned();
            }

            let mut best: BestAlternative<K> = None;
            for alternative in alternatives {
                let candidate = find_root_operator(alternative, &precedence_map);
                match &best {
                    None => best = Some((alternative.clone(), candidate)),
                    Some((_node, current_info)) => {
                        if associativity_better(candidate.as_ref(), current_info.as_ref()) {
                            best = Some((alternative.clone(), candidate));
                        }
                    }
                }
            }

            best.map(|(node, _)| node)
                .or_else(|| alternatives.first().cloned())
        }
    }
}

#[derive(Clone, Copy)]
struct PrecedenceInfo {
    precedence: u32,
    associativity: Associativity,
}

#[derive(Clone)]
struct OperatorInfo<K: SyntaxKind> {
    precedence: u32,
    associativity: Associativity,
    depth: usize,
    path: Vec<usize>,
    left_repetition: bool,
    right_repetition: bool,
    phantom: PhantomData<K>,
}

fn build_token_precedence_map<T, N>(
    grammar: &Grammar<T, N>,
) -> HashMap<<T as Token>::Kind, PrecedenceInfo>
where
    T: Token,
    N: NonTerminal,
{
    let mut map = HashMap::new();
    for (_lhs, rule) in grammar.rules() {
        if let Some(hint) = rule.metadata.get_hint::<crate::grammar::PrecedenceHint>() {
            let mut kinds = Vec::new();
            collect_token_kinds(&rule.rhs, &mut kinds);
            for kind in kinds {
                map.entry(kind).or_insert(PrecedenceInfo {
                    precedence: hint.precedence,
                    associativity: hint.associativity,
                });
            }
        }
    }
    map
}

fn collect_token_kinds<T, N>(expr: &Expr<T, N>, kinds: &mut Vec<T::Kind>)
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(token) => kinds.push(token.kind()),
        Expr::Seq(exprs) | Expr::Choice(exprs) => {
            for child in exprs {
                collect_token_kinds(child, kinds);
            }
        }
        Expr::Opt(inner)
        | Expr::Repeat {
            expr: inner,
            greedy: _,
            ..
        }
        | Expr::Lookahead(inner)
        | Expr::NotLookahead(inner)
        | Expr::Cut(inner)
        | Expr::Label { expr: inner, .. }
        | Expr::Node { expr: inner, .. }
        | Expr::Flatten(inner)
        | Expr::Prune(inner)
        | Expr::SemanticPredicate { expr: inner, .. } => {
            collect_token_kinds(inner, kinds);
        }
        Expr::Separated {
            item, separator, ..
        } => {
            collect_token_kinds(item, kinds);
            collect_token_kinds(separator, kinds);
        }
        Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            collect_token_kinds(open, kinds);
            collect_token_kinds(content, kinds);
            collect_token_kinds(close, kinds);
        }
        Expr::RecoveryPoint { expr, sync_tokens } => {
            collect_token_kinds(expr, kinds);
            for token in sync_tokens {
                kinds.push(token.kind());
            }
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_token_kinds(condition, kinds);
            collect_token_kinds(then_expr, kinds);
            if let Some(else_expr) = else_expr {
                collect_token_kinds(else_expr, kinds);
            }
        }
        // TokenClass and Backreference don't have specific token kinds
        _ => {}
    }
}

fn find_root_operator<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    precedence_map: &HashMap<K, PrecedenceInfo>,
) -> Option<OperatorInfo<K>> {
    let mut best = None;
    let mut path = Vec::new();
    visit_operator_candidates(node, precedence_map, &mut path, &mut best);
    best
}

fn visit_operator_candidates<K: SyntaxKind>(
    node: &Arc<GreenNode<K>>,
    precedence_map: &HashMap<K, PrecedenceInfo>,
    path: &mut Vec<usize>,
    best: &mut Option<OperatorInfo<K>>,
) {
    for (idx, child) in node.children().iter().enumerate() {
        path.push(idx);
        let depth = path.len();
        match child {
            GreenElement::Token(token) => {
                if let Some(info) = precedence_map.get(&token.kind()) {
                    let (left_rep, right_rep) = compute_repetition(node, idx, token.kind());
                    let candidate = OperatorInfo {
                        precedence: info.precedence,
                        associativity: info.associativity,
                        depth,
                        path: path.clone(),
                        left_repetition: left_rep,
                        right_repetition: right_rep,
                        phantom: PhantomData,
                    };

                    if operator_is_closer(&candidate, best.as_ref()) {
                        *best = Some(candidate);
                    }
                }
            }
            GreenElement::Node(child_node) => {
                visit_operator_candidates(child_node, precedence_map, path, best);
            }
        }
        path.pop();
    }
}

fn compute_repetition<K: SyntaxKind>(
    parent: &Arc<GreenNode<K>>,
    operator_idx: usize,
    target_kind: K,
) -> (bool, bool) {
    let children = parent.children();
    let left = children
        .iter()
        .take(operator_idx)
        .any(|child| subtree_contains_operator(child, target_kind));
    let right = children
        .iter()
        .skip(operator_idx + 1)
        .any(|child| subtree_contains_operator(child, target_kind));
    (left, right)
}

fn subtree_contains_operator<K: SyntaxKind>(element: &GreenElement<K>, target_kind: K) -> bool {
    match element {
        GreenElement::Token(token) => token.kind() == target_kind,
        GreenElement::Node(node) => node
            .children()
            .iter()
            .any(|child| subtree_contains_operator(child, target_kind)),
    }
}

fn operator_is_closer<K: SyntaxKind>(
    candidate: &OperatorInfo<K>,
    current: Option<&OperatorInfo<K>>,
) -> bool {
    current.is_none_or(|existing| match candidate.depth.cmp(&existing.depth) {
        Ordering::Less => true,
        Ordering::Greater => false,
        Ordering::Equal => candidate.path < existing.path,
    })
}

fn precedence_better<K: SyntaxKind>(
    candidate: Option<&OperatorInfo<K>>,
    current: Option<&OperatorInfo<K>>,
) -> bool {
    match (candidate, current) {
        (Some(cand), Some(curr)) => precedence_prefers::<K>(cand, curr),
        (Some(_), None) => true,
        (None, Some(_) | None) => false,
    }
}

fn precedence_prefers<K: SyntaxKind>(
    candidate: &OperatorInfo<K>,
    current: &OperatorInfo<K>,
) -> bool {
    if candidate.precedence < current.precedence {
        return true;
    }
    if candidate.precedence > current.precedence {
        return false;
    }
    associativity_prefers::<K>(candidate, current).unwrap_or_else(|| candidate.path < current.path)
}

fn associativity_better<K: SyntaxKind>(
    candidate: Option<&OperatorInfo<K>>,
    current: Option<&OperatorInfo<K>>,
) -> bool {
    match (candidate, current) {
        (Some(cand), Some(curr)) => {
            associativity_prefers::<K>(cand, curr).unwrap_or_else(|| cand.path < curr.path)
        }
        (Some(_), None) => true,
        (None, Some(_) | None) => false,
    }
}

fn associativity_prefers<K: SyntaxKind>(
    candidate: &OperatorInfo<K>,
    current: &OperatorInfo<K>,
) -> Option<bool> {
    match (candidate.associativity, current.associativity) {
        (Associativity::None, Associativity::None) => None,
        (Associativity::None, _) | (Associativity::Right, Associativity::Left) => Some(false),
        (_, Associativity::None) | (Associativity::Left, Associativity::Right) => Some(true),
        (Associativity::Left, Associativity::Left) => {
            if candidate.left_repetition == current.left_repetition {
                Some(candidate.path < current.path)
            } else {
                Some(candidate.left_repetition && !current.left_repetition)
            }
        }
        (Associativity::Right, Associativity::Right) => {
            if candidate.right_repetition == current.right_repetition {
                Some(candidate.path > current.path)
            } else {
                Some(candidate.right_repetition && !current.right_repetition)
            }
        }
    }
}

/// Get precedence and associativity for a production rule
#[allow(dead_code)]
pub fn get_production_precedence<T, N>(
    grammar: &Grammar<T, N>,
    lhs: &N,
) -> Option<(u32, Associativity)>
where
    T: Token,
    N: NonTerminal,
{
    grammar.get_rule(lhs).and_then(|rule| {
        rule.metadata
            .get_hint::<crate::grammar::PrecedenceHint>()
            .map(|hint| (hint.precedence, hint.associativity))
    })
}

/// Get precedence and associativity for a token
#[allow(dead_code)]
pub fn get_token_precedence<T, N>(
    grammar: &Grammar<T, N>,
    token: &T,
) -> Option<(u32, Associativity)>
where
    T: Token + PartialEq,
    N: NonTerminal,
{
    // Search through all rules to find one that uses this token and has precedence
    for (_lhs, rule) in grammar.rules() {
        if let Some(hint) = rule.metadata.get_hint::<crate::grammar::PrecedenceHint>() {
            // Check if this rule's expression uses the token
            if expr_contains_token(&rule.rhs, token) {
                return Some((hint.precedence, hint.associativity));
            }
        }
    }
    None
}

/// Check if an expression contains a specific token
#[allow(dead_code)]
fn expr_contains_token<T, N>(expr: &crate::grammar::Expr<T, N>, token: &T) -> bool
where
    T: Token + PartialEq,
    N: NonTerminal,
{
    match expr {
        crate::grammar::Expr::Token(t) => t == token,
        crate::grammar::Expr::Seq(exprs) | crate::grammar::Expr::Choice(exprs) => {
            exprs.iter().any(|e| expr_contains_token(e, token))
        }
        crate::grammar::Expr::Opt(e)
        | crate::grammar::Expr::Repeat {
            expr: e, greedy: _, ..
        }
        | crate::grammar::Expr::Lookahead(e)
        | crate::grammar::Expr::NotLookahead(e)
        | crate::grammar::Expr::Cut(e)
        | crate::grammar::Expr::Label { expr: e, .. }
        | crate::grammar::Expr::Node { expr: e, .. }
        | crate::grammar::Expr::Flatten(e)
        | crate::grammar::Expr::Prune(e)
        | crate::grammar::Expr::RecoveryPoint { expr: e, .. }
        | crate::grammar::Expr::SemanticPredicate { expr: e, .. } => expr_contains_token(e, token),
        crate::grammar::Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            expr_contains_token(condition, token)
                || expr_contains_token(then_expr, token)
                || else_expr
                    .as_ref()
                    .is_some_and(|e| expr_contains_token(e, token))
        }
        crate::grammar::Expr::Separated {
            item, separator, ..
        } => expr_contains_token(item, token) || expr_contains_token(separator, token),
        crate::grammar::Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            expr_contains_token(open, token)
                || expr_contains_token(content, token)
                || expr_contains_token(close, token)
        }
        _ => false,
    }
}
