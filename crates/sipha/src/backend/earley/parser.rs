//! Earley parser implementation

use crate::backend::earley::chart::{EarleyChart, EarleyItem, EarleySymbol};
use crate::backend::earley::config::EarleyConfig;
use crate::backend::earley::state::EarleyParserState;
use crate::error::{ParseError, ParseMetrics, ParseResult, ParseWarning};
use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use crate::syntax::builder::GreenNodeBuilder;
use crate::syntax::{GreenElement, GreenNode, GreenToken, SyntaxKind, TextRange, TextSize};
use std::sync::Arc;

/// Parse input using Earley algorithm
pub fn parse<T, N>(
    grammar: &Grammar<T, N>,
    input: &[T],
    entry: &N,
    config: &EarleyConfig,
    _state: &mut EarleyParserState<T, N>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let start_time = std::time::Instant::now();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Initialize chart
    let mut chart = EarleyChart::new(input.len());

    // Add initial items for the entry point
    if let Some(entry_rule) = grammar.get_rule(entry) {
        // For each production of the entry rule, add an item at position 0
        if let Expr::Choice(alternatives) = &entry_rule.rhs {
            for (idx, _alt) in alternatives.iter().enumerate() {
                let item = EarleyItem::new(entry.clone(), idx, 0, 0);
                chart.add(0, item);
            }
        } else {
            let item = EarleyItem::new(entry.clone(), 0, 0, 0);
            chart.add(0, item);
        }
    } else {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
            message: format!("Entry point {} not found in grammar", entry.name()),
        });

        let error_kind = input
            .first()
            .map(crate::grammar::Token::kind)
            .or_else(|| entry.default_syntax_kind())
            .or_else(|| grammar.try_get_fallback_kind())
            .unwrap_or_else(|| {
                entry.default_syntax_kind().expect(
                    "CRITICAL: Cannot create error result - no syntax kind available. \
                     This is a configuration error. You must implement \
                     NonTerminal::default_syntax_kind() to return Some for error cases.",
                )
            });

        return ParseResult {
            root: GreenNode::new(error_kind, vec![], TextSize::zero()),
            errors,
            warnings,
            metrics: ParseMetrics::default(),
            #[cfg(feature = "backend-glr")]
            forest: None,
            _phantom: std::marker::PhantomData,
        };
    }

    // Main Earley algorithm: process each position
    for i in 0..=input.len() {
        let mut changed = true;

        // Process items until no new items are added (closure)
        while changed {
            changed = false;

            // Get items to process (clone to avoid borrow issues)
            let items_to_process: Vec<EarleyItem<T, N>> = chart.get(i).iter().cloned().collect();

            for item in items_to_process {
                // Get the rule for this item
                if let Some(rule) = grammar.get_rule(&item.lhs) {
                    let rhs = match &rule.rhs {
                        Expr::Choice(alts) => {
                            if item.rule_index < alts.len() {
                                &alts[item.rule_index]
                            } else {
                                continue;
                            }
                        }
                        _ => &rule.rhs,
                    };

                    // Check what symbol comes after the dot
                    if let Some(symbol) = item.next_symbol(rhs) {
                        match symbol {
                            EarleySymbol::Terminal(expected_token) => {
                                // Scanner: if we're at position i and expect a terminal
                                // and the next token matches, advance the dot
                                if i < input.len() && input[i].kind() == expected_token.kind() {
                                    let new_item = EarleyItem::new(
                                        item.lhs.clone(),
                                        item.rule_index,
                                        item.dot + 1,
                                        item.start,
                                    );
                                    if chart.add(i + 1, new_item) {
                                        changed = true;
                                    }
                                }
                            }
                            EarleySymbol::NonTerminal(nt) => {
                                // Predictor: add items for all productions of this non-terminal
                                if let Some(nt_rule) = grammar.get_rule(&nt) {
                                    match &nt_rule.rhs {
                                        Expr::Choice(alts) => {
                                            for (alt_idx, _) in alts.iter().enumerate() {
                                                let new_item =
                                                    EarleyItem::new(nt.clone(), alt_idx, 0, i);
                                                if chart.add(i, new_item) {
                                                    changed = true;
                                                }
                                            }
                                        }
                                        _ => {
                                            let new_item = EarleyItem::new(nt.clone(), 0, 0, i);
                                            if chart.add(i, new_item) {
                                                changed = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        // Completer: item is complete, advance items that were waiting for this non-terminal
                        // Find all items at position i that are waiting for item.lhs
                        let completers: Vec<EarleyItem<T, N>> =
                            chart.get(i).iter().cloned().collect();
                        for completer in completers {
                            if let Some(completer_rule) = grammar.get_rule(&completer.lhs) {
                                let completer_rhs = match &completer_rule.rhs {
                                    Expr::Choice(alts) => {
                                        if completer.rule_index < alts.len() {
                                            &alts[completer.rule_index]
                                        } else {
                                            continue;
                                        }
                                    }
                                    _ => &completer_rule.rhs,
                                };

                                // Check if completer is waiting for item.lhs
                                if let Some(EarleySymbol::NonTerminal(nt)) =
                                    completer.next_symbol(completer_rhs)
                                {
                                    if nt == item.lhs && completer.start == item.start {
                                        // Advance the completer
                                        let new_item = EarleyItem::new(
                                            completer.lhs.clone(),
                                            completer.rule_index,
                                            completer.dot + 1,
                                            completer.start,
                                        );
                                        if chart.add(i, new_item) {
                                            changed = true;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Check if we have a complete parse
    let complete_items: Vec<_> = chart
        .get(input.len())
        .iter()
        .filter(|item| {
            item.lhs == *entry
                && item.start == 0
                && grammar.get_rule(&item.lhs).map_or(false, |rule| {
                    let rhs = match &rule.rhs {
                        Expr::Choice(alts) => {
                            if item.rule_index < alts.len() {
                                &alts[item.rule_index]
                            } else {
                                return false;
                            }
                        }
                        _ => &rule.rhs,
                    };
                    item.is_complete(rhs_len(rhs))
                })
        })
        .cloned()
        .collect();

    if complete_items.is_empty() {
        errors.push(ParseError::UnexpectedEof {
            span: crate::syntax::TextRange::at(
                TextSize::from(input.len() as u32),
                TextSize::from(1),
            ),
            expected: vec!["complete parse".to_string()],
        });
    }

    // Build parse tree from chart
    let root_kind = entry
        .to_syntax_kind()
        .or_else(|| entry.default_syntax_kind())
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| grammar.try_get_fallback_kind())
        .unwrap_or_else(|| {
            entry.default_syntax_kind().expect(
                "CRITICAL: Cannot create parse result - no syntax kind available. \
                 This is a configuration error. You must implement \
                 NonTerminal::default_syntax_kind() to return Some for error cases.",
            )
        });

    // Extract parse tree from chart
    let root = if let Some(complete_item) = complete_items.first() {
        extract_tree(grammar, &chart, complete_item, input, 0, input.len())
            .unwrap_or_else(|| GreenNode::new(root_kind, vec![], TextSize::zero()))
    } else {
        GreenNode::new(root_kind, vec![], TextSize::zero())
    };

    let metrics = ParseMetrics {
        parse_time: start_time.elapsed(),
        tokens_consumed: input.len(),
        nodes_created: 1,
        errors_recovered: 0,
        cache_hits: 0,
    };

    ParseResult {
        root,
        errors,
        warnings,
        metrics,
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: std::marker::PhantomData,
    }
}

/// Parse with incremental session support
pub fn parse_with_session<T, N>(
    grammar: &Grammar<T, N>,
    input: &[T],
    entry: &N,
    config: &EarleyConfig,
    _state: &mut EarleyParserState<T, N>,
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
) -> ParseResult<T, N>
where
    T: Token,
    N: NonTerminal,
{
    let start_time = std::time::Instant::now();
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Initialize chart
    let mut chart = EarleyChart::new(input.len());

    // Add initial items for the entry point
    if let Some(entry_rule) = grammar.get_rule(entry) {
        // For each production of the entry rule, add an item at position 0
        if let Expr::Choice(alternatives) = &entry_rule.rhs {
            for (idx, _alt) in alternatives.iter().enumerate() {
                let item = EarleyItem::new(entry.clone(), idx, 0, 0);
                chart.add(0, item);
            }
        } else {
            let item = EarleyItem::new(entry.clone(), 0, 0, 0);
            chart.add(0, item);
        }
    } else {
        errors.push(ParseError::InvalidSyntax {
            span: crate::syntax::TextRange::at(TextSize::zero(), TextSize::from(1)),
            message: format!("Entry point {} not found in grammar", entry.name()),
        });

        let error_kind = input
            .first()
            .map(crate::grammar::Token::kind)
            .or_else(|| entry.default_syntax_kind())
            .or_else(|| grammar.try_get_fallback_kind())
            .unwrap_or_else(|| {
                entry.default_syntax_kind().expect(
                    "CRITICAL: Cannot create error result - no syntax kind available. \
                     This is a configuration error. You must implement \
                     NonTerminal::default_syntax_kind() to return Some for error cases.",
                )
            });

        return ParseResult {
            root: GreenNode::new(error_kind, vec![], TextSize::zero()),
            errors,
            warnings,
            metrics: ParseMetrics::default(),
            #[cfg(feature = "backend-glr")]
            forest: None,
            _phantom: std::marker::PhantomData,
        };
    }

    // Main Earley algorithm: process each position
    for i in 0..=input.len() {
        let mut changed = true;

        // Process items until no new items are added (closure)
        while changed {
            changed = false;

            // Get items to process (clone to avoid borrow issues)
            let items_to_process: Vec<EarleyItem<T, N>> = chart.get(i).iter().cloned().collect();

            for item in items_to_process {
                // Get the rule for this item
                if let Some(rule) = grammar.get_rule(&item.lhs) {
                    let rhs = match &rule.rhs {
                        Expr::Choice(alts) => {
                            if item.rule_index < alts.len() {
                                &alts[item.rule_index]
                            } else {
                                continue;
                            }
                        }
                        _ => &rule.rhs,
                    };

                    // Check what symbol comes after the dot
                    if let Some(symbol) = item.next_symbol(rhs) {
                        match symbol {
                            EarleySymbol::Terminal(expected_token) => {
                                // Scanner: if we're at position i and expect a terminal
                                // and the next token matches, advance the dot
                                if i < input.len() && input[i].kind() == expected_token.kind() {
                                    let new_item = EarleyItem::new(
                                        item.lhs.clone(),
                                        item.rule_index,
                                        item.dot + 1,
                                        item.start,
                                    );
                                    if chart.add(i + 1, new_item) {
                                        changed = true;
                                    }
                                }
                            }
                            EarleySymbol::NonTerminal(nt) => {
                                // Predictor: add items for all productions of this non-terminal
                                if let Some(nt_rule) = grammar.get_rule(&nt) {
                                    match &nt_rule.rhs {
                                        Expr::Choice(alts) => {
                                            for (alt_idx, _) in alts.iter().enumerate() {
                                                let new_item =
                                                    EarleyItem::new(nt.clone(), alt_idx, 0, i);
                                                if chart.add(i, new_item) {
                                                    changed = true;
                                                }
                                            }
                                        }
                                        _ => {
                                            let new_item = EarleyItem::new(nt.clone(), 0, 0, i);
                                            if chart.add(i, new_item) {
                                                changed = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        // Completer: item is complete, advance items that were waiting for this non-terminal
                        // Find all items at position i that are waiting for item.lhs
                        let completers: Vec<EarleyItem<T, N>> =
                            chart.get(i).iter().cloned().collect();
                        for completer in completers {
                            if let Some(completer_rule) = grammar.get_rule(&completer.lhs) {
                                let completer_rhs = match &completer_rule.rhs {
                                    Expr::Choice(alts) => {
                                        if completer.rule_index < alts.len() {
                                            &alts[completer.rule_index]
                                        } else {
                                            continue;
                                        }
                                    }
                                    _ => &completer_rule.rhs,
                                };

                                if let Some(next_sym) = completer.next_symbol(completer_rhs) {
                                    if let EarleySymbol::NonTerminal(nt) = next_sym {
                                        if nt == item.lhs {
                                            // This completer is waiting for item.lhs
                                            let new_item = EarleyItem::new(
                                                completer.lhs.clone(),
                                                completer.rule_index,
                                                completer.dot + 1,
                                                completer.start,
                                            );
                                            if chart.add(i, new_item) {
                                                changed = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Find completed items at the end
    let complete_items: Vec<EarleyItem<T, N>> = chart
        .get(input.len())
        .iter()
        .filter(|item| {
            item.lhs == *entry
                && item.start == 0
                && if let Some(rule) = grammar.get_rule(&item.lhs) {
                    let rhs = match &rule.rhs {
                        Expr::Choice(alts) => {
                            if item.rule_index < alts.len() {
                                &alts[item.rule_index]
                            } else {
                                return false;
                            }
                        }
                        _ => &rule.rhs,
                    };
                    item.dot >= rhs_len(rhs)
                } else {
                    false
                }
        })
        .cloned()
        .collect();

    if complete_items.is_empty() {
        errors.push(ParseError::UnexpectedEof {
            span: crate::syntax::TextRange::at(
                TextSize::from(input.len() as u32),
                TextSize::from(1),
            ),
            expected: vec!["complete parse".to_string()],
        });
    }

    // Build parse tree from chart with node reuse
    let root_kind = entry
        .to_syntax_kind()
        .or_else(|| entry.default_syntax_kind())
        .or_else(|| input.first().map(crate::grammar::Token::kind))
        .or_else(|| grammar.try_get_fallback_kind())
        .unwrap_or_else(|| {
            entry.default_syntax_kind().expect(
                "CRITICAL: Cannot create parse result - no syntax kind available. \
                 This is a configuration error. You must implement \
                 NonTerminal::default_syntax_kind() to return Some for error cases.",
            )
        });

    // Extract parse tree from chart with incremental session support
    let root = if let Some(complete_item) = complete_items.first() {
        extract_tree_with_session(
            grammar,
            &chart,
            complete_item,
            input,
            0,
            input.len(),
            session,
        )
        .unwrap_or_else(|| GreenNode::new(root_kind, vec![], TextSize::zero()))
    } else {
        GreenNode::new(root_kind, vec![], TextSize::zero())
    };

    let metrics = ParseMetrics {
        parse_time: start_time.elapsed(),
        tokens_consumed: input.len(),
        nodes_created: 1,
        errors_recovered: 0,
        cache_hits: 0,
    };

    ParseResult {
        root,
        errors,
        warnings,
        metrics,
        #[cfg(feature = "backend-glr")]
        forest: None,
        _phantom: std::marker::PhantomData,
    }
}

/// Calculate the length of an expression for completion checking
fn rhs_len<T, N>(expr: &Expr<T, N>) -> usize
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Seq(items) => items.len(),
        Expr::Choice(_) => 1, // Each alternative is treated as a single unit
        Expr::Opt(_) => 1,
        Expr::Repeat { .. } => 1,
        Expr::Separated { .. } => 1,
        Expr::Delimited { .. } => 1,
        Expr::Token(_) => 1,
        Expr::Rule(_) => 1,
        Expr::Any => 1,
        Expr::Eof => 1,
        Expr::Empty => 0,
        _ => 1, // Default for other expression types
    }
}

/// Extract parse tree from Earley chart with incremental session support
fn extract_tree_with_session<T, N>(
    grammar: &Grammar<T, N>,
    chart: &EarleyChart<T, N>,
    item: &EarleyItem<T, N>,
    input: &[T],
    start: usize,
    end: usize,
    session: &crate::incremental::IncrementalSession<'_, T::Kind>,
) -> Option<Arc<GreenNode<T::Kind>>>
where
    T: Token,
    N: NonTerminal,
{
    // Calculate text position for this range
    let mut text_pos = TextSize::zero();
    for i in 0..start.min(input.len()) {
        text_pos += input[i].text_len();
    }

    // Check if we can reuse a node from the session
    let expected_kind = item
        .lhs
        .to_syntax_kind()
        .or_else(|| item.lhs.default_syntax_kind());

    if let Some(cached_node) = session.find_cached_node(item.lhs.name(), text_pos, expected_kind) {
        // Verify the cached node covers the expected range
        let node_text_len = cached_node.text_len();
        let expected_text_len: TextSize = (start..end.min(input.len()))
            .map(|i| input[i].text_len())
            .fold(TextSize::zero(), |acc, len| acc + len);

        // If the cached node's text length matches, we can reuse it
        if node_text_len == expected_text_len {
            return Some(cached_node);
        }
    }

    // Fall back to regular tree extraction
    extract_tree(grammar, chart, item, input, start, end)
}

/// Extract parse tree from Earley chart
fn extract_tree<T, N>(
    grammar: &Grammar<T, N>,
    chart: &EarleyChart<T, N>,
    item: &EarleyItem<T, N>,
    input: &[T],
    start: usize,
    end: usize,
) -> Option<Arc<GreenNode<T::Kind>>>
where
    T: Token,
    N: NonTerminal,
{
    if let Some(rule) = grammar.get_rule(&item.lhs) {
        let rhs = match &rule.rhs {
            Expr::Choice(alts) => {
                if item.rule_index < alts.len() {
                    &alts[item.rule_index]
                } else {
                    return None;
                }
            }
            _ => &rule.rhs,
        };

        let kind = item
            .lhs
            .to_syntax_kind()
            .or_else(|| item.lhs.default_syntax_kind())
            .or_else(|| input.get(start).map(crate::grammar::Token::kind))?;

        let mut builder = GreenNodeBuilder::new();
        builder.start_node(kind);

        // Extract children based on expression type
        match rhs {
            Expr::Seq(items) => {
                let mut pos = start;
                for (idx, expr_item) in items.iter().enumerate() {
                    if idx < item.dot {
                        // This item has been consumed
                        match expr_item {
                            Expr::Token(t) => {
                                if pos < input.len() && input[pos].kind() == t.kind() {
                                    let token_text = input[pos].text();
                                    let token_kind = input[pos].kind();
                                    builder.token(token_kind, token_text).ok()?;
                                    pos += 1;
                                }
                            }
                            Expr::Rule(nt) => {
                                // Find completed item for this non-terminal
                                if let Some(child_item) = find_completed_item(chart, nt, pos, end) {
                                    if let Some(child_node) =
                                        extract_tree(grammar, chart, &child_item, input, pos, end)
                                    {
                                        builder.reuse_node(child_node).ok()?;
                                        pos = end; // Simplified - should track actual end
                                    }
                                }
                            }
                            _ => {
                                // Handle other expression types as needed
                            }
                        }
                    }
                }
            }
            Expr::Token(t) => {
                if start < input.len() && input[start].kind() == t.kind() {
                    let token_text = input[start].text();
                    let token_kind = input[start].kind();
                    builder.token(token_kind, token_text).ok()?;
                }
            }
            Expr::Rule(nt) => {
                if let Some(child_item) = find_completed_item(chart, nt, start, end) {
                    if let Some(child_node) =
                        extract_tree(grammar, chart, &child_item, input, start, end)
                    {
                        builder.reuse_node(child_node).ok()?;
                    }
                }
            }
            _ => {
                // For other expression types, create a simple node
            }
        }

        builder.finish_node().ok()?;
        Some(builder.finish().ok()?.into())
    } else {
        None
    }
}

/// Find a completed item for a non-terminal in the chart
fn find_completed_item<T, N>(
    chart: &EarleyChart<T, N>,
    nt: &N,
    start: usize,
    end: usize,
) -> Option<EarleyItem<T, N>>
where
    T: Token,
    N: NonTerminal,
{
    // Look for completed items at position end that started at start
    chart
        .get(end)
        .iter()
        .find(|item| item.lhs == *nt && item.start == start)
        .cloned()
}
