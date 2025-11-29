//! Grammar documentation and EBNF export functionality.
//!
//! This module provides utilities for generating markdown documentation
//! with EBNF representation of grammars.

use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use std::fmt::Write;

/// Format an expression as EBNF.
///
/// This function is intentionally recursive to handle nested expressions.
/// It is public and used externally (in tests and by users), not just recursively.
#[allow(clippy::too_many_lines, clippy::only_used_in_recursion)] // Public API, used in tests
pub fn format_expr_ebnf<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
) -> String
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(t) => {
            // Always use the token name function for EBNF (descriptions are shown separately)
            let name = token_name(t);
            // Use single quotes for terminals in EBNF
            format!("'{name}'")
        }
        Expr::Rule(n) => n.name().to_string(),
        Expr::Any => "'ANY'".to_string(),
        Expr::Eof => "'EOF'".to_string(),
        Expr::Empty => "ε".to_string(),
        Expr::Seq(exprs) => {
            let parts: Vec<String> = exprs
                .iter()
                .map(|e| {
                    let formatted = format_expr_ebnf(e, grammar, token_name);
                    // Wrap in parentheses if it's a choice or complex expression
                    if needs_parentheses(e) {
                        format!("({formatted})")
                    } else {
                        formatted
                    }
                })
                .collect();
            parts.join(" ")
        }
        Expr::Choice(exprs) => {
            let parts: Vec<String> = exprs
                .iter()
                .map(|e| format_expr_ebnf(e, grammar, token_name))
                .collect();
            parts.join(" | ")
        }
        Expr::Opt(e) => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            if needs_parentheses(e.as_ref()) {
                format!("({inner})?")
            } else {
                format!("{inner}?")
            }
        }
        Expr::Repeat {
            expr: e,
            min,
            max,
            greedy: _,
        } => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            let wrapped = if needs_parentheses(e.as_ref()) {
                format!("({inner})")
            } else {
                inner
            };
            match (*min, *max) {
                (0, None) => format!("{wrapped}*"),
                (1, None) => format!("{wrapped}+"),
                (0, Some(1)) => format!("{wrapped}?"),
                (min, Some(max)) if min == max => format!("{wrapped}{{{min}}}"),
                (min, Some(max)) => format!("{wrapped}{{{min},{max}}}"),
                (min, None) => format!("{wrapped}{{{min},}}"),
            }
        }
        Expr::Separated {
            item,
            separator,
            min,
            trailing: _, // Trailing separator behavior shown in expression details
        } => {
            let item_str = format_expr_ebnf(item, grammar, token_name);
            let sep_str = format_expr_ebnf(separator, grammar, token_name);
            let item_wrapped = if needs_parentheses(item.as_ref()) {
                format!("({item_str})")
            } else {
                item_str
            };
            let sep_wrapped = if needs_parentheses(separator.as_ref()) {
                format!("({sep_str})")
            } else {
                sep_str
            };
            match *min {
                0 | 1 => format!("{item_wrapped} ({sep_wrapped} {item_wrapped})*"),
                n => format!("{item_wrapped} ({sep_wrapped} {item_wrapped}){{{n},}}"),
            }
        }
        Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            let open_str = format_expr_ebnf(open, grammar, token_name);
            let content_str = format_expr_ebnf(content, grammar, token_name);
            let close_str = format_expr_ebnf(close, grammar, token_name);
            // For delimited expressions, format as: open content close
            format!("{open_str} {content_str} {close_str}")
        }
        Expr::Lookahead(e) => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            let wrapped = if needs_parentheses(e.as_ref()) {
                format!("({inner})")
            } else {
                inner
            };
            format!("&{wrapped}")
        }
        Expr::NotLookahead(e) => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            let wrapped = if needs_parentheses(e.as_ref()) {
                format!("({inner})")
            } else {
                inner
            };
            format!("!{wrapped}")
        }
        // These are documentation-only and don't affect the grammar structure
        Expr::Label { expr, .. }
        | Expr::Node { expr, .. }
        | Expr::Flatten(expr)
        | Expr::Prune(expr) => format_expr_ebnf(expr, grammar, token_name),
        Expr::RecoveryPoint { expr, .. } => {
            // Recovery info is not shown in EBNF
            format_expr_ebnf(expr, grammar, token_name)
        }
        Expr::Cut(e) => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            let wrapped = if needs_parentheses(e.as_ref()) {
                format!("({inner})")
            } else {
                inner
            };
            format!("{wrapped}~") // ~ is common notation for cut operator
        }
        Expr::TokenClass { class } => {
            format!("<{}>", class.as_str())
        }
        Expr::Conditional {
            condition,
            then_expr,
            else_expr,
        } => {
            let cond_str = format_expr_ebnf(condition, grammar, token_name);
            let then_str = format_expr_ebnf(then_expr, grammar, token_name);
            else_expr.as_ref().map_or_else(
                || format!("({cond_str}) ? {then_str}"),
                |else_expr| {
                    let else_str = format_expr_ebnf(else_expr, grammar, token_name);
                    format!("({cond_str}) ? {then_str} : {else_str}")
                },
            )
        }
        Expr::SemanticPredicate { expr, .. } => {
            let inner = format_expr_ebnf(expr, grammar, token_name);
            let wrapped = if needs_parentheses(expr.as_ref()) {
                format!("({inner})")
            } else {
                inner
            };
            format!("{wrapped}?") // ? for semantic predicate
        }
        Expr::Backreference { capture_id } => {
            format!("\\{}", capture_id.as_str())
        }
    }
}

/// Check if an expression needs parentheses when used in a sequence or choice
const fn needs_parentheses<T, N>(expr: &Expr<T, N>) -> bool {
    matches!(
        expr,
        Expr::Choice(_) | Expr::Seq(_) | Expr::Separated { .. } | Expr::Delimited { .. }
    )
}

/// Configuration for markdown generation
///
/// This struct allows you to customize what information is included in the generated
/// grammar documentation. All options default to `true` for comprehensive documentation.
///
/// # Example
///
/// ```rust,no_run
/// # use sipha::grammar::docs::MarkdownConfig;
/// # use std::collections::HashMap;
/// let mut config = MarkdownConfig::default();
/// config.sections.include_token_usage = false;  // Skip token usage for simpler docs
/// config.overview = Some("This grammar describes a JSON document structure.".to_string());
/// ```
///
/// Note: This struct intentionally uses multiple boolean fields for configuration options.
/// This is a common and idiomatic pattern for configuration structs where each option
/// is independent and can be toggled separately.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarkdownConfig {
    /// Options for which sections to include in the documentation
    pub sections: SectionOptions,
    /// Use line breaks for long EBNF expressions to improve readability
    pub format_long_expressions: bool,
    /// Maximum line length before breaking (if `format_long_expressions` is true)
    ///
    /// Defaults to 80 characters. Longer expressions will be split across multiple lines.
    pub max_line_length: usize,
    /// Show nullable rules (rules that can match empty input)
    pub show_nullable: bool,
    /// Show expression details (trailing separators, recovery points, lookahead)
    pub show_expression_details: bool,
    /// Optional overview text describing the grammar
    pub overview: Option<String>,
    /// Optional examples for rules, mapping rule names to example strings
    pub rule_examples: std::collections::HashMap<String, Vec<String>>,
}

/// Options for which sections to include in the markdown documentation
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::struct_excessive_bools)] // Multiple boolean flags for clarity in API
pub struct SectionOptions {
    /// Include a table of contents with links to all sections
    pub include_toc: bool,
    /// Include grammar statistics (rule count, token count, recursive rules)
    pub include_statistics: bool,
    /// Include rule dependencies (which rules reference which other rules)
    pub include_dependencies: bool,
    /// Include token usage information (which tokens are used by each rule)
    pub include_token_usage: bool,
    /// Include a mermaid dependency graph showing rule relationships
    pub include_dependency_graph: bool,
}

impl Default for SectionOptions {
    fn default() -> Self {
        Self {
            include_toc: true,
            include_statistics: true,
            include_dependencies: true,
            include_token_usage: true,
            include_dependency_graph: true,
        }
    }
}

impl Default for MarkdownConfig {
    fn default() -> Self {
        Self {
            sections: SectionOptions::default(),
            format_long_expressions: true,
            max_line_length: 80,
            show_nullable: true,
            show_expression_details: true,
            overview: None,
            rule_examples: std::collections::HashMap::new(),
        }
    }
}

/// Generate markdown documentation for a grammar
pub fn generate_markdown<T, N>(
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
) -> String
where
    T: Token,
    N: NonTerminal + Clone,
{
    generate_markdown_with_config(grammar, token_name, &MarkdownConfig::default())
}

/// Generate markdown documentation for a grammar with custom configuration
#[allow(clippy::too_many_lines)]
pub fn generate_markdown_with_config<T, N>(
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
    config: &MarkdownConfig,
) -> String
where
    T: Token,
    N: NonTerminal + Clone,
{
    let mut output = String::new();

    // Title
    output.push_str("# Grammar Documentation\n\n");

    // Collect and sort rules for consistent output
    let mut rules: Vec<_> = grammar.rules().collect();
    rules.sort_by(|a, b| a.0.name().cmp(b.0.name()));

    // Generate table of contents if requested
    if config.sections.include_toc {
        output.push_str("## Table of Contents\n\n");
        if config.overview.is_some() {
            output.push_str("- [Overview](#overview)\n");
        }
        output.push_str("- [Entry Point](#entry-point)\n");
        if config.sections.include_statistics {
            output.push_str("- [Statistics](#statistics)\n");
        }
        output.push_str("- [Production Rules](#production-rules)\n");
        if config.sections.include_dependencies {
            output.push_str("- [Rule Dependencies](#rule-dependencies)\n");
        }
        if config.sections.include_dependency_graph {
            output.push_str("- [Rule Dependency Graph](#rule-dependency-graph)\n");
        }
        output.push_str("- [Tokens](#tokens)\n");
        if config.sections.include_token_usage {
            output.push_str("- [Token Usage](#token-usage)\n");
        }
        output.push_str("- [Grammar Design Notes](#grammar-design-notes)\n");
        output.push('\n');
    }

    // Overview section
    if let Some(overview) = &config.overview {
        output.push_str("## Overview\n\n");
        writeln!(output, "{overview}").unwrap();
        output.push('\n');
        output.push_str("---\n\n");
    }

    // Entry point
    output.push_str("## Entry Point\n\n");
    let entry_name = grammar.entry_point().name();
    writeln!(
        output,
        "The grammar entry point is [`{entry_name}`](#{}).",
        entry_name.to_lowercase()
    )
    .unwrap();
    output.push('\n');

    // Statistics section
    if config.sections.include_statistics {
        output.push_str("## Statistics\n\n");
        let rule_count = rules.len();
        let token_count = count_unique_tokens(grammar, token_name);
        let direct_recursive = find_recursive_rules(grammar);
        let indirect_recursive = find_indirectly_recursive_rules(grammar);

        // Count nullable rules
        let nullable_count = rules
            .iter()
            .filter(|(_, rule)| rule.rhs.is_nullable(grammar))
            .count();

        output.push_str("| Metric | Value | Notes |\n");
        output.push_str("|--------|-------|-------|\n");
        let rule_names = rules
            .iter()
            .map(|(n, _)| n.name())
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(output, "| **Total Rules** | {rule_count} | {rule_names} |").unwrap();
        writeln!(
            output,
            "| **Unique Tokens** | {token_count} | See [Token Reference](#tokens) |"
        )
        .unwrap();

        let recursive_note = if !indirect_recursive.is_empty() || !direct_recursive.is_empty() {
            let direct_count = direct_recursive.len();
            let indirect_count = indirect_recursive.len();
            if direct_count == 0 && indirect_count > 0 {
                format!("0 direct, {indirect_count} indirect")
            } else if direct_count > 0 && indirect_count == 0 {
                "Direct recursion only".to_string()
            } else {
                format!("{direct_count} direct, {indirect_count} indirect")
            }
        } else {
            "None".to_string()
        };
        let recursive_count = indirect_recursive.len().max(direct_recursive.len());
        writeln!(
            output,
            "| **Recursive Rules** | {recursive_count} | {recursive_note} |"
        )
        .unwrap();

        if config.show_nullable {
            let nullable_note = if nullable_count > 0 {
                let nullable_names: Vec<String> = rules
                    .iter()
                    .filter(|(_, rule)| rule.rhs.is_nullable(grammar))
                    .map(|(n, _)| format!("`{}`", n.name()))
                    .collect();
                nullable_names.join(", ")
            } else {
                "None".to_string()
            };
            writeln!(
                output,
                "| **Nullable Rules** | {nullable_count} | {nullable_note} |"
            )
            .unwrap();
        }

        if !indirect_recursive.is_empty() {
            let recursive_names: Vec<String> = indirect_recursive
                .iter()
                .map(|n| format!("`{}`", n.name()))
                .collect();
            let recursive_count = indirect_recursive.len();
            let recursive_names_str = recursive_names.join(", ");
            writeln!(
                output,
                "| **Recursive Rule Names** | {recursive_count} | {recursive_names_str} |"
            )
            .unwrap();
        }
        output.push('\n');
    }

    // Rules section
    output.push_str("## Production Rules\n\n");

    for (lhs, rule) in &rules {
        let rule_id = lhs.name().to_lowercase();
        writeln!(output, "### <a id=\"{rule_id}\"></a>{}", lhs.name()).unwrap();
        output.push('\n');

        // Add description if available
        #[cfg(feature = "grammar-docs")]
        if let Some(desc) = rule.metadata.description() {
            writeln!(output, "{desc}").unwrap();
            output.push('\n');
        }

        // Show if rule is recursive (simplified check - just direct recursion)
        if config.sections.include_statistics {
            let referenced = collect_referenced_rules(&rule.rhs);
            if referenced.iter().any(|r| r == *lhs) {
                output.push_str("> **Note**: This rule is recursive.\n\n");
            }
        }

        // Show if rule is nullable (can match empty input)
        if config.show_nullable && rule.rhs.is_nullable(grammar) {
            output.push_str("> **Note**: This rule is nullable (can match empty input).\n\n");
        }

        // Format the RHS as EBNF with improved formatting
        let ebnf = if config.format_long_expressions {
            format_expr_ebnf_formatted(&rule.rhs, grammar, token_name, config.max_line_length, 0)
        } else {
            format_expr_ebnf(&rule.rhs, grammar, token_name)
        };
        output.push_str("```ebnf\n");
        writeln!(output, "{} ::= {ebnf}", lhs.name()).unwrap();
        output.push_str("```\n\n");

        // Show which rules this rule references
        if config.sections.include_dependencies {
            let referenced_rules = collect_referenced_rules(&rule.rhs);
            if !referenced_rules.is_empty() {
                output.push_str("**References**: ");
                let ref_links: Vec<String> = referenced_rules
                    .iter()
                    .map(|n| format!("[`{}`](#{})", n.name(), n.name().to_lowercase()))
                    .collect();
                output.push_str(&ref_links.join(", "));
                output.push_str("\n\n");
            }
        }

        // Show expression details (trailing separators, recovery, lookahead)
        if config.show_expression_details {
            let details = collect_expression_details(&rule.rhs, grammar, token_name);
            if !details.is_empty() {
                output.push_str("**Expression Details**:\n");
                for detail in details {
                    writeln!(output, "- {detail}").unwrap();
                }
                output.push('\n');
            }
        }

        // Show examples if provided
        if let Some(examples) = config.rule_examples.get(lhs.name())
            && !examples.is_empty()
        {
            output.push_str("**Examples**:\n");
            for example in examples {
                writeln!(output, "- {example}").unwrap();
            }
            output.push('\n');
        }
    }

    // Rule dependencies section
    if config.sections.include_dependencies {
        output.push_str("## Rule Dependencies\n\n");
        output.push_str("This section shows which rules reference which other rules.\n\n");

        for (lhs, rule) in &rules {
            let referenced = collect_referenced_rules(&rule.rhs);
            if !referenced.is_empty() {
                write!(output, "- **`{}`** references: ", lhs.name()).unwrap();
                let ref_links: Vec<String> = referenced
                    .iter()
                    .map(|n| format!("[`{}`](#{})", n.name(), n.name().to_lowercase()))
                    .collect();
                output.push_str(&ref_links.join(", "));
                output.push('\n');
            }
        }
        output.push('\n');
    }

    // Rule dependency graph (mermaid)
    if config.sections.include_dependency_graph {
        output.push_str("## Rule Dependency Graph\n\n");
        output.push_str(&generate_mermaid_graph(grammar));
        output.push_str("\n**Legend**: Arrows indicate \"references\" relationship. The graph shows the indirect recursion between rules.\n\n");
        output.push_str("---\n\n");
    }

    // Grammar design notes section
    let features = detect_grammar_features(grammar, token_name);
    let design_notes = generate_design_notes(grammar, &features, token_name);
    if !design_notes.is_empty() {
        output.push_str("## Grammar Design Notes\n\n");
        output.push_str(&design_notes);
        output.push_str("---\n\n");
    }

    // Tokens section - show all tokens used in the grammar
    #[cfg(feature = "grammar-docs")]
    {
        // Collect all unique tokens by their name (since tokens with same kind but different text are the same token type)
        let mut token_map: hashbrown::HashMap<String, (T, Option<String>), ahash::RandomState> =
            hashbrown::HashMap::with_hasher(ahash::RandomState::new());

        // Build a map of token names to rules that use them
        let mut token_to_rules: hashbrown::HashMap<
            String,
            hashbrown::HashSet<String, ahash::RandomState>,
            ahash::RandomState,
        > = hashbrown::HashMap::with_hasher(ahash::RandomState::new());

        for (lhs, rule) in grammar.rules() {
            let used_tokens = collect_tokens(&rule.rhs);
            for token in used_tokens {
                let name = token_name(&token);
                token_to_rules
                    .entry(name.clone())
                    .or_insert_with(|| hashbrown::HashSet::with_hasher(ahash::RandomState::new()))
                    .insert(lhs.name().to_string());

                let desc = grammar.token_description(&token);

                // Only keep the first occurrence of each token type, or prefer one with a description
                token_map
                    .entry(name.clone())
                    .or_insert_with(|| (token.clone(), desc.map(std::string::ToString::to_string)));

                // If we find a token with a description, prefer it
                if let Some(desc_str) = desc
                    && let Some((_, existing_desc)) = token_map.get_mut(&name)
                    && existing_desc.is_none()
                {
                    *existing_desc = Some(desc_str.to_string());
                }
            }
        }

        if !token_map.is_empty() {
            let mut tokens: Vec<_> = token_map.into_iter().collect();
            tokens.sort_by(|a, b| a.0.cmp(&b.0));

            output.push_str("## Tokens\n\n");
            output.push_str("> **Note**: Tokens in the grammar are matched by their *kind* (type), not by their literal text. ");
            output.push_str("In practice, tokens with the same kind but different text (e.g., different string values, different numbers) will match the same grammar rules.\n\n");
            output.push_str("| Token | Literal | Description | Usage Context |\n");
            output.push_str("|-------|---------|-------------|---------------|\n");

            for (token_name_str, (token, desc)) in &tokens {
                let desc_str = desc.as_ref().map_or("—", std::string::String::as_str);

                // Get literal text from token
                let literal_text = token.text();
                // Show literal text, but truncate very long strings and format nicely
                let literal_display = if literal_text.len() > 30 {
                    format!("`{}...`", &literal_text[..30])
                } else if literal_text.is_empty() {
                    "—".to_string()
                } else {
                    // Escape backticks in the literal for markdown
                    let escaped = literal_text.replace('`', "\\`");
                    format!("`{escaped}`")
                };

                // Get usage context (which rules use this token)
                let usage_context = token_to_rules
                    .get(token_name_str)
                    .map(|rules| {
                        let mut rule_list: Vec<String> = rules.iter().cloned().collect();
                        rule_list.sort();
                        rule_list
                            .iter()
                            .map(|r| format!("`{r}`"))
                            .collect::<Vec<_>>()
                            .join(", ")
                    })
                    .map_or_else(|| String::from("—"), |s| s);

                // Escape pipe characters in markdown table
                let desc_escaped = desc_str.replace('|', "\\|");

                writeln!(
                    output,
                    "| `{token_name_str}` | {literal_display} | {desc_escaped} | {usage_context} |"
                )
                .unwrap();
            }
            output.push('\n');

            // Token usage section
            if config.sections.include_token_usage {
                output.push_str("## Token Usage\n\n");

                // By Rule
                output.push_str("### By Rule:\n\n");
                output.push_str("| Rule | Tokens Used | Required? |\n");
                output.push_str("|------|-------------|-----------|\n");

                for (lhs, rule) in &rules {
                    let used_tokens = collect_tokens(&rule.rhs);
                    if used_tokens.is_empty() {
                        writeln!(output, "| **`{}`** | *(none directly)* | N/A |", lhs.name())
                            .unwrap();
                    } else {
                        let mut token_names: hashbrown::HashSet<String, ahash::RandomState> =
                            hashbrown::HashSet::with_hasher(ahash::RandomState::new());
                        for token in &used_tokens {
                            token_names.insert(token_name(token));
                        }
                        let mut sorted_names: Vec<String> = token_names.into_iter().collect();
                        sorted_names.sort();

                        // Determine if tokens are required (simplified: if rule is not nullable, tokens are required)
                        let required = if rule.rhs.is_nullable(grammar) {
                            "Optional"
                        } else {
                            "Required"
                        };

                        let sorted_names_str = sorted_names
                            .iter()
                            .map(|n| format!("`{n}`"))
                            .collect::<Vec<_>>()
                            .join(", ");
                        writeln!(
                            output,
                            "| **`{}`** | {sorted_names_str} | {required} |",
                            lhs.name()
                        )
                        .unwrap();
                    }
                }
                output.push('\n');

                // By Token
                output.push_str("### By Token:\n\n");
                output.push_str("| Token | Used In Rules | Purpose |\n");
                output.push_str("|-------|---------------|---------|\n");

                for (token_name_str, (_, desc)) in &tokens {
                    let usage_rules = token_to_rules
                        .get(token_name_str)
                        .map(|rules| {
                            let mut rule_list: Vec<String> = rules.iter().cloned().collect();
                            rule_list.sort();
                            rule_list
                                .iter()
                                .map(|r| format!("`{r}`"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        })
                        .map_or_else(|| String::from("—"), |s| s);

                    let purpose = desc.as_ref().map_or("—", std::string::String::as_str);
                    let purpose_escaped = purpose.replace('|', "\\|");

                    writeln!(
                        output,
                        "| `{token_name_str}` | {usage_rules} | {purpose_escaped} |"
                    )
                    .unwrap();
                }
                output.push('\n');
            }
        }
    }

    output
}

/// Collect all tokens from an expression
fn collect_tokens<T, N>(expr: &Expr<T, N>) -> Vec<T>
where
    T: Token,
    N: NonTerminal,
{
    let mut tokens = Vec::new();
    collect_tokens_impl(expr, &mut tokens);
    tokens
}

fn collect_tokens_impl<T, N>(expr: &Expr<T, N>, result: &mut Vec<T>)
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(t) => {
            result.push(t.clone());
        }
        Expr::Seq(exprs) | Expr::Choice(exprs) => {
            for e in exprs {
                collect_tokens_impl(e, result);
            }
        }
        Expr::Opt(e)
        | Expr::Repeat { expr: e, .. }
        | Expr::Label { expr: e, .. }
        | Expr::Node { expr: e, .. }
        | Expr::Flatten(e)
        | Expr::Prune(e)
        | Expr::Lookahead(e)
        | Expr::NotLookahead(e)
        | Expr::RecoveryPoint { expr: e, .. } => {
            collect_tokens_impl(e, result);
        }
        Expr::Separated {
            item, separator, ..
        } => {
            collect_tokens_impl(item, result);
            collect_tokens_impl(separator, result);
        }
        Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            collect_tokens_impl(open, result);
            collect_tokens_impl(content, result);
            collect_tokens_impl(close, result);
        }
        _ => {}
    }
}

/// Format an expression as EBNF with line breaks for long expressions
fn format_expr_ebnf_formatted<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
    max_line_length: usize,
    indent: usize,
) -> String
where
    T: Token,
    N: NonTerminal,
{
    let base = format_expr_ebnf(expr, grammar, token_name);

    // If the expression is short enough, return as-is
    if base.len() <= max_line_length {
        return base;
    }

    // For long expressions, try to break them up
    match expr {
        Expr::Seq(exprs) => {
            let parts: Vec<String> = exprs
                .iter()
                .map(|e| {
                    format_expr_ebnf_formatted(e, grammar, token_name, max_line_length, indent + 2)
                })
                .collect();
            let indent_str = " ".repeat(indent);
            if parts.iter().any(|p| p.contains('\n')) {
                // Multi-line format
                format!("{}\n{}", parts.join(&format!("\n{indent_str}")), indent_str)
            } else {
                // Try to fit on one line, or break if too long
                let joined = parts.join(" ");
                if joined.len() <= max_line_length {
                    joined
                } else {
                    format!("{}\n{}", parts.join(&format!("\n{indent_str}")), indent_str)
                }
            }
        }
        Expr::Choice(exprs) => {
            let parts: Vec<String> = exprs
                .iter()
                .map(|e| {
                    format_expr_ebnf_formatted(e, grammar, token_name, max_line_length, indent + 2)
                })
                .collect();
            let indent_str = " ".repeat(indent);
            if parts.iter().any(|p| p.contains('\n')) {
                format!(
                    "{}\n{}",
                    parts.join(&format!("\n{indent_str}| ")),
                    indent_str
                )
            } else {
                let joined = parts.join(" | ");
                if joined.len() <= max_line_length {
                    joined
                } else {
                    format!(
                        "{}\n{}",
                        parts.join(&format!("\n{indent_str}| ")),
                        indent_str
                    )
                }
            }
        }
        _ => base,
    }
}

/// Count unique tokens in the grammar
fn count_unique_tokens<T, N>(grammar: &Grammar<T, N>, token_name: &impl Fn(&T) -> String) -> usize
where
    T: Token,
    N: NonTerminal,
{
    let mut token_names: hashbrown::HashSet<String, ahash::RandomState> =
        hashbrown::HashSet::with_hasher(ahash::RandomState::new());

    for token in grammar
        .rules()
        .flat_map(|(_, rule)| collect_tokens(&rule.rhs))
    {
        token_names.insert(token_name(&token));
    }

    token_names.len()
}

/// Find all recursive rules in the grammar (direct recursion only)
fn find_recursive_rules<T, N>(grammar: &Grammar<T, N>) -> Vec<N>
where
    T: Token,
    N: NonTerminal + Clone,
{
    let mut recursive = Vec::new();

    for (lhs, rule) in grammar.rules() {
        let referenced = collect_referenced_rules(&rule.rhs);
        if referenced.contains(lhs) {
            recursive.push(lhs.clone());
        }
    }

    recursive
}

/// Find all indirectly recursive rules (rules that can eventually reference themselves)
fn find_indirectly_recursive_rules<T, N>(grammar: &Grammar<T, N>) -> Vec<N>
where
    T: Token,
    N: NonTerminal + Clone,
{
    let mut indirectly_recursive = Vec::new();

    // Build a dependency graph
    let mut dependencies: hashbrown::HashMap<
        N,
        hashbrown::HashSet<N, ahash::RandomState>,
        ahash::RandomState,
    > = hashbrown::HashMap::with_hasher(ahash::RandomState::new());

    for (lhs, rule) in grammar.rules() {
        let referenced = collect_referenced_rules(&rule.rhs);
        dependencies.insert(lhs.clone(), referenced.into_iter().collect());
    }

    // For each rule, check if it can reach itself through the dependency graph
    for (start, _) in grammar.rules() {
        if can_reach(
            &dependencies,
            start,
            start,
            &mut hashbrown::HashSet::with_hasher(ahash::RandomState::new()),
        ) {
            indirectly_recursive.push(start.clone());
        }
    }

    indirectly_recursive
}

/// Check if a rule can reach another rule through the dependency graph
fn can_reach<N>(
    dependencies: &hashbrown::HashMap<
        N,
        hashbrown::HashSet<N, ahash::RandomState>,
        ahash::RandomState,
    >,
    from: &N,
    to: &N,
    visited: &mut hashbrown::HashSet<N, ahash::RandomState>,
) -> bool
where
    N: NonTerminal + Clone,
{
    if from == to && !visited.is_empty() {
        return true; // Found a cycle back to 'to'
    }

    if !visited.insert(from.clone()) {
        return false; // Already visited, avoid infinite loops
    }

    if let Some(refs) = dependencies.get(from) {
        for ref_rule in refs {
            if can_reach(dependencies, ref_rule, to, visited) {
                return true;
            }
        }
    }

    false
}

/// Generate a mermaid dependency graph showing rule relationships
fn generate_mermaid_graph<T, N>(grammar: &Grammar<T, N>) -> String
where
    T: Token,
    N: NonTerminal + Clone,
{
    let mut output = String::from("```mermaid\ngraph TD\n");

    // Collect all rules and their dependencies
    let mut rules: Vec<_> = grammar.rules().collect();
    rules.sort_by(|a, b| a.0.name().cmp(b.0.name()));

    for (lhs, rule) in &rules {
        let referenced = collect_referenced_rules(&rule.rhs);
        for ref_rule in referenced {
            // Escape rule names for mermaid (replace special chars)
            let lhs_name = lhs.name().replace(['-', ' '], "_");
            let ref_name = ref_rule.name().replace(['-', ' '], "_");
            writeln!(output, "    {lhs_name} --> {ref_name}").unwrap();
        }
    }

    output.push_str("```\n");
    output
}

/// Grammar features detected in the grammar
#[derive(Debug, Clone)]
struct GrammarFeatures {
    /// Rules with error recovery enabled (Delimited with recover: true)
    error_recovery_rules: Vec<String>,
    /// Rules that are nullable (can match empty input)
    nullable_rules: Vec<String>,
    /// Rules with direct recursion
    #[allow(dead_code)]
    direct_recursive_rules: Vec<String>,
    /// Rules with indirect recursion
    indirect_recursive_rules: Vec<String>,
    /// Whether the grammar uses token matching by kind
    #[allow(dead_code)]
    token_matching_by_kind: bool,
}

/// Detect grammar features (error recovery, nullable rules, recursion patterns)
fn detect_grammar_features<T, N>(
    grammar: &Grammar<T, N>,
    _token_name: &impl Fn(&T) -> String,
) -> GrammarFeatures
where
    T: Token,
    N: NonTerminal + Clone,
{
    let mut error_recovery_rules = Vec::new();
    let mut nullable_rules = Vec::new();

    // Check each rule for features
    for (lhs, rule) in grammar.rules() {
        // Check for error recovery (Delimited with recover: true)
        if has_error_recovery(&rule.rhs) {
            error_recovery_rules.push(lhs.name().to_string());
        }

        // Check for nullable rules
        if rule.rhs.is_nullable(grammar) {
            nullable_rules.push(lhs.name().to_string());
        }
    }

    // Find recursive rules
    let direct_recursive = find_recursive_rules(grammar);
    let indirect_recursive = find_indirectly_recursive_rules(grammar);

    GrammarFeatures {
        error_recovery_rules,
        nullable_rules,
        direct_recursive_rules: direct_recursive
            .iter()
            .map(|n| n.name().to_string())
            .collect(),
        indirect_recursive_rules: indirect_recursive
            .iter()
            .map(|n| n.name().to_string())
            .collect(),
        token_matching_by_kind: true, // Always true in this grammar system
    }
}

/// Check if an expression has error recovery enabled
fn has_error_recovery<T, N>(expr: &Expr<T, N>) -> bool {
    match expr {
        Expr::Delimited { recover, .. } => *recover,
        Expr::Seq(exprs) | Expr::Choice(exprs) => exprs.iter().any(has_error_recovery),
        Expr::Opt(e)
        | Expr::Repeat { expr: e, .. }
        | Expr::Label { expr: e, .. }
        | Expr::Node { expr: e, .. }
        | Expr::Flatten(e)
        | Expr::Prune(e)
        | Expr::Lookahead(e)
        | Expr::NotLookahead(e)
        | Expr::RecoveryPoint { expr: e, .. } => has_error_recovery(e),
        Expr::Separated {
            item, separator, ..
        } => has_error_recovery(item) || has_error_recovery(separator),
        _ => false,
    }
}

/// Generate grammar design notes section based on detected features
fn generate_design_notes<T, N>(
    grammar: &Grammar<T, N>,
    features: &GrammarFeatures,
    _token_name: &impl Fn(&T) -> String,
) -> String
where
    T: Token,
    N: NonTerminal + Clone,
{
    let mut output = String::new();
    let mut has_notes = false;

    // Error Recovery section
    if !features.error_recovery_rules.is_empty() {
        has_notes = true;
        output.push_str("### Error Recovery\n");
        let rule_list: Vec<String> = features
            .error_recovery_rules
            .iter()
            .map(|r| format!("`{r}`"))
            .collect();
        let rule_list_str = rule_list.join(", ");
        writeln!(
            output,
            "The following rules have **error recovery enabled** for their delimited expressions: {rule_list_str}."
        )
        .unwrap();
        output.push_str("This means:\n");
        output.push_str("- If a syntax error occurs inside a delimited expression, the parser can skip to the closing delimiter\n");
        output.push_str(
            "- This allows parsing to continue and report multiple errors in a single pass\n\n",
        );
    }

    // Nullable Containers section
    if !features.nullable_rules.is_empty() {
        has_notes = true;
        output.push_str("### Nullable Containers\n");

        // Add examples for common nullable patterns
        let rule_list: Vec<String> = features
            .nullable_rules
            .iter()
            .map(|r| format!("`{r}`"))
            .collect();
        let rule_list_str = rule_list.join(", ");
        writeln!(
            output,
            "The following rules are **nullable** (can match empty input): {rule_list_str}."
        )
        .unwrap();

        output.push_str("This is typically achieved by making the content optional with the `?` quantifier or using `Empty`.\n\n");
    }

    // Recursion section
    if !features.indirect_recursive_rules.is_empty() {
        has_notes = true;
        output.push_str("### Indirect Recursion\n");
        output.push_str("The grammar supports nested structures through indirect recursion:\n");

        // Try to find example recursion paths
        let mut paths_found = Vec::new();
        for rule_name in &features.indirect_recursive_rules {
            if let Some(path) = find_recursion_path(grammar, rule_name) {
                paths_found.push((rule_name.clone(), path));
            }
        }

        if paths_found.is_empty() {
            let rule_list_str = features
                .indirect_recursive_rules
                .iter()
                .map(|r| format!("`{r}`"))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(output, "- Rules with indirect recursion: {rule_list_str}").unwrap();
        } else {
            // Sort paths by length (shorter paths first) and take up to 3
            paths_found.sort_by(|a, b| {
                // Sort by path length (number of arrows), then alphabetically
                let a_len = a.1.matches("→").count();
                let b_len = b.1.matches("→").count();
                a_len.cmp(&b_len).then_with(|| a.0.cmp(&b.0))
            });
            for (rule_name, path) in paths_found.iter().take(3) {
                writeln!(output, "- `{rule_name}` → {path}").unwrap();
            }
        }
        output.push_str("This allows arbitrary nesting depth in the grammar.\n\n");
    }

    if has_notes { output } else { String::new() }
}

/// Find a recursion path for a rule (simplified - just find one path)
fn find_recursion_path<T, N>(grammar: &Grammar<T, N>, rule_name: &str) -> Option<String>
where
    T: Token,
    N: NonTerminal + Clone,
{
    // Find the rule
    let (start_rule, _) = grammar.rules().find(|(n, _)| n.name() == rule_name)?;

    // Build dependency graph
    let mut dependencies: hashbrown::HashMap<N, Vec<N>, ahash::RandomState> =
        hashbrown::HashMap::with_hasher(ahash::RandomState::new());

    for (lhs, rule) in grammar.rules() {
        let referenced = collect_referenced_rules(&rule.rhs);
        dependencies.insert(lhs.clone(), referenced);
    }

    // Try to find a path back to start_rule
    // Start from one of the rules that start_rule references
    let start_rule_clone = start_rule.clone();
    if let Some(refs) = dependencies.get(&start_rule_clone) {
        for first_ref in refs {
            if let Some(mut path) =
                find_path_to_rule(&dependencies, first_ref, &start_rule_clone, &mut Vec::new())
            {
                // Prepend the start rule to show the full cycle
                path.insert(0, start_rule_clone.clone());
                return Some(
                    path.iter()
                        .map(NonTerminal::name)
                        .collect::<Vec<_>>()
                        .join(" → "),
                );
            }
        }
    }

    None
}

/// Find a path from a rule back to itself
fn find_path_to_rule<N>(
    dependencies: &hashbrown::HashMap<N, Vec<N>, ahash::RandomState>,
    current: &N,
    target: &N,
    path: &mut Vec<N>,
) -> Option<Vec<N>>
where
    N: NonTerminal + Clone,
{
    // Check for cycle in path before processing - this prevents infinite loops
    if path.contains(current) {
        return None; // Cycle detected, but not back to target
    }

    // If we've reached the target and we have a path, we found a cycle
    if current == target && !path.is_empty() {
        path.push(current.clone());
        return Some(path.clone());
    }

    // If current == target and path is empty, we're starting at the target
    // This means we're looking for a direct self-reference (A -> A)
    // Check if this rule references itself directly
    if current == target && path.is_empty() {
        if let Some(refs) = dependencies.get(current)
            && refs.contains(current)
        {
            // Direct self-reference: A -> A
            return Some(vec![current.clone(), current.clone()]);
        }
        // No direct self-reference and we're already at target with empty path
        // This means we haven't found a cycle yet, return None
        return None;
    }

    // Add current to path before exploring
    path.push(current.clone());

    // Explore all referenced rules
    if let Some(refs) = dependencies.get(current) {
        for ref_rule in refs {
            if let Some(result) = find_path_to_rule(dependencies, ref_rule, target, path) {
                // Found a path, return it
                return Some(result);
            }
            // No path found through this ref_rule, continue to next
        }
    }

    // No path found, backtrack
    path.pop();
    None
}

/// Collect expression details (trailing separators, recovery, lookahead, etc.)
fn collect_expression_details<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
) -> Vec<String>
where
    T: Token,
    N: NonTerminal,
{
    let mut details = Vec::new();
    collect_expression_details_impl(expr, grammar, token_name, &mut details);
    details
}

fn collect_expression_details_impl<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
    result: &mut Vec<String>,
) where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Separated {
            separator,
            trailing,
            min,
            ..
        } => {
            let sep_str = format_expr_ebnf(separator, grammar, token_name);
            let trailing_note = match trailing {
                crate::grammar::TrailingSeparator::Forbid => "trailing separator forbidden",
                crate::grammar::TrailingSeparator::Allow => "trailing separator allowed",
                crate::grammar::TrailingSeparator::Require => "trailing separator required",
            };
            result.push(format!(
                "Separated list with separator `{sep_str}` ({trailing_note}, min: {min})"
            ));
        }
        Expr::Delimited { recover, .. } => {
            if *recover {
                result.push("Delimited expression with error recovery enabled".to_string());
            }
        }
        Expr::Lookahead(e) => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            result.push(format!("Positive lookahead: `&{inner}`"));
        }
        Expr::NotLookahead(e) => {
            let inner = format_expr_ebnf(e, grammar, token_name);
            result.push(format!("Negative lookahead: `!{inner}`"));
        }
        Expr::RecoveryPoint { sync_tokens, .. } => {
            if sync_tokens.is_empty() {
                result.push("Recovery point (no sync tokens specified)".to_string());
            } else {
                let token_names: Vec<String> = sync_tokens
                    .iter()
                    .map(|t| format!("`{}`", token_name(t)))
                    .collect();
                let token_names_str = token_names.join(", ");
                result.push(format!(
                    "Recovery point with sync tokens: {token_names_str}"
                ));
            }
        }
        Expr::Seq(exprs) | Expr::Choice(exprs) => {
            for e in exprs {
                collect_expression_details_impl(e, grammar, token_name, result);
            }
        }
        Expr::Opt(e)
        | Expr::Repeat { expr: e, .. }
        | Expr::Label { expr: e, .. }
        | Expr::Node { expr: e, .. }
        | Expr::Flatten(e)
        | Expr::Prune(e) => {
            collect_expression_details_impl(e, grammar, token_name, result);
        }
        _ => {}
    }
}

/// Collect all non-terminals referenced in an expression
fn collect_referenced_rules<T, N>(expr: &Expr<T, N>) -> Vec<N>
where
    T: Token,
    N: NonTerminal,
{
    let mut rules = Vec::new();
    collect_referenced_rules_impl(expr, &mut rules);
    rules
}

fn collect_referenced_rules_impl<T, N>(expr: &Expr<T, N>, result: &mut Vec<N>)
where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Rule(n) => {
            result.push(n.clone());
        }
        Expr::Seq(exprs) | Expr::Choice(exprs) => {
            for e in exprs {
                collect_referenced_rules_impl(e, result);
            }
        }
        Expr::Opt(e)
        | Expr::Repeat { expr: e, .. }
        | Expr::Label { expr: e, .. }
        | Expr::Node { expr: e, .. }
        | Expr::Flatten(e)
        | Expr::Prune(e)
        | Expr::Lookahead(e)
        | Expr::NotLookahead(e)
        | Expr::RecoveryPoint { expr: e, .. } => {
            collect_referenced_rules_impl(e, result);
        }
        Expr::Separated {
            item, separator, ..
        } => {
            collect_referenced_rules_impl(item, result);
            collect_referenced_rules_impl(separator, result);
        }
        Expr::Delimited {
            open,
            content,
            close,
            ..
        } => {
            collect_referenced_rules_impl(open, result);
            collect_referenced_rules_impl(content, result);
            collect_referenced_rules_impl(close, result);
        }
        _ => {}
    }
}

#[cfg(test)]
#[cfg(feature = "grammar-docs")]
mod tests {
    use super::*;
    use crate::grammar::{Expr, GrammarBuilder};
    use crate::syntax::SyntaxKind;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(dead_code)]
    enum TestSyntaxKind {
        Number,
        Plus,
        Minus,
        LParen,
        RParen,
        Eof,
        Expr,
        Term,
    }

    impl SyntaxKind for TestSyntaxKind {
        fn is_terminal(self) -> bool {
            !matches!(self, Self::Expr | Self::Term)
        }

        fn is_trivia(self) -> bool {
            false
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    enum TestToken {
        Number,
        Plus,
        Minus,
        LParen,
        RParen,
    }

    impl Token for TestToken {
        type Kind = TestSyntaxKind;

        fn kind(&self) -> Self::Kind {
            match self {
                Self::Number => TestSyntaxKind::Number,
                Self::Plus => TestSyntaxKind::Plus,
                Self::Minus => TestSyntaxKind::Minus,
                Self::LParen => TestSyntaxKind::LParen,
                Self::RParen => TestSyntaxKind::RParen,
            }
        }

        fn text_len(&self) -> crate::syntax::TextSize {
            crate::syntax::TextSize::from(1)
        }

        fn text(&self) -> compact_str::CompactString {
            match self {
                Self::Number => "number".into(),
                Self::Plus => "+".into(),
                Self::Minus => "-".into(),
                Self::LParen => "(".into(),
                Self::RParen => ")".into(),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    enum TestNonTerminal {
        Expr,
        Term,
    }

    impl NonTerminal for TestNonTerminal {
        fn name(&self) -> &str {
            match self {
                Self::Expr => "Expr",
                Self::Term => "Term",
            }
        }
    }

    // Wrapper to match API signature (Fn(&T) -> String)
    // Note: TestToken is Copy, but the API requires &T, so we must accept by reference
    #[allow(clippy::trivially_copy_pass_by_ref)] // Required by API signature: Fn(&T) -> String
    fn token_name(t: &TestToken) -> String {
        format!("{t:?}")
    }

    #[test]
    fn test_rule_metadata_description() {
        use crate::grammar::builder::RuleMetadata;

        let metadata = RuleMetadata::new();
        assert!(metadata.description().is_none());

        let metadata = metadata.with_description("Test description".to_string());
        assert_eq!(metadata.description(), Some("Test description"));
    }

    #[test]
    fn test_token_description_builder() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .token_description(TestToken::Plus, "Plus operator".to_string())
            .token_description(TestToken::Number, "Numeric literal".to_string())
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        assert_eq!(
            grammar.token_description(&TestToken::Plus),
            Some("Plus operator")
        );
        assert_eq!(
            grammar.token_description(&TestToken::Number),
            Some("Numeric literal")
        );
        assert_eq!(grammar.token_description(&TestToken::Minus), None);
    }

    #[test]
    fn test_rule_with_description() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule_with_description(
                TestNonTerminal::Expr,
                Expr::token(TestToken::Number),
                "An expression is a number".to_string(),
            )
            .build()
            .unwrap();

        let rule = grammar.get_rule(&TestNonTerminal::Expr).unwrap();
        assert_eq!(
            rule.metadata.description(),
            Some("An expression is a number")
        );
    }

    #[test]
    fn test_ebnf_format_token() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let ebnf = format_expr_ebnf(&Expr::token(TestToken::Number), &grammar, &token_name);
        // When no description is provided, uses the token_name function result
        assert_eq!(ebnf, "'Number'");
    }

    #[test]
    fn test_ebnf_format_token_with_description() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .token_description(TestToken::Number, "number".to_string())
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let ebnf = format_expr_ebnf(&Expr::token(TestToken::Number), &grammar, &token_name);
        // EBNF uses token names, not descriptions (descriptions are shown in Tokens section)
        assert_eq!(ebnf, "'Number'");
    }

    #[test]
    fn test_ebnf_format_rule() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let ebnf = format_expr_ebnf(&Expr::rule(TestNonTerminal::Expr), &grammar, &token_name);
        assert_eq!(ebnf, "Expr");
    }

    #[test]
    fn test_ebnf_format_choice() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(
                TestNonTerminal::Expr,
                Expr::Choice(vec![
                    Expr::token(TestToken::Number),
                    Expr::token(TestToken::Plus),
                ]),
            )
            .build()
            .unwrap();

        let expr = Expr::Choice(vec![
            Expr::token(TestToken::Number),
            Expr::token(TestToken::Plus),
        ]);
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'Number' | 'Plus'");
    }

    #[test]
    fn test_ebnf_format_seq() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(
                TestNonTerminal::Expr,
                Expr::Seq(vec![
                    Expr::token(TestToken::Number),
                    Expr::token(TestToken::Plus),
                    Expr::token(TestToken::Number),
                ]),
            )
            .build()
            .unwrap();

        let expr = Expr::Seq(vec![
            Expr::token(TestToken::Number),
            Expr::token(TestToken::Plus),
            Expr::token(TestToken::Number),
        ]);
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'Number' 'Plus' 'Number'");
    }

    #[test]
    fn test_ebnf_format_opt() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Opt(Box::new(Expr::token(TestToken::Number)));
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'Number'?");
    }

    #[test]
    fn test_ebnf_format_repeat_star() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Repeat {
            expr: Box::new(Expr::token(TestToken::Number)),
            min: 0,
            max: None,
            greedy: true,
        };
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'Number'*");
    }

    #[test]
    fn test_ebnf_format_repeat_plus() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Repeat {
            expr: Box::new(Expr::token(TestToken::Number)),
            min: 1,
            max: None,
            greedy: true,
        };
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'Number'+");
    }

    #[test]
    fn test_ebnf_format_repeat_bounded() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Repeat {
            expr: Box::new(Expr::token(TestToken::Number)),
            min: 2,
            max: Some(5),
            greedy: true,
        };
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'Number'{2,5}");
    }

    #[test]
    fn test_ebnf_format_delimited() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Delimited {
            open: Box::new(Expr::token(TestToken::LParen)),
            content: Box::new(Expr::token(TestToken::Number)),
            close: Box::new(Expr::token(TestToken::RParen)),
            recover: true,
        };
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "'LParen' 'Number' 'RParen'");
    }

    #[test]
    fn test_ebnf_format_separated() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Separated {
            item: Box::new(Expr::token(TestToken::Number)),
            separator: Box::new(Expr::token(TestToken::Plus)),
            min: 0,
            trailing: crate::grammar::TrailingSeparator::Forbid,
        };
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert!(ebnf.contains("'Number'"));
        assert!(ebnf.contains("'Plus'"));
    }

    #[test]
    fn test_ebnf_format_lookahead() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::Lookahead(Box::new(Expr::token(TestToken::Number)));
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "&'Number'");
    }

    #[test]
    fn test_ebnf_format_not_lookahead() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let expr = Expr::NotLookahead(Box::new(Expr::token(TestToken::Number)));
        let ebnf = format_expr_ebnf(&expr, &grammar, &token_name);
        assert_eq!(ebnf, "!'Number'");
    }

    #[test]
    fn test_ebnf_format_primitives() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::Empty)
            .build()
            .unwrap();

        assert_eq!(format_expr_ebnf(&Expr::Any, &grammar, &token_name), "'ANY'");
        assert_eq!(format_expr_ebnf(&Expr::Eof, &grammar, &token_name), "'EOF'");
        assert_eq!(format_expr_ebnf(&Expr::Empty, &grammar, &token_name), "ε");
    }

    #[test]
    fn test_markdown_generation_basic() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let markdown = grammar.to_markdown(&token_name);
        assert!(markdown.contains("# Grammar Documentation"));
        assert!(markdown.contains("## Entry Point"));
        assert!(markdown.contains("## Production Rules"));
        assert!(markdown.contains("### <a id=\"expr\"></a>Expr") || markdown.contains("### Expr"));
        assert!(markdown.contains("Expr ::="));
        assert!(markdown.contains("'Number'"));
    }

    #[test]
    fn test_markdown_generation_with_descriptions() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule_with_description(
                TestNonTerminal::Expr,
                Expr::token(TestToken::Number),
                "An expression is a number".to_string(),
            )
            .token_description(TestToken::Number, "A numeric literal".to_string())
            .build()
            .unwrap();

        let markdown = grammar.to_markdown(&token_name);
        assert!(markdown.contains("An expression is a number"));
        assert!(markdown.contains("## Tokens"));
        // The tokens section now uses a table format with more columns
        assert!(markdown.contains("| Token |") && markdown.contains("| Description |"));
        assert!(markdown.contains("A numeric literal"));
    }

    #[test]
    fn test_markdown_generation_multiple_rules() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule(TestNonTerminal::Expr, Expr::rule(TestNonTerminal::Term))
            .rule(TestNonTerminal::Term, Expr::token(TestToken::Number))
            .build()
            .unwrap();

        let markdown = grammar.to_markdown(&token_name);
        // Rules should be sorted alphabetically
        let expr_pos = markdown
            .find("### <a id=\"expr\"></a>Expr")
            .or_else(|| markdown.find("### Expr"))
            .unwrap();
        let term_pos = markdown
            .find("### <a id=\"term\"></a>Term")
            .or_else(|| markdown.find("### Term"))
            .unwrap();
        assert!(expr_pos < term_pos);
    }

    #[test]
    fn test_markdown_generation_complex_grammar() {
        let grammar = GrammarBuilder::new()
            .entry_point(TestNonTerminal::Expr)
            .rule_with_description(
                TestNonTerminal::Expr,
                Expr::Choice(vec![
                    Expr::Seq(vec![
                        Expr::rule(TestNonTerminal::Term),
                        Expr::token(TestToken::Plus),
                        Expr::rule(TestNonTerminal::Expr),
                    ]),
                    Expr::rule(TestNonTerminal::Term),
                ]),
                "An expression can be a term or term plus expression".to_string(),
            )
            .rule(TestNonTerminal::Term, Expr::token(TestToken::Number))
            .token_description(TestToken::Plus, "Plus operator (+)".to_string())
            .token_description(TestToken::Number, "Numeric literal".to_string())
            .build()
            .unwrap();

        let markdown = grammar.to_markdown(&token_name);
        assert!(markdown.contains("An expression can be a term or term plus expression"));
        assert!(markdown.contains("Expr ::="));
        assert!(markdown.contains("Term ::="));
        assert!(markdown.contains("Plus operator (+)"));
        assert!(markdown.contains("Numeric literal"));
    }

    #[test]
    fn test_collect_tokens() {
        let expr: Expr<TestToken, TestNonTerminal> = Expr::Seq(vec![
            Expr::token(TestToken::Number),
            Expr::token(TestToken::Plus),
            Expr::token(TestToken::Number),
        ]);

        let tokens = collect_tokens(&expr);
        assert_eq!(tokens.len(), 3);
        assert!(tokens.contains(&TestToken::Number));
        assert!(tokens.contains(&TestToken::Plus));
    }

    #[test]
    fn test_collect_tokens_nested() {
        let expr: Expr<TestToken, TestNonTerminal> = Expr::Choice(vec![
            Expr::token(TestToken::Number),
            Expr::Seq(vec![
                Expr::token(TestToken::Plus),
                Expr::token(TestToken::Minus),
            ]),
        ]);

        let tokens = collect_tokens(&expr);
        assert_eq!(tokens.len(), 3);
        assert!(tokens.contains(&TestToken::Number));
        assert!(tokens.contains(&TestToken::Plus));
        assert!(tokens.contains(&TestToken::Minus));
    }
}
