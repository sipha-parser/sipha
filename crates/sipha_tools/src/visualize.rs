//! Grammar visualization utilities
//!
//! This module provides functionality to visualize grammars as graphs,
//! including DOT/Graphviz format and HTML output.

use sipha::grammar::{Expr, Grammar, NonTerminal, Token};
use std::fmt::Write;

/// Generate a DOT/Graphviz representation of a grammar
///
/// # Example
///
/// ```rust,ignore
/// use sipha_tools::visualize::generate_dot;
///
/// let dot = generate_dot(&grammar, |t| format!("{:?}", t));
/// println!("{}", dot);
/// ```
pub fn generate_dot<T, N>(
    grammar: &Grammar<T, N>,
    token_name: impl Fn(&T) -> String,
) -> String
where
    T: Token,
    N: NonTerminal,
{
    let mut output = String::new();
    
    writeln!(output, "digraph Grammar {{").unwrap();
    writeln!(output, "  rankdir=LR;").unwrap();
    writeln!(output, "  node [shape=box];").unwrap();
    writeln!(output).unwrap();

    // Add nodes for all rules
    for (nt, _rule) in grammar.rules() {
        let name = nt.name();
        writeln!(output, "  \"{}\" [label=\"{}\", shape=ellipse];", name, name).unwrap();
    }

    writeln!(output).unwrap();

    // Add edges for rule dependencies
    let mut token_nodes = std::collections::HashSet::new();
    
    for (nt, rule) in grammar.rules() {
        let from_name = nt.name();
        let expr = &rule.rhs;
        
        // Find all rule references in the expression
        let refs = find_rule_refs(expr, grammar);
        
        for ref_name in refs {
            writeln!(output, "  \"{}\" -> \"{}\";", from_name, ref_name).unwrap();
        }
        
        // Collect tokens
        let tokens = find_token_refs(expr, grammar, &token_name);
        for token in tokens {
            token_nodes.insert(token);
        }
    }

    // Add token nodes
    writeln!(output).unwrap();
    writeln!(output, "  // Tokens").unwrap();
    for token in &token_nodes {
        writeln!(
            output,
            "  \"{}\" [label=\"{}\", shape=box, style=filled, fillcolor=lightblue];",
            token, token
        ).unwrap();
    }
    
    // Add edges from rules to tokens
    for (nt, rule) in grammar.rules() {
        let rule_name = nt.name();
        let tokens = find_token_refs(&rule.rhs, grammar, &token_name);
        
        for token in tokens {
            writeln!(output, "  \"{}\" -> \"{}\" [style=dashed];", rule_name, token).unwrap();
        }
    }

    writeln!(output, "}}").unwrap();
    output
}

/// Generate an HTML visualization of a grammar
///
/// This creates an interactive HTML page with the grammar structure.
pub fn generate_html<T, N>(
    grammar: &Grammar<T, N>,
    token_name: impl Fn(&T) -> String,
    title: Option<&str>,
) -> String
where
    T: Token,
    N: NonTerminal,
{
    generate_html_with_conflicts(grammar, token_name, title, false)
}

/// Generate an HTML visualization of a grammar with conflict highlighting
///
/// This creates an interactive HTML page with the grammar structure,
/// optionally highlighting conflicts and issues.
pub fn generate_html_with_conflicts<T, N>(
    grammar: &Grammar<T, N>,
    token_name: impl Fn(&T) -> String,
    title: Option<&str>,
    show_conflicts: bool,
) -> String
where
    T: Token,
    N: NonTerminal,
{
    use sipha::grammar::analyzer::GrammarAnalyzer;
    
    let title = title.unwrap_or("Grammar Visualization");
    let dot = if show_conflicts {
        generate_dot_with_conflicts(grammar, token_name)
    } else {
        generate_dot(grammar, token_name)
    };
    
    // Get statistics for display
    let analyzer = GrammarAnalyzer::new(grammar);
    let stats = analyzer.statistics();
    let left_recursion = analyzer.find_left_recursion();
    let ll_conflicts = analyzer.ll1_conflicts();
    let unreachable = analyzer.find_unreachable_rules();
    
    let stats_html = if show_conflicts {
        format!(
            r#"
    <div class="stats">
        <h2>Grammar Statistics</h2>
        <ul>
            <li>Rules: {}</li>
            <li>Total Alternatives: {}</li>
            <li>Max Expression Depth: {}</li>
            <li>Nullable Rules: {}</li>
            <li>Left Recursive Cycles: <span class="warning">{}</span></li>
            <li>LL Conflicts: <span class="warning">{}</span></li>
            <li>Unreachable Rules: <span class="info">{}</span></li>
        </ul>
    </div>"#,
            stats.rule_count,
            stats.total_alternatives,
            stats.max_expression_depth,
            stats.nullable_rules,
            stats.left_recursive_cycles,
            ll_conflicts.len(),
            stats.unreachable_rules
        )
    } else {
        format!(
            r#"
    <div class="stats">
        <h2>Grammar Statistics</h2>
        <ul>
            <li>Rules: {}</li>
            <li>Total Alternatives: {}</li>
            <li>Max Expression Depth: {}</li>
            <li>Nullable Rules: {}</li>
        </ul>
    </div>"#,
            stats.rule_count,
            stats.total_alternatives,
            stats.max_expression_depth,
            stats.nullable_rules
        )
    };
    
    format!(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>{}</title>
    <script src="https://cdn.jsdelivr.net/npm/viz.js@2.1.2/viz.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/viz.js@2.1.2/full.render.js"></script>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }}
        .container {{
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        h1 {{
            margin-top: 0;
            color: #333;
        }}
        .stats {{
            background: #f9f9f9;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
        }}
        .stats ul {{
            margin: 10px 0;
            padding-left: 20px;
        }}
        .stats li {{
            margin: 5px 0;
        }}
        .warning {{
            color: #d32f2f;
            font-weight: bold;
        }}
        .info {{
            color: #1976d2;
            font-weight: bold;
        }}
        #graph {{
            border: 1px solid #ddd;
            padding: 20px;
            background: white;
            border-radius: 5px;
            overflow: auto;
        }}
        #graph svg {{
            max-width: 100%;
            height: auto;
        }}
        .error {{
            background: #ffebee;
            border: 1px solid #ef5350;
            padding: 15px;
            border-radius: 5px;
            margin: 20px 0;
        }}
        .error pre {{
            background: white;
            padding: 10px;
            border-radius: 3px;
            overflow-x: auto;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>{}</h1>
        {}
        <div id="graph"></div>
    </div>
    <script>
        var viz = new Viz();
        var dot = `{}`;
        viz.renderSVGElement(dot)
            .then(function(element) {{
                element.style.maxWidth = '100%';
                element.style.height = 'auto';
                document.getElementById("graph").appendChild(element);
            }})
            .catch(error => {{
                console.error(error);
                var errorDiv = document.createElement('div');
                errorDiv.className = 'error';
                errorDiv.innerHTML = 
                    '<p><strong>Error rendering graph:</strong> Make sure you have an internet connection for viz.js</p>' +
                    '<p>You can also copy the DOT code below and use a local Graphviz installation:</p>' +
                    '<pre>' + dot.replace(/`/g, '\\`') + '</pre>';
                document.getElementById("graph").appendChild(errorDiv);
            }});
    </script>
</body>
</html>"#,
        title, title, stats_html, dot
    )
}

/// Find all rule references in an expression
fn find_rule_refs<T, N>(expr: &Expr<T, N>, grammar: &Grammar<T, N>) -> Vec<String>
where
    T: Token,
    N: NonTerminal,
{
    let mut refs = Vec::new();
    find_rule_refs_recursive(expr, grammar, &mut refs);
    refs.sort();
    refs.dedup();
    refs
}

fn find_rule_refs_recursive<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    refs: &mut Vec<String>,
) where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Rule(n) => {
            refs.push(n.name().to_string());
        }
        Expr::Seq(items) | Expr::Choice(items) => {
            for item in items {
                find_rule_refs_recursive(item, grammar, refs);
            }
        }
        Expr::Opt(inner) => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::Repeat { expr: inner, .. } => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::Separated { item, separator, .. } => {
            find_rule_refs_recursive(item, grammar, refs);
            find_rule_refs_recursive(separator, grammar, refs);
        }
        Expr::Delimited { open, content, close, .. } => {
            find_rule_refs_recursive(open, grammar, refs);
            find_rule_refs_recursive(content, grammar, refs);
            find_rule_refs_recursive(close, grammar, refs);
        }
        Expr::Lookahead(inner) | Expr::NotLookahead(inner) | Expr::Cut(inner) => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::Conditional { condition, then_expr, else_expr } => {
            find_rule_refs_recursive(condition, grammar, refs);
            find_rule_refs_recursive(then_expr, grammar, refs);
            if let Some(else_expr) = else_expr {
                find_rule_refs_recursive(else_expr, grammar, refs);
            }
        }
        Expr::SemanticPredicate { expr: inner, .. } => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::Label { expr: inner, .. } | Expr::Node { expr: inner, .. } 
        | Expr::Flatten(inner) | Expr::Prune(inner) => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::RecoveryPoint { expr: inner, .. } => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::Capture { expr: inner, .. } => {
            find_rule_refs_recursive(inner, grammar, refs);
        }
        Expr::Token(_) | Expr::Any | Expr::Eof | Expr::Empty 
        | Expr::TokenClass { .. } | Expr::Backreference { .. } => {
            // No rule references
        }
    }
}

/// Find all token references in an expression
fn find_token_refs<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
) -> Vec<String>
where
    T: Token,
    N: NonTerminal,
{
    let mut tokens = Vec::new();
    find_token_refs_recursive(expr, grammar, token_name, &mut tokens);
    tokens.sort();
    tokens.dedup();
    tokens
}

fn find_token_refs_recursive<T, N>(
    expr: &Expr<T, N>,
    grammar: &Grammar<T, N>,
    token_name: &impl Fn(&T) -> String,
    tokens: &mut Vec<String>,
) where
    T: Token,
    N: NonTerminal,
{
    match expr {
        Expr::Token(t) => {
            tokens.push(token_name(t));
        }
        Expr::Seq(items) | Expr::Choice(items) => {
            for item in items {
                find_token_refs_recursive(item, grammar, token_name, tokens);
            }
        }
        Expr::Opt(inner) => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::Repeat { expr: inner, .. } => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::Separated { item, separator, .. } => {
            find_token_refs_recursive(item, grammar, token_name, tokens);
            find_token_refs_recursive(separator, grammar, token_name, tokens);
        }
        Expr::Delimited { open, content, close, .. } => {
            find_token_refs_recursive(open, grammar, token_name, tokens);
            find_token_refs_recursive(content, grammar, token_name, tokens);
            find_token_refs_recursive(close, grammar, token_name, tokens);
        }
        Expr::Lookahead(inner) | Expr::NotLookahead(inner) | Expr::Cut(inner) => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::Conditional { condition, then_expr, else_expr } => {
            find_token_refs_recursive(condition, grammar, token_name, tokens);
            find_token_refs_recursive(then_expr, grammar, token_name, tokens);
            if let Some(else_expr) = else_expr {
                find_token_refs_recursive(else_expr, grammar, token_name, tokens);
            }
        }
        Expr::SemanticPredicate { expr: inner, .. } => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::Label { expr: inner, .. } | Expr::Node { expr: inner, .. } 
        | Expr::Flatten(inner) | Expr::Prune(inner) => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::RecoveryPoint { expr: inner, .. } => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::Capture { expr: inner, .. } => {
            find_token_refs_recursive(inner, grammar, token_name, tokens);
        }
        Expr::Rule(_) | Expr::Any | Expr::Eof | Expr::Empty 
        | Expr::TokenClass { .. } | Expr::Backreference { .. } => {
            // No token references at this level (or handled separately)
        }
    }
}

/// Highlight conflicts and left recursion in the visualization
pub fn generate_dot_with_conflicts<T, N>(
    grammar: &Grammar<T, N>,
    token_name: impl Fn(&T) -> String,
) -> String
where
    T: Token,
    N: NonTerminal,
{
    use sipha::grammar::analyzer::GrammarAnalyzer;
    
    let analyzer = GrammarAnalyzer::new(grammar);
    let left_recursion = analyzer.find_left_recursion();
    let ll_conflicts = analyzer.ll1_conflicts();
    let unreachable = analyzer.find_unreachable_rules();
    
    // Build sets for quick lookup
    let mut left_recursive_set = std::collections::HashSet::new();
    for cycle in &left_recursion {
        for nt in &cycle.nodes {
            left_recursive_set.insert(nt.name().to_string());
        }
    }
    
    let mut conflict_set = std::collections::HashSet::new();
    for conflict in &ll_conflicts {
        conflict_set.insert(conflict.non_terminal.name().to_string());
    }
    
    let mut unreachable_set = std::collections::HashSet::new();
    for nt in &unreachable {
        unreachable_set.insert(nt.name().to_string());
    }
    
    // Generate base DOT
    let mut output = String::new();
    
    writeln!(output, "digraph Grammar {{").unwrap();
    writeln!(output, "  rankdir=LR;").unwrap();
    writeln!(output, "  node [shape=box];").unwrap();
    writeln!(output).unwrap();

    // Add nodes for all rules with conflict highlighting
    for (nt, _rule) in grammar.rules() {
        let name = nt.name();
        let mut style = "shape=ellipse";
        
        if left_recursive_set.contains(name) {
            style = "shape=ellipse, style=filled, fillcolor=red, fontcolor=white";
        } else if conflict_set.contains(name) {
            style = "shape=ellipse, style=filled, fillcolor=orange";
        } else if unreachable_set.contains(name) {
            style = "shape=ellipse, style=filled, fillcolor=gray, fontcolor=white";
        }
        
        writeln!(output, "  \"{}\" [label=\"{}\", {}];", name, name, style).unwrap();
    }

    writeln!(output).unwrap();

    // Add edges for rule dependencies
    let mut token_nodes = std::collections::HashSet::new();
    
    for (nt, rule) in grammar.rules() {
        let from_name = nt.name();
        let expr = &rule.rhs;
        
        // Find all rule references in the expression
        let refs = find_rule_refs(expr, grammar);
        
        for ref_name in refs {
            // Highlight edges that are part of left recursion cycles
            let is_recursive_edge = left_recursive_set.contains(from_name) 
                && left_recursive_set.contains(&ref_name);
            
            if is_recursive_edge {
                writeln!(output, "  \"{}\" -> \"{}\" [color=red, penwidth=2];", from_name, ref_name).unwrap();
            } else {
                writeln!(output, "  \"{}\" -> \"{}\";", from_name, ref_name).unwrap();
            }
        }
        
        // Collect tokens
        let tokens = find_token_refs(expr, grammar, &token_name);
        for token in tokens {
            token_nodes.insert(token);
        }
    }

    // Add token nodes
    writeln!(output).unwrap();
    writeln!(output, "  // Tokens").unwrap();
    for token in &token_nodes {
        writeln!(
            output,
            "  \"{}\" [label=\"{}\", shape=box, style=filled, fillcolor=lightblue];",
            token, token
        ).unwrap();
    }
    
    // Add edges from rules to tokens
    for (nt, rule) in grammar.rules() {
        let rule_name = nt.name();
        let tokens = find_token_refs(&rule.rhs, grammar, &token_name);
        
        for token in tokens {
            writeln!(output, "  \"{}\" -> \"{}\" [style=dashed];", rule_name, token).unwrap();
        }
    }
    
    // Add legend for conflict types
    if !left_recursive_set.is_empty() || !conflict_set.is_empty() || !unreachable_set.is_empty() {
        writeln!(output).unwrap();
        writeln!(output, "  // Legend").unwrap();
        writeln!(output, "  subgraph cluster_legend {{").unwrap();
        writeln!(output, "    label=\"Legend\";").unwrap();
        writeln!(output, "    style=dashed;").unwrap();
        
        if !left_recursive_set.is_empty() {
            writeln!(output, "    legend_left_rec [label=\"Left Recursive\", shape=ellipse, style=filled, fillcolor=red, fontcolor=white];").unwrap();
        }
        if !conflict_set.is_empty() {
            writeln!(output, "    legend_conflict [label=\"LL Conflict\", shape=ellipse, style=filled, fillcolor=orange];").unwrap();
        }
        if !unreachable_set.is_empty() {
            writeln!(output, "    legend_unreachable [label=\"Unreachable\", shape=ellipse, style=filled, fillcolor=gray, fontcolor=white];").unwrap();
        }
        
        writeln!(output, "  }}").unwrap();
    }

    writeln!(output, "}}").unwrap();
    output
}

