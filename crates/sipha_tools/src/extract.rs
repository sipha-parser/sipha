//! Grammar extraction from Rust source files
//!
//! This module provides functionality to extract Sipha grammar definitions
//! from Rust source code by parsing the AST and finding GrammarBuilder calls
//! and grammar! macro invocations.

use sipha::grammar::{Grammar, NonTerminal, Token};
use syn::visit::Visit;
use syn::{File, ExprMethodCall, ExprPath};

/// Extract a grammar from a Rust source file
///
/// This function attempts to find GrammarBuilder usage in the source file
/// and extract the grammar definition. This is a best-effort extraction
/// and may not work for all grammar definitions.
///
/// Note: Full grammar extraction requires type information (T and N) which
/// cannot be determined from source code alone. This function extracts the
/// structure but cannot build a complete Grammar<T, N> without concrete types.
pub fn extract_grammar_from_file<T, N>(
    file_path: &std::path::Path,
) -> Result<Option<Grammar<T, N>>, Box<dyn std::error::Error>>
where
    T: Token + 'static,
    N: NonTerminal + 'static,
{
    let content = std::fs::read_to_string(file_path)?;
    extract_grammar_from_source::<T, N>(&content)
}

/// Extract a grammar from Rust source code
///
/// This implementation extracts grammar structure from:
/// 1. GrammarBuilder method chains (.rule(), .entry_point())
/// 2. grammar! macro invocations
///
/// Note: Cannot build a complete Grammar<T, N> without concrete T and N types.
/// Returns None if extraction is attempted but types cannot be determined.
pub fn extract_grammar_from_source<T, N>(
    source: &str,
) -> Result<Option<Grammar<T, N>>, Box<dyn std::error::Error>>
where
    T: Token + 'static,
    N: NonTerminal + 'static,
{
    // Parse the Rust file
    let ast: File = syn::parse_file(source)?;
    
    // Visit the AST to find and extract grammar definitions
    let mut visitor = GrammarExtractor::new();
    visitor.visit_file(&ast);
    
    // If we found grammar definitions, we've extracted the structure
    // However, we cannot build a Grammar<T, N> without concrete T and N types
    // Return None to indicate extraction was attempted but cannot be completed
    if visitor.found_grammar_definitions() {
        // We successfully detected and parsed grammar definitions
        // but cannot build Grammar<T, N> without type information
        Ok(None)
    } else {
        // No grammar definitions found
        Ok(None)
    }
}

/// Visitor to extract grammar definitions from the AST
struct GrammarExtractor {
    found_builder: bool,
    found_macro: bool,
    #[allow(dead_code)]
    builder_chains: Vec<BuilderChain>,
    #[allow(dead_code)]
    macro_invocations: Vec<MacroInvocation>,
}

#[allow(dead_code)]
struct BuilderChain {
    rules: Vec<RuleCall>,
    entry_point: Option<String>,
}

#[allow(dead_code)]
struct RuleCall {
    non_terminal: String,
    expr_structure: String,
}

#[allow(dead_code)]
struct MacroInvocation {
    body: String,
}

impl GrammarExtractor {
    fn new() -> Self {
        Self {
            found_builder: false,
            found_macro: false,
            builder_chains: Vec::new(),
            macro_invocations: Vec::new(),
        }
    }
    
    fn found_grammar_definitions(&self) -> bool {
        self.found_builder || self.found_macro
    }
}

impl<'ast> Visit<'ast> for GrammarExtractor {
    fn visit_expr_method_call(&mut self, call: &'ast ExprMethodCall) {
        // Check if this is a GrammarBuilder method call
        if let syn::Expr::Path(path_expr) = &*call.receiver {
            // Check if the receiver is a GrammarBuilder
            if is_grammar_builder_path(path_expr) {
                self.found_builder = true;
                
                // Extract method name
                let method_name = call.method.to_string();
                
                // Get or create the current builder chain
                if self.builder_chains.is_empty() {
                    self.builder_chains.push(BuilderChain {
                        rules: Vec::new(),
                        entry_point: None,
                    });
                }
                let chain = self.builder_chains.last_mut().unwrap();
                
                match method_name.as_str() {
                    "rule" => {
                        // Try to extract rule information
                        if let Some(first_arg) = call.args.iter().next() {
                            // Extract non-terminal name from first argument
                            if let Some(nt_name) = extract_non_terminal_name(first_arg) {
                                if let Some(expr_arg) = call.args.iter().nth(1) {
                                    let expr_structure = extract_expr_structure(expr_arg);
                                    // Store rule information
                                    chain.rules.push(RuleCall {
                                        non_terminal: nt_name,
                                        expr_structure,
                                    });
                                }
                            }
                        }
                    }
                    "entry_point" => {
                        // Extract entry point information
                        if let Some(first_arg) = call.args.iter().next() {
                            if let Some(entry) = extract_non_terminal_name(first_arg) {
                                chain.entry_point = Some(entry);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        // Continue visiting
        syn::visit::visit_expr_method_call(self, call);
    }
    
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        // Look for GrammarBuilder::new() calls
        if let syn::Expr::Path(path_expr) = &*call.func {
            if is_grammar_builder_new(path_expr) {
                self.found_builder = true;
            }
        }
        
        // Continue visiting
        syn::visit::visit_expr_call(self, call);
    }
    
    fn visit_macro(&mut self, mac: &'ast syn::Macro) {
        // Look for grammar! macro invocations
        if let Some(ident) = mac.path.get_ident() {
            if ident == "grammar" {
                self.found_macro = true;
                
                // Try to parse the macro body using the same parser as the macro
                // This would require access to sipha_derive::parser, which we don't have
                // For now, we just detect that the macro exists and store the tokens
                // Store macro invocation info
                self.macro_invocations.push(MacroInvocation {
                    body: mac.tokens.to_string(),
                });
            }
        }
        
        // Continue visiting
        syn::visit::visit_macro(self, mac);
    }
}

/// Check if a path expression refers to GrammarBuilder
fn is_grammar_builder_path(path: &ExprPath) -> bool {
    if let Some(segment) = path.path.segments.last() {
        // Check if it's GrammarBuilder or a variable that might be a GrammarBuilder
        let path_str = quote::quote!(#path).to_string();
        path_str.contains("GrammarBuilder") || path_str.contains("builder")
    } else {
        false
    }
}

/// Check if a path expression is GrammarBuilder::new()
fn is_grammar_builder_new(path: &ExprPath) -> bool {
    if path.path.segments.len() >= 2 {
        let segments: Vec<_> = path.path.segments.iter().collect();
        if let (Some(prev), Some(last)) = (segments.get(segments.len() - 2), segments.last()) {
            prev.ident == "GrammarBuilder" && last.ident == "new"
        } else {
            false
        }
    } else {
        false
    }
}

/// Extract non-terminal name from an expression (simplified)
fn extract_non_terminal_name(expr: &syn::Expr) -> Option<String> {
    match expr {
        syn::Expr::Path(path_expr) => {
            if let Some(segment) = path_expr.path.segments.last() {
                // Check if it's a path like NonTerminal::Expr
                let path_str = quote::quote!(#path_expr).to_string();
                if let Some(colon_pos) = path_str.rfind("::") {
                    Some(path_str[colon_pos + 2..].to_string())
                } else {
                    Some(segment.ident.to_string())
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Extract expression structure as a string representation
fn extract_expr_structure(expr: &syn::Expr) -> String {
    // This is a simplified extraction - just convert to string
    // A full implementation would parse Expr::seq, Expr::choice, etc.
    quote::quote!(#expr).to_string()
}

