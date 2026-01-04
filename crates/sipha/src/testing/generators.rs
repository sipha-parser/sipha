//! # Property-Based Testing Generators
//!
//! This module provides generators for property-based testing with `proptest`.
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::testing::{GrammarGenerator, TokenSequenceStrategy};
//! use proptest::prelude::*;
//!
//! proptest! {
//!     #[test]
//!     fn parse_random_valid_input(tokens in valid_token_sequence(&grammar, 1..100)) {
//!         let result = parser.parse(&tokens, entry_point);
//!         assert!(result.errors.is_empty());
//!     }
//! }
//! ```

use crate::grammar::{Expr, Grammar, NonTerminal, Token};
use std::sync::Arc;

/// Configuration for grammar-based input generation
#[derive(Debug, Clone)]
pub struct GeneratorConfig {
    /// Maximum depth of recursive rule expansion
    pub max_depth: usize,
    /// Maximum number of repetitions for * and + operators
    pub max_repetitions: usize,
    /// Probability of taking optional elements (0.0 to 1.0)
    pub optional_probability: f64,
    /// Seed for reproducible generation
    pub seed: Option<u64>,
}

impl Default for GeneratorConfig {
    fn default() -> Self {
        Self {
            max_depth: 10,
            max_repetitions: 5,
            optional_probability: 0.5,
            seed: None,
        }
    }
}

/// Generator for grammar-based token sequences
pub struct GrammarGenerator<T: Token, N: NonTerminal> {
    grammar: Arc<Grammar<T, N>>,
    config: GeneratorConfig,
}

impl<T: Token, N: NonTerminal> GrammarGenerator<T, N> {
    /// Create a new grammar generator
    #[must_use]
    pub fn new(grammar: Arc<Grammar<T, N>>, config: GeneratorConfig) -> Self {
        Self { grammar, config }
    }

    /// Generate a random valid token sequence
    ///
    /// This generates a sequence of tokens that should be accepted by a parser
    /// for the given entry point.
    pub fn generate(&self, entry: &N) -> Vec<T>
    where
        T: Clone + Default,
    {
        let mut result = Vec::new();
        let mut rng = if let Some(seed) = self.config.seed {
            SimpleRng::with_seed(seed)
        } else {
            SimpleRng::new()
        };

        if let Some(rule) = self.grammar.get_rule(entry) {
            self.generate_expr(&rule.rhs, 0, &mut result, &mut rng);
        }

        result
    }

    fn generate_expr(
        &self,
        expr: &Expr<T, N>,
        depth: usize,
        result: &mut Vec<T>,
        rng: &mut SimpleRng,
    ) where
        T: Clone + Default,
    {
        if depth > self.config.max_depth {
            return;
        }

        match expr {
            Expr::Token(t) => {
                result.push(t.clone());
            }
            Expr::Rule(nt) => {
                if let Some(rule) = self.grammar.get_rule(nt) {
                    self.generate_expr(&rule.rhs, depth + 1, result, rng);
                }
            }
            Expr::Seq(items) => {
                for item in items {
                    self.generate_expr(item, depth, result, rng);
                }
            }
            Expr::Choice(alts) if !alts.is_empty() => {
                let idx = rng.next_u64() as usize % alts.len();
                self.generate_expr(&alts[idx], depth, result, rng);
            }
            Expr::Opt(inner) => {
                if rng.next_f64() < self.config.optional_probability {
                    self.generate_expr(inner, depth, result, rng);
                }
            }
            Expr::Repeat {
                expr: inner,
                min,
                max,
                ..
            } => {
                let min_reps = *min;
                let max_reps = max.unwrap_or(self.config.max_repetitions);
                let reps = if max_reps > min_reps {
                    min_reps + (rng.next_u64() as usize % (max_reps - min_reps + 1))
                } else {
                    min_reps
                };
                for _ in 0..reps {
                    self.generate_expr(inner, depth, result, rng);
                }
            }
            Expr::Empty => {}
            _ => {
                // Handle other expression types as needed
            }
        }
    }
}

/// Generate mutation operations for fuzzing
#[derive(Debug, Clone)]
pub enum TokenMutation<T: Token> {
    /// Delete a token at the given index
    Delete(usize),
    /// Insert a token at the given index
    Insert(usize, T),
    /// Replace a token at the given index
    Replace(usize, T),
    /// Swap two tokens
    Swap(usize, usize),
    /// Duplicate a token
    Duplicate(usize),
}

/// Fuzzer that generates edge-case inputs
pub struct GrammarFuzzer<T: Token, N: NonTerminal> {
    generator: GrammarGenerator<T, N>,
}

impl<T: Token, N: NonTerminal> GrammarFuzzer<T, N> {
    /// Create a new grammar fuzzer
    #[must_use]
    pub fn new(grammar: Arc<Grammar<T, N>>, config: GeneratorConfig) -> Self {
        Self {
            generator: GrammarGenerator::new(grammar, config),
        }
    }

    /// Generate a mutated (possibly invalid) token sequence
    pub fn generate_mutated(&self, entry: &N, num_mutations: usize) -> Vec<T>
    where
        T: Clone + Default,
    {
        let mut tokens = self.generator.generate(entry);
        let mut rng = SimpleRng::new();

        for _ in 0..num_mutations {
            if tokens.is_empty() {
                continue;
            }
            let idx = rng.next_u64() as usize % tokens.len();
            let mutation_type = rng.next_u64() % 5;

            match mutation_type {
                0 => {
                    // Delete
                    if !tokens.is_empty() {
                        tokens.remove(idx);
                    }
                }
                1 => {
                    // Insert a default token
                    tokens.insert(idx, T::default());
                }
                2 => {
                    // Replace with default token
                    if !tokens.is_empty() {
                        tokens[idx] = T::default();
                    }
                }
                3 => {
                    // Swap
                    let other = rng.next_u64() as usize % tokens.len();
                    tokens.swap(idx, other);
                }
                4 => {
                    // Duplicate
                    if !tokens.is_empty() {
                        let token = tokens[idx].clone();
                        tokens.insert(idx, token);
                    }
                }
                _ => {}
            }
        }

        tokens
    }

    /// Generate an empty input
    #[must_use]
    pub const fn generate_empty(&self) -> Vec<T> {
        Vec::new()
    }

    /// Generate a single-token input
    pub fn generate_single(&self, entry: &N) -> Vec<T>
    where
        T: Clone + Default,
    {
        let mut tokens = self.generator.generate(entry);
        if tokens.len() > 1 {
            tokens.truncate(1);
        }
        tokens
    }
}

/// Simple RNG for deterministic testing
struct SimpleRng {
    state: u64,
}

impl SimpleRng {
    fn new() -> Self {
        Self {
            state: 0x853c_49e6_748f_ea9b,
        }
    }

    fn with_seed(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next_u64(&mut self) -> u64 {
        // XorShift algorithm
        self.state ^= self.state << 13;
        self.state ^= self.state >> 7;
        self.state ^= self.state << 17;
        self.state
    }

    fn next_f64(&mut self) -> f64 {
        (self.next_u64() as f64) / (u64::MAX as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_rng() {
        let mut rng = SimpleRng::with_seed(12345);
        let v1 = rng.next_u64();
        let v2 = rng.next_u64();
        assert_ne!(v1, v2);
    }

    #[test]
    fn test_rng_determinism() {
        let mut rng1 = SimpleRng::with_seed(12345);
        let mut rng2 = SimpleRng::with_seed(12345);
        assert_eq!(rng1.next_u64(), rng2.next_u64());
        assert_eq!(rng1.next_u64(), rng2.next_u64());
    }
}
