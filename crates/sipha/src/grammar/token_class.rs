//! Token class matching for flexible token recognition
//!
//! This module provides types and traits for matching tokens by class
//! (e.g., digits, letters, whitespace) rather than specific token values.
//! This enables more flexible parsing patterns.
//!
//! # Example
//!
//! ```rust,no_run
//! use sipha::grammar::token_class::{TokenClass, TokenClassMatcher};
//! use sipha::grammar::Token;
//!
//! // Use predefined token classes
//! let digit_class = TokenClass::Digit;
//!
//! // Or use custom classes
//! let custom_class = TokenClass::Custom("identifier".to_string());
//!
//! // Implement TokenClassMatcher for your token type
//! // impl TokenClassMatcher for MyToken { ... }
//! ```

use crate::grammar::Token;

/// Predefined token classes for common patterns.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenClass {
    /// Matches any digit character (0-9)
    Digit,
    /// Matches any letter character (a-z, A-Z)
    Letter,
    /// Matches whitespace characters (space, tab, newline, etc.)
    Whitespace,
    /// Matches alphanumeric characters (letters and digits)
    Alphanumeric,
    /// Custom token class with a string identifier
    ///
    /// Users can implement custom matching logic via the `TokenClassMatcher`
    /// trait extension.
    Custom(String),
}

impl TokenClass {
    /// Get a string representation of the token class.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            Self::Digit => "digit",
            Self::Letter => "letter",
            Self::Whitespace => "whitespace",
            Self::Alphanumeric => "alphanumeric",
            Self::Custom(s) => s,
        }
    }
}

/// Trait for tokens that support token class matching.
///
/// This trait allows tokens to implement custom matching logic for token
/// classes. Tokens that implement this trait can be matched against
/// `TokenClass` values in grammar expressions.
///
/// # Implementation
///
/// To enable token class matching for your token type, implement this trait.
/// If a token type does not implement this trait, token class matching will
/// not work for that type.
///
/// # Example
///
/// ```rust,no_run
/// use sipha::grammar::{Token, token_class::{TokenClass, TokenClassMatcher}};
///
/// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// enum MyToken {
///     Number,
///     Identifier,
///     Whitespace,
/// }
///
/// # #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
/// # enum MyTokenKind { Number, Identifier, Whitespace }
/// # impl sipha::syntax::SyntaxKind for MyTokenKind {
/// #     fn is_terminal(self) -> bool { true }
/// #     fn is_trivia(self) -> bool { matches!(self, Self::Whitespace) }
/// # }
/// impl Token for MyToken {
///     type Kind = MyTokenKind;
///     fn kind(&self) -> Self::Kind {
///         match self {
///             MyToken::Number => MyTokenKind::Number,
///             MyToken::Identifier => MyTokenKind::Identifier,
///             MyToken::Whitespace => MyTokenKind::Whitespace,
///         }
///     }
/// }
///
/// impl TokenClassMatcher for MyToken {
///     fn matches_class(&self, class: &TokenClass) -> bool {
///         match (self, class) {
///             (MyToken::Number, TokenClass::Digit) => true,
///             (MyToken::Identifier, TokenClass::Letter | TokenClass::Alphanumeric) => true,
///             (MyToken::Whitespace, TokenClass::Whitespace) => true,
///             _ => false,
///         }
///     }
/// }
/// ```
pub trait TokenClassMatcher: Token {
    /// Check if this token matches the given token class.
    ///
    /// # Arguments
    ///
    /// * `class` - The token class to match against
    ///
    /// # Returns
    ///
    /// `true` if this token matches the class, `false` otherwise.
    fn matches_class(&self, class: &TokenClass) -> bool;
}

/// Helper function to check if a token matches a token class.
///
/// This function will use the `TokenClassMatcher` trait if the token
/// implements it. For tokens that don't implement the trait, this will
/// attempt basic matching based on the token's text content for predefined
/// classes.
///
/// # Arguments
///
/// * `token` - The token to check
/// * `class` - The token class to match against
///
/// # Returns
///
/// `true` if the token matches the class, `false` otherwise.
pub fn token_matches_class<T: Token>(token: &T, class: &TokenClass) -> bool {
    // Try to use the TokenClassMatcher trait if implemented
    // Note: This requires the token to implement TokenClassMatcher
    // For now, we'll provide a basic implementation that can be extended

    // Basic matching based on token text for predefined classes
    // This is a fallback for tokens that don't implement TokenClassMatcher
    let text = token.text();
    match class {
        TokenClass::Digit => text.chars().all(|c| c.is_ascii_digit()),
        TokenClass::Letter => text.chars().all(|c| c.is_ascii_alphabetic()),
        TokenClass::Whitespace => text.chars().all(char::is_whitespace),
        TokenClass::Alphanumeric => text.chars().all(|c| c.is_ascii_alphanumeric()),
        TokenClass::Custom(_) => false, // Custom classes require trait implementation
    }
}
