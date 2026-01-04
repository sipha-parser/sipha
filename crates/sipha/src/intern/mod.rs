//! # String Interning Module
//!
//! This module provides string interning infrastructure for efficient string
//! storage and comparison in the parser library.
//!
//! ## Overview
//!
//! String interning stores each unique string only once and returns a small
//! identifier (key) that can be used to retrieve the string later. This provides:
//!
//! - **Memory efficiency**: Duplicate strings share storage
//! - **O(1) comparison**: Compare keys instead of string contents
//! - **Cache-friendly**: Small keys are more cache-friendly than string pointers
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::intern::Interner;
//!
//! let mut interner = Interner::new();
//!
//! // Intern strings
//! let key1 = interner.intern("hello");
//! let key2 = interner.intern("hello");  // Same key as key1
//! let key3 = interner.intern("world");  // Different key
//!
//! // Compare keys (O(1))
//! assert_eq!(key1, key2);
//! assert_ne!(key1, key3);
//!
//! // Resolve back to string
//! assert_eq!(interner.resolve(key1), "hello");
//! ```

use lasso::{Rodeo, Spur, ThreadedRodeo};
use std::fmt;
use std::hash::{Hash, Hasher};

/// An interned string key
///
/// This is a lightweight handle to an interned string. It can be cheaply
/// copied and compared. To get the actual string content, use
/// [`Interner::resolve`] or [`ThreadSafeInterner::resolve`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct InternedStr(Spur);

impl InternedStr {
    /// Create an `InternedStr` from a raw `Spur`
    ///
    /// This is primarily for internal use when working with lasso directly.
    #[must_use]
    pub const fn from_spur(spur: Spur) -> Self {
        Self(spur)
    }

    /// Get the raw `Spur` value
    ///
    /// This is primarily for internal use when working with lasso directly.
    #[must_use]
    pub const fn as_spur(&self) -> Spur {
        self.0
    }

    /// Resolve this interned string using the given interner
    #[must_use]
    pub fn resolve<'a>(&self, interner: &'a Interner) -> &'a str {
        interner.resolve(*self)
    }

    /// Resolve this interned string using a thread-safe interner
    #[must_use]
    pub fn resolve_threaded<'a>(&self, interner: &'a ThreadSafeInterner) -> &'a str {
        interner.resolve(*self)
    }
}

impl Hash for InternedStr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl fmt::Debug for InternedStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "InternedStr({:?})", self.0)
    }
}

/// A string interner for single-threaded use
///
/// This interner is optimized for single-threaded access and provides
/// the fastest interning and resolution operations.
pub struct Interner {
    rodeo: Rodeo,
}

impl Interner {
    /// Create a new empty interner
    #[must_use]
    pub fn new() -> Self {
        Self {
            rodeo: Rodeo::new(),
        }
    }

    /// Create a new interner with pre-allocated capacity
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        use std::num::NonZero;
        Self {
            rodeo: Rodeo::with_capacity(lasso::Capacity::new(
                capacity,
                NonZero::new(capacity).unwrap_or(NonZero::new(1).unwrap()),
            )),
        }
    }

    /// Intern a string, returning its key
    ///
    /// If the string has already been interned, returns the existing key.
    /// Otherwise, stores the string and returns a new key.
    pub fn intern(&mut self, s: &str) -> InternedStr {
        InternedStr(self.rodeo.get_or_intern(s))
    }

    /// Intern a static string, returning its key
    ///
    /// This is more efficient for string literals as it avoids allocation.
    pub fn intern_static(&mut self, s: &'static str) -> InternedStr {
        InternedStr(self.rodeo.get_or_intern_static(s))
    }

    /// Get the key for an already-interned string, if it exists
    #[must_use]
    pub fn get(&self, s: &str) -> Option<InternedStr> {
        self.rodeo.get(s).map(InternedStr)
    }

    /// Resolve an interned string key to its string content
    ///
    /// # Panics
    ///
    /// Panics if the key was not created by this interner.
    #[must_use]
    pub fn resolve(&self, key: InternedStr) -> &str {
        self.rodeo.resolve(&key.0)
    }

    /// Try to resolve an interned string key
    ///
    /// Returns `None` if the key was not created by this interner.
    #[must_use]
    pub fn try_resolve(&self, key: InternedStr) -> Option<&str> {
        self.rodeo.try_resolve(&key.0)
    }

    /// Get the number of interned strings
    #[must_use]
    pub fn len(&self) -> usize {
        self.rodeo.len()
    }

    /// Check if the interner is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rodeo.is_empty()
    }

    /// Check if the interner contains a string
    #[must_use]
    pub fn contains(&self, s: &str) -> bool {
        self.rodeo.contains(s)
    }

    /// Iterate over all interned strings
    pub fn iter(&self) -> impl Iterator<Item = (InternedStr, &str)> {
        self.rodeo.iter().map(|(k, v)| (InternedStr(k), v))
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for Interner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interner")
            .field("len", &self.rodeo.len())
            .finish()
    }
}

/// A thread-safe string interner
///
/// This interner can be safely shared across threads and provides
/// concurrent interning and resolution operations.
pub struct ThreadSafeInterner {
    rodeo: ThreadedRodeo,
}

impl ThreadSafeInterner {
    /// Create a new empty thread-safe interner
    #[must_use]
    pub fn new() -> Self {
        Self {
            rodeo: ThreadedRodeo::new(),
        }
    }

    /// Create a new thread-safe interner with pre-allocated capacity
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        use std::num::NonZero;
        Self {
            rodeo: ThreadedRodeo::with_capacity(lasso::Capacity::new(
                capacity,
                NonZero::new(capacity).unwrap_or(NonZero::new(1).unwrap()),
            )),
        }
    }

    /// Intern a string, returning its key
    ///
    /// This operation is thread-safe and can be called concurrently.
    pub fn intern(&self, s: &str) -> InternedStr {
        InternedStr(self.rodeo.get_or_intern(s))
    }

    /// Intern a static string, returning its key
    ///
    /// This is more efficient for string literals as it avoids allocation.
    pub fn intern_static(&self, s: &'static str) -> InternedStr {
        InternedStr(self.rodeo.get_or_intern_static(s))
    }

    /// Get the key for an already-interned string, if it exists
    #[must_use]
    pub fn get(&self, s: &str) -> Option<InternedStr> {
        self.rodeo.get(s).map(InternedStr)
    }

    /// Resolve an interned string key to its string content
    ///
    /// # Panics
    ///
    /// Panics if the key was not created by this interner.
    #[must_use]
    pub fn resolve(&self, key: InternedStr) -> &str {
        self.rodeo.resolve(&key.0)
    }

    /// Try to resolve an interned string key
    ///
    /// Returns `None` if the key was not created by this interner.
    #[must_use]
    pub fn try_resolve(&self, key: InternedStr) -> Option<&str> {
        self.rodeo.try_resolve(&key.0)
    }

    /// Get the number of interned strings
    #[must_use]
    pub fn len(&self) -> usize {
        self.rodeo.len()
    }

    /// Check if the interner is empty
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rodeo.is_empty()
    }

    /// Check if the interner contains a string
    #[must_use]
    pub fn contains(&self, s: &str) -> bool {
        self.rodeo.contains(s)
    }
}

impl Default for ThreadSafeInterner {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for ThreadSafeInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ThreadSafeInterner")
            .field("len", &self.rodeo.len())
            .finish()
    }
}

// SAFETY: ThreadedRodeo is already Send + Sync, and ThreadSafeInterner
// only contains a ThreadedRodeo. This implementation just delegates
// the thread-safety guarantees that ThreadedRodeo already provides.
#[allow(clippy::non_send_fields_in_send_ty, unsafe_code)]
unsafe impl Send for ThreadSafeInterner {}
#[allow(clippy::non_send_fields_in_send_ty, unsafe_code)]
unsafe impl Sync for ThreadSafeInterner {}

/// Global interner for convenient access
///
/// This is a thread-safe interner that can be accessed from anywhere.
/// Useful for interning keywords and other commonly-used strings.
#[cfg(feature = "parallel")]
static GLOBAL_INTERNER: std::sync::OnceLock<ThreadSafeInterner> = std::sync::OnceLock::new();

/// Get the global interner
///
/// This lazily initializes the global interner on first access.
#[cfg(feature = "parallel")]
#[must_use]
pub fn global_interner() -> &'static ThreadSafeInterner {
    GLOBAL_INTERNER.get_or_init(ThreadSafeInterner::new)
}

/// Intern a string in the global interner
#[cfg(feature = "parallel")]
pub fn intern_global(s: &str) -> InternedStr {
    global_interner().intern(s)
}

/// A token with interned text
///
/// This is an alternative to the standard `Token` that uses interned
/// strings for the text content, reducing memory usage when many tokens
/// share the same text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InternedToken<K> {
    /// The kind of token
    pub kind: K,
    /// The interned text of the token
    pub text: InternedStr,
    /// The range of the token in the source
    pub range: crate::syntax::TextRange,
}

impl<K> InternedToken<K> {
    /// Create a new interned token
    #[must_use]
    pub const fn new(kind: K, text: InternedStr, range: crate::syntax::TextRange) -> Self {
        Self { kind, text, range }
    }

    /// Get the text of this token by resolving from the interner
    #[must_use]
    pub fn resolve_text<'a>(&self, interner: &'a Interner) -> &'a str {
        self.text.resolve(interner)
    }

    /// Get the text length (requires resolving the string)
    #[must_use]
    pub fn text_len(&self, interner: &Interner) -> crate::syntax::TextSize {
        let text = self.text.resolve(interner);
        crate::syntax::TextSize::from(u32::try_from(text.len()).unwrap_or(u32::MAX))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner_basic() {
        let mut interner = Interner::new();

        let key1 = interner.intern("hello");
        let key2 = interner.intern("hello");
        let key3 = interner.intern("world");

        // Same string should produce same key
        assert_eq!(key1, key2);
        // Different strings should produce different keys
        assert_ne!(key1, key3);

        // Should be able to resolve back
        assert_eq!(interner.resolve(key1), "hello");
        assert_eq!(interner.resolve(key3), "world");
    }

    #[test]
    fn test_interner_get() {
        let mut interner = Interner::new();

        // Not interned yet
        assert!(interner.get("hello").is_none());

        // Intern it
        let key = interner.intern("hello");

        // Now should be found
        assert_eq!(interner.get("hello"), Some(key));
    }

    #[test]
    fn test_interner_static() {
        let mut interner = Interner::new();

        let key = interner.intern_static("hello");
        assert_eq!(interner.resolve(key), "hello");
    }

    #[test]
    fn test_interner_len() {
        let mut interner = Interner::new();

        assert!(interner.is_empty());
        assert_eq!(interner.len(), 0);

        interner.intern("a");
        assert!(!interner.is_empty());
        assert_eq!(interner.len(), 1);

        interner.intern("b");
        assert_eq!(interner.len(), 2);

        // Duplicate shouldn't increase length
        interner.intern("a");
        assert_eq!(interner.len(), 2);
    }

    #[test]
    fn test_interner_contains() {
        let mut interner = Interner::new();

        assert!(!interner.contains("hello"));
        interner.intern("hello");
        assert!(interner.contains("hello"));
    }

    #[test]
    fn test_interner_iter() {
        let mut interner = Interner::new();

        interner.intern("a");
        interner.intern("b");
        interner.intern("c");

        let items: Vec<_> = interner.iter().map(|(_, s)| s).collect();
        assert_eq!(items.len(), 3);
        assert!(items.contains(&"a"));
        assert!(items.contains(&"b"));
        assert!(items.contains(&"c"));
    }

    #[test]
    fn test_thread_safe_interner() {
        let interner = ThreadSafeInterner::new();

        let key1 = interner.intern("hello");
        let key2 = interner.intern("hello");

        assert_eq!(key1, key2);
        assert_eq!(interner.resolve(key1), "hello");
    }

    #[test]
    fn test_interned_str_hash() {
        use std::collections::HashSet;

        let mut interner = Interner::new();
        let key1 = interner.intern("hello");
        let key2 = interner.intern("hello");
        let key3 = interner.intern("world");

        let mut set = HashSet::new();
        set.insert(key1);

        // Same key should be found
        assert!(set.contains(&key2));
        // Different key should not be found
        assert!(!set.contains(&key3));
    }
}
