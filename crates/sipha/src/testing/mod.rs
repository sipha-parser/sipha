//! # Testing Utilities
//!
//! This module provides utilities for testing parsers and grammars,
//! including property-based testing generators and fuzzing support.
//!
//! ## Property-Based Testing
//!
//! The generators in this module work with the `proptest` crate to generate
//! random valid inputs based on grammar definitions.
//!
//! ## Fuzzing Support
//!
//! Grammar-aware fuzzing utilities can generate both valid and edge-case inputs
//! to stress test your parser implementation.

pub mod generators;
pub mod snapshot;

pub use generators::*;
pub use snapshot::*;
