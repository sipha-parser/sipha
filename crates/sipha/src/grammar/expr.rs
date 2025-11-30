//! Grammar expression types
//!
//! This module provides the main `Expr` type, which is a type alias for `ExtendedExpr`.
//! It also re-exports `CoreExpr` and related types.

pub use super::core_expr::{CoreExpr, TrailingSeparator};
pub use super::extended_expr::ExtendedExpr;

/// Grammar expression representing production rules
///
/// This is a type alias for `ExtendedExpr`, which includes both core expressions
/// (universally supported) and extended expressions (may require transformation
/// for some backends).
pub type Expr<T, N> = ExtendedExpr<T, N>;

// All builder methods and semantic analysis are implemented on ExtendedExpr
// and are available through the Expr type alias
