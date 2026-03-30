//! Grammar-agnostic analysis helpers for sipha syntax trees.
//!
//! Provides scope extents (offset → scope) and definition collection for LSP
//! (go-to-def, references, document symbols). Grammar-specific logic is passed
//! via callbacks.

mod definitions;
mod scope_extents;

pub use definitions::collect_definitions;
pub use scope_extents::{build_scope_extents, scope_at_offset};
