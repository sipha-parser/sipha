//! # Arena Allocation Module
//!
//! This module provides arena-based allocation for syntax trees, enabling
//! dramatic performance improvements for batch parsing scenarios.
//!
//! ## Overview
//!
//! Arena allocation eliminates per-node reference counting overhead by allocating
//! all nodes from a contiguous memory region. This provides:
//!
//! - **Faster allocation**: Bump allocation is O(1) with no synchronization
//! - **Better cache locality**: Nodes are stored contiguously in memory
//! - **Batch deallocation**: Entire arena is freed at once
//!
//! ## Usage Modes
//!
//! The arena system supports two usage modes:
//!
//! 1. **Arena-allocated trees** (`GreenNodeRef`): For batch parsing where you don't
//!    need reference counting. Nodes are cheap to copy but tied to arena lifetime.
//!
//! 2. **Shared trees** (`SharedGreenNode`): For incremental parsing where you need
//!    to share nodes across parse sessions. Uses `Arc` for reference counting.
//!
//! ## Example
//!
//! ```rust,ignore
//! use sipha::arena::TreeArena;
//!
//! // Create an arena for batch parsing
//! let arena = TreeArena::new();
//!
//! // Allocate nodes in the arena
//! let node = arena.alloc_node(kind, children, text_len);
//!
//! // All nodes are freed when arena is dropped
//! drop(arena);
//! ```

mod tree_arena;

pub use tree_arena::*;

use std::cell::Cell;

/// Statistics about arena usage
#[derive(Debug, Clone, Copy, Default)]
pub struct ArenaStats {
    /// Total bytes allocated
    pub bytes_allocated: usize,
    /// Number of nodes allocated
    pub nodes_allocated: usize,
    /// Number of tokens allocated
    pub tokens_allocated: usize,
}

/// Trait for types that can be allocated in an arena
pub trait ArenaAllocatable: Sized {
    /// Allocate this value in the given arena
    fn alloc_in(self, arena: &TreeArena) -> &Self;
}

/// A simple bump allocator for arena allocation
///
/// This is a thin wrapper around `bumpalo::Bump` that provides
/// additional tracking and convenience methods.
#[cfg(feature = "arena")]
pub struct BumpArena {
    bump: bumpalo::Bump,
    stats: Cell<ArenaStats>,
}

#[cfg(feature = "arena")]
impl BumpArena {
    /// Create a new bump arena
    #[must_use]
    pub fn new() -> Self {
        Self {
            bump: bumpalo::Bump::new(),
            stats: Cell::new(ArenaStats::default()),
        }
    }

    /// Create a new bump arena with pre-allocated capacity
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            bump: bumpalo::Bump::with_capacity(capacity),
            stats: Cell::new(ArenaStats::default()),
        }
    }

    /// Allocate a value in the arena
    pub fn alloc<T>(&self, value: T) -> &T {
        let mut stats = self.stats.get();
        stats.bytes_allocated += std::mem::size_of::<T>();
        self.stats.set(stats);
        self.bump.alloc(value)
    }

    /// Allocate a slice in the arena
    pub fn alloc_slice<T: Copy>(&self, slice: &[T]) -> &[T] {
        let mut stats = self.stats.get();
        stats.bytes_allocated += std::mem::size_of_val(slice);
        self.stats.set(stats);
        self.bump.alloc_slice_copy(slice)
    }

    /// Get arena statistics
    #[must_use]
    pub fn stats(&self) -> ArenaStats {
        self.stats.get()
    }

    /// Get the total bytes allocated
    #[must_use]
    pub fn allocated_bytes(&self) -> usize {
        self.bump.allocated_bytes()
    }

    /// Reset the arena, deallocating all allocations
    pub fn reset(&mut self) {
        self.bump.reset();
        self.stats.set(ArenaStats::default());
    }
}

#[cfg(feature = "arena")]
impl Default for BumpArena {
    fn default() -> Self {
        Self::new()
    }
}

/// Fallback arena implementation when bumpalo is not available
///
/// This uses standard allocation but provides the same API.
#[cfg(not(feature = "arena"))]
pub struct BumpArena {
    // We can't actually do bump allocation without bumpalo,
    // so this is just a marker type that provides the same API
    // but uses standard allocation.
    _private: (),
}

#[cfg(not(feature = "arena"))]
impl BumpArena {
    /// Create a new arena (no-op without bumpalo)
    #[must_use]
    pub const fn new() -> Self {
        Self { _private: () }
    }

    /// Create a new arena with capacity (no-op without bumpalo)
    #[must_use]
    pub const fn with_capacity(_capacity: usize) -> Self {
        Self { _private: () }
    }

    /// Get arena statistics (always zero without bumpalo)
    #[must_use]
    pub const fn stats(&self) -> ArenaStats {
        ArenaStats {
            bytes_allocated: 0,
            nodes_allocated: 0,
            tokens_allocated: 0,
        }
    }

    /// Get allocated bytes (always zero without bumpalo)
    #[must_use]
    pub const fn allocated_bytes(&self) -> usize {
        0
    }

    /// Reset the arena (no-op without bumpalo)
    pub fn reset(&mut self) {}
}

#[cfg(not(feature = "arena"))]
impl Default for BumpArena {
    fn default() -> Self {
        Self::new()
    }
}
