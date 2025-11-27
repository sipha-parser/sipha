//! # Parser Module
//!
//! This module provides parser traits and interfaces.
//!
//! Actual parser implementations are provided by backend modules (e.g., `backend::ll`, `backend::lr`).
//! This module currently re-exports parser state types but may be extended in the future
//! with parser-level abstractions that work across different backends.
