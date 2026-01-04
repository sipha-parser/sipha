//! # Parser Module
//!
//! This module provides parser traits and interfaces.
//!
//! Actual parser implementations are provided by backend modules (e.g., `backend::ll`, `backend::lr`).
//! This module provides the unified infrastructure that all backends share.
//!
//! ## Key Components
//!
//! - [`driver`] - Unified parser driver trait for all backends
//! - [`engine`] - Parser engine that combines drivers with common infrastructure
//! - [`recovery`] - Error recovery strategies
//!
//! ## Usage
//!
//! ```rust,ignore
//! use sipha::parser::{ParserEngine, EngineConfig};
//! use sipha::parser::recovery::RecoveryBuilder;
//!
//! // Create an engine with error recovery
//! let recovery = RecoveryBuilder::new()
//!     .sync_on([MyKind::Semicolon, MyKind::CloseBrace])
//!     .build_combined();
//!
//! let mut engine = ParserEngine::new()
//!     .with_recovery(Box::new(recovery));
//!
//! // Parse using a driver
//! let result = engine.parse(&mut driver, tokens, entry_point);
//! ```

pub mod driver;
pub mod engine;
pub mod parallel;
pub mod recovery;

pub use driver::{
    DriverResult, EngineConfig, IncrementalDriver, ParallelDriver, ParseContext, ParseEvent,
    ParseEventHandler, ParseStats, ParserDriver,
};
pub use engine::{ParseResult, ParserEngine, RecoveryContext};
pub use parallel::{
    FileParseResult, ParallelConfig, ParallelParser, ParseBatch, ParseSummary, aggregate_results,
};
pub use recovery::{
    CombinedRecovery, ErrorRecoveryStrategy, MinimalCostRecovery, PanicModeRecovery,
    PhraseRecovery, RecoveryAction, RecoveryBuilder, SkipRecovery,
};
