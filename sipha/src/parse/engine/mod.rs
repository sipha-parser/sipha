//! Parse engine (VM).

mod dispatch;
mod engine_impl;
mod error;
mod flags;
mod frames;
mod observer;
mod output;
#[cfg(feature = "trace")]
mod trace;
#[cfg(feature = "trace")]
mod tracer;
mod tree_events;
mod utf8;
mod vm;

pub use engine_impl::Engine;
pub use error::{BadGraphKind, ParseError, RecoverMultiResult};
pub use observer::VmObserver;
pub use output::ParseOutput;
#[cfg(feature = "trace")]
pub use trace::{TraceBuffer, TraceStep};
#[cfg(feature = "trace")]
pub use tracer::{ParseTracer, PrintTracer};
