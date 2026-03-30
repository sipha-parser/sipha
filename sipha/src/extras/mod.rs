//! Optional higher-level utilities (feature-gated).

#[cfg(feature = "patterns")]
#[cfg_attr(docsrs, doc(cfg(feature = "patterns")))]
pub mod patterns;

#[cfg(feature = "analysis")]
#[cfg_attr(docsrs, doc(cfg(feature = "analysis")))]
pub mod analysis;

#[cfg(feature = "display")]
#[cfg_attr(docsrs, doc(cfg(feature = "display")))]
pub mod display;

#[cfg(feature = "sourcemap")]
#[cfg_attr(docsrs, doc(cfg(feature = "sourcemap")))]
pub mod sourcemap;

#[cfg(feature = "fmt")]
#[cfg_attr(docsrs, doc(cfg(feature = "fmt")))]
pub mod fmt;

#[cfg(feature = "diff")]
#[cfg_attr(docsrs, doc(cfg(feature = "diff")))]
pub mod diff;
