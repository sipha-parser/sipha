//! Shared string pool for grammar rule names, tags, labels, and field names.
//!
//! [`StringInterner`] deduplicates strings during [`crate::parse::builder::GrammarBuilder`]
//! construction. The frozen [`StringTable`] is stored in [`crate::parse::builder::BuiltGraph`]
//! and borrowed by [`crate::parse::insn::ParseGraph`]. Indices are [`SymbolId`] values.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec::Vec};
#[cfg(feature = "std")]
use lasso::{Key, Rodeo, Spur};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

/// Index into a [`StringTable`] / [`StringInterner`] pool (stable for the lifetime of the table).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

/// Builder-time interner: deduplicates strings and assigns [`SymbolId`] indices.
#[cfg(feature = "std")]
pub struct StringInterner {
    rodeo: Rodeo<Spur>,
}

#[cfg(feature = "std")]
impl StringInterner {
    #[must_use]
    pub fn new() -> Self {
        Self {
            rodeo: Rodeo::new(),
        }
    }

    /// Insert `s` if missing; return its stable id.
    pub fn intern(&mut self, s: &str) -> SymbolId {
        let key = self.rodeo.get_or_intern(s);
        SymbolId(u32::try_from(key.into_usize()).unwrap_or(0))
    }

    /// Drop the lookup map and return the frozen table for embedding in [`BuiltGraph`](crate::parse::builder::BuiltGraph).
    #[must_use]
    pub fn finish(self) -> StringTable {
        let mut pool: Vec<Box<str>> = vec![Box::<str>::from(""); self.rodeo.len()];
        for (key, s) in self.rodeo.iter() {
            let idx = key.into_usize();
            if idx < pool.len() {
                pool[idx] = s.into();
            }
        }
        StringTable::Owned(pool)
    }

    /// Resolve a symbol while still building (e.g. error messages).
    #[must_use]
    pub fn resolve(&self, id: SymbolId) -> &str {
        Spur::try_from_usize(id.0 as usize)
            .and_then(|k| self.rodeo.try_resolve(&k))
            .unwrap_or("?")
    }
}

#[cfg(feature = "std")]
impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

/// Frozen string pool: resolve [`SymbolId`] to `&str`.
#[derive(Clone, Debug)]
pub enum StringTable {
    Owned(Vec<Box<str>>),
    /// For generated grammars: `STR_POOL` as `&[&str]`, [`SymbolId`] is an index into this slice.
    Static(&'static [&'static str]),
}

impl StringTable {
    #[must_use]
    pub fn resolve(&self, id: SymbolId) -> &str {
        match self {
            Self::Owned(v) => v
                .get(id.0 as usize)
                .map_or("?", core::convert::AsRef::as_ref),
            Self::Static(s) => s.get(id.0 as usize).copied().unwrap_or("?"),
        }
    }

    /// For codegen: ordered pool of strings (for `static STR_POOL: &[&str]`).
    #[must_use]
    pub fn pool_strings_for_codegen(&self) -> Vec<&str> {
        match self {
            Self::Owned(v) => v.iter().map(core::convert::AsRef::as_ref).collect(),
            Self::Static(s) => s.to_vec(),
        }
    }

    /// Static grammar from codegen: pool is a `&[&str]` slice.
    #[must_use]
    pub const fn from_static_pool(pool: &'static [&'static str]) -> Self {
        Self::Static(pool)
    }
}
