//! Shared string pool for grammar rule names, tags, labels, and field names.
//!
//! [`StringInterner`] deduplicates strings during [`crate::parse::builder::GrammarBuilder`]
//! construction. The frozen [`StringTable`] is stored in [`crate::parse::builder::BuiltGraph`]
//! and borrowed by [`crate::parse::insn::ParseGraph`]. Indices are [`SymbolId`] values.

use std::collections::HashMap;

/// Index into a [`StringTable`] / [`StringInterner`] pool (stable for the lifetime of the table).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

/// Builder-time interner: deduplicates strings and assigns [`SymbolId`] indices.
pub struct StringInterner {
    pool: Vec<Box<str>>,
    lookup: HashMap<String, SymbolId>,
}

impl StringInterner {
    #[must_use]
    pub fn new() -> Self {
        Self {
            pool: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    /// Insert `s` if missing; return its stable id.
    pub fn intern(&mut self, s: &str) -> SymbolId {
        if let Some(&id) = self.lookup.get(s) {
            return id;
        }
        let id = SymbolId(u32::try_from(self.pool.len()).unwrap_or(0));
        self.pool.push(s.into());
        self.lookup.insert(s.to_string(), id);
        id
    }

    /// Drop the lookup map and return the frozen table for embedding in [`BuiltGraph`](crate::parse::builder::BuiltGraph).
    #[must_use]
    pub fn finish(self) -> StringTable {
        StringTable::Owned(self.pool)
    }

    /// Resolve a symbol while still building (e.g. error messages).
    #[must_use]
    pub fn resolve(&self, id: SymbolId) -> &str {
        self.pool
            .get(id.0 as usize)
            .map_or("?", std::convert::AsRef::as_ref)
    }
}

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
                .map_or("?", std::convert::AsRef::as_ref),
            Self::Static(s) => s.get(id.0 as usize).copied().unwrap_or("?"),
        }
    }

    /// For codegen: ordered pool of strings (for `static STR_POOL: &[&str]`).
    #[must_use]
    pub fn pool_strings_for_codegen(&self) -> Vec<&str> {
        match self {
            Self::Owned(v) => v.iter().map(std::convert::AsRef::as_ref).collect(),
            Self::Static(s) => s.to_vec(),
        }
    }

    /// Static grammar from codegen: pool is a `&[&str]` slice.
    #[must_use]
    pub const fn from_static_pool(pool: &'static [&'static str]) -> Self {
        Self::Static(pool)
    }
}
