//! Resolve rule / label / class names for display diagnostics.

use crate::parse::string_table::SymbolId;
use crate::types::RuleId;

/// Tables used to turn numeric ids in [`Expected`](crate::diagnostics::error::Expected) into readable strings.
///
/// Implemented by [`crate::parse::insn::ParseGraph`] (interned strings) and
/// [`SliceGrammarNames`] for static `&[&str]` tables (e.g. tests or hand-written glue).
pub trait GrammarNames {
    fn rule_name(&self, id: RuleId) -> Option<&str>;

    /// User-facing text for [`Expected::Rule`](crate::diagnostics::error::Expected::Rule) in parse errors.
    ///
    /// Defaults to [`rule_name`](Self::rule_name). Grammars built with [`crate::parse::builder::GrammarBuilder`]
    /// store a separate interned string (often derived from the rule name, or set via
    /// [`parser_rule_with_diagnostic`](crate::parse::builder::GrammarBuilder::parser_rule_with_diagnostic)).
    fn rule_diagnostic_display(&self, id: RuleId) -> Option<&str> {
        self.rule_name(id)
    }

    fn expected_label(&self, id: u32) -> Option<&str>;
    fn class_label(&self, label_id: u32) -> Option<&str>;
    /// Resolve an interned string from the grammar's [`StringTable`](crate::parse::string_table::StringTable).
    ///
    /// Used for dynamic hints and tracing labels.
    fn resolve_symbol(&self, _id: SymbolId) -> Option<&str> {
        None
    }
}

/// Rule, expected-label, and class-label slices (e.g. from generated `static` tables).
#[derive(Clone, Copy, Debug)]
pub struct SliceGrammarNames<'a> {
    pub rule_names: &'a [&'a str],
    pub expected_labels: &'a [&'a str],
    pub class_labels: &'a [&'a str],
}

impl GrammarNames for SliceGrammarNames<'_> {
    fn rule_name(&self, id: RuleId) -> Option<&str> {
        self.rule_names.get(id as usize).copied()
    }

    fn expected_label(&self, id: u32) -> Option<&str> {
        self.expected_labels.get(id as usize).copied()
    }

    fn class_label(&self, label_id: u32) -> Option<&str> {
        self.class_labels.get(label_id as usize).copied()
    }

    fn resolve_symbol(&self, _id: SymbolId) -> Option<&str> {
        None
    }
}
