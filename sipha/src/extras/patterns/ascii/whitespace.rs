use crate::parse::builder::GrammarBuilder;

use super::classes;

/// ASCII whitespace byte: `[ \\t\\n\\r]`.
#[inline]
pub fn ws_byte(g: &mut GrammarBuilder) {
    g.class_with_label(classes::WS, "whitespace");
}

/// ASCII whitespace bytes: `[ \\t\\n\\r]*`.
#[inline]
pub fn ws0(g: &mut GrammarBuilder) {
    g.consume_while_class_with_label(classes::WS, "whitespace", 0);
}

/// ASCII whitespace bytes: `[ \\t\\n\\r]+`.
#[inline]
pub fn ws1(g: &mut GrammarBuilder) {
    g.consume_while_class1_with_label(classes::WS, "whitespace");
}

/// Horizontal whitespace bytes: `[ \\t]*`.
#[inline]
pub fn hws0(g: &mut GrammarBuilder) {
    g.consume_while_class_with_label(classes::HWS, "horizontal whitespace", 0);
}

/// Horizontal whitespace bytes: `[ \\t]+`.
#[inline]
pub fn hws1(g: &mut GrammarBuilder) {
    g.consume_while_class1_with_label(classes::HWS, "horizontal whitespace");
}
