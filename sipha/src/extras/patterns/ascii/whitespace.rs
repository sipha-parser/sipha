use crate::parse::builder::GrammarBuilder;

use super::classes;

/// ASCII whitespace byte: `[ \\t\\n\\r]`.
#[inline]
pub fn ws_byte(g: &mut GrammarBuilder) {
    g.class(classes::WS);
}

/// ASCII whitespace bytes: `[ \\t\\n\\r]*`.
#[inline]
pub fn ws0(g: &mut GrammarBuilder) {
    g.zero_or_more(|g| {
        ws_byte(g);
    });
}

/// ASCII whitespace bytes: `[ \\t\\n\\r]+`.
#[inline]
pub fn ws1(g: &mut GrammarBuilder) {
    ws_byte(g);
    ws0(g);
}

/// Horizontal whitespace bytes: `[ \\t]*`.
#[inline]
pub fn hws0(g: &mut GrammarBuilder) {
    g.zero_or_more(|g| {
        g.class(classes::HWS);
    });
}

/// Horizontal whitespace bytes: `[ \\t]+`.
#[inline]
pub fn hws1(g: &mut GrammarBuilder) {
    g.class(classes::HWS);
    hws0(g);
}

