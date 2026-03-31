use crate::parse::builder::GrammarBuilder;

use super::classes;

/// ASCII identifier: `IDENT_START IDENT_CONT*`.
#[inline]
pub fn ident(g: &mut GrammarBuilder) {
    g.class_with_label(classes::IDENT_START, "identifier start");
    g.consume_while_class_with_label(classes::IDENT_CONT, "identifier continuation", 0);
}
