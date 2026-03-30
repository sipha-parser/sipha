use crate::parse::builder::GrammarBuilder;

use super::classes;

/// ASCII identifier: `IDENT_START IDENT_CONT*`.
#[inline]
pub fn ident(g: &mut GrammarBuilder) {
    g.class(classes::IDENT_START);
    g.zero_or_more(|g| {
        g.class(classes::IDENT_CONT);
    });
}

