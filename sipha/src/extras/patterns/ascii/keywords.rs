use crate::{parse::builder::GrammarBuilder, types::IntoSyntaxKind};

use super::classes;

/// ASCII keyword bytes plus "not followed by IDENT_CONT" boundary check.
///
/// Commonly used so `if` does not match the start of `ifdef`.
#[inline]
pub fn keyword_bytes(g: &mut GrammarBuilder, word: &'static [u8]) {
    g.literal(word);
    g.not_followed_by(|g| {
        g.class(classes::IDENT_CONT);
    });
}

/// Like [`keyword_bytes`], but wrapped in a token.
#[inline]
pub fn keyword_token<K: IntoSyntaxKind>(g: &mut GrammarBuilder, kind: K, word: &'static [u8]) {
    g.token(kind, |g| {
        keyword_bytes(g, word);
    });
}

/// N-way keyword choice, using [`keyword_bytes`] for each alternative.
pub fn any_keyword_bytes(g: &mut GrammarBuilder, words: &[&'static [u8]]) {
    match words {
        [] => {}
        [w] => keyword_bytes(g, w),
        [w, rest @ ..] => g.choice(
            |g| {
                keyword_bytes(g, w);
            },
            |g| {
                any_keyword_bytes(g, rest);
            },
        ),
    }
}
