#![no_main]
use libfuzzer_sys::fuzz_target;
use sipha::backend::ParserBackend;
#[cfg(feature = "backend-peg")]
use sipha::backend::peg::{PegConfig, PegParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::syntax::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum FuzzSyntaxKind {
    Char,
    Eof,
    Root,
}

impl SyntaxKind for FuzzSyntaxKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Root)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FuzzToken {
    kind: FuzzSyntaxKind,
    text: compact_str::CompactString,
}

impl Token for FuzzToken {
    type Kind = FuzzSyntaxKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> sipha::syntax::TextSize {
        sipha::syntax::TextSize::from(u32::try_from(self.text.len()).unwrap_or(0))
    }

    fn text(&self) -> compact_str::CompactString {
        self.text.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum FuzzNonTerminal {
    Root,
}

impl NonTerminal for FuzzNonTerminal {
    fn name(&self) -> &'static str {
        "Root"
    }
}

#[cfg(feature = "backend-peg")]
fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, ignoring invalid UTF-8 for simplicity
    let Ok(input) = std::str::from_utf8(data) else {
        return; // Skip invalid UTF-8 for now
    };

    // Build a simple grammar that accepts any character sequence
    let Ok(grammar) = GrammarBuilder::new()
        .entry_point(FuzzNonTerminal::Root)
        .rule(FuzzNonTerminal::Root, Expr::Empty)
        .build()
    else {
        return;
    };

    // Create tokens from input
    let tokens: Vec<FuzzToken> = input
        .chars()
        .map(|c| FuzzToken {
            kind: FuzzSyntaxKind::Char,
            text: c.to_string().into(),
        })
        .collect();

    // Create parser
    let Ok(mut parser) = PegParser::new(&grammar, PegConfig::default()) else {
        return;
    };

    // Parse - should not panic
    let _result = parser.parse(&tokens, FuzzNonTerminal::Root);
    // We don't assert on the result, just that parsing doesn't panic
});

#[cfg(not(feature = "backend-peg"))]
fuzz_target!(|_data: &[u8]| {
    // No-op when PEG backend is not available
});

