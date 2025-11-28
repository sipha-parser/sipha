#![no_main]
use compact_str::CompactString;
use libfuzzer_sys::fuzz_target;
use sipha::backend::ParserBackend;
use sipha::backend::glr::{GlrConfig, GlrParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::incremental::{IncrementalSession, TextEdit};
use sipha::syntax::{SyntaxKind, TextRange, TextSize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
enum GlrFuzzKind {
    Number,
    Plus,
    Expr,
}

impl SyntaxKind for GlrFuzzKind {
    fn is_terminal(self) -> bool {
        !matches!(self, Self::Expr)
    }

    fn is_trivia(self) -> bool {
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct GlrFuzzToken {
    kind: GlrFuzzKind,
    text: CompactString,
}

impl Token for GlrFuzzToken {
    type Kind = GlrFuzzKind;

    fn kind(&self) -> Self::Kind {
        self.kind
    }

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(0))
    }

    fn text(&self) -> CompactString {
        self.text.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum GlrNonTerminal {
    Expr,
    Tail,
}

impl NonTerminal for GlrNonTerminal {
    fn name(&self) -> &'static str {
        match self {
            Self::Expr => "Expr",
            Self::Tail => "Tail",
        }
    }
}

fuzz_target!(|data: &[u8]| {
    if data.is_empty() {
        return;
    }

    // Build a small expression grammar: Expr -> Number Tail; Tail -> + Number Tail | ε
    let Ok(grammar) = GrammarBuilder::new()
        .entry_point(GlrNonTerminal::Expr)
        .rule(
            GlrNonTerminal::Expr,
            Expr::Seq(vec![
                Expr::Token(GlrFuzzToken {
                    kind: GlrFuzzKind::Number,
                    text: "0".into(),
                }),
                Expr::Rule(GlrNonTerminal::Tail),
            ]),
        )
        .rule(
            GlrNonTerminal::Tail,
            Expr::Choice(vec![
                Expr::Seq(vec![
                    Expr::Token(GlrFuzzToken {
                        kind: GlrFuzzKind::Plus,
                        text: "+".into(),
                    }),
                    Expr::Token(GlrFuzzToken {
                        kind: GlrFuzzKind::Number,
                        text: "0".into(),
                    }),
                    Expr::Rule(GlrNonTerminal::Tail),
                ]),
                Expr::Empty,
            ]),
        )
        .build()
    else {
        return;
    };

    // Convert bytes into alternating numbers and plus tokens
    let mut numbers: Vec<u8> = data.to_vec();
    if numbers.is_empty() {
        numbers.push(0);
    }
    let mut tokens = Vec::new();
    for (idx, value) in numbers.iter().enumerate() {
        let text = CompactString::from((value % 10).to_string());
        tokens.push(GlrFuzzToken {
            kind: GlrFuzzKind::Number,
            text,
        });
        if idx + 1 != numbers.len() {
            tokens.push(GlrFuzzToken {
                kind: GlrFuzzKind::Plus,
                text: "+".into(),
            });
        }
    }

    let Ok(mut parser) = GlrParser::new(&grammar, GlrConfig::default()) else {
        return;
    };

    let entry = GlrNonTerminal::Expr;
    let result = parser.parse(&tokens, entry.clone());

    // Build a simple incremental session that tweaks the first number (if any)
    if let Some((first_idx, first_token)) = tokens
        .iter()
        .enumerate()
        .find(|(_, t)| t.kind == GlrFuzzKind::Number)
    {
        let mut new_tokens = tokens.clone();
        new_tokens[first_idx].text = "9".into();

        // Compute edit range covering the first number
        let mut start = TextSize::from(0);
        for t in tokens.iter().take(first_idx) {
            start += t.text_len();
        }
        let end = start + first_token.text_len();
        let edit = TextEdit::replace(TextRange::new(start, end), "9");
        let edits = vec![edit];
        let session = IncrementalSession::new(Some(&result.root), &edits);

        let _ = parser.parse_with_session(&new_tokens, entry, &session);
    }
});
