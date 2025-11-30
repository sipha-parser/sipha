#![no_main]
use libfuzzer_sys::fuzz_target;
use sipha::backend::ParserBackend;
use sipha::backend::ll::{LlConfig, LlParser};
use sipha::grammar::{Expr, GrammarBuilder, NonTerminal, Token};
use sipha::incremental::{IncrementalParser, TextEdit};
use sipha::syntax::SyntaxKind;
use sipha::syntax::{TextRange, TextSize};

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

    fn text_len(&self) -> TextSize {
        TextSize::from(u32::try_from(self.text.len()).unwrap_or(0))
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

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };

    // Build grammar
    let Ok(grammar) = GrammarBuilder::new()
        .entry_point(FuzzNonTerminal::Root)
        .rule(FuzzNonTerminal::Root, Expr::empty())
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
    let Ok(parser) = LlParser::new(&grammar, LlConfig::default()) else {
        return;
    };

    // Initial parse
    let mut incremental_parser = IncrementalParser::new(parser);
    let result1 = incremental_parser.parse_incremental(
        &tokens,
        None,
        &[],
        FuzzNonTerminal::Root,
        Some(&grammar),
    );

    // Create a simple edit: replace a portion of the input
    if !input.is_empty() {
        let edit_start = 0;
        let edit_end = (input.len() / 2).min(input.len());
        let edit_range = TextRange::new(
            TextSize::from(u32::try_from(edit_start).unwrap_or(0)),
            TextSize::from(u32::try_from(edit_end).unwrap_or(0)),
        );

        let edit = TextEdit::replace(edit_range, "X");

        // Build edited text to keep tokens/edits aligned
        let mut edited_input = input.to_string();
        if edit_start < edit_end {
            edited_input.replace_range(edit_start..edit_end, "X");
        } else {
            edited_input.insert(edit_start, 'X');
        }
        let final_tokens: Vec<FuzzToken> = edited_input
            .chars()
            .map(|c| FuzzToken {
                kind: FuzzSyntaxKind::Char,
                text: c.to_string().into(),
            })
            .collect();
        let edits = [edit];

        // Incremental parse
        let result2 = incremental_parser.parse_incremental(
            &final_tokens,
            Some(&result1.root),
            &edits,
            FuzzNonTerminal::Root,
            Some(&grammar),
        );

        // Full reparse for comparison
        let Ok(mut parser2) = LlParser::new(&grammar, LlConfig::default()) else {
            return;
        };
        let result3 = parser2.parse(&final_tokens, FuzzNonTerminal::Root);

        // Compare: incremental and full parse should produce identical trees
        // (at least in structure - we compare text length as a simple check)
        assert_eq!(
            result2.root.text_len(),
            result3.root.text_len(),
            "Incremental and full parse should produce trees of same size"
        );
    }
});
