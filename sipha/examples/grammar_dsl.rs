use sipha::LexKinds;
use sipha::RuleKinds;
use sipha::prelude::*;
use sipha::sipha_grammar;

#[derive(Debug, Clone, Copy, PartialEq, Eq, LexKinds)]
#[repr(u16)]
enum Lex {
    Ident,
    Ws,
}

impl LexKind for Lex {
    fn display_name(self) -> &'static str {
        match self {
            Lex::Ident => "IDENT",
            Lex::Ws => "WS",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, RuleKinds)]
#[sipha(lex = Lex)]
#[repr(u16)]
enum Rule {
    Root,
}

impl RuleKind for Rule {
    fn display_name(self) -> &'static str {
        "ROOT"
    }
}

fn kind_name(k: SyntaxKind) -> Option<&'static str> {
    Lex::from_syntax_kind(k)
        .map(LexKind::display_name)
        .or_else(|| Rule::from_syntax_kind(k).map(RuleKind::display_name))
}

fn main() {
    // This uses the `sipha_grammar!` macro from `sipha-macros` to generate
    // builder code. It’s handy for keeping grammars compact once you’re familiar
    // with the underlying `GrammarBuilder` API.
    let built: BuiltGraph = sipha_grammar! {
        @trivia ws;
        @start start;

        #[lexer] ws = #[trivia(Lex::Ws)] ( (" " | "\t" | "\n" | "\r")* );

        // The DSL intentionally keeps atoms simple (rule calls + string literals).
        // For byte classes/ranges, use `GrammarBuilder` directly.
        #[lexer] ident = #[token(Lex::Ident)] ( "alpha" | "beta" | "gamma" );

        #[parser] start = #[node(Rule::Root)] ( ident );
    };

    let graph = built.as_graph();
    let mut engine = Engine::new();
    let src = b"alpha";
    let out = engine.parse(&graph, src).unwrap_or_else(|e| {
        if let ParseError::NoMatch(d) = e {
            let li = LineIndex::new(src);
            eprintln!(
                "{}",
                d.format_with_source(src, &li, Some(&graph.literals), Some(&graph))
            );
        } else {
            eprintln!("{e}");
        }
        std::process::exit(1);
    });
    let doc = ParsedDoc::from_slice(src, &out).unwrap();
    println!(
        "{}",
        sipha::tree::tree_display::format_syntax_tree(
            doc.root(),
            &TreeDisplayOptions::default(),
            |k| kind_name(k).unwrap_or("?").to_string()
        )
    );
}
