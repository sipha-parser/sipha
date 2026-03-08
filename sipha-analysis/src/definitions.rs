//! Collect definition symbols from one or more syntax trees for LSP (document symbols, go-to-def).
//!
//! Grammar-agnostic: the caller supplies an extract callback that returns (name, kind, span)
//! for nodes that represent definitions. The callback receives the current node and the root
//! of the tree being walked (so it can e.g. restrict to top-level symbols).

use std::collections::HashMap;
use std::path::PathBuf;

use sipha::red::SyntaxNode;
use sipha::types::Span;
use sipha::walk::{Visitor, WalkOptions, WalkResult};

/// Collect (name, kind) -> (path, `start_byte`, `end_byte`) from multiple roots.
///
/// For each `(path, root)` the tree is walked; for each node, `extract(node, root)` is called.
/// When it returns `Some((name, kind, span))`, the definition is recorded. First occurrence
/// wins (later duplicates are skipped via `or_insert`).
///
/// `K` is the symbol kind (e.g. an enum: Class, Function, Global). It must be `Eq + Hash`
/// for use as map key.
#[must_use]
pub fn collect_definitions<K: Eq + std::hash::Hash>(
    roots: &[(PathBuf, SyntaxNode)],
    extract: impl Fn(&SyntaxNode, &SyntaxNode) -> Option<(String, K, Span)>,
) -> HashMap<(String, K), (PathBuf, u32, u32)> {
    let mut map = HashMap::new();
    for (path, root) in roots {
        let mut visitor = CollectDefinitionsVisitor {
            path: path.clone(),
            root: root.clone(),
            map: &mut map,
            extract: &extract,
        };
        let _ = root.walk(&mut visitor, &WalkOptions::nodes_only());
    }
    map
}

struct CollectDefinitionsVisitor<'a, K, F> {
    path: PathBuf,
    root: SyntaxNode,
    map: &'a mut HashMap<(String, K), (PathBuf, u32, u32)>,
    extract: &'a F,
}

impl<K: Eq + std::hash::Hash, F: Fn(&SyntaxNode, &SyntaxNode) -> Option<(String, K, Span)>> Visitor
    for CollectDefinitionsVisitor<'_, K, F>
{
    fn enter_node(&mut self, node: &SyntaxNode) -> WalkResult {
        if let Some((name, kind, span)) = (self.extract)(node, &self.root) {
            self.map
                .entry((name, kind))
                .or_insert_with(|| (self.path.clone(), span.start, span.end));
        }
        WalkResult::Continue(())
    }
}
