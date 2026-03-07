//! # Red Tree
//!
//! A position-aware cursor layer on top of the immutable [`GreenNode`] tree.
//!
//! ## Design
//!
//! A [`SyntaxNode`] is a pair `(Arc<GreenNode>, offset: u32)`.  The offset is
//! the absolute byte position of the node's first byte in the source.
//! Children are enumerated lazily: walking [`SyntaxNode::children`] accumulates
//! each child's offset in O(children) time with zero extra allocation.
//!
//! The design is "purely downward" — no parent pointers are stored.  This keeps
//! nodes to two words (`Arc` + `u32`) and avoids reference cycles.  For upward
//! navigation, use [`SyntaxNode::ancestors`] or work at the grammar level.
//!
//! ## Trivia
//!
//! Trivia tokens (whitespace, comments, `is_trivia = true`) are kept as
//! ordinary children in the green tree.  The red tree provides filtered views:
//!
//! - [`SyntaxNode::child_tokens`] — all leaf tokens (including trivia).
//! - [`SyntaxNode::non_trivia_tokens`] — only semantic tokens.
//! - [`SyntaxNode::token_groups`] — groups each semantic token with its
//!   surrounding leading and trailing trivia.
//!
//! ## Text access
//!
//! Since [`crate::green::GreenToken`] stores its own `Arc<str>` text, you can
//! call [`SyntaxToken::text`] without needing the original source buffer.
//! For a `SyntaxNode`, use [`SyntaxNode::text`] (needs the source buffer, but
//! returns a `&[u8]` zero-copy slice) or [`SyntaxNode::collect_text`]
//! (source-independent, allocates).

use std::sync::Arc;
use crate::{
    green::{GreenElement, GreenNode, GreenToken},
    types::{FieldId, FromSyntaxKind, Span, SyntaxKind},
};

// ─── SyntaxElement ────────────────────────────────────────────────────────────

/// A direct child of a [`SyntaxNode`] — either a child node or a leaf token.
#[derive(Clone, Debug)]
pub enum SyntaxElement {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

impl SyntaxElement {
    pub fn text_range(&self) -> Span {
        match self {
            SyntaxElement::Node(n)  => n.text_range(),
            SyntaxElement::Token(t) => t.text_range(),
        }
    }
    pub fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxElement::Node(n)  => n.kind(),
            SyntaxElement::Token(t) => t.kind(),
        }
    }
    /// Interpret the stored kind as a custom enum that implements [`FromSyntaxKind`].
    #[inline]
    pub fn kind_as<K: FromSyntaxKind>(&self) -> Option<K> {
        K::from_syntax_kind(self.kind())
    }
    pub fn is_trivia(&self) -> bool {
        match self {
            SyntaxElement::Token(t) => t.is_trivia(),
            SyntaxElement::Node(_)  => false,
        }
    }
    pub fn into_node(self)  -> Option<SyntaxNode>  { if let Self::Node(n)  = self { Some(n) } else { None } }
    pub fn into_token(self) -> Option<SyntaxToken> { if let Self::Token(t) = self { Some(t) } else { None } }
    pub fn as_node(&self)   -> Option<&SyntaxNode>  { if let Self::Node(n)  = self { Some(n) } else { None } }
    pub fn as_token(&self)  -> Option<&SyntaxToken> { if let Self::Token(t) = self { Some(t) } else { None } }
}

// ─── SyntaxToken ─────────────────────────────────────────────────────────────

/// A positioned leaf token in the red tree.
///
/// The text is stored inside the underlying [`GreenToken`] — no source buffer
/// is required to call [`text`](SyntaxToken::text).
#[derive(Clone, Debug)]
pub struct SyntaxToken {
    green:  Arc<GreenToken>,
    offset: u32,
}

impl SyntaxToken {
    pub(crate) fn new(green: Arc<GreenToken>, offset: u32) -> Self {
        Self { green, offset }
    }

    /// Grammar-defined kind of this token.
    #[inline] pub fn kind(&self)      -> SyntaxKind { self.green.kind }
    /// Interpret the stored kind as a custom enum that implements [`FromSyntaxKind`].
    #[inline] pub fn kind_as<K: FromSyntaxKind>(&self) -> Option<K> {
        K::from_syntax_kind(self.green.kind)
    }
    /// Whether this is a trivia token (whitespace, comment, etc.).
    #[inline] pub fn is_trivia(&self) -> bool       { self.green.is_trivia }
    /// Number of UTF-8 bytes this token occupies in the source.
    #[inline] pub fn text_len(&self)  -> u32        { self.green.text_len }

    /// Absolute byte range `[start, end)` of this token in the source.
    #[inline] pub fn text_range(&self) -> Span {
        Span::new(self.offset, self.offset + self.green.text_len)
    }

    /// The token's source text.
    ///
    /// This does **not** require the original source buffer — the text is
    /// stored inside the green token's `Arc<str>`.
    #[inline] pub fn text(&self) -> &str { &self.green.text }

    /// Extract the token's raw bytes from the original source buffer.
    ///
    /// Equivalent to `self.text_range().as_slice(source)`.  Prefer
    /// [`text`](Self::text) unless you need the raw `&[u8]`.
    #[inline] pub fn text_bytes<'src>(&self, source: &'src [u8]) -> &'src [u8] {
        self.text_range().as_slice(source)
    }

    /// Access the underlying green token (position-independent).
    pub fn green(&self) -> &Arc<GreenToken> { &self.green }

    /// Byte offset of this token's first byte in the source.
    #[inline] pub fn offset(&self) -> u32 { self.offset }
}

// ─── SyntaxNode ──────────────────────────────────────────────────────────────

/// A positioned inner node in the red tree.
///
/// Backed by an `Arc<GreenNode>` and a `u32` start offset — two words total.
/// Cloning is cheap.
#[derive(Clone, Debug)]
pub struct SyntaxNode {
    green:  Arc<GreenNode>,
    offset: u32,
}

impl SyntaxNode {
    /// Construct from a green root (offset = 0).
    pub fn new_root(green: Arc<GreenNode>) -> Self {
        Self { green, offset: 0 }
    }

    /// Construct from a green node and its byte offset in the source.
    ///
    /// Used when rebuilding the tree (e.g. in transforms) to create red views of
    /// child green nodes at the correct position.
    #[inline]
    pub fn new(green: Arc<GreenNode>, offset: u32) -> Self {
        Self { green, offset }
    }

    // ── Basics ────────────────────────────────────────────────────────────────

    /// Grammar-defined kind.
    #[inline] pub fn kind(&self)     -> SyntaxKind { self.green.kind }
    /// Interpret the stored kind as a custom enum that implements [`FromSyntaxKind`].
    #[inline] pub fn kind_as<K: FromSyntaxKind>(&self) -> Option<K> {
        K::from_syntax_kind(self.green.kind)
    }
    /// Total byte length (all descendants and trivia).
    #[inline] pub fn text_len(&self) -> u32        { self.green.text_len }
    /// Absolute byte range `[start, end)` of this node in the source.
    #[inline] pub fn text_range(&self) -> Span {
        Span::new(self.offset, self.offset + self.green.text_len)
    }

    /// Extract the full text of this node as a `&[u8]` slice of `source`.
    ///
    /// This is a zero-copy slice of the original source buffer.
    /// For a source-independent `String`, use [`collect_text`](Self::collect_text).
    #[inline] pub fn text<'src>(&self, source: &'src [u8]) -> &'src [u8] {
        self.text_range().as_slice(source)
    }

    /// Reconstruct the node's full text by concatenating stored token texts.
    ///
    /// Allocates a fresh `String`; for a zero-copy `&[u8]` use
    /// [`text`](Self::text) with the source buffer.
    pub fn collect_text(&self) -> String { self.green.collect_text() }

    /// Byte offset of this node's first byte in the source.
    #[inline] pub fn offset(&self) -> u32 { self.offset }

    /// Access the underlying green node (position-independent).
    pub fn green(&self) -> &Arc<GreenNode> { &self.green }

    // ── Child iterators ───────────────────────────────────────────────────────

    /// Iterate **all** direct children (nodes and tokens, including trivia).
    pub fn children(&self) -> impl Iterator<Item = SyntaxElement> + '_ {
        let mut off = self.offset;
        self.green.children.iter().map(move |child| {
            let start = off;
            off += child.text_len();
            match child {
                GreenElement::Node(n)  => SyntaxElement::Node(SyntaxNode::new(Arc::clone(n), start)),
                GreenElement::Token(t) => SyntaxElement::Token(SyntaxToken::new(Arc::clone(t), start)),
            }
        })
    }

    /// Iterate direct child **nodes** only (skips all tokens, including trivia).
    pub fn child_nodes(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.children().filter_map(|e| e.into_node())
    }

    /// First direct child **node** with the given kind, if any.
    ///
    /// Only considers child nodes (not tokens). Useful in visitors when a node
    /// has a known structure and you want one child by kind.
    pub fn child_by_kind(&self, kind: SyntaxKind) -> Option<SyntaxNode> {
        self.child_nodes().find(|n| n.kind() == kind)
    }

    /// First direct child **node** with the given field id (named child).
    ///
    /// Use the grammar's `field_names` (or your `Kind`/schema) to resolve a
    /// name to [`FieldId`] and call this. Returns the first child node whose
    /// slot is labeled with that field.
    pub fn field_by_id(&self, id: FieldId) -> Option<SyntaxNode> {
        let fields = self.green.child_fields.as_ref()?;
        let mut off = self.offset;
        for (i, child) in self.green.children.iter().enumerate() {
            let start = off;
            off += child.text_len();
            if fields.get(i).copied().flatten() != Some(id) {
                continue;
            }
            if let GreenElement::Node(n) = child {
                return Some(SyntaxNode::new(Arc::clone(n), start));
            }
        }
        None
    }

    /// Iterate all direct child **tokens** including trivia.
    pub fn child_tokens(&self) -> impl Iterator<Item = SyntaxToken> + '_ {
        self.children().filter_map(|e| e.into_token())
    }

    /// Iterate direct child tokens, **excluding** trivia.
    pub fn non_trivia_tokens(&self) -> impl Iterator<Item = SyntaxToken> + '_ {
        self.child_tokens().filter(|t| !t.is_trivia())
    }

    // ── Trivia helpers ────────────────────────────────────────────────────────

    /// Leading trivia of this node: trivia tokens at the very start before the
    /// first non-trivia token in the node's direct children.
    pub fn leading_trivia(&self) -> Vec<SyntaxToken> {
        let mut trivia = Vec::new();
        for tok in self.child_tokens() {
            if tok.is_trivia() { trivia.push(tok); } else { break; }
        }
        trivia
    }

    /// Trailing trivia: trivia tokens after the last non-trivia token in the
    /// node's direct children.
    pub fn trailing_trivia(&self) -> Vec<SyntaxToken> {
        let all: Vec<SyntaxToken> = self.child_tokens().collect();
        let mut trivia = Vec::new();
        for tok in all.iter().rev() {
            if tok.is_trivia() { trivia.push(tok.clone()); } else { break; }
        }
        trivia.reverse();
        trivia
    }

    /// Group direct token children into [`TokenWithTrivia`] triples.
    ///
    /// Each triple contains:
    /// - `leading` — consecutive trivia tokens immediately before the semantic token
    /// - `token`   — the semantic (non-trivia) token
    /// - `trailing`— consecutive trivia tokens immediately after it, up to (not
    ///               including) the next semantic token
    ///
    /// Orphan trailing trivia after the last semantic token is attached to that
    /// last group.  This is the canonical per-token trivia view.
    ///
    /// ```text
    /// " let  foo = 42 "
    ///  ^^^                  → leading of `let`
    ///       ^^              → trailing of `let` / leading of `foo`
    ///               ^^     → trailing of `42` (node-level trailing trivia)
    /// ```
    pub fn token_groups(&self) -> Vec<TokenWithTrivia> {
        let tokens: Vec<SyntaxToken> = self.child_tokens().collect();
        let mut groups: Vec<TokenWithTrivia> = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            // Consume leading trivia before the next semantic token.
            let mut leading = Vec::new();
            while i < tokens.len() && tokens[i].is_trivia() {
                leading.push(tokens[i].clone());
                i += 1;
            }
            // Orphan trivia after the last semantic token → attach to last group.
            if i >= tokens.len() {
                if let Some(last) = groups.last_mut() {
                    last.trailing.extend(leading);
                }
                break;
            }
            // Semantic token.
            let token = tokens[i].clone();
            i += 1;
            // Trailing trivia: all consecutive trivia tokens after `token` up to
            // (but not including) the next semantic token.
            let next_semantic_rel = tokens[i..].iter().position(|t| !t.is_trivia());
            let trivia_end = match next_semantic_rel {
                Some(rel) => i + rel,
                None      => tokens.len(),
            };
            let trailing: Vec<SyntaxToken> = tokens[i..trivia_end].to_vec();
            i = trivia_end; // do NOT advance past the next semantic token
            groups.push(TokenWithTrivia { leading, token, trailing });
        }

        groups
    }

    // ── Depth-first search ────────────────────────────────────────────────────

    /// Find the first descendant node (or self) with the given `kind`.
    pub fn find_node(&self, kind: SyntaxKind) -> Option<SyntaxNode> {
        if self.kind() == kind { return Some(self.clone()); }
        self.child_nodes().find_map(|n| n.find_node(kind))
    }

    /// Collect all descendant nodes (including self) with the given `kind`.
    pub fn find_all_nodes(&self, kind: SyntaxKind) -> Vec<SyntaxNode> {
        let mut out = Vec::new();
        self.collect_nodes(kind, &mut out);
        out
    }

    fn collect_nodes(&self, kind: SyntaxKind, out: &mut Vec<SyntaxNode>) {
        if self.kind() == kind { out.push(self.clone()); }
        for child in self.child_nodes() { child.collect_nodes(kind, out); }
    }

    /// Find all descendant **tokens** (including trivia) in document order.
    pub fn descendant_tokens(&self) -> Vec<SyntaxToken> {
        let mut out = Vec::new();
        self.collect_tokens_into(&mut out);
        out
    }

    fn collect_tokens_into(&self, out: &mut Vec<SyntaxToken>) {
        for child in self.children() {
            match child {
                SyntaxElement::Token(t) => out.push(t),
                SyntaxElement::Node(n)  => n.collect_tokens_into(out),
            }
        }
    }

    /// Find all descendant **semantic** (non-trivia) tokens in document order.
    pub fn descendant_semantic_tokens(&self) -> Vec<SyntaxToken> {
        self.descendant_tokens().into_iter().filter(|t| !t.is_trivia()).collect()
    }

    /// First semantic (non-trivia) token in this node's subtree, in document order.
    pub fn first_token(&self) -> Option<SyntaxToken> {
        for child in self.children() {
            match child {
                SyntaxElement::Token(t) if !t.is_trivia() => return Some(t),
                SyntaxElement::Token(_) => continue,
                SyntaxElement::Node(n) => {
                    if let Some(t) = n.first_token() {
                        return Some(t);
                    }
                }
            }
        }
        None
    }

    /// Last semantic (non-trivia) token in this node's subtree, in document order.
    pub fn last_token(&self) -> Option<SyntaxToken> {
        let children: Vec<SyntaxElement> = self.children().collect();
        for child in children.into_iter().rev() {
            match child {
                SyntaxElement::Token(t) if !t.is_trivia() => return Some(t),
                SyntaxElement::Token(_) => continue,
                SyntaxElement::Node(n) => {
                    if let Some(t) = n.last_token() {
                        return Some(t);
                    }
                }
            }
        }
        None
    }

    /// Find the first ancestor of this node (from parent toward root) with the given kind.
    ///
    /// Returns `None` if `self` is the root or no ancestor has `kind`.
    pub fn find_ancestor(&self, root: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
        self.ancestors(root).into_iter().find(|a| a.kind() == kind)
    }

    // ── Offset-based lookup ───────────────────────────────────────────────────

    /// Return the smallest node whose range contains `offset`.
    ///
    /// Returns `None` if `offset` is outside this node's range. Otherwise
    /// returns the deepest descendant node (or self) that contains `offset`.
    pub fn node_at_offset(&self, offset: u32) -> Option<SyntaxNode> {
        if !self.text_range().contains_offset(offset) {
            return None;
        }
        let mut off = self.offset;
        for child in &self.green.children {
            let end = off + child.text_len();
            if offset < end {
                return match child {
                    GreenElement::Node(n) => {
                        let child_red = SyntaxNode::new(Arc::clone(n), off);
                        child_red.node_at_offset(offset).or(Some(child_red))
                    }
                    GreenElement::Token(_) => Some(self.clone()),
                };
            }
            off = end;
        }
        Some(self.clone())
    }

    /// Return the deepest token whose range contains `offset`.
    ///
    /// If `offset` falls on a trivia token, that trivia token is returned.
    pub fn token_at_offset(&self, offset: u32) -> Option<SyntaxToken> {
        let range = self.text_range();
        if offset < range.start || offset >= range.end { return None; }
        let mut off = self.offset;
        for child in self.green.children.iter() {
            let end = off + child.text_len();
            if offset < end {
                return match child {
                    GreenElement::Token(t) => Some(SyntaxToken::new(Arc::clone(t), off)),
                    GreenElement::Node(n)  => SyntaxNode::new(Arc::clone(n), off).token_at_offset(offset),
                };
            }
            off = end;
        }
        None
    }

    /// Like [`token_at_offset`] but skips trivia — returns the deepest
    /// semantic token whose range contains `offset`.
    pub fn semantic_token_at_offset(&self, offset: u32) -> Option<SyntaxToken> {
        self.token_at_offset(offset).filter(|t| !t.is_trivia())
    }

    // ── Ancestor walk ─────────────────────────────────────────────────────────

    /// Return every ancestor of this node up to the root by walking the
    /// provided `root` downward.
    ///
    /// The first element is the *parent* of `self`; the last is `root` (if
    /// `self` is a descendant).  Returns an empty `Vec` if `self` == `root`.
    ///
    /// This is O(depth × tree_width) but is rarely on the hot path.
    pub fn ancestors(&self, root: &SyntaxNode) -> Vec<SyntaxNode> {
        let mut path = Vec::new();
        find_path(root, self.offset, self.green.kind, self.text_len(), &mut path);
        path
    }
}

/// Walk downward collecting nodes that are ancestors of the target span.
fn find_path(
    node:       &SyntaxNode,
    target_off: u32,
    target_kind: SyntaxKind,
    target_len:  u32,
    path:       &mut Vec<SyntaxNode>,
) -> bool {
    for child in node.child_nodes() {
        let r = child.text_range();
        if r.start <= target_off && target_off + target_len <= r.end {
            if child.offset == target_off && child.kind() == target_kind && child.text_len() == target_len {
                return true; // found the target
            }
            if find_path(&child, target_off, target_kind, target_len, path) {
                path.push(node.clone());
                return true;
            }
        }
    }
    false
}

// ─── TokenWithTrivia ─────────────────────────────────────────────────────────

/// A semantic token together with its surrounding trivia, produced by
/// [`SyntaxNode::token_groups`].
///
/// Leading trivia is trivia that appears immediately before the semantic token
/// (with no intervening semantic tokens).  Trailing trivia is trivia that
/// follows, up to (not including) the next semantic token.
#[derive(Debug)]
pub struct TokenWithTrivia {
    /// Trivia tokens immediately before `token`.
    pub leading:  Vec<SyntaxToken>,
    /// The semantic (non-trivia) token.
    pub token:    SyntaxToken,
    /// Trivia tokens immediately after `token` and before the next semantic token.
    pub trailing: Vec<SyntaxToken>,
}

impl TokenWithTrivia {
    /// Total byte range: start of leading trivia → end of trailing trivia.
    pub fn full_range(&self) -> Span {
        let start = self.leading.first()
            .map(|t| t.offset)
            .unwrap_or(self.token.offset);
        let end = self.trailing.last()
            .map(|t| t.offset + t.green.text_len)
            .unwrap_or_else(|| self.token.offset + self.token.green.text_len);
        Span::new(start, end)
    }

    /// Concatenate text: leading + token + trailing.
    pub fn full_text(&self) -> String {
        let mut s = String::new();
        for t in &self.leading  { s.push_str(t.text()); }
        s.push_str(self.token.text());
        for t in &self.trailing { s.push_str(t.text()); }
        s
    }
}
