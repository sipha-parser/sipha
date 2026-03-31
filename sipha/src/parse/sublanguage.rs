//! Sub-language embedding (parse regions with another grammar).
//!
//! This operates as a post-pass over `TreeEvent`s: the host grammar marks a region
//! (typically a node) and also marks the exact byte span to be re-parsed (typically
//! a single token or a child node). After the host parse succeeds, `apply_*`
//! parses those spans with the embedded graph and splices the embedded tree events
//! under the host node.

use crate::parse::context::ParseContext;
use crate::parse::engine::{Engine, ParseError};
use crate::parse::insn::ParseGraph;
use crate::types::{Pos, Span, SyntaxKind, TreeEvent};

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// How to find the byte span to feed to the embedded parser.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmbeddedSpan {
    /// Use the span of the first (or last) token with the given kind inside the host node.
    TokenKind(SyntaxKind),
    /// Use the span of the first (or last) node with the given kind inside the host node.
    NodeKind(SyntaxKind),
}

/// One embedded-language configuration.
#[derive(Clone, Copy, Debug)]
pub struct SubLanguage<'a> {
    /// Host node kind which will receive the embedded tree as children.
    pub host_kind: SyntaxKind,
    /// How to find the exact byte span to parse within the host node.
    pub span: EmbeddedSpan,
    /// The embedded grammar.
    pub embedded: ParseGraph<'a>,
    /// Optional wrapper node kind to insert around the embedded parse output.
    pub wrapper_kind: Option<SyntaxKind>,
    /// If set, embedded parse failures are recovered by inserting an error node of this kind.
    pub error_kind: Option<SyntaxKind>,
}

/// A parse error produced while embedding a sub-language.
#[derive(Clone, Debug)]
pub struct SubLanguageError {
    pub host_span: Span,
    pub error: ParseError,
}

/// Apply embedded-language splicing to an existing host event stream.
///
/// Returns a new `TreeEvent` list and a list of embedded parse errors (if any).
///
/// Notes:
/// - Host parsing must already have succeeded; this does not affect `consumed`.
/// - Embedded events are position-adjusted by adding the host byte offset.
pub fn apply_sublanguages(
    engine: &mut Engine,
    input: &[u8],
    host_events: &[TreeEvent],
    sublanguages: &[SubLanguage<'_>],
) -> (Vec<TreeEvent>, Vec<SubLanguageError>) {
    let mut out: Vec<TreeEvent> = Vec::with_capacity(host_events.len());
    let mut errors: Vec<SubLanguageError> = Vec::new();

    #[derive(Clone, Debug)]
    struct NodeFrame {
        kind: SyntaxKind,
        open_pos: Pos,
        /// Whether this node is a host node for some sublanguage.
        host_idx: Option<usize>,
        /// Captured span inside this host node to embed.
        embed_span: Option<Span>,
    }

    let mut stack: Vec<NodeFrame> = Vec::new();

    for &ev in host_events {
        match ev {
            TreeEvent::NodeOpen { kind, field, pos } => {
                let host_idx = sublanguages.iter().position(|s| s.host_kind == kind);
                stack.push(NodeFrame {
                    kind,
                    open_pos: pos,
                    host_idx,
                    embed_span: None,
                });
                out.push(TreeEvent::NodeOpen { kind, field, pos });
            }
            TreeEvent::Token {
                kind,
                start,
                end,
                is_trivia,
            } => {
                if let Some(top) = stack.last_mut() {
                    if let Some(i) = top.host_idx {
                        if matches!(sublanguages[i].span, EmbeddedSpan::TokenKind(k) if k == kind) {
                            top.embed_span = Some(Span::new(start, end));
                        }
                    }
                }
                out.push(TreeEvent::Token {
                    kind,
                    start,
                    end,
                    is_trivia,
                });
            }
            TreeEvent::NodeClose { pos } => {
                let frame = match stack.pop() {
                    Some(f) => f,
                    None => {
                        // Malformed event stream; pass through.
                        out.push(TreeEvent::NodeClose { pos });
                        continue;
                    }
                };

                // If this is a content node, record its span into the nearest host parent.
                if let Some(parent) = stack.last_mut() {
                    if let Some(i) = parent.host_idx {
                        if matches!(sublanguages[i].span, EmbeddedSpan::NodeKind(k) if k == frame.kind)
                        {
                            parent.embed_span = Some(Span::new(frame.open_pos, pos));
                        }
                    }
                }

                // If this is a host node, splice embedded parse output before closing it.
                if let Some(i) = frame.host_idx {
                    if let Some(span) = frame.embed_span {
                        let slice = span.as_slice(input);
                        let embedded_ctx = sublanguages[i]
                            .error_kind
                            .map(|k| ParseContext::new().with_error_node_kind(k))
                            .unwrap_or_else(ParseContext::new);

                        let embedded_out = match engine.parse_with_context(
                            &sublanguages[i].embedded,
                            slice,
                            &embedded_ctx,
                        ) {
                            Ok(o) => Some(o),
                            Err(e) => {
                                // Try to recover into a partial tree if configured.
                                if sublanguages[i].error_kind.is_some() {
                                    match engine.parse_recovering_with_context(
                                        &sublanguages[i].embedded,
                                        slice,
                                        &embedded_ctx,
                                    ) {
                                        Ok(o) => Some(o),
                                        Err((partial, e2)) => {
                                            errors.push(SubLanguageError {
                                                host_span: span,
                                                error: e2,
                                            });
                                            Some(partial)
                                        }
                                    }
                                } else {
                                    errors.push(SubLanguageError {
                                        host_span: span,
                                        error: e,
                                    });
                                    None
                                }
                            }
                        };

                        if let Some(embedded_out) = embedded_out {
                            if let Some(wk) = sublanguages[i].wrapper_kind {
                                out.push(TreeEvent::NodeOpen {
                                    kind: wk,
                                    field: None,
                                    pos: span.start,
                                });
                            }

                            out.extend(
                                embedded_out
                                    .tree_events
                                    .into_iter()
                                    .map(|e| offset_event(e, span.start)),
                            );

                            if sublanguages[i].wrapper_kind.is_some() {
                                out.push(TreeEvent::NodeClose { pos: span.end });
                            }
                        }
                    }
                }

                out.push(TreeEvent::NodeClose { pos });
            }
        }
    }

    (out, errors)
}

fn offset_event(ev: TreeEvent, base: Pos) -> TreeEvent {
    match ev {
        TreeEvent::NodeOpen { kind, field, pos } => TreeEvent::NodeOpen {
            kind,
            field,
            pos: pos.saturating_add(base),
        },
        TreeEvent::NodeClose { pos } => TreeEvent::NodeClose {
            pos: pos.saturating_add(base),
        },
        TreeEvent::Token {
            kind,
            start,
            end,
            is_trivia,
        } => TreeEvent::Token {
            kind,
            start: start.saturating_add(base),
            end: end.saturating_add(base),
            is_trivia,
        },
    }
}
