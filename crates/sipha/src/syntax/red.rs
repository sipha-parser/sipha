use std::{cell::RefCell, sync::{Arc, Weak}};
use crate::syntax::{GreenNode, GreenToken, GreenElement, SyntaxKind, TextRange, TextSize};

/// Mutable cursor with parent pointers
#[derive(Clone)]
pub struct SyntaxNode<K: SyntaxKind> {
    green: std::sync::Arc<GreenNode<K>>,
    parent: Option<Weak<RefCell<SyntaxData<K>>>>,
    index: u32,
    offset: TextSize,
}

struct SyntaxData<K: SyntaxKind> {
    green: std::sync::Arc<GreenNode<K>>,
    parent: Option<Weak<RefCell<SyntaxData<K>>>>,
    index: u32,
    offset: TextSize,
}

impl<K: SyntaxKind> SyntaxNode<K> {
    #[must_use]
    pub const fn new_root(green: std::sync::Arc<GreenNode<K>>) -> Self {
        Self {
            green,
            parent: None,
            index: 0,
            offset: TextSize::zero(),
        }
    }
    
    fn new_child(
        green: std::sync::Arc<GreenNode<K>>,
        parent: &Self,
        index: u32,
    ) -> Self {
        // Calculate offset by summing text lengths of previous siblings
        let mut offset = parent.offset;
        let children = parent.green.children();
        for child in children.iter().take(index as usize) {
            offset = TextSize::from(offset.into() + child.text_len().into());
        }
        
        let arc = Arc::new(RefCell::new(SyntaxData {
            green: parent.green.clone(),
            parent: parent.parent.clone(),
            index: parent.index,
            offset: parent.offset,
        }));
        Self {
            green,
            parent: Some(Arc::downgrade(&arc)),
            index,
            offset,
        }
    }
    
    #[inline]
    #[must_use]
    pub fn kind(&self) -> K {
        self.green.kind()
    }
    
    #[inline]
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        TextRange::at(self.offset, self.green.text_len())
    }
    
    /// Get the text content of this node and all its descendants.
    ///
    /// This allocates a new `String` every time. For better performance when
    /// the text is already available or when you need to format it, consider
    /// using `collect_text()` to write directly to a buffer.
    #[must_use]
    pub fn text(&self) -> String {
        // Aggregate text from all tokens
        let mut result = String::new();
        self.collect_text(&mut result);
        result
    }
    
    /// Write the text content of this node to the given buffer.
    ///
    /// This avoids allocating a new String and allows you to reuse buffers.
    /// More efficient than `text()` when you already have a buffer or need
    /// to format the text.
    pub fn write_text(&self, buffer: &mut String) {
        self.collect_text(buffer);
    }
    
    fn collect_text(&self, result: &mut String) {
        for child in self.green.children() {
            match child {
                GreenElement::Node(n) => {
                    let node = Self::new_child(n.clone(), self, 0);
                    node.collect_text(result);
                }
                GreenElement::Token(t) => {
                    result.push_str(t.text());
                }
            }
        }
    }
    
    #[must_use]
    pub fn parent(&self) -> Option<Self> {
        self.parent.as_ref().and_then(|weak| {
            weak.upgrade().map(|rc| {
                let data = rc.borrow();
                Self {
                    green: data.green.clone(),
                    parent: data.parent.clone(),
                    index: data.index,
                    offset: data.offset,
                }
            })
        })
    }
    
    #[must_use]
    pub fn children(&self) -> SyntaxChildren<'_, K> {
        SyntaxChildren {
            parent: self,
            green_children: self.green.children(),
            index: 0,
        }
    }
    
    #[must_use]
    pub fn first_child(&self) -> Option<SyntaxElement<K>> {
        self.green.children().first().map(|green| {
            self.element_from_green(green, 0)
        })
    }
    
    #[must_use]
    pub fn last_child(&self) -> Option<SyntaxElement<K>> {
        let last_idx = self.green.children().len().saturating_sub(1);
        self.green.children().get(last_idx).map(|green| {
            self.element_from_green(green, u32::try_from(last_idx).unwrap_or(u32::MAX))
        })
    }
    
    fn element_from_green(&self, green: &GreenElement<K>, index: u32) -> SyntaxElement<K> {
        // Calculate offset by summing text lengths of previous siblings
        let mut offset = self.offset;
        let children = self.green.children();
        for child in children.iter().take(index as usize) {
            offset = TextSize::from(offset.into() + child.text_len().into());
        }
        
        match green {
            GreenElement::Node(g) => SyntaxElement::Node(Self::new_child(
                g.clone(),
                self,
                index,
            )),
            GreenElement::Token(t) => SyntaxElement::Token(SyntaxToken {
                green: t.clone(),
                parent: self.parent.clone(),
                index,
                offset,
            }),
        }
    }
    
    #[must_use]
    pub fn next_sibling(&self) -> Option<SyntaxElement<K>> {
        let parent = self.parent()?;
        let next_index = self.index + 1;
        
        parent.green.children().get(next_index as usize)
            .map(|green| parent.element_from_green(green, next_index))
    }
    
    #[must_use]
    pub fn prev_sibling(&self) -> Option<SyntaxElement<K>> {
        if self.index == 0 {
            return None;
        }
        
        let parent = self.parent()?;
        let prev_index = self.index - 1;
        
        parent.green.children().get(prev_index as usize)
            .map(|green| parent.element_from_green(green, prev_index))
    }
    
    #[must_use]
    pub fn descendants(&self) -> SyntaxDescendants<K> {
        // Start with all children (not just first_child) to get all descendants
        let mut stack = Vec::new();
        for child in self.children() {
            stack.push(Some(child));
        }
        SyntaxDescendants { stack }
    }
    
    #[must_use]
    pub fn ancestors(&self) -> SyntaxAncestors<K> {
        SyntaxAncestors {
            current: self.parent(),
        }
    }
    
    pub fn children_with_kind(&self, kind: K) -> impl Iterator<Item = Self> + '_ {
        self.children().filter_map(move |elem| match elem {
            SyntaxElement::Node(node) if node.kind() == kind => Some(node),
            _ => None,
        })
    }
    
    #[must_use]
    pub fn find_first_child<F>(&self, predicate: F) -> Option<SyntaxElement<K>>
    where
        F: Fn(&SyntaxElement<K>) -> bool,
    {
        self.children().find(|elem| predicate(elem))
    }
    
    #[must_use]
    pub fn find_last_child<F>(&self, predicate: F) -> Option<SyntaxElement<K>>
    where
        F: Fn(&SyntaxElement<K>) -> bool,
    {
        // Collect all children and iterate in reverse
        let children: Vec<_> = self.children().collect();
        children.into_iter().rev().find(|elem| predicate(elem))
    }
    
    #[must_use]
    pub fn nth_child(&self, index: usize) -> Option<SyntaxElement<K>> {
        self.green.children().get(index).map(|green| {
            self.element_from_green(green, u32::try_from(index).unwrap_or(u32::MAX))
        })
    }
    
    #[must_use]
    pub fn siblings(&self) -> SyntaxSiblings<K> {
        let parent = self.parent();
        SyntaxSiblings {
            parent: parent.clone(),
            children: parent.as_ref()
                .map(|p| p.green.children().to_vec())
                .unwrap_or_default(),
            current_index: 0,
            skip_index: self.index,
        }
    }
    
    #[must_use]
    pub fn tokens(&self) -> SyntaxTokens<K> {
        SyntaxTokens {
            stack: vec![self.first_child()],
        }
    }
    
    #[must_use]
    pub fn nodes(&self) -> SyntaxNodes<K> {
        SyntaxNodes {
            stack: vec![self.first_child()],
        }
    }
    
    #[cfg(feature = "traversal")]
    #[must_use]
    pub fn preorder(&self) -> SyntaxPreOrder<K> {
        SyntaxPreOrder {
            stack: vec![Some(self.clone())],
        }
    }
    
    #[cfg(feature = "traversal")]
    #[must_use]
    pub fn postorder(&self) -> SyntaxPostOrder<K> {
        SyntaxPostOrder {
            stack: vec![(self.clone(), false)],
        }
    }
    
    #[cfg(feature = "traversal")]
    #[must_use]
    pub fn inorder(&self) -> SyntaxInOrder<K> {
        SyntaxInOrder {
            stack: vec![(self.clone(), false)],
        }
    }
    
    #[cfg(feature = "traversal")]
    #[must_use]
    pub fn level_order(&self) -> SyntaxLevelOrder<K> {
        SyntaxLevelOrder {
            queue: std::collections::VecDeque::from(vec![self.clone()]),
        }
    }
    
    #[must_use]
    pub const fn green(&self) -> &std::sync::Arc<GreenNode<K>> {
        &self.green
    }
}

pub struct SyntaxChildren<'a, K: SyntaxKind> {
    parent: &'a SyntaxNode<K>,
    green_children: &'a [GreenElement<K>],
    index: usize,
}

impl<K: SyntaxKind> Iterator for SyntaxChildren<'_, K> {
    type Item = SyntaxElement<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.green_children.len() {
            return None;
        }
        
        let green = &self.green_children[self.index];
        let element = self.parent.element_from_green(green, u32::try_from(self.index).unwrap_or(0));
        
        self.index += 1;
        Some(element)
    }
}

pub struct SyntaxDescendants<K: SyntaxKind> {
    stack: Vec<Option<SyntaxElement<K>>>,
}

impl<K: SyntaxKind> Iterator for SyntaxDescendants<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(elem) = self.stack.pop()? {
            match elem {
                SyntaxElement::Node(node) => {
                    // Add children to stack for DFS (in reverse order)
                    let children: Vec<_> = node.children().collect();
                    for child in children.into_iter().rev() {
                        self.stack.push(Some(child));
                    }
                    return Some(node);
                }
                SyntaxElement::Token(_) => {
                    // Skip tokens in descendants iterator
                }
            }
        }
        None
    }
}

pub struct SyntaxAncestors<K: SyntaxKind> {
    current: Option<SyntaxNode<K>>,
}

impl<K: SyntaxKind> Iterator for SyntaxAncestors<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        let current = self.current.take()?;
        self.current = current.parent();
        Some(current)
    }
}

#[cfg(feature = "traversal")]
pub struct SyntaxPreOrder<K: SyntaxKind> {
    stack: Vec<Option<SyntaxNode<K>>>,
}

#[cfg(feature = "traversal")]
impl<K: SyntaxKind> Iterator for SyntaxPreOrder<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(Some(node)) = self.stack.pop() {
            // Add children to stack in reverse order (so first child is popped first)
            let children: Vec<_> = node.children()
                .filter_map(|elem| {
                    if let SyntaxElement::Node(n) = elem {
                        Some(n)
                    } else {
                        None
                    }
                })
                .collect();
            for child in children.into_iter().rev() {
                self.stack.push(Some(child));
            }
            Some(node)
        } else {
            None
        }
    }
}

#[cfg(feature = "traversal")]
pub struct SyntaxPostOrder<K: SyntaxKind> {
    stack: Vec<(SyntaxNode<K>, bool)>, // (node, visited)
}

#[cfg(feature = "traversal")]
impl<K: SyntaxKind> Iterator for SyntaxPostOrder<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, visited)) = self.stack.pop() {
            if visited {
                return Some(node);
            }
            
            // Mark as visited and push back
            self.stack.push((node.clone(), true));
            
            // Add children in reverse order
            let children: Vec<_> = node.children()
                .filter_map(|elem| {
                    if let SyntaxElement::Node(n) = elem {
                        Some(n)
                    } else {
                        None
                    }
                })
                .collect();
            for child in children.into_iter().rev() {
                self.stack.push((child, false));
            }
        }
        None
    }
}

#[cfg(feature = "traversal")]
pub struct SyntaxInOrder<K: SyntaxKind> {
    stack: Vec<(SyntaxNode<K>, bool)>, // (node, visited)
}

#[cfg(feature = "traversal")]
impl<K: SyntaxKind> Iterator for SyntaxInOrder<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, visited)) = self.stack.pop() {
            if visited {
                return Some(node);
            }
            
            let children: Vec<_> = node.children()
                .filter_map(|elem| {
                    if let SyntaxElement::Node(n) = elem {
                        Some(n)
                    } else {
                        None
                    }
                })
                .collect();
            
            match children.len() {
                0 => return Some(node), // Leaf node, visit immediately
                1 => {
                    // Single child: visit left (child), then node, then done
                    self.stack.push((node.clone(), true));
                    self.stack.push((children[0].clone(), false));
                }
                _ => {
                    // Multiple children: visit right subtree, then node, then left subtree
                    // (in reverse order on stack)
                    for child in children.iter().rev() {
                        self.stack.push((child.clone(), false));
                    }
                    self.stack.push((node.clone(), true));
                }
            }
        }
        None
    }
}

#[cfg(feature = "traversal")]
pub struct SyntaxLevelOrder<K: SyntaxKind> {
    queue: std::collections::VecDeque<SyntaxNode<K>>,
}

#[cfg(feature = "traversal")]
impl<K: SyntaxKind> Iterator for SyntaxLevelOrder<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        let node = self.queue.pop_front()?;
        
        // Add children to queue
        for child in node.children() {
            if let SyntaxElement::Node(n) = child {
                self.queue.push_back(n);
            }
        }
        
        Some(node)
    }
}

pub struct SyntaxSiblings<K: SyntaxKind> {
    parent: Option<SyntaxNode<K>>,
    children: Vec<GreenElement<K>>,
    current_index: usize,
    skip_index: u32,
}

impl<K: SyntaxKind> Iterator for SyntaxSiblings<K> {
    type Item = SyntaxElement<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        let parent = self.parent.as_ref()?;
        
        while self.current_index < self.children.len() {
            let idx = self.current_index;
            self.current_index += 1;
            
            // Skip the node itself
            if u32::try_from(idx).unwrap_or(0) == self.skip_index {
                continue;
            }
            
            let green = &self.children[idx];
            return Some(parent.element_from_green(green, u32::try_from(idx).unwrap_or(0)));
        }
        
        None
    }
}

pub struct SyntaxTokens<K: SyntaxKind> {
    stack: Vec<Option<SyntaxElement<K>>>,
}

impl<K: SyntaxKind> Iterator for SyntaxTokens<K> {
    type Item = SyntaxToken<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(Some(elem)) = self.stack.pop() {
            match elem {
                SyntaxElement::Token(token) => return Some(token),
                SyntaxElement::Node(node) => {
                    // Add children to stack in reverse order
                    let children: Vec<_> = node.children().collect();
                    for child in children.into_iter().rev() {
                        self.stack.push(Some(child));
                    }
                }
            }
        }
        None
    }
}

pub struct SyntaxNodes<K: SyntaxKind> {
    stack: Vec<Option<SyntaxElement<K>>>,
}

impl<K: SyntaxKind> Iterator for SyntaxNodes<K> {
    type Item = SyntaxNode<K>;
    
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(Some(elem)) = self.stack.pop() {
            match elem {
                SyntaxElement::Node(node) => {
                    // Add children to stack in reverse order
                    let children: Vec<_> = node.children().collect();
                    for child in children.into_iter().rev() {
                        self.stack.push(Some(child));
                    }
                    return Some(node);
                }
                SyntaxElement::Token(_) => {
                    // Skip tokens
                }
            }
        }
        None
    }
}

#[derive(Clone)]
pub struct SyntaxToken<K: SyntaxKind> {
    green: GreenToken<K>,
    parent: Option<Weak<RefCell<SyntaxData<K>>>>,
    /// Index of this token within its parent's children.
    /// Reserved for future token sibling navigation (similar to `SyntaxNode.index`).
    #[allow(dead_code)]
    index: u32,
    offset: TextSize,
}

impl<K: SyntaxKind> SyntaxToken<K> {
    #[inline]
    pub const fn kind(&self) -> K {
        self.green.kind()
    }
    
    #[inline]
    #[must_use]
    pub fn text(&self) -> &str {
        self.green.text()
    }
    
    #[inline]
    #[must_use]
    pub fn text_range(&self) -> TextRange {
        TextRange::at(self.offset, self.green.text_len())
    }
    
    #[must_use]
    pub fn parent(&self) -> Option<SyntaxNode<K>> {
        self.parent.as_ref().and_then(|weak| {
            weak.upgrade().map(|rc| {
                let data = rc.borrow();
                SyntaxNode {
                    green: data.green.clone(),
                    parent: data.parent.clone(),
                    index: data.index,
                    offset: data.offset,
                }
            })
        })
    }
}

#[derive(Clone)]
pub enum SyntaxElement<K: SyntaxKind> {
    Node(SyntaxNode<K>),
    Token(SyntaxToken<K>),
}

impl<K: SyntaxKind> SyntaxElement<K> {
    pub fn kind(&self) -> K {
        match self {
            Self::Node(n) => n.kind(),
            Self::Token(t) => t.kind(),
        }
    }
    
    pub fn text_range(&self) -> TextRange {
        match self {
            Self::Node(n) => n.text_range(),
            Self::Token(t) => t.text_range(),
        }
    }
}

