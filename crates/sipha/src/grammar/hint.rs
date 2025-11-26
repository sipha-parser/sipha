use std::any::Any;

/// Trait for backend-specific hints
pub trait BackendHint: std::fmt::Debug + Send + Sync {
    fn as_any(&self) -> &dyn Any;
    fn description(&self) -> String;
}

/// Precedence hint for operator precedence
#[derive(Debug, Clone)]
pub struct PrecedenceHint {
    pub precedence: u32,
    pub associativity: Associativity,
}

impl BackendHint for PrecedenceHint {
    fn as_any(&self) -> &dyn Any {
        self
    }
    
    fn description(&self) -> String {
        format!("Precedence: {}, Associativity: {:?}", self.precedence, self.associativity)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    None,
}

