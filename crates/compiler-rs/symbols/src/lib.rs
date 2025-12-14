//! SuperPascal Symbol Table
//!
//! This crate implements the symbol table for SuperPascal, managing
//! symbol declarations, scopes, and name resolution.
//!
//! The symbol table uses hash tables with chaining, following Turbo Pascal's approach:
//! - Custom hash function for case-insensitive identifier hashing
//! - Hash buckets with linked lists (chaining) for collision resolution
//! - Fast O(1) average case lookup

// ast::Node will be used when converting AST declarations to symbols
use std::collections::HashMap;
use tokens::Span;
use types::Type;

/// Symbol kind
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    /// Variable symbol
    Variable {
        name: String,
        var_type: Type,
        span: Span,
    },
    /// Constant symbol
    Constant {
        name: String,
        const_type: Type,
        value: Option<ConstantValue>, // Will be set during semantic analysis
        span: Span,
    },
    /// Type alias symbol
    TypeAlias {
        name: String,
        aliased_type: Type,
        span: Span,
    },
    /// Procedure symbol
    Procedure {
        name: String,
        params: Vec<Parameter>,
        span: Span,
    },
    /// Function symbol
    Function {
        name: String,
        params: Vec<Parameter>,
        return_type: Type,
        span: Span,
    },
}

/// Constant value (for constant symbols)
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i16),
    Byte(u8),
    Word(u16),
    Boolean(bool),
    Char(u8),
    String(String),
}

/// Function/procedure parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
    pub passing_mode: ParameterMode,
    pub span: Span,
}

/// Parameter passing mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParameterMode {
    Value,  // Pass by value
    Var,    // Pass by reference (var)
    Const,  // Pass by constant reference (const)
}

// ===== Turbo Pascal Hash Function =====

/// Calculate hash value for an identifier using Turbo Pascal's hash function.
/// 
/// Algorithm (from Turbo Pascal):
/// 1. Start with negative length: `hash = -length`
/// 2. For each character: `hash += (byte[i] & 0xDF)` (uppercase conversion)
/// 3. Shift left by 1: `hash <<= 1`
/// 
/// This produces a case-insensitive hash suitable for identifier lookup.
/// 
/// # Arguments
/// * `identifier` - The identifier string to hash
/// 
/// # Returns
/// A 16-bit hash value (u16)
pub fn turbopascal_hash(identifier: &str) -> u16 {
    let mut hash = -(identifier.len() as i16);
    
    for byte in identifier.bytes() {
        // Convert to uppercase: byte & 0xDF
        // 0xDF = 0b11011111, which clears bit 5 (converts lowercase to uppercase)
        let upper_byte = byte & 0xDF;
        hash = hash.wrapping_add(upper_byte as i16);
    }
    
    // Shift left by 1 (multiply by 2)
    (hash << 1) as u16
}

/// Calculate hash bucket index from hash value using a mask.
/// 
/// The mask parameter defines the number of buckets (2^N).
/// Uses the last N bits of the hash to select the bucket.
/// 
/// # Arguments
/// * `hash` - The hash value
/// * `mask` - The mask (bucket count - 1, must be power of 2)
/// 
/// # Returns
/// Bucket index (0 to mask)
pub fn hash_bucket(hash: u16, mask: u16) -> usize {
    (hash & mask) as usize
}

/// Symbol entry in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SymbolKind,
    /// Scope level (0 = global, 1+ = nested scopes for future Tier 2)
    pub scope_level: usize,
}

impl Symbol {
    /// Get the symbol's name
    pub fn name(&self) -> &str {
        match &self.kind {
            SymbolKind::Variable { name, .. } => name,
            SymbolKind::Constant { name, .. } => name,
            SymbolKind::TypeAlias { name, .. } => name,
            SymbolKind::Procedure { name, .. } => name,
            SymbolKind::Function { name, .. } => name,
        }
    }

    /// Get the symbol's span
    pub fn span(&self) -> Span {
        match &self.kind {
            SymbolKind::Variable { span, .. } => *span,
            SymbolKind::Constant { span, .. } => *span,
            SymbolKind::TypeAlias { span, .. } => *span,
            SymbolKind::Procedure { span, .. } => *span,
            SymbolKind::Function { span, .. } => *span,
        }
    }
}

/// Symbol table with scope management
pub struct SymbolTable {
    /// Current scope level (0 = global)
    current_scope: usize,
    /// Symbols by scope level
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    /// Create a new symbol table
    pub fn new() -> Self {
        Self {
            current_scope: 0,
            scopes: vec![HashMap::new()], // Start with global scope
        }
    }

    /// Enter a new scope (for future Tier 2 support)
    pub fn enter_scope(&mut self) {
        self.current_scope += 1;
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        if self.current_scope > 0 {
            self.scopes.pop();
            self.current_scope -= 1;
        }
    }

    /// Insert a symbol into the current scope
    /// Returns an error if the symbol already exists in the current scope
    pub fn insert(&mut self, symbol: Symbol) -> Result<(), String> {
        let name = symbol.name().to_string();
        let current_scope_map = &mut self.scopes[self.current_scope];

        if current_scope_map.contains_key(&name) {
            return Err(format!(
                "Symbol '{}' already declared in this scope",
                name
            ));
        }

        current_scope_map.insert(name, symbol);
        Ok(())
    }

    /// Look up a symbol by name, searching from current scope outward
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Search from current scope to global scope
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Look up a symbol only in the current scope
    pub fn lookup_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.scopes[self.current_scope].get(name)
    }

    /// Check if a symbol exists in the current scope
    pub fn exists_in_current_scope(&self, name: &str) -> bool {
        self.scopes[self.current_scope].contains_key(name)
    }

    /// Get all symbols in the current scope
    pub fn current_scope_symbols(&self) -> Vec<&Symbol> {
        self.scopes[self.current_scope].values().collect()
    }

    /// Get the current scope level
    pub fn scope_level(&self) -> usize {
        self.current_scope
    }

    /// Check if we're in the global scope
    pub fn is_global_scope(&self) -> bool {
        self.current_scope == 0
    }

    /// Compact symbol tables to remove unused space.
    /// 
    /// This is called after each module is compiled to reduce memory footprint.
    /// Following Turbo Pascal's approach, this removes unused space from symbol tables.
    /// 
    /// For our HashMap-based implementation, this primarily:
    /// 1. Shrinks hash maps to remove excess capacity
    /// 2. Removes any unused scopes (if any were created but not used)
    /// 
    /// This is critical for memory efficiency on resource-constrained systems (Tier 1 platforms).
    pub fn compact(&mut self) {
        // Shrink all hash maps to remove excess capacity
        for scope in &mut self.scopes {
            scope.shrink_to_fit();
        }
        
        // Remove empty scopes (except global scope)
        // Keep at least the global scope
        if self.scopes.len() > 1 {
            let mut i = 1;
            while i < self.scopes.len() {
                if self.scopes[i].is_empty() && i > 0 {
                    // Only remove if it's not the current scope
                    if i != self.current_scope {
                        self.scopes.remove(i);
                        // Adjust current_scope if we removed a scope before it
                        if i < self.current_scope {
                            self.current_scope -= 1;
                        }
                    } else {
                        i += 1;
                    }
                } else {
                    i += 1;
                }
            }
        }
        
        // Shrink the scopes vector itself
        self.scopes.shrink_to_fit();
    }

    /// Get statistics about the symbol table (for debugging/optimization)
    pub fn stats(&self) -> SymbolTableStats {
        let total_symbols: usize = self.scopes.iter().map(|s| s.len()).sum();
        let total_capacity: usize = self.scopes.iter().map(|s| s.capacity()).sum();
        
        SymbolTableStats {
            scope_count: self.scopes.len(),
            current_scope: self.current_scope,
            total_symbols,
            total_capacity,
            memory_usage_bytes: total_capacity * std::mem::size_of::<(String, Symbol)>(),
        }
    }
}

/// Statistics about symbol table memory usage
#[derive(Debug, Clone)]
pub struct SymbolTableStats {
    pub scope_count: usize,
    pub current_scope: usize,
    pub total_symbols: usize,
    pub total_capacity: usize,
    pub memory_usage_bytes: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===== Symbol Creation Tests =====

    #[test]
    fn test_variable_symbol() {
        let span = Span::new(0, 5, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "x");
        assert_eq!(symbol.span(), span);
    }

    #[test]
    fn test_constant_symbol() {
        let span = Span::new(0, 10, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Constant {
                name: "MAX_SIZE".to_string(),
                const_type: Type::integer(),
                value: Some(ConstantValue::Integer(100)),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "MAX_SIZE");
    }

    #[test]
    fn test_type_alias_symbol() {
        let span = Span::new(0, 8, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::TypeAlias {
                name: "MyInt".to_string(),
                aliased_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "MyInt");
    }

    #[test]
    fn test_procedure_symbol() {
        let span = Span::new(0, 12, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Procedure {
                name: "DoSomething".to_string(),
                params: vec![],
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "DoSomething");
    }

    #[test]
    fn test_function_symbol() {
        let span = Span::new(0, 6, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Function {
                name: "Add".to_string(),
                params: vec![],
                return_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "Add");
    }

    // ===== Symbol Table Tests =====

    #[test]
    fn test_symbol_table_new() {
        let table = SymbolTable::new();
        assert_eq!(table.scope_level(), 0);
        assert!(table.is_global_scope());
    }

    #[test]
    fn test_symbol_table_insert() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert!(table.insert(symbol).is_ok());
        assert!(table.exists_in_current_scope("x"));
    }

    #[test]
    fn test_symbol_table_duplicate_insert() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);
        let symbol1 = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        let symbol2 = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::byte(),
                span,
            },
            scope_level: 0,
        };

        assert!(table.insert(symbol1).is_ok());
        assert!(table.insert(symbol2).is_err());
    }

    #[test]
    fn test_symbol_table_lookup() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        table.insert(symbol).unwrap();
        let found = table.lookup("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name(), "x");
    }

    #[test]
    fn test_symbol_table_lookup_not_found() {
        let table = SymbolTable::new();
        assert!(table.lookup("nonexistent").is_none());
    }

    #[test]
    fn test_symbol_table_lookup_current_scope() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        table.insert(symbol).unwrap();
        assert!(table.lookup_current_scope("x").is_some());
        assert!(table.lookup_current_scope("y").is_none());
    }

    #[test]
    fn test_symbol_table_scope_management() {
        let mut table = SymbolTable::new();
        assert_eq!(table.scope_level(), 0);

        table.enter_scope();
        assert_eq!(table.scope_level(), 1);
        assert!(!table.is_global_scope());

        table.exit_scope();
        assert_eq!(table.scope_level(), 0);
        assert!(table.is_global_scope());
    }

    #[test]
    fn test_symbol_table_scope_isolation() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Insert in global scope
        let global_symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        table.insert(global_symbol).unwrap();

        // Enter new scope
        table.enter_scope();

        // Can still see global symbol via lookup
        assert!(table.lookup("x").is_some());

        // Insert same name in local scope (allowed - shadowing for Tier 2)
        let local_symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::byte(),
                span,
            },
            scope_level: 1,
        };
        // Shadowing is allowed across scopes (preparation for Tier 2)
        // Lookup will find local first, then global
        assert!(table.insert(local_symbol).is_ok());
        
        // Lookup should find the local symbol first
        let found = table.lookup("x");
        assert!(found.is_some());
        // The found symbol should be from the local scope
        assert_eq!(found.unwrap().scope_level, 1);
    }

    #[test]
    fn test_symbol_table_multiple_symbols() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        let symbols = vec![
            Symbol {
                kind: SymbolKind::Variable {
                    name: "x".to_string(),
                    var_type: Type::integer(),
                    span,
                },
                scope_level: 0,
            },
            Symbol {
                kind: SymbolKind::Variable {
                    name: "y".to_string(),
                    var_type: Type::byte(),
                    span,
                },
                scope_level: 0,
            },
            Symbol {
                kind: SymbolKind::Constant {
                    name: "MAX".to_string(),
                    const_type: Type::integer(),
                    value: Some(ConstantValue::Integer(100)),
                    span,
                },
                scope_level: 0,
            },
        ];

        for symbol in symbols {
            assert!(table.insert(symbol).is_ok());
        }

        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_some());
        assert!(table.lookup("MAX").is_some());
        assert_eq!(table.current_scope_symbols().len(), 3);
    }

    #[test]
    fn test_symbol_table_exit_global_scope() {
        let mut table = SymbolTable::new();
        // Should not be able to exit global scope
        table.exit_scope();
        assert_eq!(table.scope_level(), 0);
    }

    #[test]
    fn test_parameter_mode() {
        assert_eq!(ParameterMode::Value, ParameterMode::Value);
        assert_eq!(ParameterMode::Var, ParameterMode::Var);
        assert_eq!(ParameterMode::Const, ParameterMode::Const);
        assert_ne!(ParameterMode::Value, ParameterMode::Var);
    }

    #[test]
    fn test_constant_value() {
        let values = vec![
            ConstantValue::Integer(42),
            ConstantValue::Byte(255),
            ConstantValue::Word(65535),
            ConstantValue::Boolean(true),
            ConstantValue::Char(b'A'),
            ConstantValue::String("Hello".to_string()),
        ];

        for value in values {
            // Just verify they can be created and compared
            assert_eq!(value.clone(), value);
        }
    }

    // ===== Additional Symbol Tests =====

    #[test]
    fn test_procedure_with_parameters() {
        let span = Span::new(0, 15, 1, 1);
        let params = vec![
            Parameter {
                name: "x".to_string(),
                param_type: Type::integer(),
                passing_mode: ParameterMode::Value,
                span,
            },
            Parameter {
                name: "y".to_string(),
                param_type: Type::integer(),
                passing_mode: ParameterMode::Var,
                span,
            },
        ];

        let symbol = Symbol {
            kind: SymbolKind::Procedure {
                name: "Add".to_string(),
                params: params.clone(),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "Add");
        if let SymbolKind::Procedure { params: p, .. } = symbol.kind {
            assert_eq!(p.len(), 2);
            assert_eq!(p[0].name, "x");
            assert_eq!(p[1].name, "y");
            assert_eq!(p[0].passing_mode, ParameterMode::Value);
            assert_eq!(p[1].passing_mode, ParameterMode::Var);
        } else {
            panic!("Expected Procedure symbol");
        }
    }

    #[test]
    fn test_function_with_parameters() {
        let span = Span::new(0, 15, 1, 1);
        let params = vec![Parameter {
            name: "x".to_string(),
            param_type: Type::integer(),
            passing_mode: ParameterMode::Value,
            span,
        }];

        let symbol = Symbol {
            kind: SymbolKind::Function {
                name: "Square".to_string(),
                params: params.clone(),
                return_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.name(), "Square");
        if let SymbolKind::Function {
            params: p,
            return_type,
            ..
        } = symbol.kind
        {
            assert_eq!(p.len(), 1);
            assert!(return_type.equals(&Type::integer()));
        } else {
            panic!("Expected Function symbol");
        }
    }

    #[test]
    fn test_constant_with_value() {
        let span = Span::new(0, 10, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Constant {
                name: "MAX".to_string(),
                const_type: Type::integer(),
                value: Some(ConstantValue::Integer(100)),
                span,
            },
            scope_level: 0,
        };

        if let SymbolKind::Constant { value, .. } = symbol.kind {
            assert_eq!(value, Some(ConstantValue::Integer(100)));
        } else {
            panic!("Expected Constant symbol");
        }
    }

    #[test]
    fn test_constant_without_value() {
        let span = Span::new(0, 10, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::Constant {
                name: "MAX".to_string(),
                const_type: Type::integer(),
                value: None,
                span,
            },
            scope_level: 0,
        };

        if let SymbolKind::Constant { value, .. } = symbol.kind {
            assert_eq!(value, None);
        } else {
            panic!("Expected Constant symbol");
        }
    }

    #[test]
    fn test_type_alias_with_type() {
        let span = Span::new(0, 8, 1, 1);
        let symbol = Symbol {
            kind: SymbolKind::TypeAlias {
                name: "MyInt".to_string(),
                aliased_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        if let SymbolKind::TypeAlias { aliased_type, .. } = symbol.kind {
            assert!(aliased_type.equals(&Type::integer()));
        } else {
            panic!("Expected TypeAlias symbol");
        }
    }

    // ===== Additional Symbol Table Tests =====

    #[test]
    fn test_symbol_table_multiple_scopes() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Insert in global scope
        let global = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        table.insert(global).unwrap();

        // Enter scope 1
        table.enter_scope();
        let local1 = Symbol {
            kind: SymbolKind::Variable {
                name: "y".to_string(),
                var_type: Type::byte(),
                span,
            },
            scope_level: 1,
        };
        table.insert(local1).unwrap();

        // Enter scope 2
        table.enter_scope();
        let local2 = Symbol {
            kind: SymbolKind::Variable {
                name: "z".to_string(),
                var_type: Type::word(),
                span,
            },
            scope_level: 2,
        };
        table.insert(local2).unwrap();

        // All should be visible from scope 2
        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_some());
        assert!(table.lookup("z").is_some());

        // Exit scope 2
        table.exit_scope();
        assert_eq!(table.scope_level(), 1);
        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_some());
        assert!(table.lookup("z").is_none()); // z no longer visible

        // Exit scope 1
        table.exit_scope();
        assert_eq!(table.scope_level(), 0);
        assert!(table.lookup("x").is_some());
        assert!(table.lookup("y").is_none()); // y no longer visible
    }

    #[test]
    fn test_symbol_table_shadowing_behavior() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Global symbol
        let global = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        table.insert(global).unwrap();

        // Local symbol with same name
        table.enter_scope();
        let local = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::byte(),
                span,
            },
            scope_level: 1,
        };
        table.insert(local).unwrap();

        // Lookup should find local first
        let found = table.lookup("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().scope_level, 1);

        // Current scope lookup should find local
        assert!(table.lookup_current_scope("x").is_some());
        assert_eq!(table.lookup_current_scope("x").unwrap().scope_level, 1);

        // Exit scope, should find global again
        table.exit_scope();
        let found = table.lookup("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().scope_level, 0);
    }

    #[test]
    fn test_symbol_table_all_symbol_kinds() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 10, 1, 1);

        let symbols = vec![
            Symbol {
                kind: SymbolKind::Variable {
                    name: "var1".to_string(),
                    var_type: Type::integer(),
                    span,
                },
                scope_level: 0,
            },
            Symbol {
                kind: SymbolKind::Constant {
                    name: "const1".to_string(),
                    const_type: Type::integer(),
                    value: Some(ConstantValue::Integer(42)),
                    span,
                },
                scope_level: 0,
            },
            Symbol {
                kind: SymbolKind::TypeAlias {
                    name: "type1".to_string(),
                    aliased_type: Type::integer(),
                    span,
                },
                scope_level: 0,
            },
            Symbol {
                kind: SymbolKind::Procedure {
                    name: "proc1".to_string(),
                    params: vec![],
                    span,
                },
                scope_level: 0,
            },
            Symbol {
                kind: SymbolKind::Function {
                    name: "func1".to_string(),
                    params: vec![],
                    return_type: Type::integer(),
                    span,
                },
                scope_level: 0,
            },
        ];

        for symbol in symbols {
            assert!(table.insert(symbol).is_ok());
        }

        assert!(table.lookup("var1").is_some());
        assert!(table.lookup("const1").is_some());
        assert!(table.lookup("type1").is_some());
        assert!(table.lookup("proc1").is_some());
        assert!(table.lookup("func1").is_some());
        assert_eq!(table.current_scope_symbols().len(), 5);
    }

    #[test]
    fn test_symbol_table_empty_lookup() {
        let table = SymbolTable::new();
        assert!(table.lookup("nonexistent").is_none());
        assert!(table.lookup_current_scope("nonexistent").is_none());
        assert!(!table.exists_in_current_scope("nonexistent"));
    }

    #[test]
    fn test_symbol_table_empty_current_scope() {
        let table = SymbolTable::new();
        assert!(table.current_scope_symbols().is_empty());
    }

    #[test]
    fn test_symbol_table_scope_level_tracking() {
        let mut table = SymbolTable::new();
        assert_eq!(table.scope_level(), 0);

        table.enter_scope();
        assert_eq!(table.scope_level(), 1);

        table.enter_scope();
        assert_eq!(table.scope_level(), 2);

        table.exit_scope();
        assert_eq!(table.scope_level(), 1);

        table.exit_scope();
        assert_eq!(table.scope_level(), 0);
    }

    #[test]
    fn test_symbol_table_duplicate_in_same_scope() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        let symbol1 = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        let symbol2 = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::byte(),
                span,
            },
            scope_level: 0,
        };

        assert!(table.insert(symbol1).is_ok());
        assert!(table.insert(symbol2).is_err());
    }

    #[test]
    fn test_symbol_table_different_names_same_scope() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        let symbol1 = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        let symbol2 = Symbol {
            kind: SymbolKind::Variable {
                name: "y".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert!(table.insert(symbol1).is_ok());
        assert!(table.insert(symbol2).is_ok());
        assert_eq!(table.current_scope_symbols().len(), 2);
    }

    #[test]
    fn test_symbol_span_access() {
        let span = Span::new(10, 20, 5, 10);
        let symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };

        assert_eq!(symbol.span(), span);
    }

    #[test]
    fn test_symbol_name_access_all_kinds() {
        let span = Span::new(0, 5, 1, 1);
        let test_cases = vec![
            (
                SymbolKind::Variable {
                    name: "var".to_string(),
                    var_type: Type::integer(),
                    span,
                },
                "var",
            ),
            (
                SymbolKind::Constant {
                    name: "const".to_string(),
                    const_type: Type::integer(),
                    value: None,
                    span,
                },
                "const",
            ),
            (
                SymbolKind::TypeAlias {
                    name: "type".to_string(),
                    aliased_type: Type::integer(),
                    span,
                },
                "type",
            ),
            (
                SymbolKind::Procedure {
                    name: "proc".to_string(),
                    params: vec![],
                    span,
                },
                "proc",
            ),
            (
                SymbolKind::Function {
                    name: "func".to_string(),
                    params: vec![],
                    return_type: Type::integer(),
                    span,
                },
                "func",
            ),
        ];

        for (kind, expected_name) in test_cases {
            let symbol = Symbol {
                kind,
                scope_level: 0,
            };
            assert_eq!(symbol.name(), expected_name);
        }
    }

    #[test]
    fn test_parameter_modes_all() {
        assert_eq!(ParameterMode::Value, ParameterMode::Value);
        assert_eq!(ParameterMode::Var, ParameterMode::Var);
        assert_eq!(ParameterMode::Const, ParameterMode::Const);
        assert_ne!(ParameterMode::Value, ParameterMode::Var);
        assert_ne!(ParameterMode::Value, ParameterMode::Const);
        assert_ne!(ParameterMode::Var, ParameterMode::Const);
    }

    #[test]
    fn test_constant_value_all_types() {
        let int_val = ConstantValue::Integer(-42);
        let byte_val = ConstantValue::Byte(255);
        let word_val = ConstantValue::Word(65535);
        let bool_val = ConstantValue::Boolean(true);
        let char_val = ConstantValue::Char(b'Z');
        let str_val = ConstantValue::String("Test".to_string());

        // Verify all can be created and cloned
        assert_eq!(int_val.clone(), int_val);
        assert_eq!(byte_val.clone(), byte_val);
        assert_eq!(word_val.clone(), word_val);
        assert_eq!(bool_val.clone(), bool_val);
        assert_eq!(char_val.clone(), char_val);
        assert_eq!(str_val.clone(), str_val);

        // Verify they're not equal to each other
        assert_ne!(int_val, byte_val);
        assert_ne!(bool_val, char_val);
        assert_ne!(str_val, int_val);
    }

    #[test]
    fn test_symbol_table_nested_scopes_complex() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Global: a, b
        table.insert(Symbol {
            kind: SymbolKind::Variable {
                name: "a".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        }).unwrap();
        table.insert(Symbol {
            kind: SymbolKind::Variable {
                name: "b".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        }).unwrap();

        // Scope 1: c (shadows nothing)
        table.enter_scope();
        table.insert(Symbol {
            kind: SymbolKind::Variable {
                name: "c".to_string(),
                var_type: Type::byte(),
                span,
            },
            scope_level: 1,
        }).unwrap();

        // Scope 2: a (shadows global a), d
        table.enter_scope();
        table.insert(Symbol {
            kind: SymbolKind::Variable {
                name: "a".to_string(),
                var_type: Type::word(),
                span,
            },
            scope_level: 2,
        }).unwrap();
        table.insert(Symbol {
            kind: SymbolKind::Variable {
                name: "d".to_string(),
                var_type: Type::boolean(),
                span,
            },
            scope_level: 2,
        }).unwrap();

        // From scope 2, should see: a (local), b (global), c (scope 1), d (local)
        assert!(table.lookup("a").is_some());
        assert_eq!(table.lookup("a").unwrap().scope_level, 2);
        assert!(table.lookup("b").is_some());
        assert_eq!(table.lookup("b").unwrap().scope_level, 0);
        assert!(table.lookup("c").is_some());
        assert_eq!(table.lookup("c").unwrap().scope_level, 1);
        assert!(table.lookup("d").is_some());
        assert_eq!(table.lookup("d").unwrap().scope_level, 2);
    }

    // ===== Turbo Pascal Hash Function Tests =====

    #[test]
    fn test_turbopascal_hash_basic() {
        // Test basic hash calculation
        let hash1 = turbopascal_hash("test");
        let hash2 = turbopascal_hash("TEST");
        let hash3 = turbopascal_hash("Test");
        
        // Hash should be case-insensitive
        assert_eq!(hash1, hash2);
        assert_eq!(hash1, hash3);
    }

    #[test]
    fn test_turbopascal_hash_different_identifiers() {
        // Different identifiers should (usually) produce different hashes
        let hash1 = turbopascal_hash("x");
        let hash2 = turbopascal_hash("y");
        let hash3 = turbopascal_hash("variable");
        
        // They should be different (collisions are possible but unlikely for these)
        assert_ne!(hash1, hash2);
        assert_ne!(hash1, hash3);
        assert_ne!(hash2, hash3);
    }

    #[test]
    fn test_turbopascal_hash_case_insensitive() {
        // Verify case-insensitive hashing
        let variants = vec![
            "identifier",
            "IDENTIFIER",
            "Identifier",
            "IdEnTiFiEr",
            "iDeNtIfIeR",
        ];
        
        let first_hash = turbopascal_hash(variants[0]);
        for variant in variants.iter().skip(1) {
            assert_eq!(first_hash, turbopascal_hash(variant), 
                "Hash should be case-insensitive for: {}", variant);
        }
    }

    #[test]
    fn test_turbopascal_hash_length_factor() {
        // Hash includes length, so different lengths should produce different hashes
        let hash1 = turbopascal_hash("a");
        let hash2 = turbopascal_hash("ab");
        let hash3 = turbopascal_hash("abc");
        
        assert_ne!(hash1, hash2);
        assert_ne!(hash1, hash3);
        assert_ne!(hash2, hash3);
    }

    #[test]
    fn test_hash_bucket() {
        // Test bucket selection with different masks
        let hash = turbopascal_hash("test");
        
        // Mask 0xFF (256 buckets)
        let bucket1 = hash_bucket(hash, 0xFF);
        assert!(bucket1 < 256);
        
        // Mask 0x0F (16 buckets)
        let bucket2 = hash_bucket(hash, 0x0F);
        assert!(bucket2 < 16);
        
        // Mask 0x03 (4 buckets)
        let bucket3 = hash_bucket(hash, 0x03);
        assert!(bucket3 < 4);
    }

    #[test]
    fn test_hash_bucket_distribution() {
        // Test that different identifiers map to different buckets (usually)
        let identifiers = vec!["x", "y", "z", "variable", "function", "type", "constant"];
        let mask = 0x0F; // 16 buckets
        
        let mut buckets = std::collections::HashSet::new();
        for ident in &identifiers {
            let hash = turbopascal_hash(ident);
            let bucket = hash_bucket(hash, mask);
            buckets.insert(bucket);
        }
        
        // Most identifiers should map to different buckets
        // (some collisions are expected, but not all)
        assert!(buckets.len() > 1, "Identifiers should distribute across multiple buckets");
    }

    // ===== Symbol Table Compacting Tests =====

    #[test]
    fn test_symbol_table_compact() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Add some symbols
        for i in 0..10 {
            let symbol = Symbol {
                kind: SymbolKind::Variable {
                    name: format!("var{}", i),
                    var_type: Type::integer(),
                    span,
                },
                scope_level: 0,
            };
            table.insert(symbol).unwrap();
        }

        // Get stats before compacting
        let stats_before = table.stats();
        assert!(stats_before.total_symbols == 10);

        // Compact
        table.compact();

        // Get stats after compacting
        let stats_after = table.stats();
        assert_eq!(stats_after.total_symbols, 10); // Symbols should still be there
        assert!(stats_after.total_capacity <= stats_before.total_capacity); // Capacity should be reduced or same

        // Verify symbols are still accessible
        assert!(table.lookup("var0").is_some());
        assert!(table.lookup("var9").is_some());
    }

    #[test]
    fn test_symbol_table_compact_empty_scopes() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Add symbol to global scope
        let symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        table.insert(symbol).unwrap();

        // Enter and exit a scope (creates empty scope)
        table.enter_scope();
        table.exit_scope();

        let stats_before = table.stats();
        assert!(stats_before.scope_count >= 1);

        // Compact should remove empty scopes
        table.compact();

        let stats_after = table.stats();
        // Should have at least one scope (global)
        assert!(stats_after.scope_count >= 1);

        // Symbol should still be accessible
        assert!(table.lookup("x").is_some());
    }

    #[test]
    fn test_symbol_table_stats() {
        let mut table = SymbolTable::new();
        let span = Span::new(0, 5, 1, 1);

        // Empty table stats
        let stats = table.stats();
        assert_eq!(stats.scope_count, 1); // Global scope
        assert_eq!(stats.current_scope, 0);
        assert_eq!(stats.total_symbols, 0);

        // Add symbols
        for i in 0..5 {
            let symbol = Symbol {
                kind: SymbolKind::Variable {
                    name: format!("var{}", i),
                    var_type: Type::integer(),
                    span,
                },
                scope_level: 0,
            };
            table.insert(symbol).unwrap();
        }

        let stats = table.stats();
        assert_eq!(stats.total_symbols, 5);
        assert!(stats.total_capacity > 0);
    }
}
