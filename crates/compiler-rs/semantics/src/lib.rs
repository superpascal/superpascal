//! SuperPascal Semantic Analysis
//!
//! This crate performs semantic analysis on the AST, including:
//! - Type checking
//! - Symbol resolution
//! - Assignment compatibility checking
//! - Constant folding
//! - Control flow analysis (future)

mod core;
mod declarations;
mod statements;
mod expressions;
mod types;
mod constants;
mod lvalues;
pub mod feature_checker;

// Declaration analysis functions are in declarations.rs module
// They extend SemanticAnalyzer via impl blocks

use ast::Node;
use errors::Diagnostic;
use symbols::SymbolTable;

/// Semantic analyzer
pub struct SemanticAnalyzer {
    core: core::CoreAnalyzer,
}

impl SemanticAnalyzer {
    /// Create a new semantic analyzer
    pub fn new(filename: Option<String>) -> Self {
        Self {
            core: core::CoreAnalyzer::new(filename),
        }
    }

    /// Analyze a program AST
    pub fn analyze(&mut self, program: &Node) -> Vec<Diagnostic> {
        self.core.diagnostics.clear();
        self.core.symbol_table = SymbolTable::new();

        if let Node::Program(prog) = program {
            // Analyze the program block
            self.analyze_block(&prog.block);
        }

        self.core.diagnostics.clone()
    }

    /// Analyze a block (declarations and statements)
    fn analyze_block(&mut self, block: &Node) {
        if let Node::Block(blk) = block {
            // First, process all declarations
            for const_decl in &blk.const_decls {
                self.analyze_const_decl(const_decl);
            }
            for type_decl in &blk.type_decls {
                self.analyze_type_decl(type_decl);
            }
            for var_decl in &blk.var_decls {
                self.analyze_var_decl(var_decl);
            }
            for proc_decl in &blk.proc_decls {
                self.analyze_proc_decl(proc_decl);
            }
            for func_decl in &blk.func_decls {
                self.analyze_func_decl(func_decl);
            }

            // Then, analyze statements
            for stmt in &blk.statements {
                self.analyze_statement(stmt);
            }
        }
    }

    // Declaration analysis functions moved to declarations.rs module

    // Type analysis functions moved to types.rs module
    // Statement analysis functions moved to statements.rs module

    // Lvalue analysis functions moved to lvalues.rs module

    // Expression analysis functions moved to expressions.rs module

    // Constant evaluation functions moved to constants.rs module
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use tokens::Span;
    use symbols::{ConstantValue, Symbol, SymbolKind};
    use ::types::Type;

    #[test]
    fn test_semantic_analyzer_new() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        assert_eq!(analyzer.core.diagnostics.len(), 0);
        assert!(analyzer.core.symbol_table.is_global_scope());
    }

    #[test]
    fn test_analyze_simple_program() {
        // This is a basic test - full tests will be added as we implement
        let mut analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);

        // Create a simple program: program Test; begin end.
        let block = Node::Block(Block {
            directives: vec![],
            label_decls: vec![],
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            threadvar_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            operator_decls: vec![],
            statements: vec![],
            span,
        });

        let program = Node::Program(Program {
            directives: vec![],
            name: "Test".to_string(),
            block: Box::new(block),
            span,
        });

        let diagnostics = analyzer.analyze(&program);
        assert_eq!(diagnostics.len(), 0); // No errors in empty program
    }

    #[test]
    fn test_constant_folding_literal() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 5, 1, 1);
        
        // Test literal evaluation
        let lit = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(42),
            span,
        });
        
        let result = analyzer.evaluate_constant_expression(&lit);
        assert_eq!(result, Some(ConstantValue::Integer(42)));
    }

    #[test]
    fn test_constant_folding_binary_add() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);
        
        // Test: 5 + 3 = 8
        let left = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(5),
            span,
        });
        let right = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(3),
            span,
        });
        let expr = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Add,
            left: Box::new(left),
            right: Box::new(right),
            span,
        });
        
        let result = analyzer.evaluate_constant_expression(&expr);
        assert_eq!(result, Some(ConstantValue::Integer(8)));
    }

    #[test]
    fn test_constant_folding_binary_multiply() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);
        
        // Test: 6 * 7 = 42
        let expr = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Multiply,
            left: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(6),
                span,
            })),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(7),
                span,
            })),
            span,
        });
        
        let result = analyzer.evaluate_constant_expression(&expr);
        assert_eq!(result, Some(ConstantValue::Integer(42)));
    }

    #[test]
    fn test_constant_folding_binary_compare() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);
        
        // Test: 5 < 10 = true
        let expr = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Less,
            left: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(5),
                span,
            })),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            span,
        });
        
        let result = analyzer.evaluate_constant_expression(&expr);
        assert_eq!(result, Some(ConstantValue::Boolean(true)));
    }

    #[test]
    fn test_constant_folding_unary_not() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);
        
        // Test: not true = false
        let expr = Node::UnaryExpr(UnaryExpr {
            op: UnaryOp::Not,
            expr: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Boolean(true),
                span,
            })),
            span,
        });
        
        let result = analyzer.evaluate_constant_expression(&expr);
        assert_eq!(result, Some(ConstantValue::Boolean(false)));
    }

    #[test]
    fn test_constant_folding_if_true() {
        let span = Span::new(0, 10, 1, 1);
        
        // Test IF with constant true condition
        // var x: integer;
        // if true then x := 1 else x := 2
        let var_decl = Node::VarDecl(ast::VarDecl {
            names: vec!["x".to_string()],
            type_expr: Box::new(Node::NamedType(ast::NamedType {
                name: "integer".to_string(),
                span,
            })),
            is_class_var: false,
            absolute_address: None,
            span,
        });
        
        let if_stmt = IfStmt {
            condition: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Boolean(true),
                span,
            })),
            then_block: Box::new(Node::AssignStmt(AssignStmt {
                target: Box::new(Node::IdentExpr(IdentExpr {
                    name: "x".to_string(),
                    span,
                })),
                value: Box::new(Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(1),
                    span,
                })),
                span,
            })),
            else_block: Some(Box::new(Node::AssignStmt(AssignStmt {
                target: Box::new(Node::IdentExpr(IdentExpr {
                    name: "x".to_string(),
                    span,
                })),
                value: Box::new(Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(2),
                    span,
                })),
                span,
            }))),
            span,
        };
        
        // Test via analyze() - should only analyze then branch (constant folding)
        let mut analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let block = Node::Block(Block {
            directives: vec![],
            label_decls: vec![],
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![var_decl],
            threadvar_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            operator_decls: vec![],
            statements: vec![Node::IfStmt(if_stmt)],
            span,
        });
        let program = Node::Program(Program {
            directives: vec![],
            name: "Test".to_string(),
            block: Box::new(block),
            span,
        });
        let diagnostics = analyzer.analyze(&program);
        
        // Should have no errors (else branch is dead code, not analyzed)
        assert_eq!(diagnostics.len(), 0);
    }

    #[test]
    fn test_constant_folding_while_false() {
        let span = Span::new(0, 10, 1, 1);
        
        // Test WHILE with constant false condition
        // while false do x := 1
        let while_stmt = WhileStmt {
            condition: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Boolean(false),
                span,
            })),
            body: Box::new(Node::AssignStmt(AssignStmt {
                target: Box::new(Node::IdentExpr(IdentExpr {
                    name: "x".to_string(),
                    span,
                })),
                value: Box::new(Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(1),
                    span,
                })),
                span,
            })),
            span,
        };
        
        // Test via analyze() - should skip body (constant folding)
        let mut analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        // Add variable x to symbol table
        let var_symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        analyzer.core.symbol_table.insert(var_symbol).unwrap();
        
        let block = Node::Block(Block {
            directives: vec![],
            label_decls: vec![],
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            threadvar_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            operator_decls: vec![],
            statements: vec![Node::WhileStmt(while_stmt)],
            span,
        });
        let program = Node::Program(Program {
            directives: vec![],
            name: "Test".to_string(),
            block: Box::new(block),
            span,
        });
        let diagnostics = analyzer.analyze(&program);
        
        // Should have no errors (body is dead code, not analyzed)
        assert_eq!(diagnostics.len(), 0);
    }
}
