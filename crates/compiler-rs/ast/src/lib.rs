//! SuperPascal Abstract Syntax Tree (AST)
//!
//! This crate defines the AST node types for the SuperPascal compiler.
//! The AST represents the syntactic structure of Pascal programs.

use tokens::Span;

/// AST node - represents any node in the abstract syntax tree
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    // ===== Program Structure =====
    Program(Program),
    Block(Block),

    // ===== Declarations =====
    VarDecl(VarDecl),
    ConstDecl(ConstDecl),
    TypeDecl(TypeDecl),
    ProcDecl(ProcDecl),
    FuncDecl(FuncDecl),

    // ===== Statements =====
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    ForStmt(ForStmt),
    RepeatStmt(RepeatStmt),
    CaseStmt(CaseStmt),
    AssignStmt(AssignStmt),
    CallStmt(CallStmt),
    TryStmt(TryStmt),
    RaiseStmt(RaiseStmt),

    // ===== Expressions =====
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    LiteralExpr(LiteralExpr),
    IdentExpr(IdentExpr),
    CallExpr(CallExpr),
    IndexExpr(IndexExpr),
    FieldExpr(FieldExpr),

    // ===== Types =====
    RecordType(RecordType),
    ArrayType(ArrayType),
    NamedType(NamedType),
}

/// Program node - root of the AST
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub name: String,
    pub block: Box<Node>, // Block node
    pub span: Span,
}

/// Block node - contains declarations and statements
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub const_decls: Vec<Node>,  // ConstDecl nodes
    pub type_decls: Vec<Node>,   // TypeDecl nodes
    pub var_decls: Vec<Node>,    // VarDecl nodes
    pub proc_decls: Vec<Node>,   // ProcDecl nodes
    pub func_decls: Vec<Node>,    // FuncDecl nodes
    pub statements: Vec<Node>,    // Statement nodes
    pub span: Span,
}

/// Variable declaration
#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub names: Vec<String>,      // Variable names
    pub type_expr: Box<Node>,     // Type node
    pub span: Span,
}

/// Constant declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDecl {
    pub name: String,
    pub value: Box<Node>,         // Expression node
    pub span: Span,
}

/// Type declaration
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub type_expr: Box<Node>,     // Type node
    pub span: Span,
}

/// Procedure declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ProcDecl {
    pub name: String,
    pub params: Vec<Param>,        // Parameters
    pub block: Box<Node>,          // Block node
    pub span: Span,
}

/// Function declaration
#[derive(Debug, Clone, PartialEq)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<Param>,        // Parameters
    pub return_type: Box<Node>,    // Type node
    pub block: Box<Node>,           // Block node
    pub span: Span,
}

/// Function/procedure parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub names: Vec<String>,        // Parameter names
    pub param_type: ParamType,     // Parameter passing mode
    pub type_expr: Box<Node>,      // Type node
    pub span: Span,
}

/// Parameter passing mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamType {
    Value,   // Pass by value (default)
    Var,     // Pass by reference (var)
    Const,   // Pass by constant reference (const)
}

/// If statement
#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Box<Node>,      // Expression node
    pub then_block: Box<Node>,     // Statement or Block node
    pub else_block: Option<Box<Node>>, // Optional else block
    pub span: Span,
}

/// While statement
#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Box<Node>,      // Expression node
    pub body: Box<Node>,            // Statement or Block node
    pub span: Span,
}

/// For statement
#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub var_name: String,           // Loop variable
    pub start_expr: Box<Node>,      // Expression node (initial value)
    pub direction: ForDirection,    // To or Downto
    pub end_expr: Box<Node>,         // Expression node (final value)
    pub body: Box<Node>,             // Statement or Block node
    pub span: Span,
}

/// For loop direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForDirection {
    To,      // for i := 1 to 10
    Downto,  // for i := 10 downto 1
}

/// Repeat statement
#[derive(Debug, Clone, PartialEq)]
pub struct RepeatStmt {
    pub statements: Vec<Node>,     // Statement nodes
    pub condition: Box<Node>,       // Expression node
    pub span: Span,
}

/// Case statement
#[derive(Debug, Clone, PartialEq)]
pub struct CaseStmt {
    pub expr: Box<Node>,            // Expression node
    pub cases: Vec<CaseBranch>,     // Case branches
    pub else_branch: Option<Box<Node>>, // Optional else branch
    pub span: Span,
}

/// Case branch
#[derive(Debug, Clone, PartialEq)]
pub struct CaseBranch {
    pub values: Vec<Node>,           // Expression nodes (case values)
    pub statement: Box<Node>,       // Statement or Block node
    pub span: Span,
}

/// Assignment statement
#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub target: Box<Node>,          // LValue node (IdentExpr, IndexExpr, FieldExpr)
    pub value: Box<Node>,           // Expression node
    pub span: Span,
}

/// Call statement (procedure call)
#[derive(Debug, Clone, PartialEq)]
pub struct CallStmt {
    pub name: String,
    pub args: Vec<Node>,            // Expression nodes
    pub span: Span,
}

/// Try statement: TRY ... EXCEPT/FINALLY ... END
#[derive(Debug, Clone, PartialEq)]
pub struct TryStmt {
    pub try_block: Vec<Node>,        // Statements in try block
    pub except_block: Option<Vec<Node>>, // Statements in except block (if EXCEPT)
    pub finally_block: Option<Vec<Node>>, // Statements in finally block (if FINALLY)
    pub exception_handlers: Vec<ExceptionHandler>, // Exception handlers (ON ... DO)
    pub exception_else: Option<Box<Node>>, // Else clause in exception handlers
    pub span: Span,
}

/// Exception handler: ON exception_type DO statement
#[derive(Debug, Clone, PartialEq)]
pub struct ExceptionHandler {
    pub variable: Option<String>,    // Optional variable name
    pub exception_type: Box<Node>,    // Exception type (type reference)
    pub handler: Box<Node>,           // Handler statement
    pub span: Span,
}

/// Raise statement: RAISE [exception]
#[derive(Debug, Clone, PartialEq)]
pub struct RaiseStmt {
    pub exception: Option<Box<Node>>, // Optional exception expression
    pub span: Span,
}

/// Binary expression
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub op: BinaryOp,
    pub left: Box<Node>,            // Expression node
    pub right: Box<Node>,           // Expression node
    pub span: Span,
}

/// Binary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Div,      // div
    Mod,      // mod

    // Comparison
    Equal,    // =
    NotEqual, // <>
    Less,     // <
    LessEqual, // <=
    Greater,  // >
    GreaterEqual, // >=

    // Logical
    And,      // and
    Or,       // or
}

/// Unary expression
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub expr: Box<Node>,            // Expression node
    pub span: Span,
}

/// Unary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,     // + (unary plus)
    Minus,    // - (unary minus)
    Not,      // not
}

/// Literal expression
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr {
    pub value: LiteralValue,
    pub span: Span,
}

/// Literal value
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(u16),
    Char(u8),
    String(String),
    Boolean(bool),
}

/// Identifier expression
#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
    pub span: Span,
}

/// Call expression (function call)
#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub name: String,
    pub args: Vec<Node>,            // Expression nodes
    pub span: Span,
}

/// Index expression (array access)
#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub array: Box<Node>,           // Expression node (array)
    pub index: Box<Node>,           // Expression node (index)
    pub span: Span,
}

/// Field expression (record field access)
#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr {
    pub record: Box<Node>,          // Expression node (record)
    pub field: String,               // Field name
    pub span: Span,
}

/// Record type
#[derive(Debug, Clone, PartialEq)]
pub struct RecordType {
    pub fields: Vec<FieldDecl>,     // Field declarations
    pub span: Span,
}

/// Field declaration (in record type)
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub names: Vec<String>,         // Field names
    pub type_expr: Box<Node>,        // Type node
    pub span: Span,
}

/// Array type
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayType {
    pub index_type: Box<Node>,      // Type node (index type)
    pub element_type: Box<Node>,    // Type node (element type)
    pub span: Span,
}

/// Named type (type alias)
#[derive(Debug, Clone, PartialEq)]
pub struct NamedType {
    pub name: String,
    pub span: Span,
}

// ===== Helper Methods =====

impl Node {
    /// Get the span of this node
    pub fn span(&self) -> Span {
        match self {
            Node::Program(p) => p.span,
            Node::Block(b) => b.span,
            Node::VarDecl(v) => v.span,
            Node::ConstDecl(c) => c.span,
            Node::TypeDecl(t) => t.span,
            Node::ProcDecl(p) => p.span,
            Node::FuncDecl(f) => f.span,
            Node::IfStmt(i) => i.span,
            Node::WhileStmt(w) => w.span,
            Node::ForStmt(f) => f.span,
            Node::RepeatStmt(r) => r.span,
            Node::CaseStmt(c) => c.span,
            Node::AssignStmt(a) => a.span,
            Node::CallStmt(c) => c.span,
            Node::TryStmt(t) => t.span,
            Node::RaiseStmt(r) => r.span,
            Node::BinaryExpr(b) => b.span,
            Node::UnaryExpr(u) => u.span,
            Node::LiteralExpr(l) => l.span,
            Node::IdentExpr(i) => i.span,
            Node::CallExpr(c) => c.span,
            Node::IndexExpr(i) => i.span,
            Node::FieldExpr(f) => f.span,
            Node::RecordType(r) => r.span,
            Node::ArrayType(a) => a.span,
            Node::NamedType(n) => n.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===== Program Structure Tests =====

    #[test]
    fn test_program_node() {
        let span = Span::new(0, 10, 1, 1);
        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span,
        });
        let program = Node::Program(Program {
            name: "HelloWorld".to_string(),
            block: Box::new(block),
            span,
        });
        assert_eq!(program.span(), span);
    }

    #[test]
    fn test_block_node() {
        let span = Span::new(0, 20, 1, 1);
        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span,
        });
        assert_eq!(block.span(), span);
    }

    #[test]
    fn test_block_with_declarations() {
        let span = Span::new(0, 50, 1, 1);
        let var_decl = Node::VarDecl(VarDecl {
            names: vec!["x".to_string(), "y".to_string()],
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        });
        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![var_decl],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span,
        });
        assert_eq!(block.span(), span);
    }

    // ===== Declaration Tests =====

    #[test]
    fn test_var_decl() {
        let span = Span::new(0, 15, 1, 1);
        let var_decl = Node::VarDecl(VarDecl {
            names: vec!["x".to_string()],
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        });
        assert_eq!(var_decl.span(), span);
    }

    #[test]
    fn test_var_decl_multiple_names() {
        let span = Span::new(0, 20, 1, 1);
        let var_decl = Node::VarDecl(VarDecl {
            names: vec!["a".to_string(), "b".to_string(), "c".to_string()],
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        });
        assert_eq!(var_decl.span(), span);
    }

    #[test]
    fn test_const_decl() {
        let span = Span::new(0, 15, 1, 1);
        let const_decl = Node::ConstDecl(ConstDecl {
            name: "MAX_SIZE".to_string(),
            value: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(100),
                span,
            })),
            span,
        });
        assert_eq!(const_decl.span(), span);
    }

    #[test]
    fn test_type_decl() {
        let span = Span::new(0, 20, 1, 1);
        let type_decl = Node::TypeDecl(TypeDecl {
            name: "MyInt".to_string(),
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        });
        assert_eq!(type_decl.span(), span);
    }

    #[test]
    fn test_proc_decl() {
        let span = Span::new(0, 30, 1, 1);
        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span,
        });
        let proc_decl = Node::ProcDecl(ProcDecl {
            name: "DoSomething".to_string(),
            params: vec![],
            block: Box::new(block),
            span,
        });
        assert_eq!(proc_decl.span(), span);
    }

    #[test]
    fn test_proc_decl_with_params() {
        let span = Span::new(0, 40, 1, 1);
        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span,
        });
        let param = Param {
            names: vec!["x".to_string()],
            param_type: ParamType::Value,
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        };
        let proc_decl = Node::ProcDecl(ProcDecl {
            name: "Print".to_string(),
            params: vec![param],
            block: Box::new(block),
            span,
        });
        assert_eq!(proc_decl.span(), span);
    }

    #[test]
    fn test_func_decl() {
        let span = Span::new(0, 35, 1, 1);
        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span,
        });
        let func_decl = Node::FuncDecl(FuncDecl {
            name: "Add".to_string(),
            params: vec![],
            return_type: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            block: Box::new(block),
            span,
        });
        assert_eq!(func_decl.span(), span);
    }

    #[test]
    fn test_param_types() {
        assert_eq!(ParamType::Value, ParamType::Value);
        assert_eq!(ParamType::Var, ParamType::Var);
        assert_eq!(ParamType::Const, ParamType::Const);
        assert_ne!(ParamType::Value, ParamType::Var);
    }

    // ===== Statement Tests =====

    #[test]
    fn test_if_stmt() {
        let span = Span::new(0, 25, 1, 1);
        let condition = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Boolean(true),
            span,
        });
        let then_block = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![],
            span,
        });
        let if_stmt = Node::IfStmt(IfStmt {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block: None,
            span,
        });
        assert_eq!(if_stmt.span(), span);
    }

    #[test]
    fn test_if_stmt_with_else() {
        let span = Span::new(0, 35, 1, 1);
        let condition = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Boolean(true),
            span,
        });
        let then_block = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::String("yes".to_string()),
                span,
            })],
            span,
        });
        let else_block = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::String("no".to_string()),
                span,
            })],
            span,
        });
        let if_stmt = Node::IfStmt(IfStmt {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block: Some(Box::new(else_block)),
            span,
        });
        assert_eq!(if_stmt.span(), span);
    }

    #[test]
    fn test_while_stmt() {
        let span = Span::new(0, 30, 1, 1);
        let condition = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Less,
            left: Box::new(Node::IdentExpr(IdentExpr {
                name: "i".to_string(),
                span,
            })),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            span,
        });
        let body = Node::AssignStmt(AssignStmt {
            target: Box::new(Node::IdentExpr(IdentExpr {
                name: "i".to_string(),
                span,
            })),
            value: Box::new(Node::BinaryExpr(BinaryExpr {
                op: BinaryOp::Add,
                left: Box::new(Node::IdentExpr(IdentExpr {
                    name: "i".to_string(),
                    span,
                })),
                right: Box::new(Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(1),
                    span,
                })),
                span,
            })),
            span,
        });
        let while_stmt = Node::WhileStmt(WhileStmt {
            condition: Box::new(condition),
            body: Box::new(body),
            span,
        });
        assert_eq!(while_stmt.span(), span);
    }

    #[test]
    fn test_for_stmt_to() {
        let span = Span::new(0, 40, 1, 1);
        let body = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![Node::IdentExpr(IdentExpr {
                name: "i".to_string(),
                span,
            })],
            span,
        });
        let for_stmt = Node::ForStmt(ForStmt {
            var_name: "i".to_string(),
            start_expr: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(1),
                span,
            })),
            direction: ForDirection::To,
            end_expr: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            body: Box::new(body),
            span,
        });
        assert_eq!(for_stmt.span(), span);
    }

    #[test]
    fn test_for_stmt_downto() {
        let span = Span::new(0, 40, 1, 1);
        let body = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![Node::IdentExpr(IdentExpr {
                name: "i".to_string(),
                span,
            })],
            span,
        });
        let for_stmt = Node::ForStmt(ForStmt {
            var_name: "i".to_string(),
            start_expr: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            direction: ForDirection::Downto,
            end_expr: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(1),
                span,
            })),
            body: Box::new(body),
            span,
        });
        assert_eq!(for_stmt.span(), span);
    }

    #[test]
    fn test_repeat_stmt() {
        let span = Span::new(0, 35, 1, 1);
        let statements = vec![
            Node::AssignStmt(AssignStmt {
                target: Box::new(Node::IdentExpr(IdentExpr {
                    name: "i".to_string(),
                    span,
                })),
                value: Box::new(Node::BinaryExpr(BinaryExpr {
                    op: BinaryOp::Add,
                    left: Box::new(Node::IdentExpr(IdentExpr {
                        name: "i".to_string(),
                        span,
                    })),
                    right: Box::new(Node::LiteralExpr(LiteralExpr {
                        value: LiteralValue::Integer(1),
                        span,
                    })),
                    span,
                })),
                span,
            }),
        ];
        let condition = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::GreaterEqual,
            left: Box::new(Node::IdentExpr(IdentExpr {
                name: "i".to_string(),
                span,
            })),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            span,
        });
        let repeat_stmt = Node::RepeatStmt(RepeatStmt {
            statements,
            condition: Box::new(condition),
            span,
        });
        assert_eq!(repeat_stmt.span(), span);
    }

    #[test]
    fn test_case_stmt() {
        let span = Span::new(0, 50, 1, 1);
        let expr = Node::IdentExpr(IdentExpr {
            name: "x".to_string(),
            span,
        });
        let case_branch = CaseBranch {
            values: vec![Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(1),
                span,
            })],
            statement: Box::new(Node::CallStmt(CallStmt {
                name: "writeln".to_string(),
                args: vec![Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::String("One".to_string()),
                    span,
                })],
                span,
            })),
            span,
        };
        let case_stmt = Node::CaseStmt(CaseStmt {
            expr: Box::new(expr),
            cases: vec![case_branch],
            else_branch: None,
            span,
        });
        assert_eq!(case_stmt.span(), span);
    }

    #[test]
    fn test_case_stmt_with_else() {
        let span = Span::new(0, 60, 1, 1);
        let expr = Node::IdentExpr(IdentExpr {
            name: "x".to_string(),
            span,
        });
        let case_branch = CaseBranch {
            values: vec![Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(1),
                span,
            })],
            statement: Box::new(Node::CallStmt(CallStmt {
                name: "writeln".to_string(),
                args: vec![Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::String("One".to_string()),
                    span,
                })],
                span,
            })),
            span,
        };
        let else_branch = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::String("Other".to_string()),
                span,
            })],
            span,
        });
        let case_stmt = Node::CaseStmt(CaseStmt {
            expr: Box::new(expr),
            cases: vec![case_branch],
            else_branch: Some(Box::new(else_branch)),
            span,
        });
        assert_eq!(case_stmt.span(), span);
    }

    #[test]
    fn test_assign_stmt() {
        let span = Span::new(0, 20, 1, 1);
        let assign_stmt = Node::AssignStmt(AssignStmt {
            target: Box::new(Node::IdentExpr(IdentExpr {
                name: "x".to_string(),
                span,
            })),
            value: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(42),
                span,
            })),
            span,
        });
        assert_eq!(assign_stmt.span(), span);
    }

    #[test]
    fn test_call_stmt() {
        let span = Span::new(0, 25, 1, 1);
        let call_stmt = Node::CallStmt(CallStmt {
            name: "writeln".to_string(),
            args: vec![Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::String("Hello".to_string()),
                span,
            })],
            span,
        });
        assert_eq!(call_stmt.span(), span);
    }

    #[test]
    fn test_call_stmt_no_args() {
        let span = Span::new(0, 15, 1, 1);
        let call_stmt = Node::CallStmt(CallStmt {
            name: "DoSomething".to_string(),
            args: vec![],
            span,
        });
        assert_eq!(call_stmt.span(), span);
    }

    // ===== Expression Tests =====

    #[test]
    fn test_literal_expr_integer() {
        let span = Span::new(0, 3, 1, 1);
        let expr = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(42),
            span,
        });
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_literal_expr_char() {
        let span = Span::new(0, 3, 1, 1);
        let expr = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Char(b'A'),
            span,
        });
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_literal_expr_string() {
        let span = Span::new(0, 10, 1, 1);
        let expr = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::String("Hello".to_string()),
            span,
        });
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_literal_expr_boolean() {
        let span = Span::new(0, 5, 1, 1);
        let expr_true = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Boolean(true),
            span,
        });
        let expr_false = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Boolean(false),
            span,
        });
        assert_eq!(expr_true.span(), span);
        assert_eq!(expr_false.span(), span);
    }

    #[test]
    fn test_ident_expr() {
        let span = Span::new(0, 5, 1, 1);
        let expr = Node::IdentExpr(IdentExpr {
            name: "x".to_string(),
            span,
        });
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_binary_expr_arithmetic() {
        let span = Span::new(0, 10, 1, 1);
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
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_binary_expr_all_operators() {
        let span = Span::new(0, 10, 1, 1);
        let left = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(5),
            span,
        });
        let right = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(3),
            span,
        });

        let ops = vec![
            BinaryOp::Add,
            BinaryOp::Subtract,
            BinaryOp::Multiply,
            BinaryOp::Divide,
            BinaryOp::Div,
            BinaryOp::Mod,
            BinaryOp::Equal,
            BinaryOp::NotEqual,
            BinaryOp::Less,
            BinaryOp::LessEqual,
            BinaryOp::Greater,
            BinaryOp::GreaterEqual,
            BinaryOp::And,
            BinaryOp::Or,
        ];

        for op in ops {
            let expr = Node::BinaryExpr(BinaryExpr {
                op,
                left: Box::new(left.clone()),
                right: Box::new(right.clone()),
                span,
            });
            assert_eq!(expr.span(), span);
        }
    }

    #[test]
    fn test_unary_expr() {
        let span = Span::new(0, 8, 1, 1);
        let unary_plus = Node::UnaryExpr(UnaryExpr {
            op: UnaryOp::Plus,
            expr: Box::new(Node::IdentExpr(IdentExpr {
                name: "x".to_string(),
                span,
            })),
            span,
        });
        let unary_minus = Node::UnaryExpr(UnaryExpr {
            op: UnaryOp::Minus,
            expr: Box::new(Node::IdentExpr(IdentExpr {
                name: "x".to_string(),
                span,
            })),
            span,
        });
        let unary_not = Node::UnaryExpr(UnaryExpr {
            op: UnaryOp::Not,
            expr: Box::new(Node::IdentExpr(IdentExpr {
                name: "flag".to_string(),
                span,
            })),
            span,
        });
        assert_eq!(unary_plus.span(), span);
        assert_eq!(unary_minus.span(), span);
        assert_eq!(unary_not.span(), span);
    }

    #[test]
    fn test_call_expr() {
        let span = Span::new(0, 25, 1, 1);
        let call_expr = Node::CallExpr(CallExpr {
            name: "Add".to_string(),
            args: vec![
                Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(5),
                    span,
                }),
                Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(3),
                    span,
                }),
            ],
            span,
        });
        assert_eq!(call_expr.span(), span);
    }

    #[test]
    fn test_index_expr() {
        let span = Span::new(0, 20, 1, 1);
        let array = Node::IdentExpr(IdentExpr {
            name: "arr".to_string(),
            span,
        });
        let index = Node::LiteralExpr(LiteralExpr {
            value: LiteralValue::Integer(0),
            span,
        });
        let index_expr = Node::IndexExpr(IndexExpr {
            array: Box::new(array),
            index: Box::new(index),
            span,
        });
        assert_eq!(index_expr.span(), span);
    }

    #[test]
    fn test_field_expr() {
        let span = Span::new(0, 20, 1, 1);
        let record = Node::IdentExpr(IdentExpr {
            name: "point".to_string(),
            span,
        });
        let field_expr = Node::FieldExpr(FieldExpr {
            record: Box::new(record),
            field: "x".to_string(),
            span,
        });
        assert_eq!(field_expr.span(), span);
    }

    #[test]
    fn test_nested_expressions() {
        let span = Span::new(0, 30, 1, 1);
        // (5 + 3) * 2
        let inner = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Add,
            left: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(5),
                span,
            })),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(3),
                span,
            })),
            span,
        });
        let outer = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Multiply,
            left: Box::new(inner),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(2),
                span,
            })),
            span,
        });
        assert_eq!(outer.span(), span);
    }

    // ===== Type Tests =====

    #[test]
    fn test_record_type() {
        let span = Span::new(0, 40, 1, 1);
        let field_decl = FieldDecl {
            names: vec!["x".to_string(), "y".to_string()],
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        };
        let record_type = Node::RecordType(RecordType {
            fields: vec![field_decl],
            span,
        });
        assert_eq!(record_type.span(), span);
    }

    #[test]
    fn test_array_type() {
        let span = Span::new(0, 30, 1, 1);
        let array_type = Node::ArrayType(ArrayType {
            index_type: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            element_type: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        });
        assert_eq!(array_type.span(), span);
    }

    #[test]
    fn test_named_type() {
        let span = Span::new(0, 10, 1, 1);
        let named_type = Node::NamedType(NamedType {
            name: "integer".to_string(),
            span,
        });
        assert_eq!(named_type.span(), span);
    }

    // ===== Complex Structure Tests =====

    #[test]
    fn test_complex_program_structure() {
        let span = Span::new(0, 100, 1, 1);
        
        // var x: integer;
        let var_decl = Node::VarDecl(VarDecl {
            names: vec!["x".to_string()],
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        });

        // x := 10;
        let assign_stmt = Node::AssignStmt(AssignStmt {
            target: Box::new(Node::IdentExpr(IdentExpr {
                name: "x".to_string(),
                span,
            })),
            value: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            span,
        });

        // if x > 0 then writeln('Positive');
        let if_stmt = Node::IfStmt(IfStmt {
            condition: Box::new(Node::BinaryExpr(BinaryExpr {
                op: BinaryOp::Greater,
                left: Box::new(Node::IdentExpr(IdentExpr {
                    name: "x".to_string(),
                    span,
                })),
                right: Box::new(Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::Integer(0),
                    span,
                })),
                span,
            })),
            then_block: Box::new(Node::CallStmt(CallStmt {
                name: "writeln".to_string(),
                args: vec![Node::LiteralExpr(LiteralExpr {
                    value: LiteralValue::String("Positive".to_string()),
                    span,
                })],
                span,
            })),
            else_block: None,
            span,
        });

        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![var_decl],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![assign_stmt, if_stmt],
            span,
        });

        let program = Node::Program(Program {
            name: "TestProgram".to_string(),
            block: Box::new(block),
            span,
        });

        assert_eq!(program.span(), span);
    }

    #[test]
    fn test_function_with_return() {
        let span = Span::new(0, 50, 1, 1);
        
        // function Add(a, b: integer): integer;
        let param = Param {
            names: vec!["a".to_string(), "b".to_string()],
            param_type: ParamType::Value,
            type_expr: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            span,
        };

        // Add := a + b;
        let return_stmt = Node::AssignStmt(AssignStmt {
            target: Box::new(Node::IdentExpr(IdentExpr {
                name: "Add".to_string(),
                span,
            })),
            value: Box::new(Node::BinaryExpr(BinaryExpr {
                op: BinaryOp::Add,
                left: Box::new(Node::IdentExpr(IdentExpr {
                    name: "a".to_string(),
                    span,
                })),
                right: Box::new(Node::IdentExpr(IdentExpr {
                    name: "b".to_string(),
                    span,
                })),
                span,
            })),
            span,
        });

        let block = Node::Block(Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![return_stmt],
            span,
        });

        let func_decl = Node::FuncDecl(FuncDecl {
            name: "Add".to_string(),
            params: vec![param],
            return_type: Box::new(Node::NamedType(NamedType {
                name: "integer".to_string(),
                span,
            })),
            block: Box::new(block),
            span,
        });

        assert_eq!(func_decl.span(), span);
    }

    #[test]
    fn test_array_access() {
        let span = Span::new(0, 25, 1, 1);
        // arr[i]
        let index_expr = Node::IndexExpr(IndexExpr {
            array: Box::new(Node::IdentExpr(IdentExpr {
                name: "arr".to_string(),
                span,
            })),
            index: Box::new(Node::IdentExpr(IdentExpr {
                name: "i".to_string(),
                span,
            })),
            span,
        });
        // arr[i] := 42;
        let assign_stmt = Node::AssignStmt(AssignStmt {
            target: Box::new(index_expr),
            value: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(42),
                span,
            })),
            span,
        });
        assert_eq!(assign_stmt.span(), span);
    }

    #[test]
    fn test_record_field_access() {
        let span = Span::new(0, 25, 1, 1);
        // point.x
        let field_expr = Node::FieldExpr(FieldExpr {
            record: Box::new(Node::IdentExpr(IdentExpr {
                name: "point".to_string(),
                span,
            })),
            field: "x".to_string(),
            span,
        });
        // point.x := 10;
        let assign_stmt = Node::AssignStmt(AssignStmt {
            target: Box::new(field_expr),
            value: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(10),
                span,
            })),
            span,
        });
        assert_eq!(assign_stmt.span(), span);
    }

    #[test]
    fn test_nested_field_access() {
        let span = Span::new(0, 30, 1, 1);
        // point.position.x
        let inner_field = Node::FieldExpr(FieldExpr {
            record: Box::new(Node::IdentExpr(IdentExpr {
                name: "point".to_string(),
                span,
            })),
            field: "position".to_string(),
            span,
        });
        let outer_field = Node::FieldExpr(FieldExpr {
            record: Box::new(inner_field),
            field: "x".to_string(),
            span,
        });
        assert_eq!(outer_field.span(), span);
    }

    #[test]
    fn test_complex_expression() {
        let span = Span::new(0, 50, 1, 1);
        // (a + b) * (c - d) / 2
        let left_sum = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Add,
            left: Box::new(Node::IdentExpr(IdentExpr {
                name: "a".to_string(),
                span,
            })),
            right: Box::new(Node::IdentExpr(IdentExpr {
                name: "b".to_string(),
                span,
            })),
            span,
        });
        let right_diff = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Subtract,
            left: Box::new(Node::IdentExpr(IdentExpr {
                name: "c".to_string(),
                span,
            })),
            right: Box::new(Node::IdentExpr(IdentExpr {
                name: "d".to_string(),
                span,
            })),
            span,
        });
        let product = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Multiply,
            left: Box::new(left_sum),
            right: Box::new(right_diff),
            span,
        });
        let quotient = Node::BinaryExpr(BinaryExpr {
            op: BinaryOp::Divide,
            left: Box::new(product),
            right: Box::new(Node::LiteralExpr(LiteralExpr {
                value: LiteralValue::Integer(2),
                span,
            })),
            span,
        });
        assert_eq!(quotient.span(), span);
    }

    #[test]
    fn test_for_direction() {
        assert_eq!(ForDirection::To, ForDirection::To);
        assert_eq!(ForDirection::Downto, ForDirection::Downto);
        assert_ne!(ForDirection::To, ForDirection::Downto);
    }

    #[test]
    fn test_binary_op_equality() {
        assert_eq!(BinaryOp::Add, BinaryOp::Add);
        assert_ne!(BinaryOp::Add, BinaryOp::Subtract);
        assert_eq!(BinaryOp::Equal, BinaryOp::Equal);
        assert_eq!(BinaryOp::And, BinaryOp::And);
    }

    #[test]
    fn test_unary_op_equality() {
        assert_eq!(UnaryOp::Plus, UnaryOp::Plus);
        assert_eq!(UnaryOp::Minus, UnaryOp::Minus);
        assert_eq!(UnaryOp::Not, UnaryOp::Not);
        assert_ne!(UnaryOp::Plus, UnaryOp::Minus);
    }
}
