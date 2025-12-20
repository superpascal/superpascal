//! SuperPascal Semantic Analysis
//!
//! This crate performs semantic analysis on the AST, including:
//! - Type checking
//! - Symbol resolution
//! - Assignment compatibility checking
//! - Constant folding
//! - Control flow analysis (future)

use ast::Node;
use errors::{Diagnostic, ErrorSeverity};
use symbols::{ConstantValue, Parameter, ParameterMode, Symbol, SymbolKind, SymbolTable};
use tokens::Span;
use types::{Field, Type};

/// Semantic analyzer
pub struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    diagnostics: Vec<Diagnostic>,
    filename: Option<String>,
}

impl SemanticAnalyzer {
    /// Create a new semantic analyzer
    pub fn new(filename: Option<String>) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            diagnostics: vec![],
            filename,
        }
    }

    /// Analyze a program AST
    pub fn analyze(&mut self, program: &Node) -> Vec<Diagnostic> {
        self.diagnostics.clear();
        self.symbol_table = SymbolTable::new();

        if let Node::Program(prog) = program {
            // Analyze the program block
            self.analyze_block(&prog.block);
        }

        self.diagnostics.clone()
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

    /// Analyze constant declaration
    fn analyze_const_decl(&mut self, decl: &Node) {
        if let Node::ConstDecl(c) = decl {
            // Check if constant already exists
            if self.symbol_table.exists_in_current_scope(&c.name) {
                self.add_error(
                    format!("Constant '{}' already declared", c.name),
                    c.span,
                );
                return;
            }

            // Analyze the constant value expression
            let const_type = self.analyze_expression(&c.value);

            // Evaluate constant value (constant folding)
            let const_value = self.evaluate_constant_expression(&c.value);

            // Create and insert symbol
            let symbol = Symbol {
                kind: SymbolKind::Constant {
                    name: c.name.clone(),
                    const_type: const_type.clone(),
                    value: const_value, // Store evaluated constant value
                    span: c.span,
                },
                scope_level: self.symbol_table.scope_level(),
            };

            if let Err(e) = self.symbol_table.insert(symbol) {
                self.add_error(e, c.span);
            }
        }
    }

    /// Analyze type declaration
    fn analyze_type_decl(&mut self, decl: &Node) {
        if let Node::TypeDecl(t) = decl {
            // Check if type already exists
            if self.symbol_table.exists_in_current_scope(&t.name) {
                self.add_error(
                    format!("Type '{}' already declared", t.name),
                    t.span,
                );
                return;
            }

            // Analyze the type expression
            let type_expr = self.analyze_type(&t.type_expr);

            // Create and insert symbol
            let symbol = Symbol {
                kind: SymbolKind::TypeAlias {
                    name: t.name.clone(),
                    aliased_type: type_expr,
                    span: t.span,
                },
                scope_level: self.symbol_table.scope_level(),
            };

            if let Err(e) = self.symbol_table.insert(symbol) {
                self.add_error(e, t.span);
            }
        }
    }

    /// Analyze variable declaration
    fn analyze_var_decl(&mut self, decl: &Node) {
        if let Node::VarDecl(v) = decl {
            // Analyze the type
            let var_type = self.analyze_type(&v.type_expr);

            // Create symbols for each variable name
            for name in &v.names {
                // Check if variable already exists
                if self.symbol_table.exists_in_current_scope(name) {
                    self.add_error(
                        format!("Variable '{}' already declared", name),
                        v.span,
                    );
                    continue;
                }

                let symbol = Symbol {
                    kind: SymbolKind::Variable {
                        name: name.clone(),
                        var_type: var_type.clone(),
                        span: v.span,
                    },
                    scope_level: self.symbol_table.scope_level(),
                };

                if let Err(e) = self.symbol_table.insert(symbol) {
                    self.add_error(e, v.span);
                }
            }
        }
    }

    /// Analyze procedure declaration
    fn analyze_proc_decl(&mut self, decl: &Node) {
        if let Node::ProcDecl(p) = decl {
            // Check if procedure already exists
            if self.symbol_table.exists_in_current_scope(&p.name) {
                self.add_error(
                    format!("Procedure '{}' already declared", p.name),
                    p.span,
                );
                return;
            }

            // Analyze parameters
            let params = self.analyze_params(&p.params);

            // Create symbol
            let symbol = Symbol {
                kind: SymbolKind::Procedure {
                    name: p.name.clone(),
                    params: params.clone(),
                    span: p.span,
                },
                scope_level: self.symbol_table.scope_level(),
            };

            if let Err(e) = self.symbol_table.insert(symbol) {
                self.add_error(e, p.span);
            }

            // Analyze procedure body (enter new scope)
            self.symbol_table.enter_scope();
            // Add parameters to scope
            for param in &params {
                for name in &param.name.split(',').map(|s| s.trim().to_string()).collect::<Vec<_>>() {
                    if !name.is_empty() {
                        let param_symbol = Symbol {
                            kind: SymbolKind::Variable {
                                name: name.clone(),
                                var_type: param.param_type.clone(),
                                span: param.span,
                            },
                            scope_level: self.symbol_table.scope_level(),
                        };
                        let _ = self.symbol_table.insert(param_symbol);
                    }
                }
            }
            self.analyze_block(&p.block);
            self.symbol_table.exit_scope();
        }
    }

    /// Analyze function declaration
    fn analyze_func_decl(&mut self, decl: &Node) {
        if let Node::FuncDecl(f) = decl {
            // Check if function already exists
            if self.symbol_table.exists_in_current_scope(&f.name) {
                self.add_error(
                    format!("Function '{}' already declared", f.name),
                    f.span,
                );
                return;
            }

            // Analyze parameters
            let params = self.analyze_params(&f.params);
            let params_clone = params.clone();

            // Analyze return type
            let return_type = self.analyze_type(&f.return_type);
            let return_type_clone = return_type.clone();

            // Create symbol
            let symbol = Symbol {
                kind: SymbolKind::Function {
                    name: f.name.clone(),
                    params: params_clone.clone(),
                    return_type: return_type_clone,
                    span: f.span,
                },
                scope_level: self.symbol_table.scope_level(),
            };

            if let Err(e) = self.symbol_table.insert(symbol) {
                self.add_error(e, f.span);
            }

            // Analyze function body (enter new scope)
            self.symbol_table.enter_scope();
            // Add parameters to scope
            for param in &params_clone {
                for name in &param.name.split(',').map(|s| s.trim().to_string()).collect::<Vec<_>>() {
                    if !name.is_empty() {
                        let param_symbol = Symbol {
                            kind: SymbolKind::Variable {
                                name: name.clone(),
                                var_type: param.param_type.clone(),
                                span: param.span,
                            },
                            scope_level: self.symbol_table.scope_level(),
                        };
                        let _ = self.symbol_table.insert(param_symbol);
                    }
                }
            }
            self.analyze_block(&f.block);
            self.symbol_table.exit_scope();
        }
    }

    /// Analyze parameters
    fn analyze_params(&mut self, params: &[ast::Param]) -> Vec<Parameter> {
        params
            .iter()
            .map(|p| {
                let param_type = self.analyze_type(&p.type_expr);
                let passing_mode = match p.param_type {
                    ast::ParamType::Value => ParameterMode::Value,
                    ast::ParamType::Var => ParameterMode::Var,
                    ast::ParamType::Const => ParameterMode::Const,
                };
                Parameter {
                    name: p.names.join(", "), // Join multiple names
                    param_type,
                    passing_mode,
                    span: p.span,
                }
            })
            .collect()
    }

    /// Analyze type expression
    fn analyze_type(&mut self, type_expr: &Node) -> Type {
        match type_expr {
            Node::NamedType(n) => {
                // Look up named type in symbol table
                if let Some(symbol) = self.symbol_table.lookup(&n.name) {
                    if let SymbolKind::TypeAlias { aliased_type, .. } = &symbol.kind {
                        aliased_type.clone()
                    } else {
                        self.add_error(
                            format!("'{}' is not a type", n.name),
                            n.span,
                        );
                        Type::Error
                    }
                } else {
                    // Check for built-in types
                    match n.name.as_str() {
                        "integer" => Type::integer(),
                        "byte" => Type::byte(),
                        "word" => Type::word(),
                        "boolean" => Type::boolean(),
                        "char" => Type::char(),
                        _ => {
                            self.add_error(
                                format!("Type '{}' not found", n.name),
                                n.span,
                            );
                            Type::Error
                        }
                    }
                }
            }
            Node::ArrayType(a) => {
                let index_type = self.analyze_type(&a.index_type);
                let element_type = self.analyze_type(&a.element_type);
                Type::array(index_type, element_type)
            }
            Node::RecordType(r) => {
                let fields: Vec<Field> = r
                    .fields
                    .iter()
                    .map(|f| {
                        let field_type = self.analyze_type(&f.type_expr);
                        Field {
                            name: f.names[0].clone(), // Use first name for now
                            field_type: Box::new(field_type),
                            offset: None,
                        }
                    })
                    .collect();
                let mut record = Type::record(fields);
                record.calculate_record_offsets();
                record
            }
            _ => {
                self.add_error("Invalid type expression".to_string(), type_expr.span());
                Type::Error
            }
        }
    }

    /// Analyze statement
    fn analyze_statement(&mut self, stmt: &Node) {
        match stmt {
            Node::AssignStmt(a) => self.analyze_assignment(a),
            Node::CallStmt(c) => self.analyze_call_stmt(c),
            Node::IfStmt(i) => self.analyze_if_stmt(i),
            Node::WhileStmt(w) => self.analyze_while_stmt(w),
            Node::ForStmt(f) => self.analyze_for_stmt(f),
            Node::RepeatStmt(r) => self.analyze_repeat_stmt(r),
            Node::CaseStmt(c) => self.analyze_case_stmt(c),
            _ => {
                self.add_error(
                    "Unsupported statement type".to_string(),
                    stmt.span(),
                );
            }
        }
    }

    /// Analyze assignment statement
    fn analyze_assignment(&mut self, assign: &ast::AssignStmt) {
        // Analyze target (lvalue)
        let target_type = self.analyze_lvalue(&assign.target);

        // Analyze value (rvalue)
        let value_type = self.analyze_expression(&assign.value);

        // Check assignment compatibility
        if !value_type.is_assignable_to(&target_type) {
            self.add_error(
                format!(
                    "Type mismatch: cannot assign {} to {}",
                    format_type(&value_type),
                    format_type(&target_type)
                ),
                assign.span,
            );
        }
    }

    /// Analyze call statement (procedure call)
    fn analyze_call_stmt(&mut self, call: &ast::CallStmt) {
        // Look up procedure
        let params_opt = self.symbol_table.lookup(&call.name).and_then(|symbol| {
            if let SymbolKind::Procedure { params, .. } = &symbol.kind {
                Some(params.clone())
            } else {
                None
            }
        });

        if let Some(params) = params_opt {
            // Check argument count
            if call.args.len() != params.len() {
                self.add_error(
                    format!(
                        "Procedure '{}' expects {} arguments, found {}",
                        call.name,
                        params.len(),
                        call.args.len()
                    ),
                    call.span,
                );
                return;
            }

            // Check argument types
            for (arg, param) in call.args.iter().zip(params.iter()) {
                let arg_type = self.analyze_expression(arg);
                if !arg_type.is_assignable_to(&param.param_type) {
                    self.add_error(
                        format!(
                            "Argument type mismatch: expected {}, found {}",
                            format_type(&param.param_type),
                            format_type(&arg_type)
                        ),
                        arg.span(),
                    );
                }
            }
        } else if self.symbol_table.lookup(&call.name).is_some() {
            self.add_error(
                format!("'{}' is not a procedure", call.name),
                call.span,
            );
        } else {
            self.add_error(
                format!("Procedure '{}' not found", call.name),
                call.span,
            );
        }
    }

    /// Analyze if statement
    fn analyze_if_stmt(&mut self, if_stmt: &ast::IfStmt) {
        let condition_type = self.analyze_expression(&if_stmt.condition);
        if !condition_type.equals(&Type::boolean()) {
            self.add_error(
                "If condition must be boolean".to_string(),
                if_stmt.condition.span(),
            );
            return;
        }

        // Constant folding: evaluate constant conditions
        if let Some(constant_value) = self.evaluate_constant_expression(&if_stmt.condition) {
            if let ConstantValue::Boolean(condition_value) = constant_value {
                if condition_value {
                    // Condition is always true - only analyze then branch
                    self.analyze_statement(&if_stmt.then_block);
                    // Note: Dead code elimination would skip else branch in code generation
                } else {
                    // Condition is always false - only analyze else branch (if present)
                    if let Some(else_block) = &if_stmt.else_block {
                        self.analyze_statement(else_block);
                    }
                    // Note: Dead code elimination would skip then branch in code generation
                }
                return;
            }
        }

        // Non-constant condition - analyze both branches
        self.analyze_statement(&if_stmt.then_block);
        if let Some(else_block) = &if_stmt.else_block {
            self.analyze_statement(else_block);
        }
    }

    /// Analyze while statement
    fn analyze_while_stmt(&mut self, while_stmt: &ast::WhileStmt) {
        let condition_type = self.analyze_expression(&while_stmt.condition);
        if !condition_type.equals(&Type::boolean()) {
            self.add_error(
                "While condition must be boolean".to_string(),
                while_stmt.condition.span(),
            );
            return;
        }

        // Constant folding: evaluate constant conditions
        if let Some(constant_value) = self.evaluate_constant_expression(&while_stmt.condition) {
            if let ConstantValue::Boolean(condition_value) = constant_value {
                if !condition_value {
                    // Condition is always false - loop body never executes
                    // Note: Dead code elimination would skip loop body in code generation
                    return;
                }
                // Condition is always true - infinite loop (warn in future)
                // For now, still analyze body for semantic correctness
            }
        }

        // Analyze loop body (non-constant or true constant condition)
        self.analyze_statement(&while_stmt.body);
    }

    /// Analyze for statement
    fn analyze_for_stmt(&mut self, for_stmt: &ast::ForStmt) {
        // Check loop variable exists and is assignable
        let var_type_opt = self.symbol_table.lookup(&for_stmt.var_name).and_then(|symbol| {
            if let SymbolKind::Variable { var_type, .. } = &symbol.kind {
                Some(var_type.clone())
            } else {
                None
            }
        });

        if let Some(var_type) = var_type_opt {
            let start_type = self.analyze_expression(&for_stmt.start_expr);
            let end_type = self.analyze_expression(&for_stmt.end_expr);

            if !start_type.is_assignable_to(&var_type) {
                self.add_error(
                    format!(
                        "For loop start value type {} not compatible with loop variable type {}",
                        format_type(&start_type),
                        format_type(&var_type)
                    ),
                    for_stmt.start_expr.span(),
                );
            }

            if !end_type.is_assignable_to(&var_type) {
                self.add_error(
                    format!(
                        "For loop end value type {} not compatible with loop variable type {}",
                        format_type(&end_type),
                        format_type(&var_type)
                    ),
                    for_stmt.end_expr.span(),
                );
            }
        } else if self.symbol_table.lookup(&for_stmt.var_name).is_some() {
            self.add_error(
                format!("'{}' is not a variable", for_stmt.var_name),
                for_stmt.span,
            );
        } else {
            self.add_error(
                format!("Variable '{}' not found", for_stmt.var_name),
                for_stmt.span,
            );
        }

        self.analyze_statement(&for_stmt.body);
    }

    /// Analyze repeat statement
    fn analyze_repeat_stmt(&mut self, repeat_stmt: &ast::RepeatStmt) {
        for stmt in &repeat_stmt.statements {
            self.analyze_statement(stmt);
        }
        let condition_type = self.analyze_expression(&repeat_stmt.condition);
        if !condition_type.equals(&Type::boolean()) {
            self.add_error(
                "Repeat condition must be boolean".to_string(),
                repeat_stmt.condition.span(),
            );
        }
    }

    /// Analyze case statement
    fn analyze_case_stmt(&mut self, case_stmt: &ast::CaseStmt) {
        let expr_type = self.analyze_expression(&case_stmt.expr);
        // Case expression must be ordinal type (integer, byte, word, char, boolean)
        if !matches!(
            expr_type,
            Type::Primitive(_) | Type::Named { .. }
        ) {
            self.add_error(
                "Case expression must be an ordinal type".to_string(),
                case_stmt.expr.span(),
            );
        }

        for case_branch in &case_stmt.cases {
            for value in &case_branch.values {
                let value_type = self.analyze_expression(value);
                if !value_type.equals(&expr_type) {
                    self.add_error(
                        format!(
                            "Case value type {} does not match expression type {}",
                            format_type(&value_type),
                            format_type(&expr_type)
                        ),
                        value.span(),
                    );
                }
            }
            self.analyze_statement(&case_branch.statement);
        }

        if let Some(else_stmt) = &case_stmt.else_branch {
            self.analyze_statement(else_stmt);
        }
    }

    /// Analyze lvalue (left-hand side of assignment)
    fn analyze_lvalue(&mut self, lvalue: &Node) -> Type {
        match lvalue {
            Node::IdentExpr(i) => {
                if let Some(symbol) = self.symbol_table.lookup(&i.name) {
                    if let SymbolKind::Variable { var_type, .. } = &symbol.kind {
                        var_type.clone()
                    } else {
                        self.add_error(
                            format!("'{}' is not a variable", i.name),
                            i.span,
                        );
                        Type::Error
                    }
                } else {
                    self.add_error(
                        format!("Variable '{}' not found", i.name),
                        i.span,
                    );
                    Type::Error
                }
            }
            Node::IndexExpr(idx) => {
                let array_type = self.analyze_expression(&idx.array);
                if let Type::Array { element_type, .. } = array_type {
                    let _index_type = self.analyze_expression(&idx.index);
                    // Check index type is compatible with array index type
                    // For now, we assume integer indexing
                    *element_type
                } else {
                    self.add_error(
                        "Index expression must be applied to an array".to_string(),
                        idx.span,
                    );
                    Type::Error
                }
            }
            Node::FieldExpr(field) => {
                let record_type = self.analyze_expression(&field.record);
                if let Type::Record { fields, .. } = record_type {
                    // Find field
                    if let Some(f) = fields.iter().find(|f| f.name == field.field) {
                        f.field_type.as_ref().clone()
                    } else {
                        self.add_error(
                            format!("Field '{}' not found in record", field.field),
                            field.span,
                        );
                        Type::Error
                    }
                } else {
                    self.add_error(
                        "Field access must be applied to a record".to_string(),
                        field.span,
                    );
                    Type::Error
                }
            }
            _ => {
                self.add_error(
                    "Invalid lvalue (left-hand side of assignment)".to_string(),
                    lvalue.span(),
                );
                Type::Error
            }
        }
    }

    /// Analyze expression
    fn analyze_expression(&mut self, expr: &Node) -> Type {
        match expr {
            Node::LiteralExpr(lit) => match &lit.value {
                ast::LiteralValue::Integer(_) => Type::integer(),
                ast::LiteralValue::Boolean(_) => Type::boolean(),
                ast::LiteralValue::Char(_) => Type::char(),
                ast::LiteralValue::String(_) => {
                    // String literals are arrays of char
                    Type::array(Type::integer(), Type::char())
                }
            },
            Node::IdentExpr(i) => {
                if let Some(symbol) = self.symbol_table.lookup(&i.name) {
                    match &symbol.kind {
                        SymbolKind::Variable { var_type, .. } => var_type.clone(),
                        SymbolKind::Constant { const_type, .. } => const_type.clone(),
                        SymbolKind::Function { return_type, .. } => return_type.clone(),
                        _ => {
                            self.add_error(
                                format!("'{}' is not a value", i.name),
                                i.span,
                            );
                            Type::Error
                        }
                    }
                } else {
                    self.add_error(
                        format!("Identifier '{}' not found", i.name),
                        i.span,
                    );
                    Type::Error
                }
            }
            Node::BinaryExpr(bin) => {
                let left_type = self.analyze_expression(&bin.left);
                let right_type = self.analyze_expression(&bin.right);

                match bin.op {
                    ast::BinaryOp::Add | ast::BinaryOp::Subtract | ast::BinaryOp::Multiply
                    | ast::BinaryOp::Divide | ast::BinaryOp::Div | ast::BinaryOp::Mod => {
                        // Arithmetic operations
                        if left_type.equals(&Type::integer()) && right_type.is_assignable_to(&Type::integer()) {
                            Type::integer()
                        } else if left_type.equals(&Type::word()) && right_type.is_assignable_to(&Type::word()) {
                            Type::word()
                        } else {
                            self.add_error(
                                format!(
                                    "Arithmetic operation requires numeric types, found {} and {}",
                                    format_type(&left_type),
                                    format_type(&right_type)
                                ),
                                bin.span,
                            );
                            Type::Error
                        }
                    }
                    ast::BinaryOp::Equal | ast::BinaryOp::NotEqual | ast::BinaryOp::Less
                    | ast::BinaryOp::LessEqual | ast::BinaryOp::Greater | ast::BinaryOp::GreaterEqual => {
                        // Comparison operations return boolean
                        if left_type.is_assignable_to(&right_type) || right_type.is_assignable_to(&left_type) {
                            Type::boolean()
                        } else {
                            self.add_error(
                                format!(
                                    "Comparison requires compatible types, found {} and {}",
                                    format_type(&left_type),
                                    format_type(&right_type)
                                ),
                                bin.span,
                            );
                            Type::Error
                        }
                    }
                    ast::BinaryOp::And | ast::BinaryOp::Or => {
                        // Logical operations
                        if left_type.equals(&Type::boolean()) && right_type.equals(&Type::boolean()) {
                            Type::boolean()
                        } else {
                            self.add_error(
                                "Logical operations require boolean operands".to_string(),
                                bin.span,
                            );
                            Type::Error
                        }
                    }
                    ast::BinaryOp::In => {
                        // Set membership: left IN right (right must be a set type)
                        // For now, return boolean (proper type checking would verify right is a set)
                        Type::boolean()
                    }
                }
            }
            Node::UnaryExpr(unary) => {
                let expr_type = self.analyze_expression(&unary.expr);
                match unary.op {
                    ast::UnaryOp::Plus | ast::UnaryOp::Minus => {
                        // Unary plus/minus
                        if expr_type.equals(&Type::integer()) || expr_type.equals(&Type::word()) {
                            expr_type
                        } else {
                            self.add_error(
                                "Unary +/- requires numeric type".to_string(),
                                unary.span,
                            );
                            Type::Error
                        }
                    }
                    ast::UnaryOp::Not => {
                        // Logical not
                        if expr_type.equals(&Type::boolean()) {
                            Type::boolean()
                        } else {
                            self.add_error(
                                "Unary 'not' requires boolean type".to_string(),
                                unary.span,
                            );
                            Type::Error
                        }
                    }
                }
            }
            Node::CallExpr(call) => {
                // Function call
                let func_info = self.symbol_table.lookup(&call.name).and_then(|symbol| {
                    if let SymbolKind::Function { return_type, params, .. } = &symbol.kind {
                        Some((return_type.clone(), params.clone()))
                    } else {
                        None
                    }
                });

                if let Some((return_type, params)) = func_info {
                    // Check argument count
                    if call.args.len() != params.len() {
                        self.add_error(
                            format!(
                                "Function '{}' expects {} arguments, found {}",
                                call.name,
                                params.len(),
                                call.args.len()
                            ),
                            call.span,
                        );
                        return Type::Error;
                    }

                    // Check argument types
                    for (arg, param) in call.args.iter().zip(params.iter()) {
                        let arg_type = self.analyze_expression(arg);
                        if !arg_type.is_assignable_to(&param.param_type) {
                            self.add_error(
                                format!(
                                    "Argument type mismatch: expected {}, found {}",
                                    format_type(&param.param_type),
                                    format_type(&arg_type)
                                ),
                                arg.span(),
                            );
                        }
                    }

                    return_type
                } else if self.symbol_table.lookup(&call.name).is_some() {
                    self.add_error(
                        format!("'{}' is not a function", call.name),
                        call.span,
                    );
                    Type::Error
                } else {
                    self.add_error(
                        format!("Function '{}' not found", call.name),
                        call.span,
                    );
                    Type::Error
                }
            }
            Node::IndexExpr(idx) => {
                let array_type = self.analyze_expression(&idx.array);
                if let Type::Array { element_type, .. } = array_type {
                    let _index_type = self.analyze_expression(&idx.index);
                    // Check index type (for now, we assume integer indexing)
                    *element_type
                } else {
                    self.add_error(
                        "Index expression must be applied to an array".to_string(),
                        idx.span,
                    );
                    Type::Error
                }
            }
            Node::FieldExpr(field) => {
                let record_type = self.analyze_expression(&field.record);
                if let Type::Record { fields, .. } = record_type {
                    if let Some(f) = fields.iter().find(|f| f.name == field.field) {
                        f.field_type.as_ref().clone()
                    } else {
                        self.add_error(
                            format!("Field '{}' not found in record", field.field),
                            field.span,
                        );
                        Type::Error
                    }
                } else {
                    self.add_error(
                        "Field access must be applied to a record".to_string(),
                        field.span,
                    );
                    Type::Error
                }
            }
            _ => {
                self.add_error(
                    "Invalid expression".to_string(),
                    expr.span(),
                );
                Type::Error
            }
        }
    }

    /// Evaluate a constant expression, returning the constant value if the expression is constant.
    /// Returns None if the expression is not constant (contains variables, function calls, etc.).
    fn evaluate_constant_expression(&self, expr: &Node) -> Option<ConstantValue> {
        match expr {
            Node::LiteralExpr(lit) => match &lit.value {
                ast::LiteralValue::Integer(i) => Some(ConstantValue::Integer(*i as i16)),
                ast::LiteralValue::Boolean(b) => Some(ConstantValue::Boolean(*b)),
                ast::LiteralValue::Char(c) => Some(ConstantValue::Char(*c)),
                ast::LiteralValue::String(s) => Some(ConstantValue::String(s.clone())),
            },
            Node::IdentExpr(i) => {
                // Check if identifier is a constant
                if let Some(symbol) = self.symbol_table.lookup(&i.name) {
                    if let SymbolKind::Constant { value: Some(cv), .. } = &symbol.kind {
                        Some(cv.clone())
                    } else {
                        None // Not a constant or constant value not yet computed
                    }
                } else {
                    None // Identifier not found
                }
            }
            Node::BinaryExpr(bin) => {
                // Evaluate both operands
                let left = self.evaluate_constant_expression(&bin.left)?;
                let right = self.evaluate_constant_expression(&bin.right)?;

                // Evaluate binary operation
                match bin.op {
                    ast::BinaryOp::Add => self.eval_add(&left, &right),
                    ast::BinaryOp::Subtract => self.eval_subtract(&left, &right),
                    ast::BinaryOp::Multiply => self.eval_multiply(&left, &right),
                    ast::BinaryOp::Divide | ast::BinaryOp::Div => self.eval_divide(&left, &right),
                    ast::BinaryOp::Mod => self.eval_mod(&left, &right),
                    ast::BinaryOp::Equal => Some(ConstantValue::Boolean(left == right)),
                    ast::BinaryOp::NotEqual => Some(ConstantValue::Boolean(left != right)),
                    ast::BinaryOp::Less => self.eval_less(&left, &right),
                    ast::BinaryOp::LessEqual => self.eval_less_equal(&left, &right),
                    ast::BinaryOp::Greater => self.eval_greater(&left, &right),
                    ast::BinaryOp::GreaterEqual => self.eval_greater_equal(&left, &right),
                    ast::BinaryOp::And => self.eval_and(&left, &right),
                    ast::BinaryOp::Or => self.eval_or(&left, &right),
                    ast::BinaryOp::In => {
                        // Set membership: IN operator evaluation not yet implemented for constant expressions
                        None
                    }
                }
            }
            Node::UnaryExpr(unary) => {
                let operand = self.evaluate_constant_expression(&unary.expr)?;
                match unary.op {
                    ast::UnaryOp::Plus => Some(operand), // Unary plus is no-op
                    ast::UnaryOp::Minus => self.eval_unary_minus(&operand),
                    ast::UnaryOp::Not => self.eval_not(&operand),
                }
            }
            _ => None, // Not a constant expression
        }
    }

    // Helper functions for constant evaluation
    fn eval_add(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
                Some(ConstantValue::Integer(l.saturating_add(*r)))
            }
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => {
                Some(ConstantValue::Byte(l.saturating_add(*r)))
            }
            (ConstantValue::Word(l), ConstantValue::Word(r)) => {
                Some(ConstantValue::Word(l.saturating_add(*r)))
            }
            _ => None,
        }
    }

    fn eval_subtract(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
                Some(ConstantValue::Integer(l.saturating_sub(*r)))
            }
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => {
                Some(ConstantValue::Byte(l.saturating_sub(*r)))
            }
            (ConstantValue::Word(l), ConstantValue::Word(r)) => {
                Some(ConstantValue::Word(l.saturating_sub(*r)))
            }
            _ => None,
        }
    }

    fn eval_multiply(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
                Some(ConstantValue::Integer(l.saturating_mul(*r)))
            }
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => {
                Some(ConstantValue::Byte(l.saturating_mul(*r)))
            }
            (ConstantValue::Word(l), ConstantValue::Word(r)) => {
                Some(ConstantValue::Word(l.saturating_mul(*r)))
            }
            _ => None,
        }
    }

    fn eval_divide(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
                if *r == 0 {
                    None // Division by zero
                } else {
                    Some(ConstantValue::Integer(l / r))
                }
            }
            (ConstantValue::Word(l), ConstantValue::Word(r)) => {
                if *r == 0 {
                    None
                } else {
                    Some(ConstantValue::Word(l / r))
                }
            }
            _ => None,
        }
    }

    fn eval_mod(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => {
                if *r == 0 {
                    None // Modulo by zero
                } else {
                    Some(ConstantValue::Integer(l % r))
                }
            }
            (ConstantValue::Word(l), ConstantValue::Word(r)) => {
                if *r == 0 {
                    None
                } else {
                    Some(ConstantValue::Word(l % r))
                }
            }
            _ => None,
        }
    }

    fn eval_less(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => Some(ConstantValue::Boolean(l < r)),
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => Some(ConstantValue::Boolean(l < r)),
            (ConstantValue::Word(l), ConstantValue::Word(r)) => Some(ConstantValue::Boolean(l < r)),
            _ => None,
        }
    }

    fn eval_less_equal(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => Some(ConstantValue::Boolean(l <= r)),
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => Some(ConstantValue::Boolean(l <= r)),
            (ConstantValue::Word(l), ConstantValue::Word(r)) => Some(ConstantValue::Boolean(l <= r)),
            _ => None,
        }
    }

    fn eval_greater(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => Some(ConstantValue::Boolean(l > r)),
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => Some(ConstantValue::Boolean(l > r)),
            (ConstantValue::Word(l), ConstantValue::Word(r)) => Some(ConstantValue::Boolean(l > r)),
            _ => None,
        }
    }

    fn eval_greater_equal(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Integer(l), ConstantValue::Integer(r)) => Some(ConstantValue::Boolean(l >= r)),
            (ConstantValue::Byte(l), ConstantValue::Byte(r)) => Some(ConstantValue::Boolean(l >= r)),
            (ConstantValue::Word(l), ConstantValue::Word(r)) => Some(ConstantValue::Boolean(l >= r)),
            _ => None,
        }
    }

    fn eval_and(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Boolean(l), ConstantValue::Boolean(r)) => {
                Some(ConstantValue::Boolean(*l && *r))
            }
            _ => None,
        }
    }

    fn eval_or(&self, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (left, right) {
            (ConstantValue::Boolean(l), ConstantValue::Boolean(r)) => {
                Some(ConstantValue::Boolean(*l || *r))
            }
            _ => None,
        }
    }

    fn eval_unary_minus(&self, operand: &ConstantValue) -> Option<ConstantValue> {
        match operand {
            ConstantValue::Integer(i) => Some(ConstantValue::Integer(-i)),
            _ => None,
        }
    }

    fn eval_not(&self, operand: &ConstantValue) -> Option<ConstantValue> {
        match operand {
            ConstantValue::Boolean(b) => Some(ConstantValue::Boolean(!b)),
            _ => None,
        }
    }

    /// Add an error diagnostic
    fn add_error(&mut self, message: String, span: Span) {
        let diag = Diagnostic::new(ErrorSeverity::Error, message, span)
            .with_file(self.filename.clone().unwrap_or_else(|| "unknown".to_string()));
        self.diagnostics.push(diag);
    }
}

/// Format a type for error messages
fn format_type(ty: &Type) -> String {
    match ty {
        Type::Primitive(p) => match p {
            types::PrimitiveType::Integer => "integer".to_string(),
            types::PrimitiveType::Byte => "byte".to_string(),
            types::PrimitiveType::Word => "word".to_string(),
            types::PrimitiveType::Boolean => "boolean".to_string(),
            types::PrimitiveType::Char => "char".to_string(),
        },
        Type::Array { .. } => "array".to_string(),
        Type::Record { .. } => "record".to_string(),
        Type::Pointer { .. } => "pointer".to_string(),
        Type::Named { name } => name.clone(),
        Type::Error => "error".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;

    #[test]
    fn test_semantic_analyzer_new() {
        let analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        assert_eq!(analyzer.diagnostics.len(), 0);
        assert!(analyzer.symbol_table.is_global_scope());
    }

    #[test]
    fn test_analyze_simple_program() {
        // This is a basic test - full tests will be added as we implement
        let mut analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);

        // Create a simple program: program Test; begin end.
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
        let mut analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);
        
        // Add variable x to symbol table
        let var_symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        analyzer.symbol_table.insert(var_symbol).unwrap();
        
        // Test IF with constant true condition
        // if true then x := 1 else x := 2
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
        
        // Analyze - should only analyze then branch (constant folding)
        analyzer.analyze_if_stmt(&if_stmt);
        
        // Should have no errors (else branch is dead code, not analyzed)
        assert_eq!(analyzer.diagnostics.len(), 0);
    }

    #[test]
    fn test_constant_folding_while_false() {
        let mut analyzer = SemanticAnalyzer::new(Some("test.pas".to_string()));
        let span = Span::new(0, 10, 1, 1);
        
        // Add variable x to symbol table
        let var_symbol = Symbol {
            kind: SymbolKind::Variable {
                name: "x".to_string(),
                var_type: Type::integer(),
                span,
            },
            scope_level: 0,
        };
        analyzer.symbol_table.insert(var_symbol).unwrap();
        
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
        
        // Analyze - should skip body (constant folding)
        analyzer.analyze_while_stmt(&while_stmt);
        
        // Should have no errors (body is dead code, not analyzed)
        assert_eq!(analyzer.diagnostics.len(), 0);
    }
}
