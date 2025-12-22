//! Expression analysis (binary, unary, literals, identifiers, calls, etc.)

use std::collections::HashSet;
use ast::Node;
use symbols::{Symbol, SymbolKind};
use ::types::Type;
use crate::SemanticAnalyzer;
use crate::core;

impl SemanticAnalyzer {
    /// Analyze expression
    pub(crate) fn analyze_expression(&mut self, expr: &Node) -> Type {
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
                if let Some(symbol) = self.core.symbol_table.lookup(&i.name) {
                    match &symbol.kind {
                        SymbolKind::Variable { var_type, .. } => var_type.clone(),
                        SymbolKind::Constant { const_type, .. } => const_type.clone(),
                        SymbolKind::Function { return_type, .. } => return_type.clone(),
                        _ => {
                            self.core.add_error(
                                format!("'{}' is not a value", i.name),
                                i.span,
                            );
                            Type::Error
                        }
                    }
                } else {
                    self.core.add_error(
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
                            self.core.add_error(
                                format!(
                                    "Arithmetic operation requires numeric types, found {} and {}",
                                    core::CoreAnalyzer::format_type(&left_type),
                                    core::CoreAnalyzer::format_type(&right_type)
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
                            self.core.add_error(
                                format!(
                                    "Comparison requires compatible types, found {} and {}",
                                    core::CoreAnalyzer::format_type(&left_type),
                                    core::CoreAnalyzer::format_type(&right_type)
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
                            self.core.add_error(
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
                    ast::BinaryOp::Is => {
                        // Type checking: left IS right (right must be a type)
                        // Returns boolean
                        Type::boolean()
                    }
                    ast::BinaryOp::As => {
                        // Type casting: left AS right (right must be a type)
                        // Returns the type being cast to (right side)
                        // For now, return the right type (proper checking would verify compatibility)
                        self.analyze_expression(&bin.right)
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
                            self.core.add_error(
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
                            self.core.add_error(
                                "Unary 'not' requires boolean type".to_string(),
                                unary.span,
                            );
                            Type::Error
                        }
                    }
                    ast::UnaryOp::AddressOf => {
                        // Address-of operator: @variable
                        // Returns a pointer to the target type
                        Type::pointer(expr_type)
                    }
                }
            }
            Node::CallExpr(call) => {
                // Function call
                let func_info = self.core.symbol_table.lookup(&call.name).and_then(|symbol| {
                    if let SymbolKind::Function { return_type, params, .. } = &symbol.kind {
                        Some((return_type.clone(), params.clone()))
                    } else {
                        None
                    }
                });

                if let Some((return_type, params)) = func_info {
                    // Check argument count
                    if call.args.len() != params.len() {
                        self.core.add_error(
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
                            self.core.add_error(
                                format!(
                                    "Argument type mismatch: expected {}, found {}",
                                    core::CoreAnalyzer::format_type(&param.param_type),
                                    core::CoreAnalyzer::format_type(&arg_type)
                                ),
                                arg.span(),
                            );
                        }
                    }

                    return_type
                } else if self.core.symbol_table.lookup(&call.name).is_some() {
                    self.core.add_error(
                        format!("'{}' is not a function", call.name),
                        call.span,
                    );
                    Type::Error
                } else {
                    self.core.add_error(
                        format!("Function '{}' not found", call.name),
                        call.span,
                    );
                    Type::Error
                }
            }
            Node::IndexExpr(idx) => {
                let array_type = self.analyze_expression(&idx.array);
                match array_type {
                    Type::Array { element_type, .. } | Type::DynamicArray { element_type } => {
                        let _index_type = self.analyze_expression(&idx.index);
                        // Check index type (for now, we assume integer indexing)
                        *element_type
                    }
                    _ => {
                        self.core.add_error(
                            "Index expression must be applied to an array".to_string(),
                            idx.span,
                        );
                        Type::Error
                    }
                }
            }
            Node::FieldExpr(field) => {
                let record_type = self.analyze_expression(&field.record);
                if let Type::Record { fields, .. } = record_type {
                    if let Some(f) = fields.iter().find(|f| f.name == field.field) {
                        f.field_type.as_ref().clone()
                    } else {
                        self.core.add_error(
                            format!("Field '{}' not found in record", field.field),
                            field.span,
                        );
                        Type::Error
                    }
                } else {
                    self.core.add_error(
                        "Field access must be applied to a record".to_string(),
                        field.span,
                    );
                    Type::Error
                }
            }
            Node::AddressOfExpr(addr) => {
                // Address-of operator: @variable
                // Returns a pointer to the target type
                let target_type = self.analyze_expression(&addr.target);
                Type::pointer(target_type)
            }
            Node::InheritedExpr(_inherited) => {
                // INHERITED [method_name] [args]
                // For now, return error type (proper handling would resolve parent method)
                // This will be handled by semantic analysis of method calls
                Type::Error // TODO: Proper type resolution for inherited calls
            }
            Node::AnonymousFunction(anon_func) => {
                // Anonymous function: function(params): return_type begin ... end
                // Analyze parameters
                let params = self.analyze_params(&anon_func.params);
                
                // Analyze return type
                let return_type = self.analyze_type(&anon_func.return_type);
                
                // Record the scope level before entering anonymous function scope
                // This helps us detect captured variables from outer scopes
                let outer_scope_level = self.core.symbol_table.scope_level();
                
                // Enter new scope for the anonymous function body
                self.core.symbol_table.enter_scope();
                let anon_scope_level = self.core.symbol_table.scope_level();
                
                // Add parameters to scope
                for param in &params {
                    for name in param.name.split(',').map(|s| s.trim()) {
                        if !name.is_empty() {
                            let param_symbol = Symbol {
                                kind: SymbolKind::Variable {
                                    name: name.to_string(),
                                    var_type: param.param_type.clone(),
                                    span: param.span,
                                },
                                scope_level: self.core.symbol_table.scope_level(),
                            };
                            let _ = self.core.symbol_table.insert(param_symbol);
                        }
                    }
                }
                
                // Detect captured variables: variables from outer scopes referenced in the body
                // This is done by analyzing identifiers in the body and checking their scope
                let captured_vars = self.detect_captured_variables(&anon_func.block, outer_scope_level, anon_scope_level);
                
                // Analyze the function body
                self.analyze_block(&anon_func.block);
                
                // Exit scope
                self.core.symbol_table.exit_scope();
                
                // TODO: Store captured_vars for later use in code generation
                // For now, we just detect them (closure capture infrastructure)
                if !captured_vars.is_empty() {
                    // Closure capture detected - this information will be used for code generation
                    // In a full implementation, we would store this in the AST or IR
                }
                
                // For now, return the return type (full implementation would create a procedural type)
                // TODO: Create proper procedural type representation
                return_type
            }
            Node::AnonymousProcedure(anon_proc) => {
                // Anonymous procedure: procedure(params) begin ... end
                // Analyze parameters
                let params = self.analyze_params(&anon_proc.params);
                
                // Record the scope level before entering anonymous procedure scope
                let outer_scope_level = self.core.symbol_table.scope_level();
                
                // Enter new scope for the anonymous procedure body
                self.core.symbol_table.enter_scope();
                let anon_scope_level = self.core.symbol_table.scope_level();
                
                // Add parameters to scope
                for param in &params {
                    for name in param.name.split(',').map(|s| s.trim()) {
                        if !name.is_empty() {
                            let param_symbol = Symbol {
                                kind: SymbolKind::Variable {
                                    name: name.to_string(),
                                    var_type: param.param_type.clone(),
                                    span: param.span,
                                },
                                scope_level: self.core.symbol_table.scope_level(),
                            };
                            let _ = self.core.symbol_table.insert(param_symbol);
                        }
                    }
                }
                
                // Detect captured variables from outer scopes
                let captured_vars = self.detect_captured_variables(&anon_proc.block, outer_scope_level, anon_scope_level);
                
                // Analyze the procedure body
                self.analyze_block(&anon_proc.block);
                
                // Exit scope
                self.core.symbol_table.exit_scope();
                
                // TODO: Store captured_vars for later use in code generation
                if !captured_vars.is_empty() {
                    // Closure capture detected
                }
                
                // Procedures don't return a value, but for expression context we return a placeholder
                // TODO: Create proper procedural type representation
                Type::Error // Procedures in expression context need special handling
            }
            _ => {
                self.core.add_error(
                    "Invalid expression".to_string(),
                    expr.span(),
                );
                Type::Error
            }
        }
    }

    /// Detect captured variables in an anonymous function/procedure body
    /// Returns a list of variable names that are captured from outer scopes
    fn detect_captured_variables(&self, block: &Node, outer_scope_level: usize, anon_scope_level: usize) -> Vec<String> {
        let mut captured = HashSet::new();
        
        // Recursively find all identifier expressions in the block
        self.collect_identifiers(block, &mut captured, outer_scope_level, anon_scope_level);
        
        captured.into_iter().collect()
    }
    
    /// Recursively collect identifiers that reference outer-scope variables
    fn collect_identifiers(&self, node: &Node, captured: &mut HashSet<String>, outer_scope_level: usize, anon_scope_level: usize) {
        match node {
            Node::IdentExpr(i) => {
                // Check if this identifier references a variable from an outer scope
                if let Some(symbol) = self.core.symbol_table.lookup(&i.name) {
                    // If the symbol is from an outer scope (before the anonymous function scope)
                    // and it's a variable (not a parameter or local), it's captured
                    if symbol.scope_level < outer_scope_level {
                        if let SymbolKind::Variable { .. } = &symbol.kind {
                            captured.insert(i.name.clone());
                        }
                    }
                }
            }
            Node::Block(blk) => {
                // Recursively check all statements in the block
                for stmt in &blk.statements {
                    self.collect_identifiers(stmt, captured, outer_scope_level, anon_scope_level);
                }
            }
            Node::BinaryExpr(bin) => {
                self.collect_identifiers(&bin.left, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&bin.right, captured, outer_scope_level, anon_scope_level);
            }
            Node::UnaryExpr(un) => {
                self.collect_identifiers(&un.expr, captured, outer_scope_level, anon_scope_level);
            }
            Node::AssignStmt(assign) => {
                self.collect_identifiers(&assign.target, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&assign.value, captured, outer_scope_level, anon_scope_level);
            }
            Node::CallExpr(call) => {
                for arg in &call.args {
                    self.collect_identifiers(arg, captured, outer_scope_level, anon_scope_level);
                }
            }
            Node::CallStmt(call) => {
                for arg in &call.args {
                    self.collect_identifiers(arg, captured, outer_scope_level, anon_scope_level);
                }
            }
            Node::IfStmt(if_stmt) => {
                self.collect_identifiers(&if_stmt.condition, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&if_stmt.then_block, captured, outer_scope_level, anon_scope_level);
                if let Some(else_block) = &if_stmt.else_block {
                    self.collect_identifiers(else_block, captured, outer_scope_level, anon_scope_level);
                }
            }
            Node::WhileStmt(while_stmt) => {
                self.collect_identifiers(&while_stmt.condition, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&while_stmt.body, captured, outer_scope_level, anon_scope_level);
            }
            Node::ForStmt(for_stmt) => {
                self.collect_identifiers(&for_stmt.start_expr, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&for_stmt.end_expr, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&for_stmt.body, captured, outer_scope_level, anon_scope_level);
            }
            Node::RepeatStmt(repeat) => {
                for stmt in &repeat.statements {
                    self.collect_identifiers(stmt, captured, outer_scope_level, anon_scope_level);
                }
                self.collect_identifiers(&repeat.condition, captured, outer_scope_level, anon_scope_level);
            }
            Node::IndexExpr(idx) => {
                self.collect_identifiers(&idx.array, captured, outer_scope_level, anon_scope_level);
                self.collect_identifiers(&idx.index, captured, outer_scope_level, anon_scope_level);
            }
            Node::FieldExpr(field) => {
                self.collect_identifiers(&field.record, captured, outer_scope_level, anon_scope_level);
            }
            // Add other node types as needed - for now, we handle the most common cases
            _ => {
                // For other node types, we don't need to recurse (they don't contain identifiers)
            }
        }
    }
}
