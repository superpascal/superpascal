//! Expression analysis (binary, unary, literals, identifiers, calls, etc.)

use ast::Node;
use symbols::SymbolKind;
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
            _ => {
                self.core.add_error(
                    "Invalid expression".to_string(),
                    expr.span(),
                );
                Type::Error
            }
        }
    }
}
