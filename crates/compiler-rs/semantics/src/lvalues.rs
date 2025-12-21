//! Lvalue analysis (left-hand side of assignments)

use ast::Node;
use symbols::SymbolKind;
use ::types::Type;
use crate::SemanticAnalyzer;

impl SemanticAnalyzer {
    /// Analyze lvalue (left-hand side of assignment)
    pub(crate) fn analyze_lvalue(&mut self, lvalue: &Node) -> Type {
        match lvalue {
            Node::IdentExpr(i) => {
                if let Some(symbol) = self.core.symbol_table.lookup(&i.name) {
                    if let SymbolKind::Variable { var_type, .. } = &symbol.kind {
                        var_type.clone()
                    } else {
                        self.core.add_error(
                            format!("'{}' is not a variable", i.name),
                            i.span,
                        );
                        Type::Error
                    }
                } else {
                    self.core.add_error(
                        format!("Variable '{}' not found", i.name),
                        i.span,
                    );
                    Type::Error
                }
            }
            Node::IndexExpr(idx) => {
                let array_type = self.analyze_expression(&idx.array);
                match array_type {
                    Type::Array { element_type, .. } | Type::DynamicArray { element_type } => {
                        let _index_type = self.analyze_expression(&idx.index);
                        // Check index type is compatible with array index type
                        // For now, we assume integer indexing
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
                    // Find field
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
            _ => {
                self.core.add_error(
                    "Invalid lvalue (left-hand side of assignment)".to_string(),
                    lvalue.span(),
                );
                Type::Error
            }
        }
    }
}
