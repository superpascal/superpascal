//! Type analysis (named types, arrays, records, etc.)

use ast::Node;
use symbols::SymbolKind;
use ::types::{Field, Type};
use crate::SemanticAnalyzer;

impl SemanticAnalyzer {
    /// Analyze type expression
    pub(crate) fn analyze_type(&mut self, type_expr: &Node) -> Type {
        match type_expr {
            Node::NamedType(n) => {
                // Look up named type in symbol table
                if let Some(symbol) = self.core.symbol_table.lookup(&n.name) {
                    if let SymbolKind::TypeAlias { aliased_type, .. } = &symbol.kind {
                        aliased_type.clone()
                    } else {
                        self.core.add_error(
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
                            self.core.add_error(
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
            Node::DynamicArrayType(d) => {
                let element_type = self.analyze_type(&d.element_type);
                Type::dynamic_array(element_type)
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
                self.core.add_error("Invalid type expression".to_string(), type_expr.span());
                Type::Error
            }
        }
    }
}
