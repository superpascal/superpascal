//! Core semantic analyzer functionality

use errors::Diagnostic;
use symbols::SymbolTable;
use tokens::Span;
use ::types::Type;

/// Core semantic analyzer functionality
pub struct CoreAnalyzer {
    pub symbol_table: SymbolTable,
    pub diagnostics: Vec<Diagnostic>,
    pub filename: Option<String>,
}

impl CoreAnalyzer {
    /// Create a new core analyzer
    pub(super) fn new(filename: Option<String>) -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            diagnostics: vec![],
            filename,
        }
    }

    /// Add an error diagnostic
    pub fn add_error(&mut self, message: String, span: Span) {
        use errors::ErrorSeverity;
        let diag = Diagnostic::new(ErrorSeverity::Error, message, span)
            .with_file(self.filename.clone().unwrap_or_else(|| "unknown".to_string()));
        self.diagnostics.push(diag);
    }

    /// Format a type for error messages
    pub(super) fn format_type(ty: &Type) -> String {
        match ty {
            Type::Primitive(p) => format!("{:?}", p),
            Type::Array { element_type, .. } => {
                format!("array of {}", Self::format_type(element_type))
            }
            Type::DynamicArray { element_type } => {
                format!("dynamic array of {}", Self::format_type(element_type))
            }
            Type::Record { fields, .. } => {
                format!("record with {} fields", fields.len())
            }
            Type::Pointer { base_type } => {
                format!("pointer to {}", Self::format_type(base_type))
            }
            Type::Error => "error".to_string(),
            Type::Named { name, .. } => name.clone(),
        }
    }
}

