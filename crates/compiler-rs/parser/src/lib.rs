//! SuperPascal Parser
//!
//! This crate implements a recursive descent parser for SuperPascal.
//! It builds an AST from tokens produced by the lexer.

mod core;
mod statements;
mod expressions;
mod types;
mod declarations;
mod classes;
mod units;
mod properties;

use ast::Node;
use errors::{Diagnostic, ParserError, ParserResult};
use lexer::Lexer;
use tokens::{Span, Token, TokenKind};

/// Parser for SuperPascal programs
pub struct Parser {
    lexer: Lexer,
    current: Option<Token>,
    peek: Option<Token>,
    filename: Option<String>,
}

impl Parser {
    /// Create a new parser from source code
    pub fn new(source: &str) -> ParserResult<Self> {
        Self::new_with_file(source, None)
    }

    /// Create a new parser from source code with filename
    pub fn new_with_file(source: &str, filename: Option<String>) -> ParserResult<Self> {
        let lexer = Lexer::new(source);
        let mut parser = Self {
            lexer,
            current: None,
            peek: None,
            filename,
        };
        // Prime the parser with first two tokens
        parser.advance()?;
        parser.advance()?;
        Ok(parser)
    }

    /// Convert a ParserError to an enhanced Diagnostic
    pub fn error_to_diagnostic(&self, error: &ParserError) -> Diagnostic {
        let mut diag = error.to_diagnostic(self.filename.clone());
        
        // Add enhanced context based on error type
        match error {
            ParserError::UnexpectedToken { expected, found, .. } => {
                // Add suggestion for common mistakes
                if expected == "identifier" && found.contains("keyword") {
                    diag = diag.with_suggestion(
                        format!("Keywords cannot be used as identifiers. Use a different name.")
                    );
                } else if expected == ";" && found.contains("end") {
                    diag = diag.with_suggestion(
                        format!("Missing semicolon before 'end'. Add ';' after the previous statement.")
                    );
                }
            }
            ParserError::UnexpectedEof { expected, .. } => {
                diag = diag.with_suggestion(
                    format!("The file ended unexpectedly. Add '{}' before the end of file.", expected)
                );
            }
            ParserError::InvalidSyntax { message, .. } => {
                // Try to provide helpful context
                if message.contains("statement") {
                    diag = diag.with_suggestion(
                        "Check that all statements are properly terminated with semicolons.".to_string()
                    );
                }
            }
        }
        
        diag
    }

    // Core functionality is in core.rs

    /// Parse a complete program, unit, or library
    pub fn parse(&mut self) -> ParserResult<Node> {
        // Check what we're parsing: PROGRAM, UNIT, or LIBRARY
        if self.check(&TokenKind::KwProgram) {
            self.parse_program()
        } else if self.check(&TokenKind::KwUnit) {
            self.parse_unit()
        } else if self.check(&TokenKind::KwLibrary) {
            self.parse_library()
        } else {
            let span = self
                .current()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));
            Err(ParserError::InvalidSyntax {
                message: "Expected PROGRAM, UNIT, or LIBRARY".to_string(),
                span,
            })
        }
    }

    // Program parsing is in declarations.rs
    // Unit/module parsing is in units.rs

    // Declaration parsing is in declarations.rs
    // All declaration functions have been moved to declarations.rs module

    // Function, parameter parsing is in declarations.rs

    // Type parsing is in types.rs

    // Class parsing is in classes.rs

    // Field declaration parsing is in types.rs

    // Statement parsing is in statements.rs
}

#[cfg(test)]
mod tests {

    // Declaration tests (nested routines, method declarations, simple program) are in declarations.rs
    // Unit/library tests are in units.rs

    // Property tests are in properties.rs
    // Pointer type tests are in types.rs
    // Class type tests are in classes.rs
}
