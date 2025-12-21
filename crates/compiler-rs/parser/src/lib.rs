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
pub mod query;
pub mod incremental;

use ast::Node;
use errors::{CodeSnippet, Diagnostic, ParserError, ParserResult};
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
            ParserError::UnexpectedToken { expected, found, span } => {
                // Add suggestion for common mistakes
                if expected == "identifier" && found.contains("keyword") {
                    diag = diag.with_suggestion(
                        format!("Keywords cannot be used as identifiers. Use a different name.")
                    );
                } else if expected == ";" && found.contains("end") {
                    diag = diag.with_suggestion(
                        format!("Missing semicolon before 'end'. Add ';' after the previous statement.")
                    );
                } else if expected == ":" && found.contains("=") {
                    diag = diag.with_suggestion(
                        format!("Did you mean ':' instead of '='? Variable declarations use ':' for type.")
                    );
                } else if expected == "=" && found.contains(":") {
                    diag = diag.with_suggestion(
                        format!("Did you mean '=' instead of ':'? Assignments use '=' (or ':=' in Pascal).")
                    );
                } else if expected == ":=" && found.contains("=") {
                    diag = diag.with_suggestion(
                        format!("Pascal uses ':=' for assignment, not '='. Change '=' to ':='.")
                    );
                } else if expected == "BEGIN" && found.contains("begin") {
                    diag = diag.with_suggestion(
                        format!("Pascal is case-sensitive for keywords. Use 'BEGIN' (uppercase) or ensure your lexer handles case correctly.")
                    );
                } else if expected == "END" && found.contains("end") {
                    diag = diag.with_suggestion(
                        format!("Pascal is case-sensitive for keywords. Use 'END' (uppercase) or ensure your lexer handles case correctly.")
                    );
                } else if expected == ")" && found.contains("]") {
                    diag = diag.with_suggestion(
                        format!("Mismatched bracket. Use ')' to close a function call or expression.")
                    );
                } else if expected == "]" && found.contains(")") {
                    diag = diag.with_suggestion(
                        format!("Mismatched bracket. Use ']' to close an array index.")
                    );
                } else if expected == "THEN" && found.contains("then") {
                    diag = diag.with_suggestion(
                        format!("Pascal is case-sensitive. Use 'THEN' (uppercase) after IF condition.")
                    );
                } else if expected == "DO" && found.contains("do") {
                    diag = diag.with_suggestion(
                        format!("Pascal is case-sensitive. Use 'DO' (uppercase) after FOR/WHILE condition.")
                    );
                }
                
                // Add code snippet if we have source available
                if let Some(source) = self.get_source_snippet(*span) {
                    diag = diag.with_code_snippet(source);
                }
            }
            ParserError::UnexpectedEof { expected, span: _ } => {
                let suggestion = match expected.as_str() {
                    "END" => "Add 'END' to close the block, procedure, or function.",
                    ";" => "Add ';' to terminate the statement.",
                    "." => "Add '.' to end the program.",
                    ")" => "Add ')' to close the expression or parameter list.",
                    "]" => "Add ']' to close the array index.",
                    "}" => "Add '}' to close the comment.",
                    "THEN" => "Add 'THEN' after the IF condition.",
                    "DO" => "Add 'DO' after the FOR/WHILE condition.",
                    _ => &format!("Add '{}' before the end of file.", expected),
                };
                diag = diag.with_suggestion(suggestion.to_string());
                
                // Add explanation
                diag = diag.with_explanation(
                    format!(
                        "The file ended unexpectedly while the parser was expecting '{}'. \
                         This usually means a block, statement, or expression was not properly closed.",
                        expected
                    )
                );
            }
            ParserError::InvalidSyntax { message, span } => {
                // Try to provide helpful context
                if message.contains("statement") {
                    diag = diag.with_suggestion(
                        "Check that all statements are properly terminated with semicolons.".to_string()
                    );
                } else if message.contains("expression") {
                    diag = diag.with_suggestion(
                        "Check that the expression is complete and all parentheses are balanced.".to_string()
                    );
                } else if message.contains("type") {
                    diag = diag.with_suggestion(
                        "Check that the type name is correct and the type is declared.".to_string()
                    );
                } else if message.contains("Lexer error") {
                    diag = diag.with_suggestion(
                        "Check for invalid characters or syntax in the source code.".to_string()
                    );
                }
                
                // Add code snippet if available
                if let Some(source) = self.get_source_snippet(*span) {
                    diag = diag.with_code_snippet(source);
                }
            }
        }
        
        diag
    }
    
    /// Get a code snippet around the error location for display
    fn get_source_snippet(&self, _span: Span) -> Option<CodeSnippet> {
        // Try to get source from lexer if available
        // For now, return None - this can be enhanced later with source storage
        // TODO: Store source in Parser struct and extract lines around error
        None
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
