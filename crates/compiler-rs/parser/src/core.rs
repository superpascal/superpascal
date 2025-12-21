//! Core parser functionality
//!
//! This module contains the fundamental token management and parsing utilities
//! used throughout the parser.

use errors::{ParserError, ParserResult};
use tokens::{Span, Token, TokenKind};

/// Core parser functionality for token management
impl super::Parser {
    /// Advance to the next token
    pub(super) fn advance(&mut self) -> ParserResult<()> {
        self.current = self.peek.take();
        match self.lexer.next_token() {
            Ok(token) => {
                self.peek = Some(token);
                Ok(())
            }
            Err(e) => Err(ParserError::InvalidSyntax {
                message: format!("Lexer error: {}", e),
                span: self.current.as_ref().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
            }),
        }
    }

    /// Get the current token
    pub(super) fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    /// Get the peek token
    pub(super) fn peek_token(&self) -> Option<&Token> {
        self.peek.as_ref()
    }

    /// Check if current token matches a kind
    pub(super) fn check(&self, kind: &TokenKind) -> bool {
        // Special handling for Identifier and Directive - match any identifier/directive
        if matches!(kind, TokenKind::Identifier(_)) {
            matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_)))
        } else if matches!(kind, TokenKind::Directive(_)) {
            matches!(self.current().map(|t| &t.kind), Some(TokenKind::Directive(_)))
        } else {
            self.current()
                .map(|t| &t.kind == kind)
                .unwrap_or(false)
        }
    }

    /// Check if peek token matches a kind
    pub(super) fn check_peek(&self, kind: &TokenKind) -> bool {
        self.peek_token()
            .map(|t| &t.kind == kind)
            .unwrap_or(false)
    }

    /// Consume current token if it matches, otherwise error
    pub(super) fn consume(&mut self, kind: TokenKind, expected: &str) -> ParserResult<Token> {
        // Special handling for Identifier and Directive - match any identifier/directive
        let matches = if matches!(kind, TokenKind::Identifier(_)) {
            matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_)))
        } else if matches!(kind, TokenKind::Directive(_)) {
            matches!(self.current().map(|t| &t.kind), Some(TokenKind::Directive(_)))
        } else {
            self.check(&kind)
        };

        if matches {
            let token = self.current().unwrap().clone();
            self.advance()?;
            Ok(token)
        } else {
            let found = self
                .current()
                .map(|t| format!("{:?}", t.kind))
                .unwrap_or_else(|| "EOF".to_string());
            let span = self
                .current()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));
            Err(ParserError::UnexpectedToken {
                expected: expected.to_string(),
                found,
                span,
            })
        }
    }

    /// Advance to the next token and return the *previous* current token.
    /// This is useful when you need to inspect the token that was just consumed.
    pub(super) fn advance_and_get_token(&mut self) -> ParserResult<Token> {
        let current_token = self.current().ok_or_else(|| {
            let span = self.peek_token()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));
            ParserError::UnexpectedEof {
                expected: "token".to_string(),
                span,
            }
        })?.clone();
        self.advance()?;
        Ok(current_token)
    }
}

#[cfg(test)]
mod tests {
    use super::super::Parser;

    #[test]
    fn test_enhanced_error_diagnostics() {
        // Test parsing an invalid program to generate an error
        let source = r#"
            program Test;
            begin
                x :=  // Missing expression
            end.
        "#;
        let mut parser = Parser::new_with_file(source, Some("test.pas".to_string())).unwrap();
        let result = parser.parse();
        
        // Should fail with an error
        assert!(result.is_err());
        
        if let Err(error) = result {
            // Convert to enhanced diagnostic
            let diag = parser.error_to_diagnostic(&error);
            
            // Check FPC format
            let fpc_format = diag.format_fpc();
            assert!(fpc_format.contains("test.pas"));
            assert!(fpc_format.contains("Error:"));
            
            // Check enhanced format
            let enhanced = diag.format_enhanced();
            assert!(enhanced.contains("test.pas"));
            assert!(enhanced.contains("Error:"));
            // Should have suggestion if available
            if diag.suggestion.is_some() {
                assert!(enhanced.contains("Suggestion:"));
            }
        }
    }

    #[test]
    fn test_parser_with_filename() {
        let source = "program Test; begin end.";
        let parser = Parser::new_with_file(source, Some("myfile.pas".to_string()));
        assert!(parser.is_ok());
        let parser = parser.unwrap();
        assert_eq!(parser.filename, Some("myfile.pas".to_string()));
    }
}

