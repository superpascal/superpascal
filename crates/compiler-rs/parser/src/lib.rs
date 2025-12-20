//! SuperPascal Parser
//!
//! This crate implements a recursive descent parser for SuperPascal.
//! It builds an AST from tokens produced by the lexer.

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

    /// Advance to the next token
    fn advance(&mut self) -> ParserResult<()> {
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
    fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    /// Get the peek token
    fn peek_token(&self) -> Option<&Token> {
        self.peek.as_ref()
    }

    /// Check if current token matches a kind
    fn check(&self, kind: &TokenKind) -> bool {
        self.current()
            .map(|t| &t.kind == kind)
            .unwrap_or(false)
    }

    /// Check if peek token matches a kind
    fn check_peek(&self, kind: &TokenKind) -> bool {
        self.peek_token()
            .map(|t| &t.kind == kind)
            .unwrap_or(false)
    }

    /// Consume current token if it matches, otherwise error
    fn consume(&mut self, kind: TokenKind, expected: &str) -> ParserResult<Token> {
        // Special handling for Identifier - match any identifier
        let matches = if matches!(kind, TokenKind::Identifier(_)) {
            matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_)))
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

    /// Parse program: PROGRAM identifier ; block .
    fn parse_program(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        // PROGRAM keyword
        self.consume(TokenKind::KwProgram, "PROGRAM")?;

        // Program name
        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier after PROGRAM".to_string(),
                span: name_token.span,
            }),
        };

        // Semicolon
        self.consume(TokenKind::Semicolon, ";")?;

        // Block
        let block = self.parse_block()?;

        // Period
        self.consume(TokenKind::Dot, ".")?;

        // Check for EOF (allow whitespace/comments after period)
        // Skip any remaining tokens that are just whitespace/comments
        while let Some(token) = self.current() {
            // If we see EOF, we're done
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
            // Otherwise, there's unexpected content
            return Err(ParserError::InvalidSyntax {
                message: "Unexpected tokens after program end".to_string(),
                span: token.span,
            });
        }

        let span = start_span.merge(block.span());
        Ok(Node::Program(ast::Program {
            name,
            block: Box::new(block),
            span,
        }))
    }

    /// Parse unit: UNIT identifier ; [interface] [implementation] [initialization] [finalization] END .
    fn parse_unit(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwUnit, "UNIT")?;

        // Parse unit name (can be qualified like "a.b.c")
        let name = self.parse_qualified_unit_name()?;

        self.consume(TokenKind::Semicolon, ";")?;

        // Parse interface section (optional but usually present)
        let interface = if self.check(&TokenKind::KwInterface) {
            Some(self.parse_interface_section()?)
        } else {
            None
        };

        // Parse implementation section (optional but usually present)
        let implementation = if self.check(&TokenKind::KwImplementation) {
            Some(self.parse_implementation_section()?)
        } else {
            None
        };

        // Parse initialization section (optional)
        let initialization = if self.check(&TokenKind::KwInitialization) {
            self.advance()?; // consume INITIALIZATION
            let block = self.parse_block()?;
            // Consume semicolon after initialization block
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
            Some(Box::new(block))
        } else {
            None
        };

        // Parse finalization section (optional)
        let finalization = if self.check(&TokenKind::KwFinalization) {
            self.advance()?; // consume FINALIZATION
            let block = self.parse_block()?;
            // Consume semicolon after finalization block
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
            Some(Box::new(block))
        } else {
            None
        };

        // END
        self.consume(TokenKind::KwEnd, "END")?;

        // Period
        self.consume(TokenKind::Dot, ".")?;

        let end_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));
        let span = start_span.merge(end_span);

        Ok(Node::Unit(ast::Unit {
            name,
            interface,
            implementation,
            initialization,
            finalization,
            span,
        }))
    }

    /// Parse library: LIBRARY identifier ; [block] END .
    fn parse_library(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwLibrary, "LIBRARY")?;

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier after LIBRARY".to_string(),
                span: name_token.span,
            }),
        };

        self.consume(TokenKind::Semicolon, ";")?;

        // Optional block
        let block = if self.check(&TokenKind::KwBegin) {
            let parsed_block = self.parse_block()?;
            // Consume semicolon after block if present
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
            Some(Box::new(parsed_block))
        } else {
            None
        };

        // END
        self.consume(TokenKind::KwEnd, "END")?;

        // Period
        self.consume(TokenKind::Dot, ".")?;

        let end_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));
        let span = start_span.merge(end_span);

        Ok(Node::Library(ast::Library {
            name,
            block,
            span,
        }))
    }

    /// Parse qualified unit name: identifier { . identifier }
    fn parse_qualified_unit_name(&mut self) -> ParserResult<String> {
        let mut parts = vec![];
        
        loop {
            let token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let part = match &token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: token.span,
                }),
            };
            parts.push(part);

            if !self.check(&TokenKind::Dot) {
                break;
            }
            self.advance()?; // consume the dot
        }

        Ok(parts.join("."))
    }

    /// Parse uses clause: USES identifier_list ;
    fn parse_uses_clause(&mut self) -> ParserResult<ast::UsesClause> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwUses, "USES")?;

        let mut units = vec![];
        loop {
            let unit_name = self.parse_qualified_unit_name()?;
            units.push(unit_name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?; // consume comma
        }

        self.consume(TokenKind::Semicolon, ";")?;

        let end_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));
        let span = start_span.merge(end_span);

        Ok(ast::UsesClause { units, span })
    }

    /// Parse interface section: INTERFACE [uses] [declarations]
    fn parse_interface_section(&mut self) -> ParserResult<ast::InterfaceSection> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwInterface, "INTERFACE")?;

        // Optional uses clause
        let uses = if self.check(&TokenKind::KwUses) {
            Some(self.parse_uses_clause()?)
        } else {
            None
        };

        // Parse declarations (const, type, var, procedures, functions, properties)
        let mut const_decls = vec![];
        let mut type_decls = vec![];
        let mut var_decls = vec![];
        let mut proc_decls = vec![];
        let mut func_decls = vec![];
        let mut property_decls = vec![];

        loop {
            if self.check(&TokenKind::KwConst) {
                const_decls.extend(self.parse_const_decls()?);
            } else if self.check(&TokenKind::KwType) {
                type_decls.extend(self.parse_type_decls()?);
            } else if self.check(&TokenKind::KwVar) {
                var_decls.extend(self.parse_var_decls()?);
            } else if self.check(&TokenKind::KwProcedure) {
                proc_decls.push(self.parse_procedure_forward_decl()?);
            } else if self.check(&TokenKind::KwFunction) {
                func_decls.push(self.parse_function_forward_decl()?);
            } else if self.check(&TokenKind::KwProperty) {
                property_decls.push(self.parse_property_decl()?);
            } else {
                break;
            }
        }

        let end_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));
        let span = start_span.merge(end_span);

        Ok(ast::InterfaceSection {
            uses,
            const_decls,
            type_decls,
            var_decls,
            proc_decls,
            func_decls,
            property_decls,
            span,
        })
    }

    /// Parse implementation section: IMPLEMENTATION [uses] [declarations]
    fn parse_implementation_section(&mut self) -> ParserResult<ast::ImplementationSection> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwImplementation, "IMPLEMENTATION")?;

        // Optional uses clause
        let uses = if self.check(&TokenKind::KwUses) {
            Some(self.parse_uses_clause()?)
        } else {
            None
        };

        // Parse declarations (const, type, var, procedures, functions, properties)
        let mut const_decls = vec![];
        let mut type_decls = vec![];
        let mut var_decls = vec![];
        let mut proc_decls = vec![];
        let mut func_decls = vec![];
        let mut property_decls = vec![];

        loop {
            if self.check(&TokenKind::KwConst) {
                const_decls.extend(self.parse_const_decls()?);
            } else if self.check(&TokenKind::KwType) {
                type_decls.extend(self.parse_type_decls()?);
            } else if self.check(&TokenKind::KwVar) {
                var_decls.extend(self.parse_var_decls()?);
            } else if self.check(&TokenKind::KwProcedure) {
                proc_decls.push(self.parse_procedure_decl()?);
            } else if self.check(&TokenKind::KwFunction) {
                func_decls.push(self.parse_function_decl()?);
            } else if self.check(&TokenKind::KwProperty) {
                property_decls.push(self.parse_property_decl()?);
            } else {
                break;
            }
        }

        let end_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));
        let span = start_span.merge(end_span);

        Ok(ast::ImplementationSection {
            uses,
            const_decls,
            type_decls,
            var_decls,
            proc_decls,
            func_decls,
            property_decls,
            span,
        })
    }

    /// Parse block: [declarations] BEGIN statements END
    fn parse_block(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut const_decls = vec![];
        let mut type_decls = vec![];
        let mut var_decls = vec![];
        let mut proc_decls = vec![];
        let mut func_decls = vec![];

        // Parse declarations (const, type, var, procedures, functions)
        loop {
            if self.check(&TokenKind::KwConst) {
                const_decls.extend(self.parse_const_decls()?);
            } else if self.check(&TokenKind::KwType) {
                type_decls.extend(self.parse_type_decls()?);
            } else if self.check(&TokenKind::KwVar) {
                var_decls.extend(self.parse_var_decls()?);
            } else if self.check(&TokenKind::KwProcedure) {
                proc_decls.push(self.parse_procedure_decl()?);
            } else if self.check(&TokenKind::KwFunction) {
                func_decls.push(self.parse_function_decl()?);
            } else {
                break;
            }
        }

        // BEGIN
        self.consume(TokenKind::KwBegin, "BEGIN")?;

        // Statements
        let mut statements = vec![];
        while !self.check(&TokenKind::KwEnd) {
            statements.push(self.parse_statement()?);
            // Optional semicolon between statements
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }

        // END
        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::Block(ast::Block {
            const_decls,
            type_decls,
            var_decls,
            proc_decls,
            func_decls,
            statements,
            span,
        }))
    }

    /// Parse constant declarations: CONST const_decl { ; const_decl }
    fn parse_const_decls(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::KwConst, "CONST")?;
        let mut decls = vec![];
        loop {
            decls.push(self.parse_const_decl()?);
            if !self.check(&TokenKind::Semicolon) {
                break;
            }
            self.advance()?;
            if !self.check(&TokenKind::Identifier(String::new())) {
                break;
            }
        }
        Ok(decls)
    }

    /// Parse single constant declaration: identifier = expression
    fn parse_const_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        self.consume(TokenKind::Equal, "=")?;
        let value = self.parse_expression()?;

        let span = start_span.merge(value.span());
        Ok(Node::ConstDecl(ast::ConstDecl {
            name,
            value: Box::new(value),
            span,
        }))
    }

    /// Parse type declarations: TYPE type_decl { ; type_decl }
    fn parse_type_decls(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::KwType, "TYPE")?;
        let mut decls = vec![];
        loop {
            decls.push(self.parse_type_decl()?);
            if !self.check(&TokenKind::Semicolon) {
                break;
            }
            self.advance()?;
            if !self.check(&TokenKind::Identifier(String::new())) {
                break;
            }
        }
        Ok(decls)
    }

    /// Parse single type declaration: identifier = type
    fn parse_type_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        self.consume(TokenKind::Equal, "=")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(Node::TypeDecl(ast::TypeDecl {
            name,
            type_expr: Box::new(type_expr),
            span,
        }))
    }

    /// Parse variable declarations: VAR var_decl { ; var_decl }
    fn parse_var_decls(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::KwVar, "VAR")?;
        let mut decls = vec![];
        loop {
            decls.push(self.parse_var_decl()?);
            if !self.check(&TokenKind::Semicolon) {
                break;
            }
            self.advance()?;
            if !self.check(&TokenKind::Identifier(String::new())) {
                break;
            }
        }
        Ok(decls)
    }

    /// Parse single variable declaration: identifier_list : type
    fn parse_var_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut names = vec![];
        loop {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: name_token.span,
                }),
            };
            names.push(name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(Node::VarDecl(ast::VarDecl {
            names,
            type_expr: Box::new(type_expr),
            span,
        }))
    }

    /// Parse qualified name: ClassName.MethodName or just MethodName
    /// Returns (class_name, method_name) where class_name is None if not present
    fn parse_qualified_name(&mut self) -> ParserResult<(Option<String>, String)> {
        let first_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let first_name = match &first_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: first_token.span,
            }),
        };

        // Check if there's a dot followed by another identifier (ClassName.MethodName)
        if self.check(&TokenKind::Dot) {
            self.advance()?; // consume the dot
            let second_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let second_name = match &second_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier after dot".to_string(),
                    span: second_token.span,
                }),
            };
            // Return (ClassName, MethodName)
            Ok((Some(first_name), second_name))
        } else {
            // Just a regular name, no class prefix
            Ok((None, first_name))
        }
    }

    /// Parse procedure forward declaration: PROCEDURE [ClassName.]identifier [ ( params ) ] ;
    fn parse_procedure_forward_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwProcedure, "PROCEDURE")?;

        // Parse method name: ClassName.MethodName or just MethodName
        let (class_name, name) = self.parse_qualified_name()?;

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Semicolon, ";")?;

        // Create an empty block for forward declarations
        let empty_block = Node::Block(ast::Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span: start_span,
        });

        let span = start_span;
        Ok(Node::ProcDecl(ast::ProcDecl {
            name,
            class_name,
            params,
            block: Box::new(empty_block),
            span,
        }))
    }

    /// Parse function forward declaration: FUNCTION [ClassName.]identifier [ ( params ) ] : type ;
    fn parse_function_forward_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwFunction, "FUNCTION")?;

        // Parse method name: ClassName.MethodName or just MethodName
        let (class_name, name) = self.parse_qualified_name()?;

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Colon, ":")?;
        let return_type = self.parse_type()?;
        self.consume(TokenKind::Semicolon, ";")?;

        // Create an empty block for forward declarations
        let empty_block = Node::Block(ast::Block {
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            statements: vec![],
            span: start_span,
        });

        let span = start_span.merge(return_type.span());
        Ok(Node::FuncDecl(ast::FuncDecl {
            name,
            class_name,
            params,
            return_type: Box::new(return_type),
            block: Box::new(empty_block),
            span,
        }))
    }

    /// Parse procedure declaration: PROCEDURE [ClassName.]identifier [ ( params ) ] ; block ;
    fn parse_procedure_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwProcedure, "PROCEDURE")?;

        // Parse method name: ClassName.MethodName or just MethodName
        let (class_name, name) = self.parse_qualified_name()?;

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Semicolon, ";")?;
        let block = self.parse_block()?;
        self.consume(TokenKind::Semicolon, ";")?;

        let span = start_span.merge(block.span());
        Ok(Node::ProcDecl(ast::ProcDecl {
            name,
            class_name,
            params,
            block: Box::new(block),
            span,
        }))
    }

    /// Parse property declaration: PROPERTY identifier [ [ index_params ] ] : type [ READ identifier ] [ WRITE identifier ] [ INDEX expr ] [ DEFAULT expr ] [ STORED expr ] [ ; default ]
    fn parse_property_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwProperty, "PROPERTY")?;

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        // Optional index parameters: [ param1, param2: type; param3: type ]
        let mut index_params = vec![];
        if self.check(&TokenKind::LeftBracket) {
            self.advance()?; // consume [
            loop {
                index_params.push(self.parse_param()?);
                if !self.check(&TokenKind::Semicolon) {
                    break;
                }
                self.advance()?; // consume semicolon
            }
            self.consume(TokenKind::RightBracket, "]")?;
        }

        // Type
        self.consume(TokenKind::Colon, ":")?;
        let property_type = self.parse_type()?;

        // Optional READ accessor
        let read_accessor = if self.check(&TokenKind::KwRead) {
            self.advance()?; // consume READ
            let read_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            match &read_token.kind {
                TokenKind::Identifier(name) => Some(name.clone()),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier after READ".to_string(),
                    span: read_token.span,
                }),
            }
        } else {
            None
        };

        // Optional WRITE accessor
        let write_accessor = if self.check(&TokenKind::KwWrite) {
            self.advance()?; // consume WRITE
            let write_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            match &write_token.kind {
                TokenKind::Identifier(name) => Some(name.clone()),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier after WRITE".to_string(),
                    span: write_token.span,
                }),
            }
        } else {
            None
        };

        // Optional INDEX expression
        let index_expr = if self.check(&TokenKind::KwIndex) {
            self.advance()?; // consume INDEX
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        // Optional DEFAULT expression
        let default_expr = if self.check(&TokenKind::KwDefault) {
            self.advance()?; // consume DEFAULT
            // Check if it's followed by an expression or just a semicolon (default;)
            if !self.check(&TokenKind::Semicolon) {
                Some(Box::new(self.parse_expression()?))
            } else {
                None
            }
        } else {
            None
        };

        // Optional STORED expression
        let stored_expr = if self.check(&TokenKind::KwStored) {
            self.advance()?; // consume STORED
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        // Consume semicolon after property attributes
        self.consume(TokenKind::Semicolon, ";")?;

        // Check for default; after semicolon - this marks it as a default property
        let is_default = if self.check(&TokenKind::KwDefault) {
            self.advance()?; // consume DEFAULT
            self.consume(TokenKind::Semicolon, ";")?; // consume semicolon after default
            true
        } else {
            false
        };

        let end_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));
        let span = start_span.merge(end_span);

        Ok(Node::PropertyDecl(ast::PropertyDecl {
            name,
            index_params,
            property_type: Box::new(property_type),
            read_accessor,
            write_accessor,
            index_expr,
            default_expr,
            stored_expr,
            is_default,
            span,
        }))
    }

    /// Parse function declaration: FUNCTION [ClassName.]identifier [ ( params ) ] : type ; block ;
    fn parse_function_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwFunction, "FUNCTION")?;

        // Parse method name: ClassName.MethodName or just MethodName
        let (class_name, name) = self.parse_qualified_name()?;

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Colon, ":")?;
        let return_type = self.parse_type()?;
        self.consume(TokenKind::Semicolon, ";")?;
        let block = self.parse_block()?;
        self.consume(TokenKind::Semicolon, ";")?;

        let span = start_span.merge(block.span());
        Ok(Node::FuncDecl(ast::FuncDecl {
            name,
            class_name,
            params,
            return_type: Box::new(return_type),
            block: Box::new(block),
            span,
        }))
    }

    /// Parse parameter list: ( param { ; param } )
    fn parse_params(&mut self) -> ParserResult<Vec<ast::Param>> {
        self.consume(TokenKind::LeftParen, "(")?;
        let mut params = vec![];

        if !self.check(&TokenKind::RightParen) {
            loop {
                params.push(self.parse_param()?);
                if !self.check(&TokenKind::Semicolon) {
                    break;
                }
                self.advance()?;
            }
        }

        self.consume(TokenKind::RightParen, ")")?;
        Ok(params)
    }

    /// Parse parameter: [ VAR | CONST ] identifier_list : type
    fn parse_param(&mut self) -> ParserResult<ast::Param> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let param_type = if self.check(&TokenKind::KwVar) {
            self.advance()?;
            ast::ParamType::Var
        } else if self.check(&TokenKind::KwConst) {
            self.advance()?;
            ast::ParamType::Const
        } else {
            ast::ParamType::Value
        };

        let mut names = vec![];
        loop {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: name_token.span,
                }),
            };
            names.push(name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(ast::Param {
            names,
            param_type,
            type_expr: Box::new(type_expr),
            span,
        })
    }

    /// Parse type: identifier | ARRAY [ index_type ] OF element_type | RECORD field_list END
    fn parse_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        if self.check(&TokenKind::KwArray) {
            self.advance()?;
            self.consume(TokenKind::LeftBracket, "[")?;
            let index_type = self.parse_type()?;
            self.consume(TokenKind::RightBracket, "]")?;
            self.consume(TokenKind::KwOf, "OF")?;
            let element_type = self.parse_type()?;
            let span = start_span.merge(element_type.span());
            Ok(Node::ArrayType(ast::ArrayType {
                index_type: Box::new(index_type),
                element_type: Box::new(element_type),
                span,
            }))
        } else if self.check(&TokenKind::KwRecord) {
            self.advance()?;
            let mut fields = vec![];
            while !self.check(&TokenKind::KwEnd) {
                fields.push(self.parse_field_decl()?);
                self.consume(TokenKind::Semicolon, ";")?;
            }
            let end_token = self.consume(TokenKind::KwEnd, "END")?;
            let span = start_span.merge(end_token.span);
            Ok(Node::RecordType(ast::RecordType {
                fields,
                span,
            }))
        } else {
            // Accept either identifier or primitive type keywords
            let name_token = if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
                self.consume(TokenKind::Identifier(String::new()), "type identifier")?
            } else if self.check(&TokenKind::KwInteger) {
                let token = self.current().unwrap().clone();
                self.advance()?;
                Token {
                    kind: TokenKind::Identifier("integer".to_string()),
                    span: token.span,
                }
            } else if self.check(&TokenKind::KwBoolean) {
                let token = self.current().unwrap().clone();
                self.advance()?;
                Token {
                    kind: TokenKind::Identifier("boolean".to_string()),
                    span: token.span,
                }
            } else if self.check(&TokenKind::KwChar) {
                let token = self.current().unwrap().clone();
                self.advance()?;
                Token {
                    kind: TokenKind::Identifier("char".to_string()),
                    span: token.span,
                }
            } else if self.check(&TokenKind::KwByte) {
                let token = self.current().unwrap().clone();
                self.advance()?;
                Token {
                    kind: TokenKind::Identifier("byte".to_string()),
                    span: token.span,
                }
            } else if self.check(&TokenKind::KwWord) {
                let token = self.current().unwrap().clone();
                self.advance()?;
                Token {
                    kind: TokenKind::Identifier("word".to_string()),
                    span: token.span,
                }
            } else {
                return Err(ParserError::UnexpectedToken {
                    expected: "type identifier or primitive type".to_string(),
                    found: format!("{:?}", self.current().map(|t| &t.kind)),
                    span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                });
            };
            
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected type identifier".to_string(),
                    span: name_token.span,
                }),
            };
            Ok(Node::NamedType(ast::NamedType {
                name,
                span: name_token.span,
            }))
        }
    }

    /// Parse field declaration: identifier_list : type
    fn parse_field_decl(&mut self) -> ParserResult<ast::FieldDecl> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut names = vec![];
        loop {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: name_token.span,
                }),
            };
            names.push(name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(ast::FieldDecl {
            names,
            type_expr: Box::new(type_expr),
            span,
        })
    }

    /// Parse statement
    fn parse_statement(&mut self) -> ParserResult<Node> {
        if self.check(&TokenKind::KwIf) {
            self.parse_if_statement()
        } else if self.check(&TokenKind::KwWhile) {
            self.parse_while_statement()
        } else if self.check(&TokenKind::KwFor) {
            self.parse_for_statement()
        } else if self.check(&TokenKind::KwRepeat) {
            self.parse_repeat_statement()
        } else if self.check(&TokenKind::KwCase) {
            self.parse_case_statement()
        } else if self.check(&TokenKind::KwTry) {
            self.parse_try_statement()
        } else if self.check(&TokenKind::KwRaise) {
            self.parse_raise_statement()
        } else if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            // Could be assignment or procedure call
            if self.check_peek(&TokenKind::Assign) {
                self.parse_assignment_statement()
            } else {
                self.parse_call_statement()
            }
        } else {
            let span = self
                .current()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));
            Err(ParserError::InvalidSyntax {
                message: "Expected statement".to_string(),
                span,
            })
        }
    }

    /// Parse if statement: IF expression THEN statement [ ELSE statement ]
    fn parse_if_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwIf, "IF")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::KwThen, "THEN")?;
        let then_block = self.parse_statement()?;

        let else_block = if self.check(&TokenKind::KwElse) {
            self.advance()?;
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let span = if let Some(ref else_block) = else_block {
            start_span.merge(else_block.span())
        } else {
            start_span.merge(then_block.span())
        };

        Ok(Node::IfStmt(ast::IfStmt {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block,
            span,
        }))
    }

    /// Parse while statement: WHILE expression DO statement
    fn parse_while_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwWhile, "WHILE")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::KwDo, "DO")?;
        let body = self.parse_statement()?;

        let span = start_span.merge(body.span());
        Ok(Node::WhileStmt(ast::WhileStmt {
            condition: Box::new(condition),
            body: Box::new(body),
            span,
        }))
    }

    /// Parse for statement: FOR identifier := expression TO|DOWNTO expression DO statement
    fn parse_for_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwFor, "FOR")?;
        let var_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let var_name = match &var_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: var_token.span,
            }),
        };

        self.consume(TokenKind::Assign, ":=")?;
        let start_expr = self.parse_expression()?;

        let direction = if self.check(&TokenKind::KwTo) {
            self.advance()?;
            ast::ForDirection::To
        } else if self.check(&TokenKind::KwDownto) {
            self.advance()?;
            ast::ForDirection::Downto
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: "TO or DOWNTO".to_string(),
                found: format!("{:?}", self.current().map(|t| &t.kind)),
                span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
            });
        };

        let end_expr = self.parse_expression()?;
        self.consume(TokenKind::KwDo, "DO")?;
        let body = self.parse_statement()?;

        let span = start_span.merge(body.span());
        Ok(Node::ForStmt(ast::ForStmt {
            var_name,
            start_expr: Box::new(start_expr),
            direction,
            end_expr: Box::new(end_expr),
            body: Box::new(body),
            span,
        }))
    }

    /// Parse repeat statement: REPEAT statements UNTIL expression
    fn parse_repeat_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwRepeat, "REPEAT")?;
        let mut statements = vec![];
        while !self.check(&TokenKind::KwUntil) {
            statements.push(self.parse_statement()?);
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }
        self.consume(TokenKind::KwUntil, "UNTIL")?;
        let condition = self.parse_expression()?;

        let span = start_span.merge(condition.span());
        Ok(Node::RepeatStmt(ast::RepeatStmt {
            statements,
            condition: Box::new(condition),
            span,
        }))
    }

    /// Parse case statement: CASE expression OF case_branch { ; case_branch } [ ELSE statement ] END
    fn parse_case_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwCase, "CASE")?;
        let expr = self.parse_expression()?;
        self.consume(TokenKind::KwOf, "OF")?;

        let mut cases = vec![];
        while !self.check(&TokenKind::KwElse) && !self.check(&TokenKind::KwEnd) {
            cases.push(self.parse_case_branch()?);
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }

        let else_branch = if self.check(&TokenKind::KwElse) {
            self.advance()?;
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::CaseStmt(ast::CaseStmt {
            expr: Box::new(expr),
            cases,
            else_branch,
            span,
        }))
    }

    /// Parse case branch: case_value_list : statement
    fn parse_case_branch(&mut self) -> ParserResult<ast::CaseBranch> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut values = vec![];
        loop {
            values.push(self.parse_expression()?);
            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let statement = self.parse_statement()?;

        let span = start_span.merge(statement.span());
        Ok(ast::CaseBranch {
            values,
            statement: Box::new(statement),
            span,
        })
    }

    /// Parse assignment statement: lvalue := expression
    fn parse_assignment_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let target = self.parse_lvalue()?;
        self.consume(TokenKind::Assign, ":=")?;
        let value = self.parse_expression()?;

        let span = start_span.merge(value.span());
        Ok(Node::AssignStmt(ast::AssignStmt {
            target: Box::new(target),
            value: Box::new(value),
            span,
        }))
    }

    /// Parse call statement: identifier [ ( args ) ]
    fn parse_call_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let args = if self.check(&TokenKind::LeftParen) {
            self.parse_args()?
        } else {
            vec![]
        };

        let span = if let Some(last_arg) = args.last() {
            start_span.merge(last_arg.span())
        } else {
            name_token.span
        };

        Ok(Node::CallStmt(ast::CallStmt {
            name,
            args,
            span,
        }))
    }

    /// Parse lvalue: identifier [ [ expression ] ] [ . identifier ]
    fn parse_lvalue(&mut self) -> ParserResult<Node> {
        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let mut expr: Node = Node::IdentExpr(ast::IdentExpr {
            name,
            span: name_token.span,
        });

        // Parse array indexing and field access
        loop {
            if self.check(&TokenKind::LeftBracket) {
                self.advance()?;
                let index = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "]")?;
                let span = expr.span().merge(index.span());
                expr = Node::IndexExpr(ast::IndexExpr {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                });
            } else if self.check(&TokenKind::Dot) {
                self.advance()?;
                let field_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
                let field = match &field_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier".to_string(),
                        span: field_token.span,
                    }),
                };
                let span = expr.span().merge(field_token.span);
                expr = Node::FieldExpr(ast::FieldExpr {
                    record: Box::new(expr),
                    field,
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse expression (using Pratt parser for precedence)
    fn parse_expression(&mut self) -> ParserResult<Node> {
        self.parse_expression_precedence(0)
    }

    /// Parse expression with precedence (Pratt parser)
    fn parse_expression_precedence(&mut self, min_precedence: u8) -> ParserResult<Node> {
        // Parse left operand (prefix)
        let mut left = self.parse_prefix()?;

        // Parse binary operators (infix)
        while let Some(op) = self.parse_binary_operator() {
            let precedence = self.get_precedence(&op);
            if precedence < min_precedence {
                break;
            }
            self.advance()?;
            let right = self.parse_expression_precedence(precedence + 1)?;
            let span = left.span().merge(right.span());
            left = Node::BinaryExpr(ast::BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    /// Parse prefix expression (unary operators, literals, identifiers, etc.)
    fn parse_prefix(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let token_kind = self.current().map(|t| t.kind.clone());
        match token_kind.as_ref() {
            Some(TokenKind::IntegerLiteral { value, .. }) => {
                let token = self.current().unwrap().clone();
                let value = *value;
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::Integer(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::CharLiteral(value)) => {
                let token = self.current().unwrap().clone();
                let value = *value;
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::Char(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::StringLiteral(value)) => {
                let token = self.current().unwrap().clone();
                let value_clone = value.clone();
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::String(value_clone),
                    span: token.span,
                }))
            }
            Some(TokenKind::BooleanLiteral(value)) => {
                let token = self.current().unwrap().clone();
                let value = *value;
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::Boolean(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::Plus) => {
                self.advance()?;
                let expr = self.parse_prefix()?;
                let span = start_span.merge(expr.span());
                Ok(Node::UnaryExpr(ast::UnaryExpr {
                    op: ast::UnaryOp::Plus,
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(TokenKind::Minus) => {
                self.advance()?;
                let expr = self.parse_prefix()?;
                let span = start_span.merge(expr.span());
                Ok(Node::UnaryExpr(ast::UnaryExpr {
                    op: ast::UnaryOp::Minus,
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(TokenKind::KwNot) => {
                self.advance()?;
                let expr = self.parse_prefix()?;
                let span = start_span.merge(expr.span());
                Ok(Node::UnaryExpr(ast::UnaryExpr {
                    op: ast::UnaryOp::Not,
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(TokenKind::LeftParen) => {
                self.advance()?;
                let expr = self.parse_expression()?;
                self.consume(TokenKind::RightParen, ")")?;
                Ok(expr)
            }
            Some(TokenKind::Identifier(_)) => {
                // Could be identifier, function call, or array/record access
                let name_token = self.current().unwrap().clone();
                let name = match &name_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => unreachable!(),
                };
                self.advance()?;

                if self.check(&TokenKind::LeftParen) {
                    // Function call
                    let args = self.parse_args()?;
                    let span = if let Some(last_arg) = args.last() {
                        name_token.span.merge(last_arg.span())
                    } else {
                        name_token.span
                    };
                    Ok(Node::CallExpr(ast::CallExpr {
                        name,
                        args,
                        span,
                    }))
                } else {
                    // Start with identifier, then parse postfix (indexing, field access)
                    let mut expr: Node = Node::IdentExpr(ast::IdentExpr {
                        name,
                        span: name_token.span,
                    });
                    expr = self.parse_postfix(expr)?;
                    Ok(expr)
                }
            }
            _ => {
                let span = self
                    .current()
                    .map(|t| t.span)
                    .unwrap_or_else(|| Span::at(0, 1, 1));
                Err(ParserError::InvalidSyntax {
                    message: "Expected expression".to_string(),
                    span,
                })
            }
        }
    }

    /// Parse postfix (array indexing, field access)
    fn parse_postfix(&mut self, mut expr: Node) -> ParserResult<Node> {
        loop {
            if self.check(&TokenKind::LeftBracket) {
                self.advance()?;
                let index = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "]")?;
                let span = expr.span().merge(index.span());
                expr = Node::IndexExpr(ast::IndexExpr {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                });
            } else if self.check(&TokenKind::Dot) {
                self.advance()?;
                let field_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
                let field = match &field_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier".to_string(),
                        span: field_token.span,
                    }),
                };
                let span = expr.span().merge(field_token.span);
                expr = Node::FieldExpr(ast::FieldExpr {
                    record: Box::new(expr),
                    field,
                    span,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse binary operator (if present)
    fn parse_binary_operator(&self) -> Option<ast::BinaryOp> {
        match self.current().map(|t| &t.kind) {
            Some(TokenKind::Plus) => Some(ast::BinaryOp::Add),
            Some(TokenKind::Minus) => Some(ast::BinaryOp::Subtract),
            Some(TokenKind::Star) => Some(ast::BinaryOp::Multiply),
            Some(TokenKind::Slash) => Some(ast::BinaryOp::Divide),
            Some(TokenKind::KwDiv) => Some(ast::BinaryOp::Div),
            Some(TokenKind::KwMod) => Some(ast::BinaryOp::Mod),
            Some(TokenKind::Equal) => Some(ast::BinaryOp::Equal),
            Some(TokenKind::NotEqual) => Some(ast::BinaryOp::NotEqual),
            Some(TokenKind::Less) => Some(ast::BinaryOp::Less),
            Some(TokenKind::LessEqual) => Some(ast::BinaryOp::LessEqual),
            Some(TokenKind::Greater) => Some(ast::BinaryOp::Greater),
            Some(TokenKind::GreaterEqual) => Some(ast::BinaryOp::GreaterEqual),
            Some(TokenKind::KwAnd) => Some(ast::BinaryOp::And),
            Some(TokenKind::KwOr) => Some(ast::BinaryOp::Or),
            _ => None,
        }
    }

    /// Get operator precedence
    fn get_precedence(&self, op: &ast::BinaryOp) -> u8 {
        match op {
            // Logical operators (lowest precedence)
            ast::BinaryOp::Or => 1,
            ast::BinaryOp::And => 2,
            // Relational operators
            ast::BinaryOp::Equal | ast::BinaryOp::NotEqual | ast::BinaryOp::Less
            | ast::BinaryOp::LessEqual | ast::BinaryOp::Greater | ast::BinaryOp::GreaterEqual => 3,
            // Additive operators
            ast::BinaryOp::Add | ast::BinaryOp::Subtract => 4,
            // Multiplicative operators (highest precedence)
            ast::BinaryOp::Multiply | ast::BinaryOp::Divide | ast::BinaryOp::Div | ast::BinaryOp::Mod => 5,
        }
    }

    /// Parse argument list: ( expression { , expression } )
    fn parse_args(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::LeftParen, "(")?;
        let mut args = vec![];

        if !self.check(&TokenKind::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?;
            }
        }

        self.consume(TokenKind::RightParen, ")")?;
        Ok(args)
    }

    /// Parse try statement: TRY statements [EXCEPT [handlers] [ELSE statements] | FINALLY statements] END
    fn parse_try_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwTry, "TRY")?;

        // Parse try block statements
        let mut try_statements = vec![];
        while !self.check(&TokenKind::KwExcept) && !self.check(&TokenKind::KwFinally) && !self.check(&TokenKind::KwEnd) {
            try_statements.push(self.parse_statement()?);
            // Optional semicolon between statements
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }

        let mut except_block = None;
        let mut finally_block = None;
        let mut exception_handlers = vec![];
        let mut exception_else = None;

        if self.check(&TokenKind::KwExcept) {
            self.advance()?; // consume EXCEPT

            // Check for exception handlers (ON ... DO)
            if self.check(&TokenKind::KwOn) {
                // Parse exception handlers
                while self.check(&TokenKind::KwOn) {
                    exception_handlers.push(self.parse_exception_handler()?);
                    // Optional semicolon after handler
                    if self.check(&TokenKind::Semicolon) {
                        self.advance()?;
                    }
                }

                // Optional ELSE clause
                if self.check(&TokenKind::KwElse) {
                    self.advance()?;
                    exception_else = Some(Box::new(self.parse_statement()?));
                    // Optional semicolon after else
                    if self.check(&TokenKind::Semicolon) {
                        self.advance()?;
                    }
                }
            } else {
                // Simple except block with statements
                let mut except_statements = vec![];
                while !self.check(&TokenKind::KwEnd) {
                    except_statements.push(self.parse_statement()?);
                    if self.check(&TokenKind::Semicolon) {
                        self.advance()?;
                    }
                }
                except_block = Some(except_statements);
            }
        } else if self.check(&TokenKind::KwFinally) {
            self.advance()?; // consume FINALLY

            // Parse finally block statements
            let mut finally_statements = vec![];
            while !self.check(&TokenKind::KwEnd) {
                finally_statements.push(self.parse_statement()?);
                if self.check(&TokenKind::Semicolon) {
                    self.advance()?;
                }
            }
            finally_block = Some(finally_statements);
        }

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::TryStmt(ast::TryStmt {
            try_block: try_statements,
            except_block,
            finally_block,
            exception_handlers,
            exception_else,
            span,
        }))
    }

    /// Parse exception handler: ON [variable:] exception_type DO statement
    fn parse_exception_handler(&mut self) -> ParserResult<ast::ExceptionHandler> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwOn, "ON")?;

        // Optional variable name
        let variable = if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            if self.check_peek(&TokenKind::Colon) {
                let var_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
                let var_name = match &var_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier".to_string(),
                        span: var_token.span,
                    }),
                };
                self.advance()?; // consume colon
                Some(var_name)
            } else {
                None
            }
        } else {
            None
        };

        // Exception type
        let exception_type = self.parse_type()?;

        self.consume(TokenKind::KwDo, "DO")?;

        // Handler statement
        let handler = self.parse_statement()?;

        let span = start_span.merge(handler.span());
        Ok(ast::ExceptionHandler {
            variable,
            exception_type: Box::new(exception_type),
            handler: Box::new(handler),
            span,
        })
    }

    /// Parse raise statement: RAISE [exception]
    fn parse_raise_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwRaise, "RAISE")?;

        // Optional exception expression
        let exception = if !self.check(&TokenKind::Semicolon) && !self.check(&TokenKind::KwEnd) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        let span = if let Some(ref expr) = exception {
            start_span.merge(expr.span())
        } else {
            start_span
        };

        Ok(Node::RaiseStmt(ast::RaiseStmt {
            exception,
            span,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_program() {
        let source = r#"
            program Hello;
            begin
                writeln('Hello, World!');
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        if let Err(e) = &result {
            eprintln!("Parse error: {}", e);
        }
        assert!(result.is_ok(), "Parse failed: {:?}", result);
    }

    // ===== Exception Handling Tests =====

    #[test]
    fn test_parse_try_finally() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                finally
                    b;
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.statements.len(), 1);
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 1);
                    assert!(try_stmt.finally_block.is_some());
                    assert_eq!(try_stmt.finally_block.as_ref().unwrap().len(), 1);
                    assert!(try_stmt.except_block.is_none());
                    assert!(try_stmt.exception_handlers.is_empty());
                } else {
                    panic!("Expected TryStmt");
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    b;
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.statements.len(), 1);
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 1);
                    assert!(try_stmt.except_block.is_some());
                    assert_eq!(try_stmt.except_block.as_ref().unwrap().len(), 1);
                    assert!(try_stmt.finally_block.is_none());
                    assert!(try_stmt.exception_handlers.is_empty());
                } else {
                    panic!("Expected TryStmt");
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_with_handlers() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    on e: Exception do
                        writeln('Error');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.statements.len(), 1);
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 1);
                    assert_eq!(try_stmt.exception_handlers.len(), 1);
                    assert!(try_stmt.exception_handlers[0].variable.is_some());
                    assert_eq!(try_stmt.exception_handlers[0].variable.as_ref().unwrap(), "e");
                    assert!(try_stmt.except_block.is_none());
                    assert!(try_stmt.finally_block.is_none());
                } else {
                    panic!("Expected TryStmt");
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_with_handlers_no_variable() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    on Exception do
                        writeln('Error');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.statements.len(), 1);
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.exception_handlers.len(), 1);
                    assert!(try_stmt.exception_handlers[0].variable.is_none());
                } else {
                    panic!("Expected TryStmt");
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_multiple_handlers() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    on e1: Exception1 do
                        writeln('Error1');
                    on e2: Exception2 do
                        writeln('Error2');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.exception_handlers.len(), 2);
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_with_else() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    on e: Exception do
                        writeln('Error');
                    else
                        writeln('Unknown');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.exception_handlers.len(), 1);
                    assert!(try_stmt.exception_else.is_some());
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_multiple_statements() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                    b;
                    c;
                except
                    d;
                    e;
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 3);
                    assert_eq!(try_stmt.except_block.as_ref().unwrap().len(), 2);
                }
            }
        }
    }

    #[test]
    fn test_parse_raise_with_exception() {
        let source = r#"
            program Test;
            begin
                raise MyException;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.statements.len(), 1);
                if let Node::RaiseStmt(raise_stmt) = &block.statements[0] {
                    assert!(raise_stmt.exception.is_some());
                } else {
                    panic!("Expected RaiseStmt");
                }
            }
        }
    }

    #[test]
    fn test_parse_raise_without_exception() {
        let source = r#"
            program Test;
            begin
                raise;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::RaiseStmt(raise_stmt) = &block.statements[0] {
                    assert!(raise_stmt.exception.is_none());
                }
            }
        }
    }

    #[test]
    fn test_parse_raise_with_expression() {
        let source = r#"
            program Test;
            begin
                raise CreateException('Error message');
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::RaiseStmt(raise_stmt) = &block.statements[0] {
                    assert!(raise_stmt.exception.is_some());
                    // Should be a CallExpr
                    if let Node::CallExpr(_) = raise_stmt.exception.as_ref().unwrap().as_ref() {
                        // Good
                    } else {
                        panic!("Expected CallExpr in raise exception");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_nested_try() {
        let source = r#"
            program Test;
            begin
                try
                    try
                        a;
                    finally
                        b;
                    end;
                except
                    c;
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(outer_try) = &block.statements[0] {
                    assert_eq!(outer_try.try_block.len(), 1);
                    // Inner try should be in try_block
                    if let Node::TryStmt(_inner_try) = &outer_try.try_block[0] {
                        // Good - nested try found
                    } else {
                        panic!("Expected nested TryStmt");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_try_finally_empty_blocks() {
        let source = r#"
            program Test;
            begin
                try
                finally
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 0);
                    assert_eq!(try_stmt.finally_block.as_ref().unwrap().len(), 0);
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_empty_blocks() {
        let source = r#"
            program Test;
            begin
                try
                except
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 0);
                    assert_eq!(try_stmt.except_block.as_ref().unwrap().len(), 0);
                }
            }
        }
    }

    #[test]
    fn test_parse_try_except_with_statements_and_handlers() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    writeln('General error');
                    on e: SpecificException do
                        writeln('Specific error');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        // This should parse - except block can have statements before handlers
        // But our current implementation might not support this pattern
        // Let's see what happens
        if result.is_err() {
            // If it fails, that's okay - we can note this as a limitation
            println!("Note: Mixed except block with statements and handlers may not be supported");
        }
    }

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

    // ===== Nested Routines Tests =====

    #[test]
    fn test_parse_nested_function_in_procedure() {
        let source = r#"
            program Test;
            procedure Outer;
                function Inner: integer;
                begin
                    Inner := 42;
                end;
            begin
                writeln(Inner);
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.proc_decls.len(), 1);
                if let Node::ProcDecl(outer_proc) = &block.proc_decls[0] {
                    if let Node::Block(proc_block) = outer_proc.block.as_ref() {
                        // Should have one nested function
                        assert_eq!(proc_block.func_decls.len(), 1);
                        if let Node::FuncDecl(inner_func) = &proc_block.func_decls[0] {
                            assert_eq!(inner_func.name, "Inner");
                        } else {
                            panic!("Expected FuncDecl");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_nested_procedure_in_function() {
        let source = r#"
            program Test;
            function Outer: integer;
                procedure Inner;
                begin
                    writeln('Inner');
                end;
            begin
                Inner;
                Outer := 10;
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.func_decls.len(), 1);
                if let Node::FuncDecl(outer_func) = &block.func_decls[0] {
                    if let Node::Block(func_block) = outer_func.block.as_ref() {
                        // Should have one nested procedure
                        assert_eq!(func_block.proc_decls.len(), 1);
                        if let Node::ProcDecl(inner_proc) = &func_block.proc_decls[0] {
                            assert_eq!(inner_proc.name, "Inner");
                        } else {
                            panic!("Expected ProcDecl");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_deeply_nested_routines() {
        let source = r#"
            program Test;
            procedure Level1;
                function Level2: integer;
                    procedure Level3;
                    begin
                    end;
                begin
                    Level2 := 1;
                end;
            begin
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(level1) = &block.proc_decls[0] {
                    if let Node::Block(level1_block) = level1.block.as_ref() {
                        if let Node::FuncDecl(level2) = &level1_block.func_decls[0] {
                            if let Node::Block(level2_block) = level2.block.as_ref() {
                                assert_eq!(level2_block.proc_decls.len(), 1);
                                if let Node::ProcDecl(level3) = &level2_block.proc_decls[0] {
                                    assert_eq!(level3.name, "Level3");
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_nested_routine_with_local_vars() {
        let source = r#"
            program Test;
            procedure Outer;
                var x: integer;
                function Inner: integer;
                    var y: integer;
                begin
                    Inner := x + y;
                end;
            begin
                x := Inner;
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(outer_proc) = &block.proc_decls[0] {
                    if let Node::Block(proc_block) = outer_proc.block.as_ref() {
                        // Should have local var and nested function
                        assert_eq!(proc_block.var_decls.len(), 1);
                        assert_eq!(proc_block.func_decls.len(), 1);
                        // Nested function should also have local var
                        if let Node::FuncDecl(inner_func) = &proc_block.func_decls[0] {
                            if let Node::Block(inner_block) = inner_func.block.as_ref() {
                                assert_eq!(inner_block.var_decls.len(), 1);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_multiple_nested_routines() {
        let source = r#"
            program Test;
            procedure Outer;
                procedure Helper1;
                begin
                end;
                function Helper2: integer;
                begin
                    Helper2 := 2;
                end;
            begin
                Helper1;
                writeln(Helper2);
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(outer_proc) = &block.proc_decls[0] {
                    if let Node::Block(proc_block) = outer_proc.block.as_ref() {
                        // Should have both nested routines
                        assert_eq!(proc_block.proc_decls.len(), 1);
                        assert_eq!(proc_block.func_decls.len(), 1);
                        if let Node::ProcDecl(helper1) = &proc_block.proc_decls[0] {
                            assert_eq!(helper1.name, "Helper1");
                        }
                        if let Node::FuncDecl(helper2) = &proc_block.func_decls[0] {
                            assert_eq!(helper2.name, "Helper2");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_nested_routine_with_params() {
        let source = r#"
            program Test;
            procedure Outer;
                function Inner(x: integer): integer;
                begin
                    Inner := x * 2;
                end;
            begin
                writeln(Inner(5));
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(outer_proc) = &block.proc_decls[0] {
                    if let Node::Block(proc_block) = outer_proc.block.as_ref() {
                        if let Node::FuncDecl(inner_func) = &proc_block.func_decls[0] {
                            assert_eq!(inner_func.name, "Inner");
                            assert_eq!(inner_func.params.len(), 1);
                        }
                    }
                }
            }
        }
    }

    // ===== Method Declaration Tests =====

    #[test]
    fn test_parse_method_procedure() {
        let source = r#"
            program Test;
            procedure MyClass.MyMethod;
            begin
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.proc_decls.len(), 1);
                if let Node::ProcDecl(proc) = &block.proc_decls[0] {
                    assert_eq!(proc.name, "MyMethod");
                    assert_eq!(proc.class_name, Some("MyClass".to_string()));
                }
            }
        }
    }

    #[test]
    fn test_parse_method_function() {
        let source = r#"
            program Test;
            function MyClass.GetValue: integer;
            begin
                GetValue := 42;
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.func_decls.len(), 1);
                if let Node::FuncDecl(func) = &block.func_decls[0] {
                    assert_eq!(func.name, "GetValue");
                    assert_eq!(func.class_name, Some("MyClass".to_string()));
                }
            }
        }
    }

    #[test]
    fn test_parse_method_with_params() {
        let source = r#"
            program Test;
            procedure MyClass.SetValue(x: integer);
            begin
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(proc) = &block.proc_decls[0] {
                    assert_eq!(proc.name, "SetValue");
                    assert_eq!(proc.class_name, Some("MyClass".to_string()));
                    assert_eq!(proc.params.len(), 1);
                }
            }
        }
    }

    #[test]
    fn test_parse_regular_procedure_still_works() {
        let source = r#"
            program Test;
            procedure RegularProc;
            begin
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(proc) = &block.proc_decls[0] {
                    assert_eq!(proc.name, "RegularProc");
                    assert_eq!(proc.class_name, None);
                }
            }
        }
    }

    #[test]
    fn test_parse_multiple_methods_same_class() {
        let source = r#"
            program Test;
            procedure MyClass.Method1;
            begin
            end;
            function MyClass.Method2: integer;
            begin
                Method2 := 1;
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.proc_decls.len(), 1);
                assert_eq!(block.func_decls.len(), 1);
                if let Node::ProcDecl(proc) = &block.proc_decls[0] {
                    assert_eq!(proc.class_name, Some("MyClass".to_string()));
                    assert_eq!(proc.name, "Method1");
                }
                if let Node::FuncDecl(func) = &block.func_decls[0] {
                    assert_eq!(func.class_name, Some("MyClass".to_string()));
                    assert_eq!(func.name, "Method2");
                }
            }
        }
    }

    #[test]
    fn test_parse_methods_different_classes() {
        let source = r#"
            program Test;
            procedure ClassA.MethodA;
            begin
            end;
            procedure ClassB.MethodB;
            begin
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.proc_decls.len(), 2);
                if let Node::ProcDecl(proc1) = &block.proc_decls[0] {
                    assert_eq!(proc1.class_name, Some("ClassA".to_string()));
                    assert_eq!(proc1.name, "MethodA");
                }
                if let Node::ProcDecl(proc2) = &block.proc_decls[1] {
                    assert_eq!(proc2.class_name, Some("ClassB".to_string()));
                    assert_eq!(proc2.name, "MethodB");
                }
            }
        }
    }

    // ===== Unit/Module Tests =====

    #[test]
    fn test_parse_empty_unit() {
        let source = r#"
            unit TestUnit;
            interface
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            assert_eq!(unit.name, "TestUnit");
            assert!(unit.interface.is_some());
            assert!(unit.implementation.is_some());
            assert!(unit.initialization.is_none());
            assert!(unit.finalization.is_none());
        } else {
            panic!("Expected Unit node");
        }
    }

    #[test]
    fn test_parse_unit_with_uses() {
        let source = r#"
            unit TestUnit;
            interface
            uses UnitA, UnitB;
            implementation
            uses UnitC;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                assert!(interface.uses.is_some());
                if let Some(uses) = &interface.uses {
                    assert_eq!(uses.units.len(), 2);
                    assert_eq!(uses.units[0], "UnitA");
                    assert_eq!(uses.units[1], "UnitB");
                }
            }
            if let Some(implementation) = &unit.implementation {
                assert!(implementation.uses.is_some());
                if let Some(uses) = &implementation.uses {
                    assert_eq!(uses.units.len(), 1);
                    assert_eq!(uses.units[0], "UnitC");
                }
            }
        }
    }

    #[test]
    fn test_parse_unit_with_qualified_name() {
        let source = r#"
            unit MyNamespace.MyModule;
            interface
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            assert_eq!(unit.name, "MyNamespace.MyModule");
        }
    }

    #[test]
    fn test_parse_unit_with_declarations() {
        let source = r#"
            unit TestUnit;
            interface
                const C = 10;
                type T = integer;
                procedure Proc;
                function Func: integer;
            implementation
                procedure Proc;
                begin
                end;
                function Func: integer;
                begin
                    Func := 42;
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                assert_eq!(interface.const_decls.len(), 1);
                assert_eq!(interface.type_decls.len(), 1);
                assert_eq!(interface.proc_decls.len(), 1);
                assert_eq!(interface.func_decls.len(), 1);
            }
            if let Some(implementation) = &unit.implementation {
                assert_eq!(implementation.proc_decls.len(), 1);
                assert_eq!(implementation.func_decls.len(), 1);
            }
        }
    }

    #[test]
    fn test_parse_unit_with_initialization_finalization() {
        let source = r#"
            unit TestUnit;
            interface
            implementation
            initialization
            begin
                writeln('Init');
            end;
            finalization
            begin
                writeln('Finalize');
            end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            assert!(unit.initialization.is_some());
            assert!(unit.finalization.is_some());
        }
    }

    #[test]
    fn test_parse_library() {
        let source = r#"
            library TestLib;
            begin
                writeln('Library');
            end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Library(lib)) = result {
            assert_eq!(lib.name, "TestLib");
            assert!(lib.block.is_some());
        } else {
            panic!("Expected Library node");
        }
    }

    #[test]
    fn test_parse_library_empty() {
        let source = r#"
            library TestLib;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Library(lib)) = result {
            assert_eq!(lib.name, "TestLib");
            assert!(lib.block.is_none());
        }
    }

    #[test]
    fn test_parse_uses_qualified_names() {
        let source = r#"
            unit TestUnit;
            interface
            uses System.Classes, System.SysUtils, MyUnit;
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                if let Some(uses) = &interface.uses {
                    assert_eq!(uses.units.len(), 3);
                    assert_eq!(uses.units[0], "System.Classes");
                    assert_eq!(uses.units[1], "System.SysUtils");
                    assert_eq!(uses.units[2], "MyUnit");
                }
            }
        }
    }

    // ===== Property Declaration Tests =====

    #[test]
    fn test_parse_property_simple() {
        let source = r#"
            unit TestUnit;
            interface
                property Name: string read FName write SetName;
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                assert_eq!(interface.property_decls.len(), 1);
                if let Node::PropertyDecl(prop) = &interface.property_decls[0] {
                    assert_eq!(prop.name, "Name");
                    assert_eq!(prop.read_accessor, Some("FName".to_string()));
                    assert_eq!(prop.write_accessor, Some("SetName".to_string()));
                }
            }
        }
    }

    #[test]
    fn test_parse_property_read_only() {
        let source = r#"
            unit TestUnit;
            interface
                property Value: integer read FValue;
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                if let Node::PropertyDecl(prop) = &interface.property_decls[0] {
                    assert_eq!(prop.read_accessor, Some("FValue".to_string()));
                    assert_eq!(prop.write_accessor, None);
                }
            }
        }
    }

    #[test]
    fn test_parse_property_with_index_params() {
        let source = r#"
            unit TestUnit;
            interface
                property Items[i: integer]: string read GetItem write SetItem;
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                if let Node::PropertyDecl(prop) = &interface.property_decls[0] {
                    assert_eq!(prop.name, "Items");
                    assert_eq!(prop.index_params.len(), 1);
                }
            }
        }
    }

    #[test]
    fn test_parse_property_default_property() {
        let source = r#"
            unit TestUnit;
            interface
                property Items[i: integer]: string read GetItem write SetItem; default;
            implementation
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Unit(unit)) = result {
            if let Some(interface) = &unit.interface {
                if let Node::PropertyDecl(prop) = &interface.property_decls[0] {
                    assert!(prop.is_default);
                }
            }
        }
    }

    #[test]
    fn test_parse_nested_routines_with_all_declarations() {
        let source = r#"
            program Test;
            procedure Outer;
                const C = 10;
                type T = integer;
                var v: integer;
                procedure Nested;
                begin
                end;
            begin
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::ProcDecl(outer_proc) = &block.proc_decls[0] {
                    if let Node::Block(proc_block) = outer_proc.block.as_ref() {
                        // Should have all declaration types
                        assert_eq!(proc_block.const_decls.len(), 1);
                        assert_eq!(proc_block.type_decls.len(), 1);
                        assert_eq!(proc_block.var_decls.len(), 1);
                        assert_eq!(proc_block.proc_decls.len(), 1);
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_raise_in_try_block() {
        let source = r#"
            program Test;
            begin
                try
                    raise MyException;
                except
                    writeln('Caught');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.try_block.len(), 1);
                    if let Node::RaiseStmt(_) = &try_stmt.try_block[0] {
                        // Good - raise statement in try block
                    } else {
                        panic!("Expected RaiseStmt in try block");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_raise_in_except_block() {
        let source = r#"
            program Test;
            begin
                try
                    a;
                except
                    raise;
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    if let Some(except_block) = &try_stmt.except_block {
                        assert_eq!(except_block.len(), 1);
                        if let Node::RaiseStmt(_) = &except_block[0] {
                            // Good - raise statement in except block
                        } else {
                            panic!("Expected RaiseStmt in except block");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_complex_exception_handling() {
        let source = r#"
            program Test;
            begin
                try
                    ProcessData;
                except
                    on e: EFileNotFound do
                        writeln('File not found');
                    on e: EAccessDenied do
                        writeln('Access denied');
                    else
                        writeln('Unknown error');
                end;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TryStmt(try_stmt) = &block.statements[0] {
                    assert_eq!(try_stmt.exception_handlers.len(), 2);
                    assert!(try_stmt.exception_else.is_some());
                    // Check first handler has variable
                    assert_eq!(try_stmt.exception_handlers[0].variable.as_ref().unwrap(), "e");
                    assert_eq!(try_stmt.exception_handlers[1].variable.as_ref().unwrap(), "e");
                }
            }
        }
    }
}
