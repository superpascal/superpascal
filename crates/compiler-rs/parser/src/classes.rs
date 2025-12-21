//! Class parsing
//!
//! This module handles parsing of Object Pascal class types, constructors, and destructors.

use ast;
use ast::Node;
use errors::{ParserError, ParserResult};
use tokens::{Span, TokenKind};

/// Class parsing functionality
impl super::Parser {
    /// Parse class type: CLASS [ ( base_classes ) ] [ OF type ] [ members ] END
    pub(crate) fn parse_class_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwClass, "CLASS")?;

        // Check for forward declaration: class;
        if self.check(&TokenKind::Semicolon) {
            self.advance()?;
            let span = start_span;
            return Ok(Node::ClassType(ast::ClassType {
                base_classes: vec![],
                is_forward_decl: true,
                is_meta_class: false,
                meta_class_type: None,
                members: vec![],
                span,
            }));
        }

        // Check for meta-class: class of Type
        let (is_meta_class, meta_class_type) = if self.check(&TokenKind::KwOf) {
            self.advance()?; // consume OF
            // For meta-class, the type can be an identifier (class name)
            let meta_type = if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
                let name_token = self.consume(TokenKind::Identifier(String::new()), "class name")?;
                let name = match &name_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected class name".to_string(),
                        span: name_token.span,
                    }),
                };
                Node::NamedType(ast::NamedType {
                    name,
                    span: name_token.span,
                })
            } else {
                self.parse_type()?
            };
            // Meta-class ends with semicolon, not END
            if self.check(&TokenKind::Semicolon) {
                self.advance()?; // consume semicolon
                let span = start_span;
                return Ok(Node::ClassType(ast::ClassType {
                    base_classes: vec![],
                    is_forward_decl: false,
                    is_meta_class: true,
                    meta_class_type: Some(Box::new(meta_type)),
                    members: vec![],
                    span,
                }));
            }
            (true, Some(Box::new(meta_type)))
        } else {
            (false, None)
        };

        // Parse base classes: ( BaseClass1, BaseClass2 )
        let mut base_classes = vec![];
        if self.check(&TokenKind::LeftParen) {
            self.advance()?; // consume (
            loop {
                let base_token = self.consume(TokenKind::Identifier(String::new()), "base class name")?;
                let base_name = match &base_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier for base class".to_string(),
                        span: base_token.span,
                    }),
                };
                base_classes.push(base_name);

                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?; // consume comma
            }
            self.consume(TokenKind::RightParen, ")")?;
        }

        // Check if it's just "class" with no members
        if self.check(&TokenKind::KwEnd) {
            let end_token = self.consume(TokenKind::KwEnd, "END")?;
            let span = start_span.merge(end_token.span);
            return Ok(Node::ClassType(ast::ClassType {
                base_classes,
                is_forward_decl: false,
                is_meta_class,
                meta_class_type,
                members: vec![],
                span,
            }));
        }

        // Parse class members with visibility sections
        let mut members = vec![];
        let mut current_visibility = ast::Visibility::Default;

        while !self.check(&TokenKind::KwEnd) {
            // Check for visibility sections
            if self.check(&TokenKind::KwStrict) {
                self.advance()?; // consume STRICT
                if self.check(&TokenKind::KwPrivate) {
                    self.advance()?; // consume PRIVATE
                    current_visibility = ast::Visibility::StrictPrivate;
                } else if self.check(&TokenKind::KwProtected) {
                    self.advance()?; // consume PROTECTED
                    current_visibility = ast::Visibility::StrictProtected;
                } else {
                    return Err(ParserError::InvalidSyntax {
                        message: "STRICT must be followed by PRIVATE or PROTECTED".to_string(),
                        span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                    });
                }
            } else if self.check(&TokenKind::KwPrivate) {
                self.advance()?; // consume PRIVATE
                current_visibility = ast::Visibility::Private;
            } else if self.check(&TokenKind::KwProtected) {
                self.advance()?; // consume PROTECTED
                current_visibility = ast::Visibility::Protected;
            } else if self.check(&TokenKind::KwPublic) {
                self.advance()?; // consume PUBLIC
                current_visibility = ast::Visibility::Public;
            } else if self.check(&TokenKind::KwPublished) {
                self.advance()?; // consume PUBLISHED
                current_visibility = ast::Visibility::Published;
            } else if self.check(&TokenKind::KwConst) {
                // Nested constant declarations
                let const_decls = self.parse_const_decls()?;
                for decl in const_decls {
                    members.push((current_visibility, ast::ClassMember::Const(decl)));
                }
            } else if self.check(&TokenKind::KwType) {
                // Nested type declarations
                let type_decls = self.parse_type_decls()?;
                for decl in type_decls {
                    members.push((current_visibility, ast::ClassMember::Type(decl)));
                }
            } else if self.check(&TokenKind::KwClass) && self.check_peek(&TokenKind::KwVar) {
                // Class variable declarations: CLASS VAR
                self.advance()?; // consume CLASS
                let var_decls = self.parse_var_decls_with_class_flag(true)?;
                for var_decl in var_decls {
                    members.push((current_visibility, ast::ClassMember::Field(var_decl)));
                }
            } else if self.check(&TokenKind::KwVar) {
                // Field declarations (regular instance variables)
                let var_decls = self.parse_var_decls_with_class_flag(false)?;
                for decl in var_decls {
                    members.push((current_visibility, ast::ClassMember::Field(decl)));
                }
            } else if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
                // Field declaration without VAR keyword: identifier_list : type ;
                // Check if it's a field by looking for colon after identifier(s)
                let saved_current = self.current().cloned();
                let saved_peek = self.peek.clone();
                
                // Try parsing as field declaration
                match self.parse_field_decl() {
                    Ok(field_decl) => {
                        // Convert FieldDecl to VarDecl for consistency
                        let var_decl = Node::VarDecl(ast::VarDecl {
                            names: field_decl.names,
                            type_expr: field_decl.type_expr,
                            absolute_address: None,
                            is_class_var: false, // Field declarations are instance variables
                            span: field_decl.span,
                        });
                        members.push((current_visibility, ast::ClassMember::Field(var_decl)));
                        self.consume(TokenKind::Semicolon, ";")?;
                    }
                    Err(_) => {
                        // Not a field - restore and break
                        self.current = saved_current;
                        self.peek = saved_peek;
                        break;
                    }
                }
            } else if self.check(&TokenKind::KwConstructor) {
                // Constructor
                let constructor = self.parse_constructor_decl()?;
                members.push((current_visibility, ast::ClassMember::Constructor(constructor)));
            } else if self.check(&TokenKind::KwDestructor) {
                // Destructor
                let destructor = self.parse_destructor_decl()?;
                members.push((current_visibility, ast::ClassMember::Destructor(destructor)));
            } else if self.check(&TokenKind::KwProcedure) {
                // Method (can be class procedure) - in class context, these are forward declarations
                // We need to parse them specially to avoid treating following procedures/functions as nested
                let proc = self.parse_procedure_decl_in_class()?;
                members.push((current_visibility, ast::ClassMember::Method(proc)));
            } else if self.check(&TokenKind::KwFunction) {
                // Method (can be class function) - in class context, these are forward declarations
                let func = self.parse_function_decl_in_class()?;
                members.push((current_visibility, ast::ClassMember::Method(func)));
            } else if self.check(&TokenKind::KwClass) && self.check_peek(&TokenKind::KwProperty) {
                // Class property: CLASS PROPERTY
                // parse_property_decl already handles CLASS keyword
                let prop = super::properties::parse_property_decl(self)?;
                members.push((current_visibility, ast::ClassMember::Property(prop)));
            } else if self.check(&TokenKind::KwProperty) {
                // Property (regular instance property)
                let prop = super::properties::parse_property_decl(self)?;
                members.push((current_visibility, ast::ClassMember::Property(prop)));
            } else {
                // Unknown member - break and let caller handle
                break;
            }
        }

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::ClassType(ast::ClassType {
            base_classes,
            is_forward_decl: false,
            is_meta_class,
            meta_class_type,
            members,
            span,
        }))
    }

    /// Parse constructor declaration: CONSTRUCTOR identifier [ ( params ) ] ;
    fn parse_constructor_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwConstructor, "CONSTRUCTOR")?;

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Semicolon, ";")?;

        // Create an empty block for forward declarations
        let empty_block = Node::Block(ast::Block {
            directives: vec![],
            label_decls: vec![],
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            threadvar_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            operator_decls: vec![],
            statements: vec![],
            span: start_span,
        });

        let span = start_span;
        Ok(Node::ProcDecl(ast::ProcDecl {
            name,
            class_name: None,
            params,
            block: Box::new(empty_block),
            is_forward: false,
            is_external: false,
            external_name: None,
            is_class_method: false, // Constructors are not class methods
            span,
        }))
    }

    /// Parse destructor declaration: DESTRUCTOR identifier [ ( params ) ] ;
    fn parse_destructor_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwDestructor, "DESTRUCTOR")?;

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Semicolon, ";")?;

        // Create an empty block for forward declarations
        let empty_block = Node::Block(ast::Block {
            directives: vec![],
            label_decls: vec![],
            const_decls: vec![],
            type_decls: vec![],
            var_decls: vec![],
            threadvar_decls: vec![],
            proc_decls: vec![],
            func_decls: vec![],
            operator_decls: vec![],
            statements: vec![],
            span: start_span,
        });

        let span = start_span;
        Ok(Node::ProcDecl(ast::ProcDecl {
            name,
            class_name: None,
            params,
            block: Box::new(empty_block),
            is_forward: false,
            is_external: false,
            external_name: None,
            is_class_method: false, // Destructors are not class methods
            span,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::super::Parser;
    use ast::{self, Node};

    // ===== Class Type Tests =====

    #[test]
    fn test_parse_class_forward_declaration() {
        let source = r#"
            program Test;
            type
                MyClass = class;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert!(class_type.is_forward_decl);
                        assert_eq!(class_type.base_classes.len(), 0);
                        assert_eq!(class_type.members.len(), 0);
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_empty_class() {
        let source = r#"
            program Test;
            type
                MyClass = class
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert!(!class_type.is_forward_decl);
                        assert_eq!(class_type.base_classes.len(), 0);
                        assert_eq!(class_type.members.len(), 0);
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_with_inheritance() {
        let source = r#"
            program Test;
            type
                MyClass = class(BaseClass)
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(class_type.base_classes.len(), 1);
                        assert_eq!(class_type.base_classes[0], "BaseClass");
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_with_multiple_inheritance() {
        let source = r#"
            program Test;
            type
                MyClass = class(BaseClass1, BaseClass2)
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(class_type.base_classes.len(), 2);
                        assert_eq!(class_type.base_classes[0], "BaseClass1");
                        assert_eq!(class_type.base_classes[1], "BaseClass2");
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_with_members() {
        let source = r#"
            program Test;
            type
                MyClass = class
                    x: integer;
                    procedure DoSomething;
                    function GetValue: integer;
                    property Value: integer read x write x;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(class_type.members.len(), 4);
                        // Check first member is a field
                        if let (_, ast::ClassMember::Field(_)) = &class_type.members[0] {
                            // OK
                        } else {
                            panic!("Expected Field as first member");
                        }
                        // Check second member is a method
                        if let (_, ast::ClassMember::Method(_)) = &class_type.members[1] {
                            // OK
                        } else {
                            panic!("Expected Method as second member");
                        }
                        // Check third member is a method
                        if let (_, ast::ClassMember::Method(_)) = &class_type.members[2] {
                            // OK
                        } else {
                            panic!("Expected Method as third member");
                        }
                        // Check fourth member is a property
                        if let (_, ast::ClassMember::Property(_)) = &class_type.members[3] {
                            // OK
                        } else {
                            panic!("Expected Property as fourth member");
                        }
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_with_visibility_sections() {
        let source = r#"
            program Test;
            type
                MyClass = class
                    x: integer;
                private
                    y: integer;
                protected
                    z: integer;
                public
                    w: integer;
                published
                    v: integer;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(class_type.members.len(), 5);
                        // Check visibility
                        assert_eq!(class_type.members[0].0, ast::Visibility::Default);
                        assert_eq!(class_type.members[1].0, ast::Visibility::Private);
                        assert_eq!(class_type.members[2].0, ast::Visibility::Protected);
                        assert_eq!(class_type.members[3].0, ast::Visibility::Public);
                        assert_eq!(class_type.members[4].0, ast::Visibility::Published);
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_with_constructor_destructor() {
        let source = r#"
            program Test;
            type
                MyClass = class
                    constructor Create;
                    destructor Destroy;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(class_type.members.len(), 2);
                        if let (_, ast::ClassMember::Constructor(_)) = &class_type.members[0] {
                            // OK
                        } else {
                            panic!("Expected Constructor");
                        }
                        if let (_, ast::ClassMember::Destructor(_)) = &class_type.members[1] {
                            // OK
                        } else {
                            panic!("Expected Destructor");
                        }
                    } else {
                        panic!("Expected ClassType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_meta_class() {
        let source = r#"
            program Test;
            type
                MyClass = class
                end;
            type
                MyClassRef = class of MyClass;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                // Find the meta-class type declaration
                let meta_class_decl = block.type_decls.iter()
                    .find(|td| {
                        if let Node::TypeDecl(type_decl) = td {
                            if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                                class_type.is_meta_class
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    });
                
                if let Some(Node::TypeDecl(type_decl)) = meta_class_decl {
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        assert!(class_type.is_meta_class);
                        assert!(class_type.meta_class_type.is_some());
                    } else {
                        panic!("Expected ClassType");
                    }
                } else {
                    panic!("Could not find meta-class type declaration");
                }
            }
        }
    }
}
