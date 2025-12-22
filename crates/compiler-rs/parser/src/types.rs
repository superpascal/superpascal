//! Type parsing
//!
//! This module handles parsing of type declarations and type expressions.

use ast;
use ast::Node;
use errors::{ParserError, ParserResult};
use tokens::{Span, Token, TokenKind};

/// Type parsing functionality
impl super::Parser {
    /// Parse type: identifier | ^type | ARRAY [ index_type ] OF element_type | RECORD field_list END | CLASS ...
    pub(super) fn parse_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        // Check for PACKED keyword (applies to RECORD or ARRAY)
        let is_packed = if self.check(&TokenKind::KwPacked) {
            self.advance()?; // consume PACKED
            true
        } else {
            false
        };

        // Check for pointer type: ^type
        if self.check(&TokenKind::Caret) {
            self.advance()?; // consume ^
            let base_type = self.parse_type()?; // Recursively parse the base type
            let span = start_span.merge(base_type.span());
            Ok(Node::PointerType(ast::PointerType {
                base_type: Box::new(base_type),
                span,
            }))
        } else if self.check(&TokenKind::KwArray) {
            self.advance()?; // consume ARRAY
            // Check if it's a dynamic array (ARRAY OF) or static array (ARRAY [ index ] OF)
            if self.check(&TokenKind::KwOf) {
                // Dynamic array: ARRAY OF element_type
                self.advance()?; // consume OF
                let element_type = self.parse_type()?;
                let span = start_span.merge(element_type.span());
                Ok(Node::DynamicArrayType(ast::DynamicArrayType {
                    element_type: Box::new(element_type),
                    span,
                }))
            } else {
                // Static array: ARRAY [ index_type ] OF element_type
                self.consume(TokenKind::LeftBracket, "[")?;
                let index_type = self.parse_type()?;
                self.consume(TokenKind::RightBracket, "]")?;
                self.consume(TokenKind::KwOf, "OF")?;
                let element_type = self.parse_type()?;
                let span = start_span.merge(element_type.span());
                Ok(Node::ArrayType(ast::ArrayType {
                    is_packed,
                    index_type: Box::new(index_type),
                    element_type: Box::new(element_type),
                    span,
                }))
            }
        } else if self.check(&TokenKind::KwSet) {
            // SET OF type
            self.advance()?; // consume SET
            self.consume(TokenKind::KwOf, "OF")?;
            let element_type = self.parse_type()?;
            let span = start_span.merge(element_type.span());
            Ok(Node::SetType(ast::SetType {
                element_type: Box::new(element_type),
                span,
            }))
        } else if self.check(&TokenKind::KwString) {
            // STRING or STRING[n]
            self.advance()?; // consume STRING
            let length = if self.check(&TokenKind::LeftBracket) {
                self.advance()?; // consume [
                let length_expr = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "]")?;
                Some(Box::new(length_expr))
            } else {
                None
            };
            let span = if let Some(ref len_expr) = length {
                start_span.merge(len_expr.span())
            } else {
                start_span
            };
            Ok(Node::StringType(ast::StringType {
                length,
                span,
            }))
        } else if self.check(&TokenKind::KwFile) {
            // FILE or FILE OF type
            self.advance()?; // consume FILE
            let element_type = if self.check(&TokenKind::KwOf) {
                self.advance()?; // consume OF
                Some(Box::new(self.parse_type()?))
            } else {
                None
            };
            let span = if let Some(ref elem_type) = element_type {
                start_span.merge(elem_type.span())
            } else {
                start_span
            };
            Ok(Node::FileType(ast::FileType {
                element_type,
                span,
            }))
        } else if self.check(&TokenKind::KwClass) && self.check_peek(&TokenKind::KwHelper) {
            // Class helper: class helper for Type (check before regular class)
            self.parse_helper_type(ast::HelperKind::Class)
        } else if self.check(&TokenKind::KwRecord) && self.check_peek(&TokenKind::KwHelper) {
            // Record helper: record helper for Type (check before regular record)
            self.parse_helper_type(ast::HelperKind::Record)
        } else if self.check(&TokenKind::KwType) && self.check_peek(&TokenKind::KwHelper) {
            // Type helper: type helper for Type
            self.parse_helper_type(ast::HelperKind::Type)
        } else if self.check(&TokenKind::KwRecord) {
            self.advance()?;
            let mut fields = vec![];
            let mut variant = None;
            
            // Parse fixed fields
            while !self.check(&TokenKind::KwCase) && !self.check(&TokenKind::KwEnd) {
                fields.push(self.parse_field_decl()?);
                self.consume(TokenKind::Semicolon, ";")?;
            }
            
            // Parse variant part if present
            if self.check(&TokenKind::KwCase) {
                variant = Some(self.parse_variant_part()?);
            }
            
            let end_token = self.consume(TokenKind::KwEnd, "END")?;
            let span = start_span.merge(end_token.span);
            Ok(Node::RecordType(ast::RecordType {
                is_packed,
                fields,
                variant,
                span,
            }))
        } else if self.check(&TokenKind::KwObject) {
            // Old-style object: object ... end (Turbo Pascal-style)
            self.parse_object_type()
        } else if self.check(&TokenKind::KwClass) {
            // Class parsing is in classes.rs
            self.parse_class_type()
        } else if self.check(&TokenKind::KwInterface) {
            // Interface type: INTERFACE [base_interfaces] [GUID] [methods/properties] END
            self.parse_interface_type()
        } else if self.check(&TokenKind::KwProcedure) || self.check(&TokenKind::KwFunction) {
            // Procedural type: PROCEDURE [params] [OF OBJECT] or FUNCTION [params]: return_type [OF OBJECT]
            self.parse_procedural_type()
        } else if self.check(&TokenKind::LeftParen) {
            // Enum type: ( identifier, identifier, ... )
            self.parse_enum_type()
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
            } else if self.check(&TokenKind::KwString) {
                let token = self.current().unwrap().clone();
                self.advance()?;
                Token {
                    kind: TokenKind::Identifier("string".to_string()),
                    span: token.span,
                }
            } else {
                return Err(ParserError::InvalidSyntax {
                    message: format!(
                        "Expected type identifier, found: {:?}",
                        self.current().map(|t| &t.kind)
                    ),
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
            
            // Check for generic type arguments: <T1, T2, ...>
            let generic_args = if self.check(&TokenKind::Less) {
                self.advance()?; // consume <
                let args = self.parse_generic_type_arguments()?;
                // After parse_generic_type_arguments, we've consumed the closing >
                // The span will be calculated from name_token to the last token we saw
                args
            } else {
                vec![]
            };
            
            // Calculate span: from name to end of generic args (if any)
            let span = if !generic_args.is_empty() {
                // Find the maximum end position from all generic args
                let max_end = generic_args.iter()
                    .map(|arg| arg.span().end)
                    .max()
                    .unwrap_or(name_token.span.end);
                Span::new(
                    name_token.span.start,
                    max_end,
                    name_token.span.line,
                    name_token.span.column,
                )
            } else {
                name_token.span
            };
            
            Ok(Node::NamedType(ast::NamedType {
                name,
                generic_args,
                span,
            }))
        }
    }

    /// Parse generic type arguments: <T1, T2, ...>
    /// Used in type instantiations like `TList<integer>`
    /// Note: The opening < has already been consumed by the caller
    fn parse_generic_type_arguments(&mut self) -> ParserResult<Vec<Box<Node>>> {
        let mut args = vec![];
        loop {
            // Parse a type argument
            let arg_type = self.parse_type()?;
            args.push(Box::new(arg_type));
            
            // Check for comma (more arguments) or closing >
            if self.check(&TokenKind::Comma) {
                self.advance()?;
            } else if self.check(&TokenKind::Greater) {
                self.advance()?; // consume >
                break;
            } else {
                return Err(ParserError::InvalidSyntax {
                    message: "Expected ',' or '>' in generic type arguments".to_string(),
                    span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                });
            }
        }
        
        Ok(args)
    }

    /// Parse generic type parameters: <T> or <T: constraint> or <T, U>
    /// Used in type declarations like `TList<T> = class ... end;`
    pub(crate) fn parse_generic_type_parameters(&mut self) -> ParserResult<Vec<ast::GenericParam>> {
        self.consume(TokenKind::Less, "<")?;
        
        let mut params = vec![];
        loop {
            // Parse parameter name(s) - can be comma-separated: <T, U> or <T; U: constraint>
            let mut param_names = vec![];
            
            // First parameter name
            let name_token = self.current().ok_or_else(|| ParserError::UnexpectedEof {
                expected: "identifier".to_string(),
                span: Span::at(0, 1, 1),
            })?.clone();
            self.advance()?;
            let param_name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier in generic type parameter".to_string(),
                    span: name_token.span,
                }),
            };
            param_names.push(param_name);
            
            // Check for more names (comma-separated): <T, U>
            while self.check(&TokenKind::Comma) {
                self.advance()?; // consume comma
                let next_name_token = self.current().ok_or_else(|| ParserError::UnexpectedEof {
                    expected: "identifier".to_string(),
                    span: Span::at(0, 1, 1),
                })?.clone();
                self.advance()?;
                let next_name = match &next_name_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier in generic type parameter".to_string(),
                        span: next_name_token.span,
                    }),
                };
                param_names.push(next_name);
            }
            
            // Check for constraint: <T: constraint> or <T; U: constraint>
            let constraint = if self.check(&TokenKind::Colon) {
                self.advance()?; // consume :
                Some(Box::new(self.parse_type()?))
            } else {
                None
            };
            
            // Create GenericParam for each name (they share the same constraint if present)
            for name in param_names {
                params.push(ast::GenericParam {
                    name,
                    constraint: constraint.as_ref().map(|c| (*c).clone()),
                    span: name_token.span, // Use first name's span
                });
            }
            
            // Check for semicolon (separates parameter groups with different constraints)
            // or closing >
            if self.check(&TokenKind::Semicolon) {
                self.advance()?; // consume ;
                // Continue to next parameter group
            } else if self.check(&TokenKind::Greater) {
                self.advance()?; // consume >
                break;
            } else if self.check(&TokenKind::Comma) {
                // Another parameter without constraint, continue
                continue;
            } else {
                return Err(ParserError::InvalidSyntax {
                    message: "Expected ',', ';', or '>' in generic type parameters".to_string(),
                    span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                });
            }
        }
        
        Ok(params)
    }

    /// Parse enum type: ( identifier, identifier, ... )
    fn parse_enum_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::LeftParen, "(")?;

        let mut values = vec![];
        loop {
            let value_token = self.consume(TokenKind::Identifier(String::new()), "enum value identifier")?;
            let value = match &value_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected enum value identifier".to_string(),
                    span: value_token.span,
                }),
            };
            values.push(value);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?; // consume comma
        }

        let end_token = self.consume(TokenKind::RightParen, ")")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::EnumType(ast::EnumType {
            values,
            span,
        }))
    }

    /// Parse procedural type: PROCEDURE [params] [OF OBJECT] or FUNCTION [params]: return_type [OF OBJECT]
    fn parse_procedural_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let is_function = self.check(&TokenKind::KwFunction);
        self.advance()?; // consume PROCEDURE or FUNCTION

        // Parse optional parameter list
        let params = if self.check(&TokenKind::LeftParen) {
            self.consume(TokenKind::LeftParen, "(")?;
            let mut params = vec![];
            if !self.check(&TokenKind::RightParen) {
                loop {
                    // Parse parameter: [var|const|constref|out] identifier {, identifier} : type
                    let mut param_names = vec![];
                    let param_mode = if self.check(&TokenKind::KwVar) {
                        self.advance()?;
                        ast::ParamType::Var
                    } else if self.check(&TokenKind::KwConst) {
                        self.advance()?;
                        ast::ParamType::Const
                    } else if self.check(&TokenKind::KwConstref) {
                        self.advance()?;
                        ast::ParamType::ConstRef
                    } else if self.check(&TokenKind::KwOut) {
                        self.advance()?;
                        ast::ParamType::Out
                    } else {
                        ast::ParamType::Value
                    };
                    
                    loop {
                        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
                        let name = match &name_token.kind {
                            TokenKind::Identifier(name) => name.clone(),
                            _ => unreachable!(),
                        };
                        param_names.push(name);
                        if !self.check(&TokenKind::Comma) {
                            break;
                        }
                        self.advance()?; // consume comma
                    }
                    
                    self.consume(TokenKind::Colon, ":")?;
                    let param_type = self.parse_type()?;
                    let param_span = start_span.merge(param_type.span());
                    
                    params.push(ast::Param {
                        names: param_names,
                        param_type: param_mode,
                        type_expr: Box::new(param_type),
                        default_value: None,
                        span: param_span,
                    });
                    if !self.check(&TokenKind::Semicolon) {
                        break;
                    }
                    self.advance()?; // consume semicolon
                }
            }
            self.consume(TokenKind::RightParen, ")")?;
            params
        } else {
            vec![]
        };

        // Parse return type for functions
        let return_type = if is_function {
            self.consume(TokenKind::Colon, ":")?;
            Some(Box::new(self.parse_type()?))
        } else {
            None
        };

        // Parse optional "OF OBJECT" for method pointers
        let is_method_pointer = if self.check(&TokenKind::KwOf) {
            self.advance()?; // consume OF
            if self.check(&TokenKind::KwObject) {
                self.advance()?; // consume OBJECT
                true
            } else {
                return Err(ParserError::InvalidSyntax {
                    message: "Expected 'OBJECT' after 'OF'".to_string(),
                    span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                });
            }
        } else {
            false
        };

        let span = if let Some(ref ret_type) = return_type {
            start_span.merge(ret_type.span())
        } else {
            start_span
        };

        Ok(Node::ProceduralType(ast::ProceduralType {
            is_function,
            params,
            return_type,
            is_method_pointer,
            span,
        }))
    }

    /// Parse interface type: INTERFACE [base_interfaces] [GUID] [methods/properties] END
    fn parse_interface_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwInterface, "INTERFACE")?;

        // Parse optional base interfaces: INTERFACE(IBase1, IBase2)
        let base_interfaces = if self.check(&TokenKind::LeftParen) {
            self.advance()?; // consume (
            let mut bases = vec![];
            loop {
                let name_token = self.consume(TokenKind::Identifier(String::new()), "interface name")?;
                let name = match &name_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => unreachable!(),
                };
                bases.push(name);
                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?; // consume comma
            }
            self.consume(TokenKind::RightParen, ")")?;
            bases
        } else {
            vec![]
        };

        // Parse optional GUID: ['{GUID-STRING}']
        let guid = if self.check(&TokenKind::LeftBracket) {
            self.advance()?; // consume [
            if let Some(TokenKind::StringLiteral(_)) = self.current().map(|t| &t.kind) {
                let guid_token = self.current().unwrap().clone();
                let guid_value = match &guid_token.kind {
                    TokenKind::StringLiteral(s) => s.clone(),
                    _ => unreachable!(),
                };
                self.advance()?; // consume string
                self.consume(TokenKind::RightBracket, "]")?;
                Some(guid_value)
            } else {
                return Err(ParserError::InvalidSyntax {
                    message: "Expected GUID string in brackets".to_string(),
                    span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                });
            }
        } else {
            None
        };

        // Parse methods and properties
        let mut methods = vec![];
        let mut properties = vec![];

        while !self.check(&TokenKind::KwEnd) {
            if self.check(&TokenKind::KwProcedure) {
                // Parse procedure forward declaration
                let proc = self.parse_procedure_forward_decl()?;
                methods.push(proc);
            } else if self.check(&TokenKind::KwFunction) {
                // Parse function forward declaration
                let func = self.parse_function_forward_decl()?;
                methods.push(func);
            } else if self.check(&TokenKind::KwProperty) {
                // Parse property declaration
                let property = super::properties::parse_property_decl(self)?;
                properties.push(property);
            } else {
                return Err(ParserError::InvalidSyntax {
                    message: "Expected method or property declaration in interface".to_string(),
                    span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
                });
            }
        }

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::InterfaceType(ast::InterfaceType {
            name: None, // Interface name is set by the type declaration
            guid,
            base_interfaces,
            methods,
            properties,
            span,
        }))
    }

    /// Parse field declaration: identifier_list : type
    pub(super) fn parse_field_decl(&mut self) -> ParserResult<ast::FieldDecl> {
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

    /// Parse variant part: CASE [tag_field :] tag_type OF variant { ; variant } [ ELSE fields ]
    fn parse_variant_part(&mut self) -> ParserResult<ast::VariantPart> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwCase, "CASE")?;

        // Optional tag field name: tag_field : tag_type
        let tag_field = if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            let name_token = self.current().unwrap().clone();
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => unreachable!(),
            };
            // Check if next token is colon (then it's a tag field) or OF (then it's the type)
            if self.check_peek(&TokenKind::Colon) {
                self.advance()?; // consume identifier
                self.advance()?; // consume :
                Some(name)
            } else {
                // No colon, so this identifier is the tag type name
                None
            }
        } else {
            None
        };

        // Parse tag type
        let tag_type = self.parse_type()?;
        self.consume(TokenKind::KwOf, "OF")?;

        // Parse variants
        let mut variants = vec![];
        let mut else_variant = None;

        while !self.check(&TokenKind::KwElse) && !self.check(&TokenKind::KwEnd) {
            let variant_start = self
                .current()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));

            // Parse case values (can be multiple, separated by commas)
            let mut values = vec![];
            loop {
                // Parse a case value (can be expression or range)
                let value = self.parse_expression()?;
                values.push(value);

                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?; // consume comma
            }

            self.consume(TokenKind::Colon, ":")?;
            self.consume(TokenKind::LeftParen, "(")?;

            // Parse variant fields
            let mut variant_fields = vec![];
            while !self.check(&TokenKind::RightParen) {
                variant_fields.push(self.parse_field_decl()?);
                if self.check(&TokenKind::Semicolon) {
                    self.advance()?;
                }
            }

            self.consume(TokenKind::RightParen, ")")?;

            let variant_span = if let Some(last_field) = variant_fields.last() {
                variant_start.merge(last_field.span)
            } else {
                variant_start
            };

            variants.push(ast::Variant {
                values,
                fields: variant_fields,
                span: variant_span,
            });

            // Optional semicolon between variants
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }

        // Optional ELSE variant
        if self.check(&TokenKind::KwElse) {
            self.advance()?; // consume ELSE
            // Check if there's a colon (some Pascal dialects use "else:" but standard is just "else (")
            if self.check(&TokenKind::Colon) {
                self.advance()?; // consume optional colon
            }
            self.consume(TokenKind::LeftParen, "(")?;
            let mut else_fields = vec![];
            while !self.check(&TokenKind::RightParen) {
                else_fields.push(self.parse_field_decl()?);
                if self.check(&TokenKind::Semicolon) {
                    self.advance()?;
                }
            }
            self.consume(TokenKind::RightParen, ")")?;
            // Optional semicolon after else variant (some Pascal dialects allow it)
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
            else_variant = Some(else_fields);
        }

        let span = if let Some(ref last_variant) = variants.last() {
            start_span.merge(last_variant.span)
        } else {
            start_span
        };

        Ok(ast::VariantPart {
            tag_field,
            tag_type: Box::new(tag_type),
            variants,
            else_variant,
            span,
        })
    }

    /// Parse helper type: [class|record|type] helper [ ( base_helpers ) ] for target_type [ members ] end
    pub(super) fn parse_helper_type(&mut self, helper_kind: ast::HelperKind) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        // Consume the helper kind keyword (class, record, or type)
        match helper_kind {
            ast::HelperKind::Class => {
                self.consume(TokenKind::KwClass, "CLASS")?;
            }
            ast::HelperKind::Record => {
                self.consume(TokenKind::KwRecord, "RECORD")?;
            }
            ast::HelperKind::Type => {
                self.consume(TokenKind::KwType, "TYPE")?;
            }
        }

        // Consume HELPER keyword
        self.consume(TokenKind::KwHelper, "HELPER")?;

        // Parse optional base helpers: ( BaseHelper1, BaseHelper2 )
        let mut base_helpers = vec![];
        if self.check(&TokenKind::LeftParen) {
            self.advance()?; // consume (
            loop {
                let base_token = self.consume(TokenKind::Identifier(String::new()), "base helper name")?;
                let base_name = match &base_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier for base helper".to_string(),
                        span: base_token.span,
                    }),
                };
                base_helpers.push(base_name);

                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?; // consume comma
            }
            self.consume(TokenKind::RightParen, ")")?;
        }

        // Consume FOR keyword
        self.consume(TokenKind::KwFor, "FOR")?;

        // Parse target type (the type being extended)
        let target_type = Box::new(self.parse_type()?);

        // Parse helper members (same as class members)
        let mut members = vec![];
        let mut current_visibility = ast::Visibility::Default;

        while !self.check(&TokenKind::KwEnd) {
            // Check for visibility specifiers
            if self.check(&TokenKind::KwPrivate) {
                self.advance()?;
                current_visibility = ast::Visibility::Private;
                continue;
            } else if self.check(&TokenKind::KwProtected) {
                self.advance()?;
                current_visibility = ast::Visibility::Protected;
                continue;
            } else if self.check(&TokenKind::KwPublic) {
                self.advance()?;
                current_visibility = ast::Visibility::Public;
                continue;
            } else if self.check(&TokenKind::KwPublished) {
                self.advance()?;
                current_visibility = ast::Visibility::Published;
                continue;
            } else if self.check(&TokenKind::KwStrict) {
                self.advance()?;
                if self.check(&TokenKind::KwPrivate) {
                    self.advance()?;
                    current_visibility = ast::Visibility::StrictPrivate;
                } else if self.check(&TokenKind::KwProtected) {
                    self.advance()?;
                    current_visibility = ast::Visibility::StrictProtected;
                }
                continue;
            }

            // Parse members (similar to class members)
            if self.check(&TokenKind::KwProcedure) {
                let proc = self.parse_procedure_decl_in_class()?;
                members.push((current_visibility, ast::ClassMember::Method(proc)));
            } else if self.check(&TokenKind::KwFunction) {
                let func = self.parse_function_decl_in_class()?;
                members.push((current_visibility, ast::ClassMember::Method(func)));
            } else if self.check(&TokenKind::KwProperty) {
                let prop = super::properties::parse_property_decl(self)?;
                members.push((current_visibility, ast::ClassMember::Property(prop)));
            } else {
                // Unknown member - break and let caller handle
                break;
            }
        }

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::HelperType(ast::HelperType {
            helper_kind,
            base_helpers,
            target_type,
            members,
            span,
        }))
    }

    /// Parse old-style object type: OBJECT [ ( base_objects ) ] [ members ] END
    /// Old-style objects are value types (like records) but can have methods and inheritance
    pub(super) fn parse_object_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwObject, "OBJECT")?;

        // Check for forward declaration: object;
        if self.check(&TokenKind::Semicolon) {
            self.advance()?;
            let span = start_span;
            return Ok(Node::ObjectType(ast::ObjectType {
                base_objects: vec![],
                is_forward_decl: true,
                members: vec![],
                span,
            }));
        }

        // Parse base objects (inheritance): ( BaseObject1, BaseObject2 )
        let mut base_objects = vec![];
        if self.check(&TokenKind::LeftParen) {
            self.advance()?; // consume (
            loop {
                let base_token = self.consume(TokenKind::Identifier(String::new()), "base object name")?;
                let base_name = match &base_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier for base object".to_string(),
                        span: base_token.span,
                    }),
                };
                base_objects.push(base_name);

                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?; // consume comma
            }
            self.consume(TokenKind::RightParen, ")")?;
        }

        // Check if it's just "object" with no members
        if self.check(&TokenKind::KwEnd) {
            let end_token = self.consume(TokenKind::KwEnd, "END")?;
            let span = start_span.merge(end_token.span);
            return Ok(Node::ObjectType(ast::ObjectType {
                base_objects,
                is_forward_decl: false,
                members: vec![],
                span,
            }));
        }

        // Parse object members (similar to class members, but simpler - no constructors/destructors)
        let mut members = vec![];
        let mut current_visibility = ast::Visibility::Default;

        while !self.check(&TokenKind::KwEnd) {
            // Check for visibility specifiers
            if self.check(&TokenKind::KwPrivate) {
                self.advance()?;
                current_visibility = ast::Visibility::Private;
                continue;
            } else if self.check(&TokenKind::KwProtected) {
                self.advance()?;
                current_visibility = ast::Visibility::Protected;
                continue;
            } else if self.check(&TokenKind::KwPublic) {
                self.advance()?;
                current_visibility = ast::Visibility::Public;
                continue;
            } else if self.check(&TokenKind::KwPublished) {
                self.advance()?;
                current_visibility = ast::Visibility::Published;
                continue;
            } else if self.check(&TokenKind::KwStrict) {
                self.advance()?;
                if self.check(&TokenKind::KwPrivate) {
                    self.advance()?;
                    current_visibility = ast::Visibility::StrictPrivate;
                } else if self.check(&TokenKind::KwProtected) {
                    self.advance()?;
                    current_visibility = ast::Visibility::StrictProtected;
                }
                continue;
            }

            // Parse members
            if self.check(&TokenKind::KwType) {
                // Nested type declarations
                let type_decls = self.parse_type_decls()?;
                for decl in type_decls {
                    members.push((current_visibility, ast::ClassMember::Type(decl)));
                }
            } else if self.check(&TokenKind::KwVar) {
                // Field declarations (with VAR keyword)
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
                        // parse_field_decl returns FieldDecl struct directly
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
            } else if self.check(&TokenKind::KwProcedure) {
                // Method (procedure)
                let proc = self.parse_procedure_decl_in_class()?;
                members.push((current_visibility, ast::ClassMember::Method(proc)));
            } else if self.check(&TokenKind::KwFunction) {
                // Method (function)
                let func = self.parse_function_decl_in_class()?;
                members.push((current_visibility, ast::ClassMember::Method(func)));
            } else if self.check(&TokenKind::KwProperty) {
                // Property
                let prop = super::properties::parse_property_decl(self)?;
                members.push((current_visibility, ast::ClassMember::Property(prop)));
            } else {
                // Unknown member - break and let caller handle
                break;
            }
        }

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::ObjectType(ast::ObjectType {
            base_objects,
            is_forward_decl: false,
            members,
            span,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::super::Parser;
    use ast::Node;

    // ===== Pointer Type Tests =====

    #[test]
    fn test_parse_pointer_type() {
        let source = r#"
            program Test;
            var p: ^integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::PointerType(ptr_type) = var_decl.type_expr.as_ref() {
                        if let Node::NamedType(named) = ptr_type.base_type.as_ref() {
                            assert_eq!(named.name, "integer");
                        } else {
                            panic!("Expected NamedType in pointer base");
                        }
                    } else {
                        panic!("Expected PointerType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_pointer_to_pointer() {
        let source = r#"
            program Test;
            var p: ^^integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::PointerType(ptr_type) = var_decl.type_expr.as_ref() {
                        if let Node::PointerType(inner_ptr) = ptr_type.base_type.as_ref() {
                            if let Node::NamedType(named) = inner_ptr.base_type.as_ref() {
                                assert_eq!(named.name, "integer");
                            } else {
                                panic!("Expected NamedType in inner pointer base");
                            }
                        } else {
                            panic!("Expected inner PointerType");
                        }
                    } else {
                        panic!("Expected PointerType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_pointer_dereference() {
        let source = r#"
            program Test;
            var p: ^integer;
            begin
                p^ := 42;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::AssignStmt(assign) = &block.statements[0] {
                    if let Node::DerefExpr(deref) = assign.target.as_ref() {
                        if let Node::IdentExpr(ident) = deref.pointer.as_ref() {
                            assert_eq!(ident.name, "p");
                        } else {
                            panic!("Expected IdentExpr in dereference");
                        }
                    } else {
                        panic!("Expected DerefExpr");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_pointer_dereference_chain() {
        let source = r#"
            program Test;
            var p: ^^integer;
            begin
                p^^ := 42;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::AssignStmt(assign) = &block.statements[0] {
                    if let Node::DerefExpr(outer_deref) = assign.target.as_ref() {
                        if let Node::DerefExpr(inner_deref) = outer_deref.pointer.as_ref() {
                            if let Node::IdentExpr(ident) = inner_deref.pointer.as_ref() {
                                assert_eq!(ident.name, "p");
                            } else {
                                panic!("Expected IdentExpr in inner dereference");
                            }
                        } else {
                            panic!("Expected inner DerefExpr");
                        }
                    } else {
                        panic!("Expected outer DerefExpr");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_pointer_in_expression() {
        let source = r#"
            program Test;
            var p: ^integer;
            begin
                writeln(p^);
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::CallStmt(call) = &block.statements[0] {
                    assert_eq!(call.name, "writeln");
                    assert_eq!(call.args.len(), 1);
                    if let Node::DerefExpr(deref) = &call.args[0] {
                        if let Node::IdentExpr(ident) = deref.pointer.as_ref() {
                            assert_eq!(ident.name, "p");
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_pointer_field_access() {
        let source = r#"
            program Test;
            type Rec = record
                x: integer;
            end;
            var p: ^Rec;
            begin
                p^.x := 10;
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::AssignStmt(assign) = &block.statements[0] {
                    if let Node::FieldExpr(field) = assign.target.as_ref() {
                        assert_eq!(field.field, "x");
                        if let Node::DerefExpr(deref) = field.record.as_ref() {
                            if let Node::IdentExpr(ident) = deref.pointer.as_ref() {
                                assert_eq!(ident.name, "p");
                            }
                        } else {
                            panic!("Expected DerefExpr in field access");
                        }
                    } else {
                        panic!("Expected FieldExpr");
                    }
                }
            }
        }
    }

    // ===== Set Type Tests =====

    #[test]
    fn test_parse_set_of_integer() {
        let source = r#"
            program Test;
            type IntSet = set of integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::SetType(set_type) = type_decl.type_expr.as_ref() {
                        if let Node::NamedType(named) = set_type.element_type.as_ref() {
                            assert_eq!(named.name, "integer");
                        } else {
                            panic!("Expected NamedType in set element type");
                        }
                    } else {
                        panic!("Expected SetType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_set_of_char() {
        let source = r#"
            program Test;
            type CharSet = set of char;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::SetType(set_type) = type_decl.type_expr.as_ref() {
                        if let Node::NamedType(named) = set_type.element_type.as_ref() {
                            assert_eq!(named.name, "char");
                        } else {
                            panic!("Expected NamedType in set element type");
                        }
                    } else {
                        panic!("Expected SetType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_set_of_named_type() {
        let source = r#"
            program Test;
            type MyType = integer;
            type MyTypeSet = set of MyType;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    if let Node::SetType(set_type) = type_decl.type_expr.as_ref() {
                        if let Node::NamedType(named) = set_type.element_type.as_ref() {
                            assert_eq!(named.name, "MyType");
                        } else {
                            panic!("Expected NamedType in set element type");
                        }
                    } else {
                        panic!("Expected SetType");
                    }
                }
            }
        }
    }

    // ===== String Type Tests =====

    #[test]
    fn test_parse_string_type() {
        let source = r#"
            program Test;
            var s: string;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::StringType(string_type) = var_decl.type_expr.as_ref() {
                        assert!(string_type.length.is_none(), "Expected unbounded string");
                    } else {
                        panic!("Expected StringType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_string_type_fixed_length() {
        let source = r#"
            program Test;
            var s: string[80];
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::StringType(string_type) = var_decl.type_expr.as_ref() {
                        assert!(string_type.length.is_some(), "Expected fixed-length string");
                        if let Some(length_expr) = &string_type.length {
                            if let Node::LiteralExpr(lit) = length_expr.as_ref() {
                                if let ast::LiteralValue::Integer(v) = lit.value {
                                    assert_eq!(v, 80);
                                } else {
                                    panic!("Expected Integer literal for string length");
                                }
                            } else {
                                panic!("Expected LiteralExpr for string length");
                            }
                        }
                    } else {
                        panic!("Expected StringType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_string_type_with_expression() {
        let source = r#"
            program Test;
            const MaxLen = 100;
            var s: string[MaxLen];
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::StringType(string_type) = var_decl.type_expr.as_ref() {
                        assert!(string_type.length.is_some(), "Expected fixed-length string");
                        if let Some(length_expr) = &string_type.length {
                            if let Node::IdentExpr(ident) = length_expr.as_ref() {
                                assert_eq!(ident.name, "MaxLen");
                            } else {
                                panic!("Expected IdentExpr for string length");
                            }
                        }
                    } else {
                        panic!("Expected StringType");
                    }
                }
            }
        }
    }

    // ===== Variant Record Tests =====

    #[test]
    fn test_parse_variant_record_simple() {
        let source = r#"
            program Test;
            type Shape = record
                x, y: integer;
                case kind: integer of
                    1: (radius: integer);
                    2: (width, height: integer);
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
                    if let Node::RecordType(record_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(record_type.fields.len(), 1); // x, y
                        assert!(record_type.variant.is_some());
                        if let Some(ref variant) = record_type.variant {
                            assert_eq!(variant.tag_field.as_ref().unwrap(), "kind");
                            assert_eq!(variant.variants.len(), 2);
                            assert_eq!(variant.variants[0].fields.len(), 1); // radius
                            assert_eq!(variant.variants[1].fields.len(), 1); // width, height (one FieldDecl with 2 names)
                        }
                    } else {
                        panic!("Expected RecordType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_variant_record_without_tag_field() {
        let source = r#"
            program Test;
            type Shape = record
                x, y: integer;
                case integer of
                    1: (radius: integer);
                    2: (width, height: integer);
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
                    if let Node::RecordType(record_type) = type_decl.type_expr.as_ref() {
                        assert!(record_type.variant.is_some());
                        if let Some(ref variant) = record_type.variant {
                            assert!(variant.tag_field.is_none());
                            assert_eq!(variant.variants.len(), 2);
                        }
                    } else {
                        panic!("Expected RecordType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_variant_record_with_else() {
        let source = r#"
            program Test;
            type Shape = record
                x, y: integer;
                case kind: integer of
                    1: (radius: integer);
                    2: (width, height: integer);
                    else: (data: integer);
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
                    if let Node::RecordType(record_type) = type_decl.type_expr.as_ref() {
                        assert!(record_type.variant.is_some());
                        if let Some(ref variant) = record_type.variant {
                            assert!(variant.else_variant.is_some());
                            if let Some(ref else_fields) = variant.else_variant {
                                assert_eq!(else_fields.len(), 1);
                            }
                        }
                    } else {
                        panic!("Expected RecordType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_variant_record_multiple_case_values() {
        let source = r#"
            program Test;
            type Shape = record
                case kind: integer of
                    1, 2: (radius: integer);
                    3, 4, 5: (width, height: integer);
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
                    if let Node::RecordType(record_type) = type_decl.type_expr.as_ref() {
                        assert!(record_type.variant.is_some());
                        if let Some(ref variant) = record_type.variant {
                            assert_eq!(variant.variants.len(), 2);
                            assert_eq!(variant.variants[0].values.len(), 2); // 1, 2
                            assert_eq!(variant.variants[1].values.len(), 3); // 3, 4, 5
                        }
                    } else {
                        panic!("Expected RecordType");
                    }
                }
            }
        }
    }

    // ===== Interface Type Tests =====

    #[test]
    fn test_parse_interface_type_simple() {
        let source = r#"
            program Test;
            type
                IMyInterface = interface
                    procedure Method1;
                    function Method2: integer;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        let program = result.unwrap();
        if let Node::Program(prog) = program {
            if let Node::Block(block) = *prog.block {
                assert_eq!(block.type_decls.len(), 1);
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    assert_eq!(type_decl.name, "IMyInterface");
                    if let Node::InterfaceType(interface) = type_decl.type_expr.as_ref() {
                        assert_eq!(interface.base_interfaces.len(), 0);
                        assert_eq!(interface.guid, None);
                        assert_eq!(interface.methods.len(), 2);
                        assert_eq!(interface.properties.len(), 0);
                    } else {
                        panic!("Expected InterfaceType");
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            } else {
                panic!("Expected Block");
            }
        } else {
            panic!("Expected Program");
        }
    }

    #[test]
    fn test_parse_interface_type_with_base() {
        let source = r#"
            program Test;
            type
                IMyInterface = interface(IBaseInterface)
                    procedure Method1;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        let program = result.unwrap();
        if let Node::Program(prog) = program {
            if let Node::Block(block) = *prog.block {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::InterfaceType(interface) = type_decl.type_expr.as_ref() {
                        assert_eq!(interface.base_interfaces.len(), 1);
                        assert_eq!(interface.base_interfaces[0], "IBaseInterface");
                    } else {
                        panic!("Expected InterfaceType");
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            } else {
                panic!("Expected Block");
            }
        } else {
            panic!("Expected Program");
        }
    }

    #[test]
    fn test_parse_interface_type_with_guid() {
        let source = r#"
            program Test;
            type
                IMyInterface = interface
                    ['{12345678-1234-1234-1234-123456789ABC}']
                    procedure Method1;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        let program = result.unwrap();
        if let Node::Program(prog) = program {
            if let Node::Block(block) = *prog.block {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::InterfaceType(interface) = type_decl.type_expr.as_ref() {
                        assert!(interface.guid.is_some());
                        assert_eq!(interface.guid.as_ref().unwrap(), "{12345678-1234-1234-1234-123456789ABC}");
                    } else {
                        panic!("Expected InterfaceType");
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            } else {
                panic!("Expected Block");
            }
        } else {
            panic!("Expected Program");
        }
    }

    #[test]
    fn test_parse_interface_type_with_properties() {
        let source = r#"
            program Test;
            type
                IMyInterface = interface
                    property Prop1: integer read GetProp1 write SetProp1;
                    procedure Method1;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        let program = result.unwrap();
        if let Node::Program(prog) = program {
            if let Node::Block(block) = *prog.block {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::InterfaceType(interface) = type_decl.type_expr.as_ref() {
                        assert_eq!(interface.properties.len(), 1);
                        assert_eq!(interface.methods.len(), 1);
                    } else {
                        panic!("Expected InterfaceType");
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            } else {
                panic!("Expected Block");
            }
        } else {
            panic!("Expected Program");
        }
    }

    // ========== File Type Tests ==========

    #[test]
    fn test_parse_file_type_untyped() {
        let source = r#"
            program Test;
            var f: file;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::FileType(file_type) = var_decl.type_expr.as_ref() {
                        assert!(file_type.element_type.is_none(), "Expected untyped file");
                    } else {
                        panic!("Expected FileType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_file_type_typed() {
        let source = r#"
            program Test;
            var f: file of integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::FileType(file_type) = var_decl.type_expr.as_ref() {
                        assert!(file_type.element_type.is_some(), "Expected typed file");
                        if let Some(element_type) = &file_type.element_type {
                            if let Node::NamedType(named) = element_type.as_ref() {
                                assert_eq!(named.name, "integer");
                            } else {
                                panic!("Expected NamedType for element type");
                            }
                        }
                    } else {
                        panic!("Expected FileType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_file_type_in_type_declaration() {
        let source = r#"
            program Test;
            type
                TextFile = file;
                IntFile = file of integer;
            var
                tf: TextFile;
                ifile: IntFile;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.type_decls.len(), 2);
                // Check first type declaration (TextFile = file)
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    assert_eq!(type_decl.name, "TextFile");
                    if let Node::FileType(file_type) = type_decl.type_expr.as_ref() {
                        assert!(file_type.element_type.is_none());
                    } else {
                        panic!("Expected FileType for TextFile");
                    }
                }
                // Check second type declaration (IntFile = file of integer)
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    assert_eq!(type_decl.name, "IntFile");
                    if let Node::FileType(file_type) = type_decl.type_expr.as_ref() {
                        assert!(file_type.element_type.is_some());
                    } else {
                        panic!("Expected FileType for IntFile");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_file_type_with_record() {
        let source = r#"
            program Test;
            type
                Person = record
                    name: string;
                    age: integer;
                end;
            var f: file of Person;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::FileType(file_type) = var_decl.type_expr.as_ref() {
                        assert!(file_type.element_type.is_some());
                        if let Some(element_type) = &file_type.element_type {
                            if let Node::NamedType(named) = element_type.as_ref() {
                                assert_eq!(named.name, "Person");
                            } else {
                                panic!("Expected NamedType for Person");
                            }
                        }
                    } else {
                        panic!("Expected FileType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_enum_type() {
        let source = r#"
            program Test;
            type
                Color = (Red, Green, Blue);
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    assert_eq!(type_decl.name, "Color");
                    if let Node::EnumType(enum_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(enum_type.values.len(), 3);
                        assert_eq!(enum_type.values[0], "Red");
                        assert_eq!(enum_type.values[1], "Green");
                        assert_eq!(enum_type.values[2], "Blue");
                    } else {
                        panic!("Expected EnumType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_enum_type_single_value() {
        let source = r#"
            program Test;
            type
                Direction = (North);
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::EnumType(enum_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(enum_type.values.len(), 1);
                        assert_eq!(enum_type.values[0], "North");
                    } else {
                        panic!("Expected EnumType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_enum_type_multiple_values() {
        let source = r#"
            program Test;
            type
                Status = (Pending, InProgress, Completed, Cancelled);
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::EnumType(enum_type) = type_decl.type_expr.as_ref() {
                        assert_eq!(enum_type.values.len(), 4);
                        assert_eq!(enum_type.values[0], "Pending");
                        assert_eq!(enum_type.values[1], "InProgress");
                        assert_eq!(enum_type.values[2], "Completed");
                        assert_eq!(enum_type.values[3], "Cancelled");
                    } else {
                        panic!("Expected EnumType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_dynamic_array() {
        let source = r#"
            program Test;
            type
                IntArray = array of integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::DynamicArrayType(dyn_arr) = type_decl.type_expr.as_ref() {
                        if let Node::NamedType(named) = dyn_arr.element_type.as_ref() {
                            assert_eq!(named.name, "integer");
                        } else {
                            panic!("Expected NamedType for element type");
                        }
                    } else {
                        panic!("Expected DynamicArrayType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_dynamic_array_vs_static_array() {
        let source = r#"
            program Test;
            type
                StaticArray = array[integer] of integer;
                DynamicArray = array of integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                // First type should be static array
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    assert!(matches!(type_decl.type_expr.as_ref(), Node::ArrayType(_)));
                }
                // Second type should be dynamic array
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    assert!(matches!(type_decl.type_expr.as_ref(), Node::DynamicArrayType(_)));
                }
            }
        }
    }

    #[test]
    fn test_parse_dynamic_array_of_string() {
        let source = r#"
            program Test;
            type
                StringArray = array of string;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    if let Node::DynamicArrayType(dyn_arr) = type_decl.type_expr.as_ref() {
                        if let Node::StringType(_) = dyn_arr.element_type.as_ref() {
                            // Good - element type is string
                        } else {
                            panic!("Expected StringType for element type");
                        }
                    } else {
                        panic!("Expected DynamicArrayType");
                    }
                }
            }
        }
    }

    // ===== Generic Type Tests =====

    #[test]
    fn test_parse_generic_type_declaration_simple() {
        let source = r#"
            program Test;
            type
                TList<T> = class
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
                    assert_eq!(type_decl.name, "TList");
                    assert_eq!(type_decl.generic_params.len(), 1);
                    assert_eq!(type_decl.generic_params[0].name, "T");
                    assert!(type_decl.generic_params[0].constraint.is_none());
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_declaration_multiple_params() {
        let source = r#"
            program Test;
            type
                TDictionary<TKey, TValue> = class
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
                    assert_eq!(type_decl.name, "TDictionary");
                    assert_eq!(type_decl.generic_params.len(), 2);
                    assert_eq!(type_decl.generic_params[0].name, "TKey");
                    assert_eq!(type_decl.generic_params[1].name, "TValue");
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_declaration_with_constraint() {
        let source = r#"
            program Test;
            type
                TComparable<T: IComparable> = class
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
                    assert_eq!(type_decl.name, "TComparable");
                    assert_eq!(type_decl.generic_params.len(), 1);
                    assert_eq!(type_decl.generic_params[0].name, "T");
                    assert!(type_decl.generic_params[0].constraint.is_some());
                    if let Some(constraint) = &type_decl.generic_params[0].constraint {
                        if let Node::NamedType(named) = constraint.as_ref() {
                            assert_eq!(named.name, "IComparable");
                        } else {
                            panic!("Expected NamedType constraint");
                        }
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_instantiation_simple() {
        let source = r#"
            program Test;
            var
                list: TList<integer>;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::NamedType(named) = var_decl.type_expr.as_ref() {
                        assert_eq!(named.name, "TList");
                        assert_eq!(named.generic_args.len(), 1);
                        if let Node::NamedType(arg) = named.generic_args[0].as_ref() {
                            assert_eq!(arg.name, "integer");
                        } else {
                            panic!("Expected NamedType as generic argument");
                        }
                    } else {
                        panic!("Expected NamedType");
                    }
                } else {
                    panic!("Expected VarDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_instantiation_multiple_args() {
        let source = r#"
            program Test;
            var
                dict: TDictionary<string, integer>;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::NamedType(named) = var_decl.type_expr.as_ref() {
                        assert_eq!(named.name, "TDictionary");
                        assert_eq!(named.generic_args.len(), 2);
                        // First argument should be string (can be StringType or NamedType)
                        match named.generic_args[0].as_ref() {
                            Node::StringType(_) => {
                                // string keyword parsed as StringType - this is acceptable
                            }
                            Node::NamedType(arg1) => {
                                assert_eq!(arg1.name, "string");
                            }
                            _ => {
                                panic!("Expected StringType or NamedType as first generic argument, got: {:?}", named.generic_args[0]);
                            }
                        }
                        if let Node::NamedType(arg2) = named.generic_args[1].as_ref() {
                            assert_eq!(arg2.name, "integer");
                        } else {
                            panic!("Expected NamedType as second generic argument");
                        }
                    } else {
                        panic!("Expected NamedType");
                    }
                } else {
                    panic!("Expected VarDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_instantiation_nested() {
        let source = r#"
            program Test;
            var
                matrix: TList<TList<integer>>;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::VarDecl(var_decl) = &block.var_decls[0] {
                    if let Node::NamedType(named) = var_decl.type_expr.as_ref() {
                        assert_eq!(named.name, "TList");
                        assert_eq!(named.generic_args.len(), 1);
                        // The nested generic arg should be TList<integer>
                        if let Node::NamedType(nested) = named.generic_args[0].as_ref() {
                            assert_eq!(nested.name, "TList");
                            assert_eq!(nested.generic_args.len(), 1);
                            if let Node::NamedType(inner) = nested.generic_args[0].as_ref() {
                                assert_eq!(inner.name, "integer");
                            } else {
                                panic!("Expected NamedType as inner generic argument");
                            }
                        } else {
                            panic!("Expected NamedType as nested generic argument");
                        }
                    } else {
                        panic!("Expected NamedType");
                    }
                } else {
                    panic!("Expected VarDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_procedure() {
        let source = r#"
            program Test;
            procedure Swap<T>(var a, b: T);
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
                    assert_eq!(proc.name, "Swap");
                    assert_eq!(proc.generic_params.len(), 1);
                    assert_eq!(proc.generic_params[0].name, "T");
                    assert_eq!(proc.params.len(), 1);
                } else {
                    panic!("Expected ProcDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_function() {
        let source = r#"
            program Test;
            function Max<T>(a, b: T): T;
            begin
                Max := a;
            end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::FuncDecl(func) = &block.func_decls[0] {
                    assert_eq!(func.name, "Max");
                    assert_eq!(func.generic_params.len(), 1);
                    assert_eq!(func.generic_params[0].name, "T");
                    assert_eq!(func.params.len(), 1);
                    // Return type should be T
                    if let Node::NamedType(return_type) = func.return_type.as_ref() {
                        assert_eq!(return_type.name, "T");
                        assert_eq!(return_type.generic_args.len(), 0);
                    } else {
                        panic!("Expected NamedType as return type");
                    }
                } else {
                    panic!("Expected FuncDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_function_multiple_params() {
        let source = r#"
            program Test;
            function Combine<TKey, TValue>(key: TKey; value: TValue): TDictionary<TKey, TValue>;
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
                if let Node::FuncDecl(func) = &block.func_decls[0] {
                    assert_eq!(func.name, "Combine");
                    assert_eq!(func.generic_params.len(), 2);
                    assert_eq!(func.generic_params[0].name, "TKey");
                    assert_eq!(func.generic_params[1].name, "TValue");
                    // Return type should be TDictionary<TKey, TValue>
                    if let Node::NamedType(return_type) = func.return_type.as_ref() {
                        assert_eq!(return_type.name, "TDictionary");
                        assert_eq!(return_type.generic_args.len(), 2);
                    } else {
                        panic!("Expected NamedType as return type");
                    }
                } else {
                    panic!("Expected FuncDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_in_class() {
        let source = r#"
            program Test;
            type
                TList<T> = class
                    Items: array of T;
                    procedure Add(Item: T);
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
                    assert_eq!(type_decl.name, "TList");
                    assert_eq!(type_decl.generic_params.len(), 1);
                    if let Node::ClassType(class_type) = type_decl.type_expr.as_ref() {
                        // Check that Items field uses T
                        if let Some((_, ast::ClassMember::Field(field_node))) = class_type.members.iter().find(|(_, m)| {
                            if let ast::ClassMember::Field(f) = m {
                                if let Node::VarDecl(var) = f {
                                    var.names.contains(&"Items".to_string())
                                } else {
                                    false
                                }
                            } else {
                                false
                            }
                        }) {
                            if let Node::VarDecl(var) = field_node {
                                if let Node::DynamicArrayType(dyn_arr) = var.type_expr.as_ref() {
                                    if let Node::NamedType(elem_type) = dyn_arr.element_type.as_ref() {
                                        assert_eq!(elem_type.name, "T");
                                    } else {
                                        panic!("Expected NamedType for array element");
                                    }
                                } else {
                                    panic!("Expected DynamicArrayType");
                                }
                            } else {
                                panic!("Expected VarDecl in Field");
                            }
                        } else {
                            panic!("Expected Items field");
                        }
                    } else {
                        panic!("Expected ClassType");
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_with_constraint_multiple() {
        let source = r#"
            program Test;
            type
                TComparable<T: IComparable; U: ICloneable> = class
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
                    assert_eq!(type_decl.name, "TComparable");
                    assert_eq!(type_decl.generic_params.len(), 2);
                    assert_eq!(type_decl.generic_params[0].name, "T");
                    assert!(type_decl.generic_params[0].constraint.is_some());
                    assert_eq!(type_decl.generic_params[1].name, "U");
                    assert!(type_decl.generic_params[1].constraint.is_some());
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_type_parameters_comma_separated() {
        let source = r#"
            program Test;
            type
                TPair<T, U> = class
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
                    assert_eq!(type_decl.name, "TPair");
                    assert_eq!(type_decl.generic_params.len(), 2);
                    assert_eq!(type_decl.generic_params[0].name, "T");
                    assert_eq!(type_decl.generic_params[1].name, "U");
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_non_generic_type_after_generic() {
        let source = r#"
            program Test;
            type
                TList<T> = class
                end;
                TSimple = integer;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                assert_eq!(block.type_decls.len(), 2);
                // First should be generic
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    assert_eq!(type_decl.name, "TList");
                    assert_eq!(type_decl.generic_params.len(), 1);
                }
                // Second should be non-generic
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    assert_eq!(type_decl.name, "TSimple");
                    assert_eq!(type_decl.generic_params.len(), 0);
                }
            }
        }
    }

    #[test]
    fn test_parse_generic_in_parameter_type() {
        let source = r#"
            program Test;
            procedure Process(list: TList<integer>);
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
                    assert_eq!(proc.params.len(), 1);
                    if let Node::NamedType(param_type) = proc.params[0].type_expr.as_ref() {
                        assert_eq!(param_type.name, "TList");
                        assert_eq!(param_type.generic_args.len(), 1);
                    } else {
                        panic!("Expected NamedType for parameter type");
                    }
                } else {
                    panic!("Expected ProcDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_class_helper() {
        let source = r#"
            program Test;
            type
                TStringHelper = class helper for string
                    function Reverse: string;
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
                    assert_eq!(type_decl.name, "TStringHelper");
                    if let Node::HelperType(helper) = type_decl.type_expr.as_ref() {
                        assert_eq!(helper.helper_kind, ast::HelperKind::Class);
                        assert_eq!(helper.members.len(), 1);
                        // Check target type (string is parsed as StringType, not NamedType)
                        match helper.target_type.as_ref() {
                            Node::StringType(_) => {
                                // Correct - string is a built-in type
                            }
                            Node::NamedType(named_type) => {
                                // Also valid if parsed as named type
                                assert_eq!(named_type.name, "string");
                            }
                            _ => {
                                panic!("Expected StringType or NamedType for target type, got: {:?}", helper.target_type);
                            }
                        }
                    } else {
                        panic!("Expected HelperType, got: {:?}", type_decl.type_expr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_record_helper() {
        let source = r#"
            program Test;
            type
                TMyRecord = record
                    x: integer;
                end;
                TMyRecordHelper = record helper for TMyRecord
                    procedure DoSomething;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    assert_eq!(type_decl.name, "TMyRecordHelper");
                    if let Node::HelperType(helper) = type_decl.type_expr.as_ref() {
                        assert_eq!(helper.helper_kind, ast::HelperKind::Record);
                        assert_eq!(helper.members.len(), 1);
                    } else {
                        panic!("Expected HelperType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_helper_with_base() {
        let source = r#"
            program Test;
            type
                TBaseHelper = class helper for integer
                    function ToString: string;
                end;
                TExtendedHelper = class helper (TBaseHelper) for integer
                    function ToHex: string;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    assert_eq!(type_decl.name, "TExtendedHelper");
                    if let Node::HelperType(helper) = type_decl.type_expr.as_ref() {
                        assert_eq!(helper.base_helpers.len(), 1);
                        assert_eq!(helper.base_helpers[0], "TBaseHelper");
                        assert_eq!(helper.members.len(), 1);
                    } else {
                        panic!("Expected HelperType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_class_helper_with_properties() {
        let source = r#"
            program Test;
            type
                TStringHelper = class helper for string
                    property Length: integer read GetLength;
                    function GetLength: integer;
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
                    if let Node::HelperType(helper) = type_decl.type_expr.as_ref() {
                        assert_eq!(helper.members.len(), 2);
                        // Should have both property and method
                        let has_property = helper.members.iter()
                            .any(|(_, member)| matches!(member, ast::ClassMember::Property(_)));
                        let has_method = helper.members.iter()
                            .any(|(_, member)| matches!(member, ast::ClassMember::Method(_)));
                        assert!(has_property, "Expected property in helper");
                        assert!(has_method, "Expected method in helper");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_old_style_object() {
        let source = r#"
            program Test;
            type
                TMyObject = object
                    x: integer;
                    procedure DoSomething;
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
                    assert_eq!(type_decl.name, "TMyObject");
                    if let Node::ObjectType(obj) = type_decl.type_expr.as_ref() {
                        assert!(!obj.is_forward_decl);
                        assert_eq!(obj.members.len(), 2); // field + method
                    } else {
                        panic!("Expected ObjectType, got: {:?}", type_decl.type_expr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_old_style_object_with_inheritance() {
        let source = r#"
            program Test;
            type
                TBaseObject = object
                    x: integer;
                end;
                TDerivedObject = object(TBaseObject)
                    y: integer;
                    procedure DoSomething;
                end;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[1] {
                    assert_eq!(type_decl.name, "TDerivedObject");
                    if let Node::ObjectType(obj) = type_decl.type_expr.as_ref() {
                        assert_eq!(obj.base_objects.len(), 1);
                        assert_eq!(obj.base_objects[0], "TBaseObject");
                        assert_eq!(obj.members.len(), 2); // field + method
                    } else {
                        panic!("Expected ObjectType");
                    }
                }
            }
        }
    }

    #[test]
    fn test_parse_old_style_object_forward_decl() {
        let source = r#"
            program Test;
            type
                TMyObject = object;
            begin
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        assert!(result.is_ok(), "Parse failed: {:?}", result);
        
        if let Ok(Node::Program(program)) = result {
            if let Node::Block(block) = program.block.as_ref() {
                if let Node::TypeDecl(type_decl) = &block.type_decls[0] {
                    match type_decl.type_expr.as_ref() {
                        Node::ObjectType(obj) => {
                            assert!(obj.is_forward_decl);
                            assert_eq!(obj.members.len(), 0);
                        }
                        other => {
                            panic!("Expected ObjectType, got: {:?}", other);
                        }
                    }
                } else {
                    panic!("Expected TypeDecl");
                }
            }
        }
    }

    #[test]
    fn test_parse_old_style_object_with_properties() {
        let source = r#"
            program Test;
            type
                TMyObject = object
                    FValue: integer;
                    property Value: integer read FValue write FValue;
                    procedure SetValue(v: integer);
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
                    if let Node::ObjectType(obj) = type_decl.type_expr.as_ref() {
                        // Should have field, property, and method (3 members)
                        assert!(obj.members.len() >= 2, "Expected at least property and method, got {}", obj.members.len());
                        let has_property = obj.members.iter()
                            .any(|(_, member)| matches!(member, ast::ClassMember::Property(_)));
                        let has_method = obj.members.iter()
                            .any(|(_, member)| matches!(member, ast::ClassMember::Method(_)));
                        assert!(has_property, "Expected property in object");
                        assert!(has_method, "Expected method in object");
                    }
                }
            }
        }
    }
}
