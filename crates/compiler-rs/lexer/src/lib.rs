//! SuperPascal Lexer
//!
//! This crate implements the lexical analysis (tokenization) phase of the SuperPascal compiler.
//! It converts source code into a stream of tokens.

use tokens::{lookup_keyword, Span, Token, TokenKind};

/// Lexer error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    /// Unterminated string literal
    UnterminatedString { line: usize, column: usize },
    /// Unterminated comment
    UnterminatedComment { line: usize, column: usize },
    /// Invalid character
    InvalidCharacter { ch: char, line: usize, column: usize },
    /// Invalid escape sequence
    InvalidEscape { seq: String, line: usize, column: usize },
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnterminatedString { line, column } => {
                write!(f, "Unterminated string literal at {}:{}", line, column)
            }
            LexerError::UnterminatedComment { line, column } => {
                write!(f, "Unterminated comment at {}:{}", line, column)
            }
            LexerError::InvalidCharacter { ch, line, column } => {
                write!(f, "Invalid character '{}' at {}:{}", ch, line, column)
            }
            LexerError::InvalidEscape { seq, line, column } => {
                write!(f, "Invalid escape sequence '{}' at {}:{}", seq, line, column)
            }
        }
    }
}

impl std::error::Error for LexerError {}

/// Lexer (scanner) for SuperPascal
pub struct Lexer {
    /// Source code
    source: Vec<char>,
    /// Current position (byte offset)
    position: usize,
    /// Current line (1-based)
    line: usize,
    /// Current column (1-based)
    column: usize,
    /// Lookahead buffer (for peek)
    lookahead: Option<Token>,
}

impl Lexer {
    /// Create a new lexer from source code
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            position: 0,
            line: 1,
            column: 1,
            lookahead: None,
        }
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        // Return lookahead if available
        if let Some(token) = self.lookahead.take() {
            return Ok(token);
        }

        self.skip_whitespace();
        
        // Check for directives before comments
        let ch = if !self.is_at_end() { Some(self.current_char()) } else { None };
        if ch == Some('{') && self.peek_char() == Some('$') {
            // Compiler directive: {$...}
            return self.scan_directive_curly();
        } else if ch == Some('(') && self.peek_char() == Some('*') {
            let peek2 = self.peek_char_at(2);
            if peek2 == Some('$') {
                // Compiler directive: (*$...*)
                return self.scan_directive_paren();
            }
        }
        
        self.skip_comments()?;

        // Check for EOF
        if self.is_at_end() {
            return Ok(Token::new(
                TokenKind::Eof,
                Span::at(self.position, self.line, self.column),
            ));
        }

        let start_pos = self.position;
        let start_line = self.line;
        let start_col = self.column;

        let ch = self.current_char();

        let kind = if ch.is_ascii_alphabetic() || ch == '_' {
            self.scan_identifier_or_keyword()
        } else if ch.is_ascii_digit() {
            self.scan_number()?
        } else if ch == '$' && self.peek_char().map_or(false, |c| c.is_ascii_hexdigit()) {
            // Pascal-style hex literal: $FF
            self.scan_hex_dollar()?
        } else if ch == '\'' {
            self.scan_char_or_string()?
        } else if ch == '"' {
            self.scan_string_double()?
        } else if ch == '{' {
            // Comment start - should have been skipped, but handle just in case
            self.skip_comment_curly()?;
            // Skip whitespace after comment
            self.skip_whitespace();
            // Recursively get next token after comment
            return self.next_token();
        } else if ch == '(' && self.peek_char() == Some('*') {
            // Comment start - should have been skipped, but handle just in case
            self.skip_comment_paren()?;
            // Skip whitespace after comment
            self.skip_whitespace();
            // Recursively get next token after comment
            return self.next_token();
        } else {
            self.scan_operator_or_delimiter()?
        };

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, start_line, start_col);

        Ok(Token::new(kind, span))
    }

    /// Peek at the next token without consuming it
    pub fn peek_token(&mut self) -> Result<&Token, LexerError> {
        if self.lookahead.is_none() {
            let token = self.next_token()?;
            self.lookahead = Some(token);
        }
        Ok(self.lookahead.as_ref().unwrap())
    }

    /// Check if we're at the end of source
    fn is_at_end(&self) -> bool {
        self.position >= self.source.len()
    }

    /// Get current character
    fn current_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.position]
        }
    }

    /// Peek at next character without advancing
    fn peek_char(&self) -> Option<char> {
        if self.position + 1 >= self.source.len() {
            None
        } else {
            Some(self.source[self.position + 1])
        }
    }

    /// Peek at character at offset without advancing
    fn peek_char_at(&self, offset: usize) -> Option<char> {
        if self.position + offset >= self.source.len() {
            None
        } else {
            Some(self.source[self.position + offset])
        }
    }

    /// Advance to next character
    fn advance(&mut self) {
        if !self.is_at_end() {
            let ch = self.current_char();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.position += 1;
        }
    }

    /// Skip whitespace
    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skip comments (both { } and (* *) styles)
    fn skip_comments(&mut self) -> Result<(), LexerError> {
        loop {
            if self.current_char() == '{' {
                self.skip_comment_curly()?;
                self.skip_whitespace();
            } else if self.current_char() == '(' && self.peek_char() == Some('*') {
                self.skip_comment_paren()?;
                self.skip_whitespace();
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Skip curly brace comment { ... }
    fn skip_comment_curly(&mut self) -> Result<(), LexerError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip '{'

        while !self.is_at_end() {
            if self.current_char() == '}' {
                self.advance(); // Skip '}'
                return Ok(());
            }
            self.advance();
        }

        Err(LexerError::UnterminatedComment {
            line: start_line,
            column: start_col,
        })
    }

    /// Skip paren-star comment (* ... *)
    fn skip_comment_paren(&mut self) -> Result<(), LexerError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip '('
        self.advance(); // Skip '*'

        while !self.is_at_end() {
            if self.current_char() == '*' && self.peek_char() == Some(')') {
                self.advance(); // Skip '*'
                self.advance(); // Skip ')'
                return Ok(());
            }
            self.advance();
        }

        Err(LexerError::UnterminatedComment {
            line: start_line,
            column: start_col,
        })
    }

    /// Scan compiler directive: {$...}
    fn scan_directive_curly(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip '{'
        self.advance(); // Skip '$'

        let mut directive_content = String::new();
        while !self.is_at_end() {
            if self.current_char() == '}' {
                self.advance(); // Skip '}'
                break;
            }
            directive_content.push(self.current_char());
            self.advance();
        }

        if directive_content.is_empty() {
            return Err(LexerError::InvalidCharacter {
                ch: '}',
                line: start_line,
                column: start_col,
            });
        }

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, start_line, start_col);

        Ok(Token::new(
            TokenKind::Directive(directive_content.trim().to_string()),
            span,
        ))
    }

    /// Scan compiler directive: (*$...*)
    fn scan_directive_paren(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.position;
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip '('
        self.advance(); // Skip '*'
        self.advance(); // Skip '$'

        let mut directive_content = String::new();
        while !self.is_at_end() {
            if self.current_char() == '*' && self.peek_char() == Some(')') {
                self.advance(); // Skip '*'
                self.advance(); // Skip ')'
                break;
            }
            directive_content.push(self.current_char());
            self.advance();
        }

        if directive_content.is_empty() {
            return Err(LexerError::InvalidCharacter {
                ch: '*',
                line: start_line,
                column: start_col,
            });
        }

        let end_pos = self.position;
        let span = Span::new(start_pos, end_pos, start_line, start_col);

        Ok(Token::new(
            TokenKind::Directive(directive_content.trim().to_string()),
            span,
        ))
    }

    /// Scan identifier or keyword
    fn scan_identifier_or_keyword(&mut self) -> TokenKind {
        let start = self.position;
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_ascii_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let text: String = self.source[start..self.position].iter().collect();
        lookup_keyword(&text).unwrap_or(TokenKind::Identifier(text))
    }

    /// Scan number (integer literal, decimal or hex)
    fn scan_number(&mut self) -> Result<TokenKind, LexerError> {
        let start_line = self.line;
        let start_col = self.column;

        // Check for hex prefix
        if self.current_char() == '0' && self.peek_char() == Some('x') {
            // Hexadecimal: 0x...
            self.advance(); // Skip '0'
            self.advance(); // Skip 'x'

            let start = self.position;
            while !self.is_at_end() {
                let ch = self.current_char();
                if ch.is_ascii_hexdigit() {
                    self.advance();
                } else {
                    break;
                }
            }

            if self.position == start {
                return Err(LexerError::InvalidCharacter {
                    ch: 'x',
                    line: start_line,
                    column: start_col,
                });
            }

            let hex_str: String = self.source[start..self.position].iter().collect();
            let value = u16::from_str_radix(&hex_str, 16).unwrap_or(0);
            return Ok(TokenKind::IntegerLiteral {
                value,
                is_hex: true,
            });
        }


        // Decimal number
        let start = self.position;
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        let dec_str: String = self.source[start..self.position].iter().collect();
        let value = dec_str.parse::<u16>().unwrap_or(0);
        Ok(TokenKind::IntegerLiteral {
            value,
            is_hex: false,
        })
    }

    /// Scan Pascal-style hex literal ($FF)
    fn scan_hex_dollar(&mut self) -> Result<TokenKind, LexerError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip '$'

        let start = self.position;
        while !self.is_at_end() {
            let ch = self.current_char();
            if ch.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }

        if self.position == start {
            return Err(LexerError::InvalidCharacter {
                ch: '$',
                line: start_line,
                column: start_col,
            });
        }

        let hex_str: String = self.source[start..self.position].iter().collect();
        let value = u16::from_str_radix(&hex_str, 16).unwrap_or(0);
        Ok(TokenKind::IntegerLiteral {
            value,
            is_hex: true,
        })
    }

    /// Scan character or string literal (single quotes)
    fn scan_char_or_string(&mut self) -> Result<TokenKind, LexerError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip opening quote

        if self.is_at_end() {
            return Err(LexerError::UnterminatedString {
                line: start_line,
                column: start_col,
            });
        }

        // Check if it's a character literal (single char) or string (multiple chars)
        let mut chars = Vec::new();
        let mut is_char = true;

        while !self.is_at_end() {
            let ch = self.current_char();

            if ch == '\'' {
                // Check for escaped quote ('')
                if self.peek_char() == Some('\'') {
                    chars.push('\'');
                    self.advance(); // Skip first quote
                    self.advance(); // Skip second quote
                    is_char = false; // Multiple chars = string
                } else {
                    // Closing quote
                    self.advance(); // Skip closing quote
                    break;
                }
            } else if ch == '\\' {
                // Escape sequence
                self.advance(); // Skip backslash
                let escaped = self.scan_escape_sequence(start_line, start_col)?;
                chars.push(escaped);
                is_char = false; // Escape sequences = string
            } else if ch == '\n' || ch == '\r' {
                return Err(LexerError::UnterminatedString {
                    line: start_line,
                    column: start_col,
                });
            } else {
                chars.push(ch);
                self.advance();
                if chars.len() > 1 {
                    is_char = false;
                }
            }
        }

        if self.is_at_end() && self.source[self.position - 1] != '\'' {
            return Err(LexerError::UnterminatedString {
                line: start_line,
                column: start_col,
            });
        }

        if is_char && chars.len() == 1 {
            Ok(TokenKind::CharLiteral(chars[0] as u8))
        } else {
            Ok(TokenKind::StringLiteral(chars.iter().collect()))
        }
    }

    /// Scan string literal (double quotes)
    fn scan_string_double(&mut self) -> Result<TokenKind, LexerError> {
        let start_line = self.line;
        let start_col = self.column;

        self.advance(); // Skip opening quote

        let mut chars = Vec::new();

        while !self.is_at_end() {
            let ch = self.current_char();

            if ch == '"' {
                // Check for escaped quote ("")
                if self.peek_char() == Some('"') {
                    chars.push('"');
                    self.advance(); // Skip first quote
                    self.advance(); // Skip second quote
                } else {
                    // Closing quote
                    self.advance(); // Skip closing quote
                    break;
                }
            } else if ch == '\\' {
                // Escape sequence
                self.advance(); // Skip backslash
                let escaped = self.scan_escape_sequence(start_line, start_col)?;
                chars.push(escaped);
            } else if ch == '\n' || ch == '\r' {
                return Err(LexerError::UnterminatedString {
                    line: start_line,
                    column: start_col,
                });
            } else {
                chars.push(ch);
                self.advance();
            }
        }

        if self.is_at_end() && self.source[self.position - 1] != '"' {
            return Err(LexerError::UnterminatedString {
                line: start_line,
                column: start_col,
            });
        }

        Ok(TokenKind::StringLiteral(chars.iter().collect()))
    }

    /// Scan escape sequence
    fn scan_escape_sequence(
        &mut self,
        line: usize,
        column: usize,
    ) -> Result<char, LexerError> {
        if self.is_at_end() {
            return Err(LexerError::InvalidEscape {
                seq: "\\".to_string(),
                line,
                column,
            });
        }

        let ch = self.current_char();
        self.advance();

        match ch {
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '\\' => Ok('\\'),
            '\'' => Ok('\''),
            '"' => Ok('"'),
            '0' => Ok('\0'),
            _ => Err(LexerError::InvalidEscape {
                seq: format!("\\{}", ch),
                line,
                column,
            }),
        }
    }

    /// Scan operator or delimiter
    fn scan_operator_or_delimiter(&mut self) -> Result<TokenKind, LexerError> {
        let ch = self.current_char();
        let next = self.peek_char();

        let kind = match (ch, next) {
            // Two-character operators
            (':', Some('=')) => {
                self.advance();
                self.advance();
                TokenKind::Assign
            }
            ('<', Some('>')) => {
                self.advance();
                self.advance();
                TokenKind::NotEqual
            }
            ('<', Some('=')) => {
                self.advance();
                self.advance();
                TokenKind::LessEqual
            }
            ('>', Some('=')) => {
                self.advance();
                self.advance();
                TokenKind::GreaterEqual
            }
            ('.', Some('.')) => {
                self.advance();
                self.advance();
                TokenKind::DotDot
            }
            // Single-character operators/delimiters
            ('+', _) => {
                self.advance();
                TokenKind::Plus
            }
            ('-', _) => {
                self.advance();
                TokenKind::Minus
            }
            ('*', _) => {
                self.advance();
                TokenKind::Star
            }
            ('/', _) => {
                self.advance();
                TokenKind::Slash
            }
            ('=', _) => {
                self.advance();
                TokenKind::Equal
            }
            ('<', _) => {
                self.advance();
                TokenKind::Less
            }
            ('>', _) => {
                self.advance();
                TokenKind::Greater
            }
            ('(', _) => {
                self.advance();
                TokenKind::LeftParen
            }
            (')', _) => {
                self.advance();
                TokenKind::RightParen
            }
            ('[', _) => {
                self.advance();
                TokenKind::LeftBracket
            }
            (']', _) => {
                self.advance();
                TokenKind::RightBracket
            }
            ('.', _) => {
                self.advance();
                TokenKind::Dot
            }
            (',', _) => {
                self.advance();
                TokenKind::Comma
            }
            (';', _) => {
                self.advance();
                TokenKind::Semicolon
            }
            (':', _) => {
                self.advance();
                TokenKind::Colon
            }
            ('^', _) => {
                self.advance();
                TokenKind::Caret
            }
            ('@', _) => {
                self.advance();
                TokenKind::At
            }
            _ => {
                return Err(LexerError::InvalidCharacter {
                    ch,
                    line: self.line,
                    column: self.column,
                });
            }
        };

        Ok(kind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ===== Basic Token Recognition =====

    #[test]
    fn test_keywords_tier1() {
        let mut lexer = Lexer::new("program begin end var const type");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwBegin);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwEnd);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwVar);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwConst);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwType);
    }

    #[test]
    fn test_keywords_tier1_control_flow() {
        let mut lexer = Lexer::new("if then else while do for to downto repeat until case of");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwIf);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwThen);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwElse);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwWhile);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwDo);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwFor);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwTo);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwDownto);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwRepeat);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwUntil);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwCase);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwOf);
    }

    #[test]
    fn test_keywords_tier1_types() {
        let mut lexer = Lexer::new("integer byte word boolean char array record set");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwInteger);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwByte);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwWord);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwBoolean);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwChar);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwArray);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwRecord);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwSet);
    }

    #[test]
    fn test_keywords_tier1_procedures() {
        let mut lexer = Lexer::new("procedure function");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwProcedure);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwFunction);
    }

    #[test]
    fn test_keywords_tier2_units() {
        let mut lexer = Lexer::new("unit uses interface implementation");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwUnit);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwUses);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwInterface);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwImplementation);
    }

    #[test]
    fn test_keywords_tier3_oop() {
        let mut lexer = Lexer::new("class constructor destructor private protected public virtual override");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwClass);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwConstructor);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwDestructor);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwPrivate);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwProtected);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwPublic);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwVirtual);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwOverride);
    }

    #[test]
    fn test_keywords_exceptions() {
        let mut lexer = Lexer::new("try except finally raise");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwTry);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwExcept);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwFinally);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwRaise);
    }

    #[test]
    fn test_keywords_special() {
        let mut lexer = Lexer::new("nil true false");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwNil);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwTrue);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwFalse);
    }

    #[test]
    fn test_keywords_case_insensitive() {
        let mut lexer = Lexer::new("PROGRAM Begin END");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwBegin);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwEnd);
    }

    #[test]
    fn test_identifiers() {
        let mut lexer = Lexer::new("hello world x123");
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "world"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "x123"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_identifiers_with_underscores() {
        let mut lexer = Lexer::new("my_var _private MAX_VALUE");
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "my_var"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "_private"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "MAX_VALUE"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_identifiers_vs_keywords() {
        // Identifiers that look like keywords but aren't
        let mut lexer = Lexer::new("programName beginVar endFunction");
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "programName"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "beginVar"),
            _ => panic!("Expected identifier"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "endFunction"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_integer_literals() {
        let mut lexer = Lexer::new("123 456");
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 123);
                assert!(!is_hex);
            }
            _ => panic!("Expected integer literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 456);
                assert!(!is_hex);
            }
            _ => panic!("Expected integer literal"),
        }
    }

    #[test]
    fn test_integer_literals_edge_cases() {
        let mut lexer = Lexer::new("0 1 65535");
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 0);
                assert!(!is_hex);
            }
            _ => panic!("Expected integer literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 1);
                assert!(!is_hex);
            }
            _ => panic!("Expected integer literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 65535);
                assert!(!is_hex);
            }
            _ => panic!("Expected integer literal"),
        }
    }

    #[test]
    fn test_hex_literals() {
        let mut lexer = Lexer::new("0xFF $FF");
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 255);
                assert!(is_hex);
            }
            _ => panic!("Expected hex literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 255);
                assert!(is_hex);
            }
            _ => panic!("Expected hex literal"),
        }
    }

    #[test]
    fn test_hex_literals_various() {
        let mut lexer = Lexer::new("0x0 0x1234 $ABCD $ff");
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 0);
                assert!(is_hex);
            }
            _ => panic!("Expected hex literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 0x1234);
                assert!(is_hex);
            }
            _ => panic!("Expected hex literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 0xABCD);
                assert!(is_hex);
            }
            _ => panic!("Expected hex literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::IntegerLiteral { value, is_hex } => {
                assert_eq!(value, 0xFF);
                assert!(is_hex);
            }
            _ => panic!("Expected hex literal"),
        }
    }

    #[test]
    fn test_string_literals() {
        let mut lexer = Lexer::new("'hello' \"world\"");
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "world"),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_string_literals_escaped_quotes() {
        let mut lexer = Lexer::new("'It''s a test' \"Say \"\"hello\"\"\"");
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "It's a test"),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Say \"hello\""),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_string_literals_escape_sequences() {
        let mut lexer = Lexer::new("'Line 1\\nLine 2' 'Path: C:\\\\Users' 'Tab\\tHere'");
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Line 1\nLine 2"),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Path: C:\\Users"),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Tab\tHere"),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_string_literals_empty() {
        let mut lexer = Lexer::new("'' \"\"");
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, ""),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, ""),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_string_literals_special_chars() {
        let mut lexer = Lexer::new("'Hello, World!' 'Price: $99.99' 'Email: user@example.com'");
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Hello, World!"),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Price: $99.99"),
            _ => panic!("Expected string literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Email: user@example.com"),
            _ => panic!("Expected string literal"),
        }
    }

    #[test]
    fn test_char_literals() {
        let mut lexer = Lexer::new("'a' 'Z'");
        match lexer.next_token().unwrap().kind {
            TokenKind::CharLiteral(c) => assert_eq!(c, b'a'),
            _ => panic!("Expected char literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::CharLiteral(c) => assert_eq!(c, b'Z'),
            _ => panic!("Expected char literal"),
        }
    }

    #[test]
    fn test_char_literals_special() {
        let mut lexer = Lexer::new("' ' '0' '!'");
        match lexer.next_token().unwrap().kind {
            TokenKind::CharLiteral(c) => assert_eq!(c, b' '),
            _ => panic!("Expected char literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::CharLiteral(c) => assert_eq!(c, b'0'),
            _ => panic!("Expected char literal"),
        }
        match lexer.next_token().unwrap().kind {
            TokenKind::CharLiteral(c) => assert_eq!(c, b'!'),
            _ => panic!("Expected char literal"),
        }
    }

    #[test]
    fn test_operators() {
        let mut lexer = Lexer::new("+ - * / := = <> < <= > >=");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Plus);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Minus);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Star);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Slash);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Assign);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Equal);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::NotEqual);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Less);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::LessEqual);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Greater);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::GreaterEqual);
    }

    #[test]
    fn test_delimiters() {
        let mut lexer = Lexer::new("( ) [ ] . , ; :");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::LeftParen);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::RightParen);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::LeftBracket);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::RightBracket);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Dot);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Comma);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Colon);
    }

    #[test]
    fn test_dot_dot() {
        let mut lexer = Lexer::new("..");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::DotDot);
    }

    #[test]
    fn test_caret() {
        let mut lexer = Lexer::new("^");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Caret);
    }

    #[test]
    fn test_comments_curly() {
        let mut lexer = Lexer::new("{ This is a comment } program");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
    }

    #[test]
    fn test_comments_paren() {
        let mut lexer = Lexer::new("(* This is a comment *) program");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
    }

    #[test]
    fn test_comments_multiline() {
        let mut lexer = Lexer::new("{ This is a\nmulti-line\ncomment } program");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
    }

    #[test]
    fn test_comments_nested_curly() {
        // Note: Pascal doesn't support nested comments - first } closes the comment
        // So "{ Outer { Inner } } program" becomes: comment ends at first }, then " } program" is parsed
        // This should result in an error or unexpected tokens
        let mut lexer = Lexer::new("{ Outer { Inner } } program");
        // The comment ends at the first }, leaving " } program" which will cause an error
        // Let's test that it handles this correctly
        let result = lexer.next_token();
        // Should either succeed with program (if we skip the extra }) or error
        // For now, let's just verify it doesn't crash
        match result {
            Ok(token) => {
                // If it succeeds, should be program
                assert_eq!(token.kind, TokenKind::KwProgram);
            }
            Err(_) => {
                // Error is also acceptable for nested comments
            }
        }
    }

    #[test]
    fn test_comments_multiple() {
        let mut lexer = Lexer::new("{ First } { Second } (* Third *) program");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
    }

    #[test]
    fn test_comments_empty() {
        let mut lexer = Lexer::new("{} (* *) program");
        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
    }

    #[test]
    fn test_simple_program() {
        let source = "program HelloWorld;
begin
  WriteLn('Hello, World!');
end.";
        let mut lexer = Lexer::new(source);

        assert_eq!(
            lexer.next_token().unwrap().kind,
            TokenKind::KwProgram
        );
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "HelloWorld"),
            _ => panic!("Expected identifier"),
        }
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwBegin);
        match lexer.next_token().unwrap().kind {
            TokenKind::Identifier(s) => assert_eq!(s, "WriteLn"),
            _ => panic!("Expected identifier"),
        }
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::LeftParen);
        match lexer.next_token().unwrap().kind {
            TokenKind::StringLiteral(s) => assert_eq!(s, "Hello, World!"),
            _ => panic!("Expected string literal"),
        }
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::RightParen);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Semicolon);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwEnd);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Dot);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Eof);
    }

    // ===== Complex Program Tests =====

    #[test]
    fn test_program_with_variables() {
        let source = "program TestVars;
var
  x, y: integer;
  flag: boolean;
begin
  x := 10;
  y := 20;
  flag := true;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 15);
    }

    #[test]
    fn test_program_with_constants() {
        let source = "program TestConsts;
const
  MAX_SIZE = 100;
  PI = 3.14;
begin
  writeln(MAX_SIZE);
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 10);
    }

    #[test]
    fn test_program_with_procedure() {
        let source = "program TestProc;
procedure DoSomething;
begin
  writeln('Hello');
end;
begin
  DoSomething;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 15);
    }

    #[test]
    fn test_program_with_function() {
        let source = "program TestFunc;
function Add(a, b: integer): integer;
begin
  Add := a + b;
end;
begin
  writeln(Add(5, 3));
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 20);
    }

    #[test]
    fn test_program_with_if_statement() {
        let source = "program TestIf;
var x: integer;
begin
  if x > 0 then
    writeln('Positive')
  else
    writeln('Non-positive');
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 15);
    }

    #[test]
    fn test_program_with_while_loop() {
        let source = "program TestWhile;
var i: integer;
begin
  i := 0;
  while i < 10 do
  begin
    writeln(i);
    i := i + 1;
  end;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 20);
    }

    #[test]
    fn test_program_with_for_loop() {
        let source = "program TestFor;
var i: integer;
begin
  for i := 1 to 10 do
    writeln(i);
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 15);
    }

    #[test]
    fn test_program_with_case_statement() {
        let source = "program TestCase;
var x: integer;
begin
  case x of
    1: writeln('One');
    2: writeln('Two');
    else writeln('Other');
  end;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 20);
    }

    #[test]
    fn test_program_with_array() {
        let source = "program TestArray;
var
  arr: array[1..10] of integer;
  i: integer;
begin
  for i := 1 to 10 do
    arr[i] := i * 2;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 20);
    }

    #[test]
    fn test_program_with_record() {
        let source = "program TestRecord;
type
  Point = record
    x, y: integer;
  end;
var p: Point;
begin
  p.x := 10;
  p.y := 20;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 20);
    }

    #[test]
    fn test_program_with_expressions() {
        let source = "program TestExpr;
var a, b, c: integer;
begin
  a := 5 + 3 * 2;
  b := (10 - 2) div 4;
  c := a * b + 1;
end.";
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            tokens.push(token);
        }
        assert!(tokens.len() > 25);
    }

    // ===== Error Handling Tests =====

    #[test]
    fn test_error_unterminated_string_single() {
        let mut lexer = Lexer::new("'unterminated");
        let result = lexer.next_token();
        assert!(result.is_err());
        match result.unwrap_err() {
            LexerError::UnterminatedString { .. } => {}
            _ => panic!("Expected UnterminatedString error"),
        }
    }

    #[test]
    fn test_error_unterminated_string_double() {
        let mut lexer = Lexer::new("\"unterminated");
        let result = lexer.next_token();
        assert!(result.is_err());
        match result.unwrap_err() {
            LexerError::UnterminatedString { .. } => {}
            _ => panic!("Expected UnterminatedString error"),
        }
    }

    #[test]
    fn test_error_unterminated_comment_curly() {
        let mut lexer = Lexer::new("{ unterminated");
        let result = lexer.next_token();
        assert!(result.is_err());
        match result.unwrap_err() {
            LexerError::UnterminatedComment { .. } => {}
            _ => panic!("Expected UnterminatedComment error"),
        }
    }

    #[test]
    fn test_error_unterminated_comment_paren() {
        let mut lexer = Lexer::new("(* unterminated");
        let result = lexer.next_token();
        assert!(result.is_err());
        match result.unwrap_err() {
            LexerError::UnterminatedComment { .. } => {}
            _ => panic!("Expected UnterminatedComment error"),
        }
    }

    #[test]
    fn test_error_invalid_escape() {
        let mut lexer = Lexer::new("'test\\q'");
        let result = lexer.next_token();
        assert!(result.is_err());
        match result.unwrap_err() {
            LexerError::InvalidEscape { .. } => {}
            _ => panic!("Expected InvalidEscape error"),
        }
    }

    #[test]
    fn test_error_invalid_hex_dollar() {
        let mut lexer = Lexer::new("$");
        let result = lexer.next_token();
        assert!(result.is_err());
        match result.unwrap_err() {
            LexerError::InvalidCharacter { ch, .. } => assert_eq!(ch, '$'),
            _ => panic!("Expected InvalidCharacter error"),
        }
    }

    // ===== Edge Cases =====

    #[test]
    fn test_empty_source() {
        let mut lexer = Lexer::new("");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Eof);
    }

    #[test]
    fn test_whitespace_only() {
        let mut lexer = Lexer::new("   \n\t  ");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::Eof);
    }

    #[test]
    fn test_peek_token() {
        let mut lexer = Lexer::new("program begin");
        let peeked = lexer.peek_token().unwrap();
        assert_eq!(peeked.kind, TokenKind::KwProgram);
        
        // Peek again should return same token
        let peeked2 = lexer.peek_token().unwrap();
        assert_eq!(peeked2.kind, TokenKind::KwProgram);
        
        // Now consume it
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwProgram);
        
        // Next token should be begin
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwBegin);
    }

    #[test]
    fn test_line_column_tracking() {
        let source = "program Test;\nvar x: integer;\nbegin\n  x := 1;\nend.";
        let mut lexer = Lexer::new(source);
        
        let token = lexer.next_token().unwrap();
        assert_eq!(token.span.line, 1);
        assert_eq!(token.span.column, 1);
        
        let token = lexer.next_token().unwrap();
        assert_eq!(token.span.line, 1);
        
        let token = lexer.next_token().unwrap();
        assert_eq!(token.span.line, 1);
        
        // After semicolon and newline, should be on line 2
        let _ = lexer.next_token(); // semicolon
        let token = lexer.next_token().unwrap(); // var
        assert_eq!(token.span.line, 2);
    }

    #[test]
    fn test_span_merging() {
        let span1 = Span::new(0, 5, 1, 1);
        let span2 = Span::new(5, 10, 1, 6);
        let merged = span1.merge(span2);
        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 10);
    }

    #[test]
    fn test_boolean_literals() {
        // true and false are keywords, not literals
        let mut lexer = Lexer::new("true false");
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwTrue);
        assert_eq!(lexer.next_token().unwrap().kind, TokenKind::KwFalse);
    }

    #[test]
    fn test_complex_real_world_program() {
        let source = r#"
program Calculator;

{ Simple calculator program }

var
  a, b, result: integer;
  op: char;

function Add(x, y: integer): integer;
begin
  Add := x + y;
end;

procedure PrintResult(value: integer);
begin
  writeln('Result: ', value);
end;

begin
  a := 10;
  b := 5;
  op := '+';
  
  if op = '+' then
    result := Add(a, b)
  else if op = '-' then
    result := a - b
  else if op = '*' then
    result := a * b
  else
    result := a div b;
  
  PrintResult(result);
end.
"#;
        let mut lexer = Lexer::new(source);
        let mut token_count = 0;
        loop {
            let token = lexer.next_token().unwrap();
            if token.kind == TokenKind::Eof {
                break;
            }
            token_count += 1;
        }
        assert!(token_count > 50, "Expected many tokens in complex program");
    }
}
