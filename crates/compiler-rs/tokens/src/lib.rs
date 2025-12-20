//! SuperPascal Token Definitions
//!
//! This crate defines all token types for the SuperPascal compiler.
//! Tokens are the atomic units of the language that the lexer produces.

/// Source code location information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Starting byte offset in source file
    pub start: usize,
    /// Ending byte offset (exclusive)
    pub end: usize,
    /// Line number (1-based)
    pub line: usize,
    /// Column number (1-based)
    pub column: usize,
}

impl Span {
    /// Create a new span
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self {
            start,
            end,
            line,
            column,
        }
    }

    /// Create a zero-length span at a position
    pub fn at(pos: usize, line: usize, column: usize) -> Self {
        Self {
            start: pos,
            end: pos,
            line,
            column,
        }
    }

    /// Merge two spans (from start of first to end of second)
    pub fn merge(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            line: self.line,
            column: self.column,
        }
    }
}

/// Token kinds for SuperPascal
///
/// Based on the lexical structure specification:
/// - Keywords (Tier 1, Tier 2, Tier 3)
/// - Identifiers
/// - Literals (integer, character, string, boolean)
/// - Operators
/// - Delimiters
/// - Directives
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // ===== Keywords (Tier 1: Core) =====
    KwAnd,
    KwArray,
    KwBegin,
    KwBoolean,
    KwByte,
    KwCase,
    KwChar,
    KwConst,
    KwDiv,
    KwDo,
    KwDownto,
    KwElse,
    KwEnd,
    KwFalse,
    KwFor,
    KwFunction,
    KwGoto,
    KwIf,
    KwIn,      // in (set membership operator)
    KwInteger,
    KwMod,
    KwNot,
    KwOf,
    KwOr,
    KwProcedure,
    KwProgram,
    KwRecord,
    KwRepeat,
    KwSet,
    KwString,
    KwStruct,  // SuperPascal extension
    KwThen,
    KwTo,
    KwTrue,
    KwType,
    KwUntil,
    KwVar,
    KwWhile,
    KwWord,

    // ===== Keywords (Tier 2: Units) =====
    KwImplementation,
    KwInterface,
    KwUnit,
    KwUses,
    KwLibrary,
    KwInitialization,
    KwFinalization,
    KwNamespace,  // Future
    KwUsing,      // Future

    // ===== Keywords (Tier 3: Object Pascal) =====
    KwClass,
    KwConstructor,
    KwDestructor,
    KwOverride,
    KwPrivate,
    KwProtected,
    KwPublic,
    KwPublished,
    KwStrict,
    KwVirtual,
    KwProperty,
    KwRead,
    KwWrite,
    KwIndex,
    KwDefault,
    KwStored,

    // ===== Keywords (Exceptions) =====
    KwExcept,
    KwFinally,
    KwRaise,
    KwTry,
    KwOn,      // ON exception_type DO (exception handler)

    // ===== Keywords (Special) =====
    KwNil,
    KwSelf,
    KwInherited,

    // ===== Identifiers =====
    Identifier(String),

    // ===== Literals =====
    /// Integer literal (decimal or hexadecimal)
    IntegerLiteral {
        value: u16,
        is_hex: bool,
    },
    /// Character literal
    CharLiteral(u8),
    /// String literal
    StringLiteral(String),
    /// Boolean literal
    BooleanLiteral(bool),

    // ===== Operators =====
    // Arithmetic
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    // Comparison
    Equal,     // =
    NotEqual,  // <>
    Less,      // <
    LessEqual, // <=
    Greater,   // >
    GreaterEqual, // >=
    // Logical
    // (and, or, not are keywords)
    // Assignment
    Assign,    // :=
    // Other
    Dot,       // .
    DotDot,    // ..
    Caret,     // ^

    // ===== Delimiters =====
    Semicolon,  // ;
    Comma,      // ,
    Colon,      // :
    LeftParen,  // (
    RightParen, // )
    LeftBracket, // [
    RightBracket, // ]
    LeftBrace,  // {
    RightBrace, // }
    At,         // @

    // ===== Directives =====
    /// Compiler directive: {$...}
    Directive(String),

    // ===== Special =====
    /// End of file
    Eof,
    /// Invalid token (for error recovery)
    Invalid(String),
}

/// A token with source location information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    /// Create a new token
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Check if token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::KwAnd
                | TokenKind::KwArray
                | TokenKind::KwBegin
                | TokenKind::KwBoolean
                | TokenKind::KwByte
                | TokenKind::KwCase
                | TokenKind::KwChar
                | TokenKind::KwConst
                | TokenKind::KwDiv
                | TokenKind::KwDo
                | TokenKind::KwDownto
                | TokenKind::KwElse
                | TokenKind::KwEnd
                | TokenKind::KwFalse
                | TokenKind::KwFor
                | TokenKind::KwFunction
                | TokenKind::KwGoto
                | TokenKind::KwIf
                | TokenKind::KwIn
                | TokenKind::KwInteger
                | TokenKind::KwMod
                | TokenKind::KwNot
                | TokenKind::KwOf
                | TokenKind::KwOr
                | TokenKind::KwProcedure
                | TokenKind::KwProgram
                | TokenKind::KwRecord
                | TokenKind::KwRepeat
                | TokenKind::KwSet
                | TokenKind::KwString
                | TokenKind::KwStruct
                | TokenKind::KwThen
                | TokenKind::KwTo
                | TokenKind::KwTrue
                | TokenKind::KwType
                | TokenKind::KwUntil
                | TokenKind::KwVar
                | TokenKind::KwWhile
                | TokenKind::KwWord
                | TokenKind::KwImplementation
                | TokenKind::KwInterface
                | TokenKind::KwUnit
                | TokenKind::KwUses
                | TokenKind::KwLibrary
                | TokenKind::KwInitialization
                | TokenKind::KwFinalization
                | TokenKind::KwNamespace
                | TokenKind::KwUsing
                | TokenKind::KwClass
                | TokenKind::KwConstructor
                | TokenKind::KwDestructor
                | TokenKind::KwOverride
                | TokenKind::KwPrivate
                | TokenKind::KwProtected
                | TokenKind::KwPublic
                | TokenKind::KwVirtual
                | TokenKind::KwProperty
                | TokenKind::KwRead
                | TokenKind::KwWrite
                | TokenKind::KwIndex
                | TokenKind::KwDefault
                | TokenKind::KwStored
                | TokenKind::KwExcept
                | TokenKind::KwFinally
                | TokenKind::KwRaise
                | TokenKind::KwTry
                | TokenKind::KwOn
                | TokenKind::KwNil
                | TokenKind::KwSelf
                | TokenKind::KwInherited
        )
    }

    /// Check if token is an operator
    pub fn is_operator(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Assign
                | TokenKind::Dot
                | TokenKind::DotDot
                | TokenKind::Caret
        )
    }

    /// Check if token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(
            self.kind,
            TokenKind::IntegerLiteral { .. }
                | TokenKind::CharLiteral(_)
                | TokenKind::StringLiteral(_)
                | TokenKind::BooleanLiteral(_)
        )
    }
}

/// Operator precedence levels (higher = tighter binding)
///
/// Based on Pascal operator precedence rules
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    /// Lowest precedence (assignment, etc.)
    Lowest = 0,
    /// Logical OR
    Or = 1,
    /// Logical AND
    And = 2,
    /// Comparison operators (=, <>, <, <=, >, >=)
    Comparison = 3,
    /// Addition/subtraction (+, -)
    Add = 4,
    /// Multiplication/division (*, /, div, mod)
    Mul = 5,
    /// Unary operators (+, -, not, ^)
    Unary = 6,
    /// Highest precedence (parentheses, function calls)
    Highest = 7,
}

impl TokenKind {
    /// Get operator precedence (if this is an operator)
    ///
    /// Note: Plus and Minus can be unary or binary. This returns their binary precedence.
    /// The parser will determine if they're unary based on context.
    pub fn precedence(&self) -> Option<Precedence> {
        match self {
            // Unary-only operators
            TokenKind::KwNot | TokenKind::Caret => Some(Precedence::Unary),
            // Multiplicative
            TokenKind::Star | TokenKind::Slash | TokenKind::KwDiv | TokenKind::KwMod => {
                Some(Precedence::Mul)
            }
            // Additive (binary - parser handles unary case)
            TokenKind::Plus | TokenKind::Minus => Some(Precedence::Add),
            // Comparison
            TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => Some(Precedence::Comparison),
            // Logical AND
            TokenKind::KwAnd => Some(Precedence::And),
            // Logical OR
            TokenKind::KwOr => Some(Precedence::Or),
            // Assignment
            TokenKind::Assign => Some(Precedence::Lowest),
            _ => None,
        }
    }

    /// Check if this is a binary operator
    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::KwDiv
                | TokenKind::KwMod
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::KwAnd
                | TokenKind::KwOr
        )
    }

    /// Check if this is a unary operator
    pub fn is_unary_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus | TokenKind::Minus | TokenKind::KwNot | TokenKind::Caret
        )
    }
}

/// Keyword lookup table
///
/// Maps keyword strings (case-insensitive) to TokenKind
/// Fast case-insensitive ASCII character comparison
/// 
/// Turbo Pascal-style optimization: compare without allocating.
/// Uses bit manipulation to convert uppercase to lowercase (ASCII only).
#[inline]
fn ascii_to_lower(ch: u8) -> u8 {
    // ASCII: 'A' (65) to 'Z' (90) -> 'a' (97) to 'z' (122)
    // Bit 5 (0x20) is the case bit: set it to convert uppercase to lowercase
    if ch >= b'A' && ch <= b'Z' {
        ch | 0x20
    } else {
        ch
    }
}

/// Fast case-insensitive string comparison (ASCII only)
/// 
/// Turbo Pascal-style: early exit on mismatch, no allocation.
/// Returns true if strings are equal ignoring case.
#[inline]
pub fn eq_ignore_ascii_case(a: &str, b: &str) -> bool {
    if a.len() != b.len() {
        return false;
    }
    a.bytes()
        .zip(b.bytes())
        .all(|(a_ch, b_ch)| ascii_to_lower(a_ch) == ascii_to_lower(b_ch))
}

/// Fast case-insensitive string comparison with early exit
/// 
/// Returns the position of first mismatch, or None if equal.
#[inline]
pub fn compare_ignore_ascii_case(a: &str, b: &str) -> Option<usize> {
    let a_bytes = a.bytes();
    let b_bytes = b.bytes();
    for (i, (a_ch, b_ch)) in a_bytes.zip(b_bytes).enumerate() {
        if ascii_to_lower(a_ch) != ascii_to_lower(b_ch) {
            return Some(i);
        }
    }
    if a.len() != b.len() {
        Some(a.len().min(b.len()))
    } else {
        None
    }
}

pub fn lookup_keyword(s: &str) -> Option<TokenKind> {
    // Fast case-insensitive lookup without allocation
    // Use eq_ignore_ascii_case for comparison
    // Tier 1: Core keywords
    if eq_ignore_ascii_case(s, "and") { return Some(TokenKind::KwAnd); }
    if eq_ignore_ascii_case(s, "array") { return Some(TokenKind::KwArray); }
    if eq_ignore_ascii_case(s, "begin") { return Some(TokenKind::KwBegin); }
    if eq_ignore_ascii_case(s, "boolean") { return Some(TokenKind::KwBoolean); }
    if eq_ignore_ascii_case(s, "byte") { return Some(TokenKind::KwByte); }
    if eq_ignore_ascii_case(s, "case") { return Some(TokenKind::KwCase); }
    if eq_ignore_ascii_case(s, "char") { return Some(TokenKind::KwChar); }
    if eq_ignore_ascii_case(s, "const") { return Some(TokenKind::KwConst); }
    if eq_ignore_ascii_case(s, "div") { return Some(TokenKind::KwDiv); }
    if eq_ignore_ascii_case(s, "do") { return Some(TokenKind::KwDo); }
    if eq_ignore_ascii_case(s, "downto") { return Some(TokenKind::KwDownto); }
    if eq_ignore_ascii_case(s, "else") { return Some(TokenKind::KwElse); }
    if eq_ignore_ascii_case(s, "end") { return Some(TokenKind::KwEnd); }
    if eq_ignore_ascii_case(s, "false") { return Some(TokenKind::KwFalse); }
    if eq_ignore_ascii_case(s, "for") { return Some(TokenKind::KwFor); }
    if eq_ignore_ascii_case(s, "function") { return Some(TokenKind::KwFunction); }
    if eq_ignore_ascii_case(s, "goto") { return Some(TokenKind::KwGoto); }
    if eq_ignore_ascii_case(s, "if") { return Some(TokenKind::KwIf); }
    if eq_ignore_ascii_case(s, "in") { return Some(TokenKind::KwIn); }
    if eq_ignore_ascii_case(s, "integer") { return Some(TokenKind::KwInteger); }
    if eq_ignore_ascii_case(s, "mod") { return Some(TokenKind::KwMod); }
    if eq_ignore_ascii_case(s, "not") { return Some(TokenKind::KwNot); }
    if eq_ignore_ascii_case(s, "of") { return Some(TokenKind::KwOf); }
    if eq_ignore_ascii_case(s, "or") { return Some(TokenKind::KwOr); }
    if eq_ignore_ascii_case(s, "procedure") { return Some(TokenKind::KwProcedure); }
    if eq_ignore_ascii_case(s, "program") { return Some(TokenKind::KwProgram); }
    if eq_ignore_ascii_case(s, "record") { return Some(TokenKind::KwRecord); }
    if eq_ignore_ascii_case(s, "repeat") { return Some(TokenKind::KwRepeat); }
    if eq_ignore_ascii_case(s, "set") { return Some(TokenKind::KwSet); }
    if eq_ignore_ascii_case(s, "string") { return Some(TokenKind::KwString); }
    if eq_ignore_ascii_case(s, "struct") { return Some(TokenKind::KwStruct); }
    if eq_ignore_ascii_case(s, "then") { return Some(TokenKind::KwThen); }
    if eq_ignore_ascii_case(s, "to") { return Some(TokenKind::KwTo); }
    if eq_ignore_ascii_case(s, "true") { return Some(TokenKind::KwTrue); }
    if eq_ignore_ascii_case(s, "type") { return Some(TokenKind::KwType); }
    if eq_ignore_ascii_case(s, "until") { return Some(TokenKind::KwUntil); }
    if eq_ignore_ascii_case(s, "var") { return Some(TokenKind::KwVar); }
    if eq_ignore_ascii_case(s, "while") { return Some(TokenKind::KwWhile); }
    if eq_ignore_ascii_case(s, "word") { return Some(TokenKind::KwWord); }
    // Tier 2: Unit keywords
    if eq_ignore_ascii_case(s, "implementation") { return Some(TokenKind::KwImplementation); }
    if eq_ignore_ascii_case(s, "interface") { return Some(TokenKind::KwInterface); }
    if eq_ignore_ascii_case(s, "unit") { return Some(TokenKind::KwUnit); }
    if eq_ignore_ascii_case(s, "uses") { return Some(TokenKind::KwUses); }
    if eq_ignore_ascii_case(s, "library") { return Some(TokenKind::KwLibrary); }
    if eq_ignore_ascii_case(s, "initialization") { return Some(TokenKind::KwInitialization); }
    if eq_ignore_ascii_case(s, "finalization") { return Some(TokenKind::KwFinalization); }
    if eq_ignore_ascii_case(s, "namespace") { return Some(TokenKind::KwNamespace); }
    if eq_ignore_ascii_case(s, "using") { return Some(TokenKind::KwUsing); }
    // Tier 3: Object Pascal
    if eq_ignore_ascii_case(s, "class") { return Some(TokenKind::KwClass); }
    if eq_ignore_ascii_case(s, "constructor") { return Some(TokenKind::KwConstructor); }
    if eq_ignore_ascii_case(s, "destructor") { return Some(TokenKind::KwDestructor); }
    if eq_ignore_ascii_case(s, "override") { return Some(TokenKind::KwOverride); }
    if eq_ignore_ascii_case(s, "private") { return Some(TokenKind::KwPrivate); }
    if eq_ignore_ascii_case(s, "protected") { return Some(TokenKind::KwProtected); }
    if eq_ignore_ascii_case(s, "public") { return Some(TokenKind::KwPublic); }
    if eq_ignore_ascii_case(s, "published") { return Some(TokenKind::KwPublished); }
    if eq_ignore_ascii_case(s, "strict") { return Some(TokenKind::KwStrict); }
    if eq_ignore_ascii_case(s, "virtual") { return Some(TokenKind::KwVirtual); }
    if eq_ignore_ascii_case(s, "property") { return Some(TokenKind::KwProperty); }
    if eq_ignore_ascii_case(s, "read") { return Some(TokenKind::KwRead); }
    if eq_ignore_ascii_case(s, "write") { return Some(TokenKind::KwWrite); }
    if eq_ignore_ascii_case(s, "index") { return Some(TokenKind::KwIndex); }
    if eq_ignore_ascii_case(s, "default") { return Some(TokenKind::KwDefault); }
    if eq_ignore_ascii_case(s, "stored") { return Some(TokenKind::KwStored); }
    // Exceptions
    if eq_ignore_ascii_case(s, "except") { return Some(TokenKind::KwExcept); }
    if eq_ignore_ascii_case(s, "finally") { return Some(TokenKind::KwFinally); }
    if eq_ignore_ascii_case(s, "raise") { return Some(TokenKind::KwRaise); }
    if eq_ignore_ascii_case(s, "try") { return Some(TokenKind::KwTry); }
    if eq_ignore_ascii_case(s, "on") { return Some(TokenKind::KwOn); }
    // Special
    if eq_ignore_ascii_case(s, "nil") { return Some(TokenKind::KwNil); }
    if eq_ignore_ascii_case(s, "self") { return Some(TokenKind::KwSelf); }
    if eq_ignore_ascii_case(s, "inherited") { return Some(TokenKind::KwInherited); }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_lookup() {
        // Case-insensitive lookup
        assert_eq!(lookup_keyword("if"), Some(TokenKind::KwIf));
        assert_eq!(lookup_keyword("IF"), Some(TokenKind::KwIf));
        assert_eq!(lookup_keyword("If"), Some(TokenKind::KwIf));
        
        // Non-keywords return None
        assert_eq!(lookup_keyword("myvar"), None);
        assert_eq!(lookup_keyword("x"), None);
    }

    #[test]
    fn test_eq_ignore_ascii_case() {
        // Basic equality
        assert!(eq_ignore_ascii_case("hello", "hello"));
        assert!(eq_ignore_ascii_case("HELLO", "hello"));
        assert!(eq_ignore_ascii_case("Hello", "HELLO"));
        assert!(eq_ignore_ascii_case("HeLlO", "hElLo"));
        
        // Inequality
        assert!(!eq_ignore_ascii_case("hello", "world"));
        assert!(!eq_ignore_ascii_case("hello", "hell"));
        assert!(!eq_ignore_ascii_case("hello", "helloo"));
        
        // Empty strings
        assert!(eq_ignore_ascii_case("", ""));
        assert!(!eq_ignore_ascii_case("", "a"));
        
        // Single character
        assert!(eq_ignore_ascii_case("A", "a"));
        assert!(eq_ignore_ascii_case("z", "Z"));
    }

    #[test]
    fn test_compare_ignore_ascii_case() {
        // Equal strings
        assert_eq!(compare_ignore_ascii_case("hello", "hello"), None);
        assert_eq!(compare_ignore_ascii_case("HELLO", "hello"), None);
        assert_eq!(compare_ignore_ascii_case("Hello", "HELLO"), None);
        
        // Different strings
        assert_eq!(compare_ignore_ascii_case("hello", "world"), Some(0));
        assert_eq!(compare_ignore_ascii_case("hello", "hell"), Some(4));
        assert_eq!(compare_ignore_ascii_case("hello", "helloo"), Some(5));
        
        // Different lengths
        assert_eq!(compare_ignore_ascii_case("", "a"), Some(0));
        assert_eq!(compare_ignore_ascii_case("a", ""), Some(0));
    }

    #[test]
    fn test_token_kind_precedence() {
        // Unary operators (not keyword has unary precedence)
        assert_eq!(
            TokenKind::KwNot.precedence(),
            Some(Precedence::Unary)
        );
        
        // Multiplicative operators
        assert_eq!(
            TokenKind::Star.precedence(),
            Some(Precedence::Mul)
        );
        
        // Additive operators (Plus/Minus can be binary, so they return Add)
        // Note: Parser will determine if they're unary or binary based on context
        assert_eq!(
            TokenKind::Plus.precedence(),
            Some(Precedence::Add)  // Binary plus has Add precedence
        );
        
        // Comparison operators
        assert_eq!(
            TokenKind::Equal.precedence(),
            Some(Precedence::Comparison)
        );
        
        // Logical operators
        assert_eq!(
            TokenKind::KwAnd.precedence(),
            Some(Precedence::And)
        );
        
        // Non-operators return None
        assert_eq!(TokenKind::KwIf.precedence(), None);
    }

    #[test]
    fn test_span_merge() {
        let span1 = Span::new(0, 5, 1, 1);
        let span2 = Span::new(10, 15, 1, 11);
        let merged = span1.merge(span2);
        
        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 15);
    }

    #[test]
    fn test_token_checks() {
        let token = Token::new(
            TokenKind::KwIf,
            Span::new(0, 2, 1, 1),
        );
        
        assert!(token.is_keyword());
        assert!(!token.is_operator());
        assert!(!token.is_literal());
        
        let op_token = Token::new(
            TokenKind::Plus,
            Span::new(0, 1, 1, 1),
        );
        
        assert!(!op_token.is_keyword());
        assert!(op_token.is_operator());
        assert!(!op_token.is_literal());
    }
}
