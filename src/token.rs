use std::hash::{Hash, Hasher};
use crate::literal::LiteralType;

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    Or, And, DotDot,

    // Literals
    Identifier, String, Number,

    // Keywords
    Class, Else, False, Fn, For, If, Nil,
    Return, Super, This, True, Let, While,

    // Comments
    LineComment, BlockComment,

    // Void characters
    Whitespace, Newline,

    // Special characters
    Eof, Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Self {
        Location { line, column }
    }
}

impl From<(u32, u32)> for Location {
    fn from((line, column): (u32, u32)) -> Self {
        Location { line, column }
    }
}

pub trait TokenMatch {
    fn matches(&self, token: &Token) -> bool;
}

impl TokenMatch for TokenKind {
    fn matches(&self, token: &Token) -> bool {
        self == &token.kind
    }
}

impl<const N: usize> TokenMatch for &[TokenKind; N] {
    fn matches(&self, token: &Token) -> bool {
        self.iter().any(|kind| kind.matches(token))
    }
}

// Token struct: a token is a group of characters having
// collective meaning. It is a string (the lexeme) identified
// with a token type (separator, keyword, identifier, etc.) and
// a literal value (float, int, string, nil...) in a context
// (the line number).
#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: LiteralType,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, literal: LiteralType, location: Location) -> Self {
        Token {
            kind,
            lexeme,
            literal,
            location,
        }
    }

    pub fn eof(eof: Location) -> Self {
        Token {
            kind: TokenKind::Eof,
            lexeme: '\0'.to_string(),
            literal: LiteralType::Nil,
            location: eof,
        }
    }
}

impl Eq for Token {}

impl Hash for Token {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
        self.lexeme.hash(state);
    }
}