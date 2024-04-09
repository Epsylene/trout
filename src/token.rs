#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Identifier, String, Number,

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    // Comments
    LineComment, BlockComment,

    // Void characters
    Whitespace, Newline,

    // Special characters
    Eof, Unknown,
}

#[derive(Debug, PartialEq)]
pub enum LiteralType {
    Nil,
    String(String),
    Float(f32),
    Int(u32),
    Bool(bool),
}

// Token struct: a token is a group of characters having
// collective meaning. It is a string (the lexeme) identified
// with a token type (separator, keyword, identifier, etc.) and
// a literal value (float, int, string, nil...) in a context
// (the line number).
#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub literal: LiteralType,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, literal: LiteralType) -> Self {
        Token {
            kind,
            lexeme,
            literal,
        }
    }

    pub fn eof() -> Self {
        Token {
            kind: TokenKind::Eof,
            lexeme: '\0'.to_string(),
            literal: LiteralType::Nil,
        }
    }
}