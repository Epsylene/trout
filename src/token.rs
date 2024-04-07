#[derive(Debug, Clone, Copy)]
pub enum TokenType {
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
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: LiteralType,
    pub line: u32,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, literal: LiteralType, line: u32) -> Self {
        Token {
            token_type,
            lexeme,
            literal,
            line,
        }
    }

    pub fn eof(line: u32) -> Self {
        Token {
            token_type: TokenType::Eof,
            lexeme: '\0'.to_string(),
            literal: LiteralType::Nil,
            line,
        }
    }
}