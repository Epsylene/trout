enum TokenType {
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

    // End-of-file
    EOF,
}

// Token struct: a token is a group of characters having
// collective meaning. It is a string (the lexeme) identified
// with a token type (separator, keyword, identifier, etc.) in
// a context (the line number). The process of taking an input
// string of characters and converting it into a sequence of
// tokens is called lexical analysis.
struct Token {
    token_type: TokenType,
    lexeme: String,
    line: u32,
}