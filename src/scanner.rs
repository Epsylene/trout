use crate::literal::LiteralType;
use crate::token::{Token, TokenKind, Location};
use crate::error::{Error, ErrorKind, Result};

// A source code is a string of characters from which we want
// to extract meaning. The first step in doing so is
// determining how the language groups characters into lexemes
// (that is, keywords, identifiers, literals, operators...),
// what is called its "lexical grammar". The rules of our
// language are such that it can be defined by a regular
// expression, making it a regular language; in practice, this
// means that it can be parsed from left to right with no
// backtracking (it is recognized by a finite automaton). The
// process of taking an input string of characters and
// converting it into a sequence of tokens is called lexical
// analysis.

pub struct Cursor {
    // For iterating: start marks the beginning of the
    // currently parsed token, current is the position of the
    // cursor in the source code, and eof is the maximum
    // position of the cursor.
    start: usize,
    current: usize,
    eof: usize,
    // For error reporting
    pub line: u32,
    pub column: u32,
}

impl Cursor {
    fn new(max: usize) -> Self {
        Cursor {
            start: 0,
            current: 0,
            eof: max,
            line: 1,
            column: 0,
        }
    }

    fn token_start(&self) -> Location {
        Location {
            line: self.line,
            column: self.column_at_start(),
        }
    }

    fn token_end(&self) -> Location {
        Location {
            line: self.line,
            column: self.column + 1,
        }
    }

    fn column_at_start(&self) -> u32 {
        // The column at the start of the token (1-index from
        // the beginning of the line)
        self.column - (self.current - self.start - 1) as u32
    }

    fn reset_start(&mut self) {
        self.start = self.current;
    }

    fn advance(&mut self) {
        self.current += 1;
        self.column += 1;
    }

    fn at_eof(&self) -> bool {
        self.current >= self.eof
    }

    fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

pub struct Scanner {
    pub source: Vec<u8>,
    cursor: Cursor,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        // Push an extra null byte at the end of the source
        // code to avoid having to check for EOF at each
        // character read (the EOF is marked at the original
        // source buffer length)
        let mut chars = source.as_bytes().to_vec();
        chars.push(b'\0');

        Scanner {
            source: chars,
            cursor: Cursor::new(source.len()),
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>, Vec<Error>> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        
        // Starting at the beginning of the source code, we
        // read characters until the EOF is reached.
        while !self.cursor.at_eof() {
            // At the start of each iteration, we set the
            // 'start' marker to the current position of the
            // cursor...
            self.cursor.reset_start();

            // ...get the token starting at this position, and
            // push it to the list of tokens. Errors during
            // lexing are pushed to a vector, so that they are
            // all propagated together at the end. Tokens are
            // options, because we do not want to keep the ones
            // corresponding to whitespace in the code.
            match self.next_token() {
                Ok(Some(token)) => tokens.push(token),
                Err(e) => errors.push(e),
                _ => (),
            }
            
            // By this point, the cursor has been advanced by
            // the number of characters of the token, and we
            // can get the next token during the next iteration
            // from the new position.
        }

        // We add an EOF token at the end of the list as well.
        tokens.push(Token::eof(self.cursor.token_end()));

        match errors.len() {
            0 => Ok(tokens),
            _ => Err(errors),
        }
    }

    fn next_token(&mut self) -> Result<Option<Token>, Error> {
        // First, we get the character at the current position
        // and advance the cursor by one.
        let c = self.advance();

        // Then we can match the character to get the token
        // kind:
        let kind = match c {
            // - One-character tokens
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Star,
            // - Both one- and two-character tokens ("!" and
            // "!=", "=" and "==", etc)
            '!' => self.match_next('=', TokenKind::BangEqual, TokenKind::Bang),
            '=' => self.match_next('=', TokenKind::EqualEqual, TokenKind::Equal),
            '<' => self.match_next('=', TokenKind::LessEqual, TokenKind::Less),
            '>' => self.match_next('=', TokenKind::GreaterEqual, TokenKind::Greater),
            '.' => self.match_next('.', TokenKind::DotDot, TokenKind::Dot),
            '&' => {
                if self.advance() == '&' {
                    TokenKind::And
                } else {
                    return Err(Error::new(
                        &self.cursor.token_start(),
                        ErrorKind::InvalidToken("&".to_string(), "&&".to_string()),
                    ));
                }
            },
            '|' => {
                if self.advance() == '|' {
                    TokenKind::Or
                } else {
                    return Err(Error::new(
                        &self.cursor.token_start(),
                        ErrorKind::InvalidToken("|".to_string(), "||".to_string()),
                    ));
                }
            },
            // - Tokens that end with a special character
            //   (comments, strings...)
            '/' => {
                if self.zero() == '/' {
                    TokenKind::LineComment
                } else {
                    TokenKind::Slash
                }
            },
            '"' => TokenKind::String,
            // - Special tokens: note that although the newline
            //   in Windows is "\r\n", we only need to check
            //   for '\n' and consider '\r' as whitespace.
            ' '|'\r'|'\t' => TokenKind::Whitespace,
            '\n' => {
                self.cursor.newline();
                TokenKind::Newline
            },
            // Other more complex cases are handled in
            // specialized default branches:
            // - If the character is a number digit, then the
            //   token must be a number, either int or float.
            _ if is_num(c) => TokenKind::Number,
            // - If it is a letter (a-Z A-Z) or an underscore,
            //   then it is an identifier (either a reserved
            //   word or a general identifier).
            _ if is_alpha(c) => TokenKind::Identifier,
            // - If it is none of the above, then it is an
            //   unknown character.
            _ => {
                return Err(Error::new(
                    &self.cursor.token_start(),
                    ErrorKind::UnexpectedCharacter(c),
                ));
            },
        };

        // Then we can match the kind to get the token itself.
        let token = match kind {
            TokenKind::Number => self.get_number_token(),
            TokenKind::String => self.get_string_token(),
            TokenKind::Identifier => self.get_identifier_token(),
            TokenKind::LineComment => self.get_comment_token(),
            _ => self.get_token(kind),
        }?;

        // If the token is neither whitespace nor a comment, we
        // add it to the list of tokens.
        if matches!(kind, TokenKind::Whitespace | TokenKind::LineComment) {
            Ok(None)
        } else {
            Ok(Some(token))
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.cursor.current];
        self.cursor.advance();
        c as char
    }

    fn match_next(&mut self, expected: char, token_type: TokenKind, else_type: TokenKind) -> TokenKind {
        // For tokens that can be either one- or two-character
        // long, we need to check the next character to decide
        // (for example, "!" can be followed by "=" to form
        // "!="). Since the cursor is advanced by one at each
        // read character, the "next character" is the one at
        // the current cursor position (zero). If the expected
        // second character is matched, we can advance by one
        // more, or return the one-character token type in the
        // negative case.
        if self.zero() == expected {
            self.advance();
            token_type
        } else {
            else_type
        }
    }

    fn zero(&self) -> char {
        self.source[self.cursor.current] as char
    }

    fn first(&self) -> char {
        if self.cursor.at_eof() {
            return '\0';
        }

        self.source[self.cursor.current + 1] as char
    }

    fn eat_while(&mut self, predicate: impl Fn(char) -> bool) {
        // While the condition on the character at the cursor
        // position is met and we are not at the EOF, keep
        // reading.
        while !self.cursor.at_eof() && predicate(self.zero()) {
            // Check for '\n' while reading, to keep track of
            // the line number.
            if let '\n' = self.advance() {
                self.cursor.newline();
            }
        }
    }

    fn get_current_lexeme(&self) -> Result<String> {
        self.get_lexeme(self.cursor.start, self.cursor.current)
    }

    fn get_lexeme(&self, start: usize, end: usize) -> Result<String> {
        // The lexeme, that is, the string of characters that
        // make up the token, is the slice of the source code
        // between the start and end positions found during
        // tokenisation.
        let lexeme = self.source.get(start..end)
            .ok_or(Error::new(
                &self.cursor.token_start(), 
                ErrorKind::LexemeOutOfBounds(start, end, self.cursor.eof)
            )
        )?;
        
        let lexeme = std::str::from_utf8(lexeme).map_err(|_|
            Error::new(
                &self.cursor.token_start(), 
                ErrorKind::NotValidUTF8
            )
        )?;

        Ok(lexeme.to_string())
    }

    fn get_identifier_token(&mut self) -> Result<Token> {
        // Eat while the character is alphanumeric (letter or
        // number). Here is displayed the principle of "maximal
        // munch": when two lexical grammar rules can both
        // match a chunk of code that the scanner is looking at
        // (for example, the keyword "or" and the identifier
        // "orchid"), whichever one matches the most characters
        // wins. This allows the scanner to parse the code only
        // based on lexical rules, not semantic ones.
        self.eat_while(is_alphanumeric);

        // Get the lexeme and check if it is a keyword.
        let lexeme = self.get_current_lexeme()?;
        let (kind, lit) = match lexeme.as_str() {
            "true" => (TokenKind::True, LiteralType::Bool(true)),
            "false" => (TokenKind::False, LiteralType::Bool(false)),
            _ => (get_keyword(lexeme.as_str()), LiteralType::Nil),
        };

        Ok(Token::new(
            kind,
            lexeme,
            lit,
            self.cursor.token_start(),
        ))
    }

    fn get_number_token(&mut self) -> Result<Token> {
        // First, consume all digits encountered.
        self.eat_while(is_num);

        let lexeme;
        let literal;

        // At this point, we have either reached the end of the
        // number (in which case it is an integer) or a decimal
        // point (in which case it is a float, and we have to
        // keep reading). Note that we want at least one digit
        // after the dot to avoid numbers like "3.", which
        // might conflight the parser if we want to add methods
        // on numbers later.
        if self.zero() == '.' && is_num(self.first()) {
            self.advance(); // Consume the '.'
            self.eat_while(is_num);

            // Then get the lexeme and parse it as a float.
            lexeme = self.get_current_lexeme()?;
            let float = lexeme.parse::<f32>()
                .map_err(|_| Error::new(
                    &self.cursor.token_start(), 
                    ErrorKind::FloatParseError(lexeme.clone())
                )
            )?;

            literal = LiteralType::Float(float);
        } else {
            // Same, but parsing as an integer (specifically,
            // an unsigned 32-bit integer: negative numbers are
            // handled as the combination of a - operator and a
            // number literal).
            lexeme = self.get_current_lexeme()?;
            let int = lexeme.parse::<i32>()
                .map_err(|_| Error::new(
                    &self.cursor.token_start(), 
                    ErrorKind::IntParseError(lexeme.clone())
                )
            )?;

            literal = LiteralType::Int(int);
        }

        Ok(Token::new(
            TokenKind::Number,
            lexeme.to_string(),
            literal,
            self.cursor.token_start(),
        ))
    }

    fn get_string_token(&mut self) -> Result<Token> {
        // Eat until the closing " is reached. Note here that
        // this means that multiline strings are supported.
        self.eat_while(|c| c != '"');
        self.advance(); // Consume the closing quote
        
        // Trim by one character at each side to get rid of the
        // quotes (which are not part of the string itself,
        // only its delimiters)
        let lexeme = self.get_lexeme(self.cursor.start+1, self.cursor.current-1)?;
        let literal = LiteralType::String(lexeme.to_string());

        Ok(Token::new(
            TokenKind::String,
            lexeme.to_string(),
            literal,
            self.cursor.token_start(),
        ))
    }

    fn get_comment_token(&mut self) -> Result<Token> {
        // A line comment goes until the end of the line.
        self.eat_while(|c| c != '\n');
        
        Ok(Token::new(
            TokenKind::LineComment,
            "".to_string(),
            LiteralType::Nil,
            self.cursor.token_start(),
        ))
    }

    fn get_token(&self, token_type: TokenKind) -> Result<Token> {
        // The lexeme is found between the start position
        // (marker) and the current position (pos).
        let lexeme = self.get_current_lexeme()?;

        Ok(Token::new(
            token_type,
            lexeme.to_string(),
            LiteralType::Nil,
            self.cursor.token_start(),
        ))
    }
}

fn is_alpha(c: char) -> bool {
    // "Alphabetic" characters for identifiers are characters
    // in the range a-Z A-Z plus the underscore.
    c.is_ascii_alphabetic() || c == '_'
}

fn is_num(c: char) -> bool {
    // A digit in the range 0-9.
    c.is_ascii_digit()
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || is_num(c)
}

fn get_keyword(keyword: &str) -> TokenKind {
    // This is as fast or faster than a hash map, as it will
    // compile to a jump table.
    match keyword {
        "class" => TokenKind::Class,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "for" => TokenKind::For,
        "while" => TokenKind::While,
        "fn" => TokenKind::Fn,
        "nil" => TokenKind::Nil,
        "return" => TokenKind::Return,
        "super" => TokenKind::Super,
        "this" => TokenKind::This,
        "let" => TokenKind::Let,
        _ => TokenKind::Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokens() {
        let source = "var a = 0";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan().unwrap();

        let expected = vec![
            Token::new(TokenKind::Let, "var".to_string(), LiteralType::Nil, Location::new(1,1)),
            Token::new(TokenKind::Identifier, "a".to_string(), LiteralType::Nil, Location::new(1,5)),
            Token::new(TokenKind::Equal, "=".to_string(), LiteralType::Nil, Location::new(1,7)),
            Token::new(TokenKind::Number, "0".to_string(), LiteralType::Int(0), Location::new(1,9)),
            Token::eof((1, 10).into()),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_numbers() {
        let source = "123 123.456 3.";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan().unwrap();

        let expected = vec![
            Token::new(TokenKind::Number, "123".to_string(), LiteralType::Int(123), Location::new(1,1)),
            Token::new(TokenKind::Number, "123.456".to_string(), LiteralType::Float(123.456), Location::new(1,5)),
            Token::new(TokenKind::Number, "3".to_string(), LiteralType::Int(3), (1, 13).into()),
            Token::new(TokenKind::Dot, ".".to_string(), LiteralType::Nil, (1, 14).into()),
            Token::eof((1, 15).into()),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_strings() {
        let source = "\"Hello, world!\"";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan().unwrap();

        let expected = vec![
            Token::new(TokenKind::String, "Hello, world!".to_string(), LiteralType::String("Hello, world!".to_string()), Location::new(1,1)),
            Token::eof((1, 16).into()),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_comments() {
        let source = "var a = 0 // This is a comment";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan().unwrap();

        let expected = vec![
            Token::new(TokenKind::Let, "var".to_string(), LiteralType::Nil, Location::new(1,1)),
            Token::new(TokenKind::Identifier, "a".to_string(), LiteralType::Nil, Location::new(1,5)),
            Token::new(TokenKind::Equal, "=".to_string(), LiteralType::Nil, Location::new(1,7)),
            Token::new(TokenKind::Number, "0".to_string(), LiteralType::Int(0), Location::new(1,9)),
            Token::eof((1, 31).into()),
        ];

        assert_eq!(tokens, expected);
    }
}