use std::borrow::Borrow;

use crate::token::*;
use anyhow::{Result, anyhow};

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
pub struct Scanner {
    pub source: Vec<u8>,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.as_bytes().to_vec(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<()> {
        // Starting at the beginning of the source code, we
        // read characters until the EOF is reached.
        while !self.is_eof() {
            // At the start of each iteration, we set the
            // 'start' marker to the current position of the
            // cursor, get the token starting at this position,
            // and push it to the list of tokens.
            self.start = self.current;
            let token = self.next_token()?;
            self.tokens.push(token);
            // By this point, the cursor has been advanced by
            // the number of characters of the token, and we
            // can get the next token during the next iteration
            // from the new position.
        }

        // We add an EOF token at the end of the list as well.
        self.tokens.push(Token::eof(self.line));

        Ok(())
    }

    fn next_token(&mut self) -> Result<Token> {
        // First, we get the character at the current position
        // and advance the cursor by one.
        let c = self.advance();

        // Then we can match the character to get the token
        // kind:
        let kind = match c {
            // - One-character tokens
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            // - Both one- and two-character tokens ("!" and
            // "!=", "=" and "==", etc)
            '!' => self.match_next('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.match_next('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => self.match_next('=', TokenType::LessEqual, TokenType::Less),
            '>' => self.match_next('=', TokenType::GreaterEqual, TokenType::Greater),
            // - Tokens that end with a special character
            //   (comments, strings...)
            '/' => {
                if self.zero() == '/' {
                    self.eat_while(|c| c != '\n');
                    TokenType::LineComment
                } else {
                    TokenType::Slash
                }
            },
            '"' => {
                self.eat_while(|c| c != '"');
                self.advance(); // Consume the closing quote
                TokenType::String
            },
            // - Special tokens: note that although the newline
            //   in Windows is "\r\n", we only need to check
            //   for '\n' and consider '\r' as whitespace.
            ' '|'\r'|'\t' => TokenType::Whitespace,
            '\n' => {
                self.line += 1;
                TokenType::Newline
            },
            // Other more complex cases are handled in the
            // default branch:
            _ => {
                eprintln!("Unexpected character: '{}'", c);
                TokenType::Unknown
            },
        };

        match kind {
            // TokenType::Number => self.get_number_token(),
            TokenType::String => self.get_string_token(),
            // TokenType::Identifier => self.get_identifier_token(),
            _ => self.get_token(kind),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        c as char
    }

    fn match_next(&mut self, expected: char, token_type: TokenType, else_type: TokenType) -> TokenType {
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
        self.source[self.current] as char
    }

    fn first(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.source[self.current + 1] as char
    }

    fn eat_while(&mut self, condition: impl Fn(char) -> bool) {
        // While the condition on the character at the cursor
        // position is met and we are not at EOF, keep reading.
        while condition(self.zero()) && !self.is_eof() {
            self.advance();
        }
    }

    fn is_eof(&self) -> bool {
        // Check if we are beyond the end of the source code.
        self.current >= self.source.len()
    }

    fn get_lexeme(&self, start: usize, end: usize) -> Result<&str> {
        // The lexeme, that is, the string of characters that
        // make up the token, is the slice of the source code
        // between the start and end positions found during
        // tokenisation.
        let lexeme = self.source.get(start..end)
            .ok_or(anyhow!("Error reading lexeme"))?;
        let lexeme = std::str::from_utf8(lexeme)?;

        Ok(lexeme)
    }

    fn get_string_token(&mut self) -> Result<Token> {
        // Trim by one character at each side to get rid of the
        // quotes (which are not part of the string itself,
        // only its delimiters)
        let lexeme = self.get_lexeme(self.start+1, self.current-1)?;
        let literal = LiteralType::String(lexeme.to_string());

        dbg!(lexeme);

        Ok(Token::new(
            TokenType::String,
            lexeme.to_string(),
            literal,
            self.line
        ))
    }

    fn get_token(&mut self, token_type: TokenType) -> Result<Token> {
        // The lexeme is found between the start position
        // (marker) and the current position (pos).
        let lexeme = self.get_lexeme(self.start, self.current)?;
        let literal = LiteralType::Nil;

        dbg!(lexeme);

        Ok(Token::new(
            token_type,
            lexeme.to_string(),
            literal,
            self.line
        ))
    }
}