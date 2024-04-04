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
    pub source: Vec<char>,
    pub tokens: Vec<Token>,
    marker: usize,
    pos: usize,
    line: u32,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.chars().collect(),
            tokens: Vec::new(),
            marker: 0,
            pos: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<()> {
        // Starting at the beginning of the source code, we
        // iterate over each character until we reach the end
        // of file. For each character, we determine if it is a
        // token and add it to the list of tokens.
        while self.pos < self.source.len() {
            // For each token, we advance one character at a
            // time and check a match. If we find one, we add
            // the token to the list and move on to the next
            // character.
            self.marker = self.pos;
            let token = self.next_token()?;
            self.tokens.push(token);
        }

        // We add an EOF token at the end of the list as well.
        self.tokens.push(Token::eof(self.line));

        Ok(())
    }

    fn next_token(&mut self) -> Result<Token> {
        let c = self.advance();

        let kind = match c {
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
            '!' => self.match_next('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.match_next('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => self.match_next('=', TokenType::LessEqual, TokenType::Less),
            '>' => self.match_next('=', TokenType::GreaterEqual, TokenType::Greater),
            // '/' => {
            //     if self.peek_next() == '/' {
            //         while self.peek_next() != '\n'  {
            //             self.current += 1;
            //         }

            //         self.get_token(TokenType::LineComment)
            //     } else {
            //         self.get_token(TokenType::Slash)
            //     }
            // },
            // '"' => {
            //     while self.peek_next() != '"' {
            //         self.current += 1;
            //     }

            //     if self.peek_next() == '\n' {
            //         self.line += 1;
            //     }

            //     if self.current >= self.source.len() {
            //         eprintln!("Unterminated string");
            //         self.get_token(TokenType::Unknown)
            //     } else {
            //         self.current += 1;
            //         self.get_literal_token(TokenType::String, LiteralType::String(
            //             self.source.get(self.start+1..self.current-1)
            //                 .ok_or(anyhow!("Error reading string"))?
            //                 .iter().collect()
            //         ))
            //     }
            // },
            ' '|'\r'|'\t' => TokenType::Whitespace,
            '\n' => {
                self.line += 1;
                TokenType::Newline
            },
            _ => {
                eprintln!("Unexpected character: '{}'", c);
                TokenType::Unknown
            },
        };

        match kind {
            // TokenType::Number => self.get_number_token(),
            // TokenType::String => self.get_string_token(),
            // TokenType::Identifier => self.get_identifier_token(),
            _ => self.get_token(kind),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.pos];
        self.pos += 1;
        c
    }

    fn match_next(&mut self, expected: char, token_type: TokenType, else_type: TokenType) -> TokenType {
        if self.zero() == expected {
            self.advance();
            token_type
        } else {
            else_type
        }
    }

    fn zero(&self) -> char {
        self.source[self.pos]
    }

    fn first(&self) -> char {
        if self.pos + 1 >= self.source.len() {
            return '\0';
        }

        self.source[self.pos + 1]
    }

    fn get_token(&mut self, token_type: TokenType) -> Result<Token> {
        self.get_literal_token(token_type, LiteralType::Nil)
    }

    fn get_literal_token(&mut self, token_type: TokenType, literal: LiteralType) -> Result<Token> {
        // Get the lexeme between 'start' and 'current'
        let lexeme = self.source.get(self.marker..self.pos)
            .ok_or(anyhow!("Error reading lexeme"))?;

        dbg!(self.marker, self.pos, lexeme);

        Ok(Token::new(
            token_type,
            lexeme.iter().collect(),
            literal,
            self.line
        ))
    }
}