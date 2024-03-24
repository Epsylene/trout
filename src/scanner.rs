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
struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u32,
}

impl Scanner {
    fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn scan_tokens(&mut self) -> Result<()> {
        // Starting at the beginning of the source code, we
        // iterate over each character until we reach the end
        // of file. For each character, we determine if it is a
        // token and add it to the list of tokens.
        while self.current < self.source.len() {
            self.start = self.current;
            self.scan_token()?;
        }

        // We add an EOF token at the end of the list as well.
        self.tokens.push(Token::eof(self.line));

        Ok(())
    }

    fn scan_token(&mut self) -> Result<()> {
        // For each token, we advance one character at a time
        // and check a match. If we find one, we add the token
        // to the list and move on to the next character.
        let c = self.source
            .chars()
            .nth(self.current)
            .ok_or(anyhow!("No character at {}", self.current))?;
        
        self.current += 1;

        match c {
            '(' => self.add_token(TokenType::LeftParen)?,
            ')' => self.add_token(TokenType::RightParen)?,
            '{' => self.add_token(TokenType::LeftBrace)?,
            '}' => self.add_token(TokenType::RightBrace)?,
            ',' => self.add_token(TokenType::Comma)?,
            '.' => self.add_token(TokenType::Dot)?,
            '-' => self.add_token(TokenType::Minus)?,
            '+' => self.add_token(TokenType::Plus)?,
            ';' => self.add_token(TokenType::Semicolon)?,
            '*' => self.add_token(TokenType::Star)?,
            _ => return Err(anyhow!("Unexpected character")),
        }

        Ok(())
    }

    fn add_token(&mut self, token_type: TokenType) -> Result<()> {
        self.add_token_literal(token_type, LiteralType::Nil)
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: LiteralType) -> Result<()> {
        // Get the lexeme between 'start' and 'current'
        let lexeme = self.source.get(self.start..self.current)
            .ok_or(anyhow!("No lexeme between positions {} and {}", self.start, self.current))?;
        
        // Push the new token to the list
        self.tokens.push(Token::new(
            token_type, 
            lexeme.to_string(),
            literal,
            self.line
        ));

        Ok(())
    }
}