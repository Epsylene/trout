use std::fmt::Display;
use crate::scanner::Cursor;

#[derive(Debug)]
pub enum AppError {
    Compiler(Vec<Error>),
    Sys(String),
    Cli(String),
}

impl AppError {
    pub fn print(&self) {
        match self {
            AppError::Sys(message) => {
                eprintln!("System error: {}", message);
            }
            AppError::Cli(message) => {
                eprintln!("CLI error: {}", message);
            }
            AppError::Compiler(errors) => {
                errors.iter().for_each(|e| eprintln!("{}", e));
            }
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    LexemeOutOfBounds(usize, usize, usize),
    FloatParseError(String),
    IntParseError(String),
    UnexpectedCharacter(char),
    NotValidUTF8,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ErrorKind::LexemeOutOfBounds(start, end, eof) => {
                write!(f, "Lexeme out of bounds: {}-{} (EOF at {})", start, end, eof)
            }
            ErrorKind::FloatParseError(lexeme) => {
                write!(f, "Error parsing float: {}", lexeme)
            }
            ErrorKind::IntParseError(lexeme) => {
                write!(f, "Error parsing int: {}", lexeme)
            }
            ErrorKind::NotValidUTF8 => {
                write!(f, "Character is not valid UTF-8")
            }
            ErrorKind::UnexpectedCharacter(character) => {
                write!(f, "Unexpected character: {}", character)
            }
        }
    }
}

#[derive(Debug)]
pub struct Error {
    line: u32,
    column: u32,
    kind: ErrorKind,
}

impl Error {
    pub fn new(cursor: &Cursor, kind: ErrorKind) -> Self {
        Error {
            line: cursor.line,
            column: cursor.column,
            kind,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.column, self.kind)
    }
}