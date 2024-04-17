use std::fmt::Display;
use crate::token::Location;

#[derive(Debug)]
pub enum AppError {
    Compiler(Vec<Error>),
    Sys(String),
    Cli(String),
}

impl Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AppError::Sys(message) => {
                write!(f, "System error: {}", message)
            }
            AppError::Cli(message) => {
                write!(f, "CLI error: {}", message)
            }
            AppError::Compiler(errors) => {
                errors.iter().fold(Ok(()), |_, e| writeln!(f, "{}", e))
            }
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    // Scanner
    LexemeOutOfBounds(usize, usize, usize),
    FloatParseError(String),
    IntParseError(String),
    UnexpectedCharacter(char),
    NotValidUTF8,

    // Parser
    UnknownPrimary(String),
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

            ErrorKind::UnknownPrimary(lexeme) => {
                write!(f, "Token {} is not a known primary (number, string, true, false, nil, or a grouping expression)", lexeme)
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
    pub fn new(location: &Location, kind: ErrorKind) -> Self {
        Error {
            line: location.line,
            column: location.column,
            kind,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}:{}) {}", self.line, self.column, self.kind)
    }
}