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
    IncorrectPrimary(String),
    ExpectedRightParen,
    ExpectedSemicolon,
    ExpectedIdentifier,

    // Interpreter
    NotUnaryOperator(String),
    NotBinaryOperator(String),
    NotIntOrFloat,
    NotAddOrConcat,
    NotDeclaredVariable(String),
    UndefinedVariable(String),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            // Scanner
            ErrorKind::LexemeOutOfBounds(start, end, eof) => {
                write!(f, "Lexeme out of bounds: {}-{} (EOF at {})", start, end, eof)
            }
            ErrorKind::FloatParseError(token) => {
                write!(f, "Error parsing float: {}", token)
            }
            ErrorKind::IntParseError(token) => {
                write!(f, "Error parsing int: {}", token)
            }
            ErrorKind::NotValidUTF8 => {
                write!(f, "Character is not valid UTF-8")
            }
            ErrorKind::UnexpectedCharacter(character) => {
                write!(f, "Unexpected character: {}", character)
            }

            // Parser
            ErrorKind::IncorrectPrimary(token) => {
                write!(f, "Could not parse: token '{}' did not match a literal (number, string, true, false, nil) or a grouping", token)
            }
            ErrorKind::ExpectedRightParen => {
                write!(f, "Expected ')' after expression")
            }
            ErrorKind::ExpectedSemicolon => {
                write!(f, "Expected semicolon")
            }
            ErrorKind::ExpectedIdentifier => {
                write!(f, "Expected identifier (word starting with a letter or an underscore)")
            }

            // Interpreter
            ErrorKind::NotUnaryOperator(token) => {
                write!(f, "Token '{}' is not a unary operator", token)
            }
            ErrorKind::NotBinaryOperator(token) => {
                write!(f, "Token '{}' is not a binary operator", token)
            }
            ErrorKind::NotIntOrFloat => {
                write!(f, "Operand(s) must be a number (int or float)")
            }
            ErrorKind::NotAddOrConcat => {
                write!(f, "Operands must be two numbers (int or float) or two strings")
            }
            ErrorKind::NotDeclaredVariable(name) => {
                write!(f, "Variable '{}' has not been declared", name)
            }
            ErrorKind::UndefinedVariable(name) => {
                write!(f, "Variable '{}' has been declared but not initialized", name)
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

impl From<Error> for Vec<Error> {
    fn from(error: Error) -> Self {
        vec![error]
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}:{}) {}", self.line, self.column, self.kind)
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;