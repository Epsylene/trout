use std::fmt::Display;

use crate::token::Location;
use crate::value::Value;

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
    InvalidToken(String, String),
    NotValidUTF8,

    // Parser
    IncorrectPrimary(String),
    ExpectedRightParenGrouping,
    ExpectedSeparator,
    ExpectedIdentifierAssignment,
    ExpectedRightBrace,
    ForExpectedEqual,
    ForExpectedDoubleDot,
    ExpectedIdentifierFn,
    ExpectedLeftParenFn,
    ExpectedIdentifierArg,
    ExpectedRightParenArgList,
    ExpectedFnOrLambda,

    // Resolver
    VariableNotDefined(String),

    // Interpreter
    NotUnaryOperator(String),
    NotBinaryOperator(String),
    NotIntOrFloat,
    NotAddOrConcat,
    VariableNotDeclared(String),
    UndefinedVariable(String),
    NotLogicalOperator(String),
    ForStartStopStepInt,
    NotCallable,
    FunctionArity(usize, usize),
    Return(Value),
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
            ErrorKind::InvalidToken(token, maybe) => {
                write!(f, "Invalid token '{}'; maybe you meant '{}'?", token, maybe)
            }

            // Parser
            ErrorKind::IncorrectPrimary(token) => {
                write!(f, "Could not parse: token '{}' did not match a literal (number, string, true, false, nil) or a grouping", token)
            }
            ErrorKind::ExpectedRightParenGrouping => {
                write!(f, "Expected ')' to close grouping")
            }
            ErrorKind::ExpectedSeparator => {
                write!(f, "Expected separator (semicolon or newline) after statement")
            }
            ErrorKind::ExpectedIdentifierAssignment => {
                write!(f, "Invalid assignment target: expected identifier")
            }
            ErrorKind::ExpectedRightBrace => {
                write!(f, "Expected '}}' to end block")
            }
            ErrorKind::ForExpectedEqual => {
                write!(f, "Expected '=' after 'for' loop variable")
            }
            ErrorKind::ForExpectedDoubleDot => {
                write!(f, "Expected '..' in 'for' loop between start and stop")
            }
            ErrorKind::ExpectedIdentifierFn => {
                write!(f, "Expected identifier after 'fn'")
            }
            ErrorKind::ExpectedLeftParenFn => {
                write!(f, "Expected '(' to open argument list")
            }
            ErrorKind::ExpectedIdentifierArg => {
                write!(f, "Expected identifier in argument list")
            }
            ErrorKind::ExpectedRightParenArgList => {
                write!(f, "Expected ')' to close argument list")
            }
            ErrorKind::ExpectedFnOrLambda => {
                write!(f, "Expected identifier or list of arguments after 'fn'")
            }

            // Resolver
            ErrorKind::VariableNotDefined(name) => {
                write!(f, "Variable '{}' has not been defined yet", name)
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
            ErrorKind::VariableNotDeclared(name) => {
                write!(f, "Variable '{}' has not been declared", name)
            }
            ErrorKind::UndefinedVariable(name) => {
                write!(f, "Variable '{}' has been declared but not initialized", name)
            }
            ErrorKind::NotLogicalOperator(token) => {
                write!(f, "Token '{}' is not a logical operator", token)
            }
            ErrorKind::ForStartStopStepInt => {
                write!(f, "The start, stop, and step values of a 'for' loop must be integers")
            }
            ErrorKind::NotCallable => {
                write!(f, "Value is not callable")
            }
            ErrorKind::FunctionArity(expected, got) => {
                write!(f, "Function expected {} arguments, got {}", expected, got)
            }
            ErrorKind::Return(value) => {
                write!(f, "Return statement outside of function: {:?}", value)
            }
        }
    }
}

#[derive(Debug)]
pub struct Error {
    line: u32,
    column: u32,
    pub kind: ErrorKind,
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

impl<T> From<Error> for Result<T, Error> {
    fn from(error: Error) -> Self {
        Err(error)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}:{}) {}", self.line, self.column, self.kind)
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;