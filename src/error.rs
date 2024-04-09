#[derive(Debug)]
pub enum TroutError {
    Compiler(Vec<Error>),
    Sys(String),
    Cli(String),
}

impl TroutError {
    pub fn print(&self) {
        match self {
            TroutError::Sys(message) => {
                println!("System error: {}", message);
            }
            TroutError::Cli(message) => {
                println!("CLI error: {}", message);
            }
            TroutError::Compiler(errors) => {
                // !todo
            }
        }
    }
}

#[derive(Debug)]
pub struct ErrorKind {
    // !todo
}

#[derive(Debug)]
pub struct Error {
    line: u32,
    column: u32,
    line_string: String,
    message: String,
    kind: ErrorKind,
}

impl Error {
    pub fn new(line: u32, column: u32, line_string: String, message: String, kind: ErrorKind) -> Self {
        Error {
            line,
            column,
            line_string,
            message,
            kind,
        }
    }

    // !todo
}