use std::{env, fs, io::{self, Write}};
use interpreter::Interpreter;
use scanner::Scanner;
use parser::Parser;
use value::Value;
use error::AppError;

mod literal;
mod token;
mod scanner;
mod error;
mod ast;
mod parser;
mod interpreter;
mod environment;
mod function;
mod value;

fn run_prompt() -> Result<(), AppError> {
    // REPL loop (Read-Eval-Print-Loop): a simple interactive
    // programming environment that takes input code one line
    // at a time, interprets it, and returns the result to the
    // user. We need to define the program environment ouside
    // of the loop, so that it persists between iterations.
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        // stdout is line-buffered by default, so we need to
        // flush it to make sure the prompt is displayed.
        io::stdout().flush().map_err(|_|
            AppError::Sys("Error flushing stdout".to_string())
        )?;
        
        let mut input = String::new();
        io::stdin().read_line(&mut input).map_err(|_|
            AppError::Sys("Error reading input".to_string())
        )?;
        
        if input.trim() == "exit" {
            break;
        }

        // We don't want to break the loop on an error, only to
        // print it.
        match run(&input, &mut interpreter) {
            Ok(Value::Nil) => (),
            Ok(value) => println!("{}", value),
            Err(e) => eprintln!("{}", e),
        }
    }

    Ok(())
}

fn run_file(path: &str) -> Result<(), AppError> {
    let contents = fs::read_to_string(path).map_err(|_|
        AppError::Sys(format!("Error reading file {}", path))
    )?;

    let mut interpreter = Interpreter::new();

    // When running a file, we don't care about the "result of
    // the last statement", so we just map that to unit.
    run(&contents, &mut interpreter).map(|_| ())
}

fn run(source: &str, interpreter: &mut Interpreter) -> Result<Value, AppError> {
    let mut scan = Scanner::new(source);
    let tokens = scan.scan().map_err(AppError::Compiler)?;
    
    let mut parser = Parser::new(tokens);
    let program = parser.parse().map_err(AppError::Compiler)?;
    
    interpreter.interpret(&program).map_err(|e| AppError::Compiler(e.into()))
}

fn main() {
    let args: Vec<_> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => Err(AppError::Cli(r"
        Invalid number of arguments.
        Usage: trout [script]
        ".to_string())),
    }.unwrap_or_else(|e| eprintln!("{}", e));
}