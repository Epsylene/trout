use std::{env, fs, io, io::Write};
use scanner::Scanner;
use error::AppError;

mod token;
mod scanner;
mod error;
mod ast;
mod parser;

fn run_prompt() -> Result<(), AppError> {
    loop {
        print!("> ");
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

        if let Err(e) = run(&input) {
            eprintln!("{}", e);
        }
    }

    Ok(())
}

fn run_file(path: &str) -> Result<(), AppError> {
    let contents = fs::read_to_string(path).map_err(|_|
        AppError::Sys(format!("Error reading file {}", path))
    )?;

    if let Err(e) = run(&contents) {
        eprintln!("{}", e);
    }

    Ok(())
}

fn run(source: &str) -> Result<(), AppError> {
    let mut scan = Scanner::new(source);
    scan.scan_tokens().map_err(AppError::Compiler)?;

    Ok(())
}

fn main() {
    let args: Vec<_> = env::args().collect();

    println!("A new trout has appearead!");
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => Err(AppError::Cli(r"
        Invalid number of arguments.
        Usage: trout [script]
        ".to_string())),
    }.unwrap_or_else(|e| eprintln!("{}", e));
}