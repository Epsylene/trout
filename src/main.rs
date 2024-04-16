use std::{env, fs, io, io::Write};
use scanner::Scanner;
use error::AppError;

mod token;
mod scanner;
mod error;
mod ast;
mod parser;

fn run_prompt() -> Result<(), AppError> {
    // REPL loop (Read-Eval-Print-Loop): a simple interactive
    // programming environment that takes input code one line
    // at a time, interprets it, and returns the result to the
    // user.
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

    run(&contents)
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