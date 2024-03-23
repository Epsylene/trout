use std::{env, fs, io, io::Write};
use anyhow::{Result, anyhow};

fn run_prompt() -> Result<()> {
    loop {
        print!("> ");
        let _ = io::stdout().flush();

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        
        if input.trim().is_empty() {
            break;
        }

        print!("{}", input);
    }

    Ok(())
}

fn run_file(path: &str) -> Result<()> {
    let contents = fs::read_to_string(path)?;
    println!("{}", contents);

    Ok(())
}

fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();
    
    println!("A new trout has appearead!");
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => Err(anyhow!(r"
        Invalid number of arguments.
        Usage: trout [script]
        ")),
    }
}