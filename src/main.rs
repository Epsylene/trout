use std::{env, fs, io, io::Write};
use anyhow::{Result, anyhow};

fn run_prompt() -> Result<()> {
    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");
        
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        
        if input.trim().is_empty() {
            break;
        }

        if let Err(e) = run(&input) {
            eprintln!("{}", e);
        }
    }

    Ok(())
}

fn run_file(path: &str) -> Result<()> {
    let contents = fs::read_to_string(path)?;
    run(&contents)?;

    Ok(())
}

fn run(source: &str) -> Result<()> {
    if source.trim() == "error" {
        return Err(anyhow!("Error"));
    }
    else {
        print!("{}", source);
    }

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