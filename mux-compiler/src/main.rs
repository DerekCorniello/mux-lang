mod source;
mod lexer;
mod parser;

use std::env;
use std::process;
use source::Source;
use lexer::Lexer;
use parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        process::exit(1);
    }
    let file_path = &args[1];
    let mut src = match Source::new(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    };
    let mut lex = Lexer::new(&mut src);
    match lex.lex_all() {
        Ok(tokens) => {
            let mut this_parser = Parser::new(&tokens);
            match this_parser.parse() {
                Ok(nodes) => {
                    println!("Successfully parsed AST:");
                    for node in nodes {
                        println!("  {:?}", node);
                    }
                }
                Err((nodes, errors)) => {
                    eprintln!("\nEncountered {} parsing errors:", errors.len());
                    for err in errors {
                        eprintln!("  {}", err);
                    }
                    if !nodes.is_empty() {
                        println!("\nPartial AST (may be incomplete due to errors):");
                        for node in nodes {
                            println!("  {:?}", node);
                        }
                    }
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            if let Ok(source) = std::fs::read_to_string(file_path) {
                let lines: Vec<&str> = source.lines().collect();
                if e.span.row_start > 0 && e.span.row_start <= lines.len() {
                    let line = lines[e.span.row_start - 1];
                    eprintln!("  --> {}:{}", file_path, e.span.row_start);
                    eprintln!("   |");
                    eprintln!("{:4} | {}", e.span.row_start, line);
                    eprint!("   | ");
                    for _ in 0..(e.span.col_start - 1) {
                        eprint!(" ");
                    }
                    eprintln!("^--- {}", e.message);
                }
            }
            process::exit(1);
        }
    }
}
