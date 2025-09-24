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
                    print!("AST retrieved: {:?}", nodes);
                }
                Err(nodes_with_err) => {
                    print!("Got parsing errors: {:?}", nodes_with_err.1);
                    print!("AST retrieved: {:?}", nodes_with_err.0);
                }
            }
        }
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    }
}
