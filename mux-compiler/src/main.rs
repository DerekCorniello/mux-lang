mod source;
mod lexer;

use std::env;
use std::process;
use source::Source;
use lexer::Lexer;

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
        Ok(tokens) => print!("Lexer Succeeded:\n\n{:?}", tokens),
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    }
}
