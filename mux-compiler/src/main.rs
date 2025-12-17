mod lexer;
mod parser;
mod semantics;
mod source;
mod codegen;
mod monomorphize;

use lexer::Lexer;
use parser::Parser;
use semantics::{SemanticAnalyzer, SymbolTable};
use source::Source;
use monomorphize::monomorphize;
use std::env;
use std::process;

fn print_symbol_table(symbol_table: &SymbolTable, scope_name: &str) {
    println!("\n=== {} Symbol Table ===", scope_name);
    symbol_table.print();
}

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
            println!("Lexing successful. Tokens:");
            for token in &tokens {
                println!("  {:?}", token);
            }

            let mut this_parser = Parser::new(&tokens);
            match this_parser.parse() {
                Ok(nodes) => {
                    println!("\nParsing successful. AST:");
                    for node in &nodes {
                        println!("  {:?}", node);
                    }

                    // Run semantic analysis
                    println!("\n=== Running Semantic Analysis ===");
                    let mut analyzer = SemanticAnalyzer::new();
                    let errors = analyzer.analyze(&nodes);

                    if errors.is_empty() {
                        println!("✅ Semantic analysis completed successfully!");
                        println!("✅ No semantic errors found.");

                        // TODO: Implement AST traversal and code generation here
                    } else {
                        println!("❌ Found {} semantic errors:", errors.len());
                        for error in errors {
                            println!("  {}", error);
                        }
                    }
                    print_symbol_table(analyzer.symbol_table(), "Global");
                }
                Err((nodes, errors)) => {
                    eprintln!("\nEncountered {} parsing errors:", errors.len());
                    for err in errors {
                        eprintln!("  {}", err);
                    }
                    println!("\nTokens (lexing successful):");
                    for token in &tokens {
                        println!("  {:?}", token);
                    }
                    if !nodes.is_empty() {
                        println!("\nPartial AST (parsing failed):");
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
