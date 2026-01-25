mod codegen;
mod lexer;
mod parser;
mod semantics;
mod source;

use lexer::Lexer;
use parser::Parser;
use semantics::SemanticAnalyzer;
use source::Source;
use std::env;
use std::process::{self, Command};

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
                    let mut analyzer = SemanticAnalyzer::new();
                    let errors = analyzer.analyze(&nodes);
                    if !errors.is_empty() {
                        println!("Errors: {:?}", errors);
                    }

                    let context = inkwell::context::Context::create();
                    let mut codegen = codegen::CodeGenerator::new(&context, &mut analyzer);
                    if let Err(e) = codegen.generate(&nodes, file_path) {
                        println!("Codegen error: {}", e);
                        process::exit(1);
                    }
                    let ir_file = format!("{}.ll", file_path.trim_end_matches(".mux"));
                    if let Err(e) = codegen.emit_ir_to_file(&ir_file) {
                        eprintln!("Failed to emit IR: {}", e);
                        process::exit(1);
                    }

                    // Try to compile and link directly with clang
                    let exe_file = file_path.trim_end_matches(".mux");
                    let exe_path = std::env::current_exe().unwrap();
                    let lib_path = exe_path
                        .parent()
                        .unwrap()
                        .parent()
                        .unwrap()
                        .parent()
                        .unwrap()
                        .join("target")
                        .join("debug");
                    let lib_path_str = lib_path.to_str().unwrap();
                    let clang_output = Command::new("clang")
                        .args([
                            &ir_file,
                            "-L",
                            lib_path_str,
                            "-Wl,-rpath,",
                            lib_path_str,
                            "-lmux_runtime",
                            "-o",
                            exe_file,
                        ])
                        .output();

                    match clang_output {
                        Ok(output) if output.status.success() => {
                            println!("Executable generated: {}", exe_file);
                        }
                        Ok(output) => {
                            eprintln!("clang failed: {}", String::from_utf8_lossy(&output.stderr));
                        }
                        Err(e) => {
                            eprintln!(
                                "Failed to run clang: {}. IR file generated at: {}",
                                e, ir_file
                            );
                        }
                    }
                }
                Err((_, errors)) => {
                    if let Some(first_error) = errors.first() {
                        eprintln!("Parsing failed: {}", first_error.message);
                        // Print error location if available
                        if let Ok(source) = std::fs::read_to_string(file_path) {
                            let lines: Vec<&str> = source.lines().collect();
                            if first_error.span.row_start > 0
                                && first_error.span.row_start <= lines.len()
                            {
                                let line = lines[first_error.span.row_start - 1];
                                eprintln!("  --> {}:{}", file_path, first_error.span.row_start);
                                eprintln!("   |");
                                eprintln!("{:4} | {}", first_error.span.row_start, line);
                                eprint!("   | ");
                                for _ in 0..(first_error.span.col_start.saturating_sub(1)) {
                                    eprint!(" ");
                                }
                                eprintln!("^--- {}", first_error.message);
                            }
                        }
                    } else {
                        eprintln!("Parsing failed: unknown error");
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
