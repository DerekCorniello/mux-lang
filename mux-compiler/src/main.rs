mod codegen;
mod lexer;
mod module_resolver;
mod parser;
mod semantics;
mod source;

use lexer::{Lexer, LexerError, Span};
use module_resolver::ModuleResolver;
use parser::{Parser, ParserError};
use semantics::SemanticAnalyzer;
use semantics::SemanticError;
use source::Source;
use std::cell::RefCell;
use std::env;
use std::path::PathBuf;
use std::process::{self, Command};
use std::rc::Rc;

fn print_error_with_location(
    file_path: &str,
    source: &str,
    message: &str,
    span: Span,
    help: Option<&str>,
) {
    let lines: Vec<&str> = source.lines().collect();
    let row = span.row_start.saturating_sub(1);
    let col = span.col_start.saturating_sub(1);

    eprintln!();
    if row < lines.len() {
        eprintln!("In {}, line {}:", file_path, span.row_start);
        eprintln!("    {}", lines[row].trim_end());

        let indicator = "^".to_string();

        eprint!("    ");
        for _ in 0..col {
            eprint!(" ");
        }
        eprintln!("{}", indicator);

        eprintln!("{}", message);
        if let Some(help_text) = help {
            eprintln!("Help: {}", help_text);
        }
    } else {
        eprintln!("In {}, line {}:", file_path, span.row_start);
        eprintln!("{}", message);
    }
    eprintln!();
}

fn handle_lexer_error(file_path: &str, source: &str, error: &LexerError) {
    print_error_with_location(file_path, source, &error.message, error.span, None);
}

fn handle_parser_errors(file_path: &str, source: &str, errors: &[ParserError]) {
    for error in errors {
        print_error_with_location(file_path, source, &error.message, error.span, None);
    }
}

fn handle_semantic_errors(file_path: &str, source: &str, errors: &[SemanticError]) {
    for error in errors {
        print_error_with_location(file_path, source, &error.message, error.span, None);
    }
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
            let mut this_parser = Parser::new(&tokens);
            match this_parser.parse() {
                Ok(nodes) => {
                    // Create module resolver with base path (directory of the main file)
                    let file_path_buf = PathBuf::from(file_path);
                    let base_path = file_path_buf
                        .parent()
                        .unwrap_or_else(|| std::path::Path::new("."))
                        .to_path_buf();
                    let resolver = Rc::new(RefCell::new(ModuleResolver::new(base_path)));

                    // Create semantic analyzer with resolver
                    let mut analyzer = SemanticAnalyzer::new_with_resolver(resolver);
                    let errors = analyzer.analyze(&nodes);
                    if !errors.is_empty() {
                        if let Ok(source) = std::fs::read_to_string(file_path) {
                            handle_semantic_errors(file_path, &source, &errors);
                        } else {
                            for error in &errors {
                                eprintln!("{}", error);
                            }
                        }
                        process::exit(1);
                    }

                    let context = inkwell::context::Context::create();
                    let mut codegen = codegen::CodeGenerator::new(&context, &mut analyzer);
                    if let Err(e) = codegen.generate(&nodes) {
                        eprintln!("Codegen error: {}", e);
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
                    if let Ok(source) = std::fs::read_to_string(file_path) {
                        handle_parser_errors(file_path, &source, &errors);
                    } else {
                        for error in &errors {
                            eprintln!("{}", error);
                        }
                    }
                    process::exit(1);
                }
            }
        }
        Err(e) => {
            if let Ok(source) = std::fs::read_to_string(file_path) {
                handle_lexer_error(file_path, &source, &e);
            } else {
                eprintln!("Lexer error: {}", e);
            }
            process::exit(1);
        }
    }
}
