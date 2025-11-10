mod lexer;
mod parser;
mod semantics;
mod source;
mod codegen;

use lexer::Lexer;
use parser::Parser;
use semantics::{SemanticAnalyzer, SymbolTable};
use source::Source;
use std::env;
use std::process::{self, Command};

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
                    println!("Errors len: {}", errors.len());

                    if errors.is_empty() {
                        println!("✅ Semantic analysis completed successfully!");
                        println!("✅ No semantic errors found.");
                        println!("About to start codegen");

                        // Code generation
                        println!("\n=== Running Code Generation ===");
                        // inkwell::targets::Target::initialize_native(&inkwell::targets::InitializationConfig::default()).unwrap();
                        let context = inkwell::context::Context::create();
                        println!("Context created");
                        let mut codegen = codegen::CodeGenerator::new(&context, analyzer.symbol_table());
                        println!("Codegen created");
                        println!("Creating codegen");
                        if let Err(e) = codegen.generate(&nodes) {
                            println!("Codegen error: {}", e);
                            process::exit(1);
                        }
                         println!("Codegen done");
                         codegen.print_ir();
                         println!("IR printed");

                         // Emit LLVM IR to file
                         let ir_file = format!("{}.ll", file_path.trim_end_matches(".mux"));
                         if let Err(e) = codegen.emit_ir_to_file(&ir_file) {
                             eprintln!("Failed to emit IR: {}", e);
                             process::exit(1);
                         }
                         println!("LLVM IR emitted to {}", ir_file);

                          // Try to compile and link directly with clang
                          let exe_file = file_path.trim_end_matches(".mux");
                          let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap();
                          let project_root = std::path::Path::new(&manifest_dir).parent().unwrap();
                          let lib_path = project_root.join("target/release");
                          let lib_path_str = lib_path.to_str().unwrap();
                          let clang_output = Command::new("clang")
                              .args([
                                  &ir_file,
                                  "-L", lib_path_str,
                                  "-Wl,-rpath,", lib_path_str,
                                  "-lmux_runtime",
                                  "-o", exe_file
                              ])
                              .output();

                         match clang_output {
                             Ok(output) if output.status.success() => {
                                 println!("Executable generated: {}", exe_file);
                             }
                             Ok(output) => {
                                 eprintln!("clang failed: {}", String::from_utf8_lossy(&output.stderr));
                                 eprintln!("Note: LLVM compilation tools may not be installed. IR file generated at: {}", ir_file);
                             }
                             Err(e) => {
                                 eprintln!("Failed to run clang: {}. IR file generated at: {}", e, ir_file);
                             }
                         }
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
