mod codegen;
mod lexer;
mod module_resolver;
mod parser;
mod semantics;
mod source;

use clap::{Parser as ClapParser, Subcommand};
use lexer::{Lexer, LexerError, Span};
use module_resolver::ModuleResolver;
use parser::{Parser, ParserError};
use semantics::SemanticAnalyzer;
use semantics::SemanticError;
use source::Source;
use std::cell::RefCell;
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

/// Mux compiler CLI
#[derive(ClapParser)]
#[command(name = "mux")]
#[command(version = "0.1.0")]
#[command(about = "CLI tool for Mux Programming Language", long_about = None)]
struct Cli {
    /// Name of the output executable
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Emit intermediate LLVM IR (.ll)
    #[arg(short, long)]
    intermediate: bool,

    /// The command to run
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        file: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
        #[arg(short, long)]
        intermediate: bool,
    },
    Run {
        file: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
        #[arg(short, long)]
        intermediate: bool,
    },
    Format {
        file: PathBuf,
    },
    Try {
        file: PathBuf,
    },
    Version {},
}

fn main() {
    let cli = Cli::parse();
    let (file_path, do_run, output, intermediate) = match &cli.command {
        Commands::Version {} => {
            // Print version from Cargo.toml automatically
            println!("mux version {}", env!("CARGO_PKG_VERSION"));
            return;
        }
        Commands::Build {
            file,
            output,
            intermediate,
        } => (file, false, output, *intermediate),
        Commands::Run {
            file,
            output,
            intermediate,
        } => (file, true, output, *intermediate),
        Commands::Format { file } => {
            println!("Formatting is not yet implemented for {}", file.display());
            return;
        }
        Commands::Try { file } => {
            println!("Trying is not yet implemented for {}", file.display());
            return;
        }
    };

    if !file_path.to_string_lossy().ends_with(".mux") {
        eprintln!("Error: Input file must have a .mux extension.");
        process::exit(1);
    }

    let mut src = match Source::new(file_path.to_str().unwrap()) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    };

    let mut lex = Lexer::new(&mut src);
    let tokens = match lex.lex_all() {
        Ok(t) => t,
        Err(e) => {
            if let Ok(source_str) = std::fs::read_to_string(file_path) {
                handle_lexer_error(file_path.to_str().unwrap(), &source_str, &e);
            } else {
                eprintln!("Lexer error: {}", e);
            }
            process::exit(1);
        }
    };

    let mut parser = Parser::new(&tokens);
    let nodes = match parser.parse() {
        Ok(n) => n,
        Err((_, errors)) => {
            if let Ok(source_str) = std::fs::read_to_string(file_path) {
                handle_parser_errors(file_path.to_str().unwrap(), &source_str, &errors);
            } else {
                for error in &errors {
                    eprintln!("{}", error);
                }
            }
            process::exit(1);
        }
    };

    let base_path = file_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();
    let resolver = Rc::new(RefCell::new(ModuleResolver::new(base_path)));

    let mut analyzer = SemanticAnalyzer::new_with_resolver(resolver);
    let errors = analyzer.analyze(&nodes);
    if !errors.is_empty() {
        if let Ok(source_str) = std::fs::read_to_string(file_path) {
            handle_semantic_errors(file_path.to_str().unwrap(), &source_str, &errors);
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

    let ir_file = format!(
        "{}.ll",
        file_path.to_string_lossy().trim_end_matches(".mux")
    );
    if let Err(e) = codegen.emit_ir_to_file(&ir_file) {
        eprintln!("Failed to emit IR: {}", e);
        process::exit(1);
    }

    // build executable
    let exe_file = if let Some(out) = &cli.output {
        out.to_path_buf()
    } else {
        PathBuf::from(file_path.to_string_lossy().trim_end_matches(".mux"))
    };

    let exe_path = std::env::current_exe().unwrap();
    // todo:
    // clean up the resolution of the library path and anything else needed
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
            exe_file.to_str().unwrap(),
        ])
        .output();

    match clang_output {
        Ok(output) if output.status.success() => { /* Executable built successfully */ }
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

    // remove IR if requested to not keep
    if !cli.intermediate {
        Command::new("rm")
            .arg(&ir_file)
            .status()
            .expect("Failed to remove intermediate IR file");
    }

    // run executable if requested
    if do_run {
        let status = Command::new(&exe_file).status();
        match status {
            Ok(status) if status.success() => {}
            Ok(status) => {
                eprintln!("Program exited with status: {}", status);
                process::exit(1);
            }
            Err(e) => {
                eprintln!("Failed to execute program: {}", e);
                process::exit(1);
            }
        }
    }
}
