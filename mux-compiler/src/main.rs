mod ast;
mod codegen;
mod diagnostic;
mod lexer;
mod module_resolver;
mod parser;
mod semantics;
mod source;

use clap::{Parser as ClapParser, Subcommand};
use diagnostic::{ColorConfig, DiagnosticEmitter, FileId, Files, StandardEmitter};
use lexer::{Lexer, LexerError};
use module_resolver::ModuleResolver;
use parser::{Parser, ParserError};
use semantics::SemanticAnalyzer;
use semantics::SemanticError;
use source::Source;
use std::cell::RefCell;
use std::path::PathBuf;
use std::process::{self, Command};
use std::rc::Rc;

fn handle_lexer_error(files: &Files, file_id: FileId, error: &LexerError) {
    let emitter = StandardEmitter::new(ColorConfig::Auto);
    let diagnostic = error.to_diagnostic(file_id);
    emitter.emit(&diagnostic, files);
}

fn handle_parser_errors(files: &Files, file_id: FileId, errors: &[ParserError]) {
    let emitter = StandardEmitter::new(ColorConfig::Auto);
    let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(file_id)).collect();
    emitter.emit_batch(&diagnostics, files);
}

fn handle_semantic_errors(files: &Files, file_id: FileId, errors: &[SemanticError]) {
    let emitter = StandardEmitter::new(ColorConfig::Auto);
    let diagnostics: Vec<_> = errors.iter().map(|e| e.to_diagnostic(file_id)).collect();
    emitter.emit_batch(&diagnostics, files);
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

    // Create Files registry for diagnostic tracking
    let mut files = Files::new();

    // Read source and register it
    let source_str = match std::fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    };
    let file_id = files.add(file_path, source_str.clone());

    let mut src = match Source::new(
        file_path
            .to_str()
            .expect("file path should be valid Unicode"),
    ) {
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
            handle_lexer_error(&files, file_id, &e);
            process::exit(1);
        }
    };

    let mut parser = Parser::new(&tokens);
    let nodes = match parser.parse() {
        Ok(n) => n,
        Err((_, errors)) => {
            handle_parser_errors(&files, file_id, &errors);
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
        handle_semantic_errors(&files, file_id, &errors);
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
    let exe_file = if let Some(out) = &output {
        out.to_path_buf()
    } else {
        PathBuf::from(file_path.to_string_lossy().trim_end_matches(".mux"))
    };

    let exe_path = std::env::current_exe().expect("current executable path should exist");
    let lib_path = exe_path
        .parent()
        .expect("executable should be in a directory")
        .parent()
        .expect("parent directory should exist (expected target/debug structure)")
        .parent()
        .expect("grandparent directory should exist (expected workspace structure)")
        .join("target")
        .join("debug");
    let lib_path_str = lib_path
        .to_str()
        .expect("library path should be valid Unicode");

    let clang_output = Command::new("clang")
        .args([
            &ir_file,
            "-L",
            lib_path_str,
            "-Wl,-rpath,",
            lib_path_str,
            "-lmux_runtime",
            "-o",
            exe_file
                .to_str()
                .expect("executable path should be valid Unicode"),
        ])
        .output();

    match clang_output {
        Ok(output) if output.status.success() => {
            // Show success message only for build command (not run)
            if !do_run {
                let emitter = StandardEmitter::new(ColorConfig::Auto);
                let file_name = file_path
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or("unknown");
                eprintln!(
                    "\n   {} `{}`\n",
                    emitter.styles.success("Finished building"),
                    file_name
                );
            }
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

    if !intermediate {
        Command::new("rm")
            .arg(&ir_file)
            .status()
            .expect("Failed to remove intermediate IR file");
    }

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
