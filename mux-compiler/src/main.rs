mod ast;
mod build_config;
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
use std::path::{Path, PathBuf};
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
    /// Compile a Mux file without running it
    Build {
        file: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
        #[arg(short, long)]
        intermediate: bool,
    },
    /// Compile and run a Mux file
    Run {
        file: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
        #[arg(short, long)]
        intermediate: bool,
    },
    /// Format a Mux file
    Format { file: PathBuf },
    /// Try running a Mux file (for quick experimentation)
    Try { file: PathBuf },
    /// Check system dependencies for the Mux compiler
    Doctor {},
    /// Print the Mux version
    Version {},
}

fn validate_llvm_installed() -> bool {
    match Command::new("clang").arg("--version").output() {
        Ok(output) => output.status.success(),
        Err(_) => false,
    }
}

fn validate_clang_installed() -> bool {
    match Command::new("clang").arg("--version").output() {
        Ok(output) => output.status.success(),
        Err(_) => false,
    }
}

fn main() {
    let cli = Cli::parse();
    let (file_path, do_run, output, intermediate) = match &cli.command {
        Commands::Version {} => {
            // Print version from Cargo.toml automatically
            println!("mux version {}", env!("CARGO_PKG_VERSION"));
            return;
        }
        Commands::Doctor {} => {
            let llvm_installed = validate_llvm_installed();
            let clang_installed = validate_clang_installed();

            if llvm_installed {
                println!("LLVM is installed.");
            } else {
                println!("LLVM is not installed. Please install LLVM to use the Mux compiler.");
            }

            if clang_installed {
                println!("Clang is installed.");
            } else {
                println!("Clang is not installed. Please install Clang to use the Mux compiler.");
            }

            if llvm_installed && clang_installed {
                println!("Your system is ready to use the Mux compiler!");
            } else {
                println!("Please install the missing dependencies and try again.");
            }
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
    let errors = analyzer.analyze(&nodes, Some(&mut files));
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
    // Use ./ prefix to ensure we run the local executable, not a system command
    // (e.g., "test" would find the shell built-in instead of ./test)
    // Executable goes next to the source file unless -o is specified
    let exe_file = if let Some(out) = &output {
        let out_path = out.to_path_buf();
        if out_path.parent().is_some_and(|p| !p.as_os_str().is_empty()) {
            out_path
        } else {
            PathBuf::from("./").join(out_path)
        }
    } else {
        let source_path = PathBuf::from(file_path.to_string_lossy().trim_end_matches(".mux"));
        let parent = source_path.parent().unwrap_or(Path::new("."));
        let file_stem = source_path
            .file_stem()
            .expect("executable name should be valid Unicode");
        parent.join(file_stem)
    };

    use crate::build_config::{MUX_RUNTIME_DYNAMIC, MUX_RUNTIME_STATIC};

    let static_path = PathBuf::from(MUX_RUNTIME_STATIC);
    let dynamic_path = PathBuf::from(MUX_RUNTIME_DYNAMIC);

    let (lib_dir, _lib_name) = if static_path.exists() {
        (static_path.parent().unwrap(), "mux_runtime")
    } else if dynamic_path.exists() {
        (dynamic_path.parent().unwrap(), "mux_runtime")
    } else {
        eprintln!();
        eprintln!("Please ensure the runtime is built:");
        eprintln!("  cargo build --workspace");
        eprintln!();
        eprintln!("Or set MUX_RUNTIME_LIB environment variable to override:");
        eprintln!("  MUX_RUNTIME_LIB=/path/to/libmux_runtime.a cargo run -- run file.mux");
        process::exit(1);
    };

    let lib_path_str = lib_dir
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
        let run_path = if exe_file.is_absolute() {
            exe_file.clone()
        } else {
            PathBuf::from("./").join(&exe_file)
        };
        let output = Command::new(&run_path).output();

        match output {
            Ok(output) if output.status.success() => {
                // Optional: print stdout on success
                let stdout = String::from_utf8_lossy(&output.stdout);
                print!("{}", stdout);
            }
            Ok(output) => {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);

                if !stdout.is_empty() {
                    eprintln!("stdout:\n{}", stdout);
                }
                if !stderr.is_empty() {
                    eprintln!("stderr:\n{}", stderr);
                }

                eprintln!("Program exited with status: {}", output.status);
                process::exit(1);
            }
            Err(e) => {
                eprintln!("Failed to execute program: {}", e);
                process::exit(1);
            }
        }
    }
}
