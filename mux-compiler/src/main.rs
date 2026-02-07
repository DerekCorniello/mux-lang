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
use std::env;
use std::fs;
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
#[command(version)]
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

fn runtime_profile() -> &'static str {
    if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    }
}

fn find_runtime_lib_in_dir(dir: &Path) -> Option<PathBuf> {
    let static_lib = if cfg!(target_family = "windows") {
        dir.join("mux_runtime.lib")
    } else {
        dir.join("libmux_runtime.a")
    };
    if static_lib.exists() {
        return Some(static_lib);
    }

    let dynamic_lib = if cfg!(target_family = "windows") {
        dir.join("mux_runtime.dll")
    } else if cfg!(target_os = "macos") {
        dir.join("libmux_runtime.dylib")
    } else {
        dir.join("libmux_runtime.so")
    };
    if dynamic_lib.exists() {
        return Some(dynamic_lib);
    }

    None
}

fn runtime_lib_from_env() -> Option<PathBuf> {
    let path = env::var("MUX_RUNTIME_LIB").ok()?;
    let path = PathBuf::from(path);
    if path.exists() {
        return path.parent().map(|p| p.to_path_buf());
    }

    eprintln!("MUX_RUNTIME_LIB is set but does not exist: {}", path.display());
    None
}

fn runtime_lib_from_build_config() -> Option<PathBuf> {
    use crate::build_config::{MUX_RUNTIME_DYNAMIC, MUX_RUNTIME_STATIC};

    let static_path = PathBuf::from(MUX_RUNTIME_STATIC);
    if static_path.exists() {
        return static_path.parent().map(|p| p.to_path_buf());
    }

    let dynamic_path = PathBuf::from(MUX_RUNTIME_DYNAMIC);
    if dynamic_path.exists() {
        return dynamic_path.parent().map(|p| p.to_path_buf());
    }

    None
}

fn cargo_home_dir() -> Option<PathBuf> {
    if let Ok(val) = env::var("CARGO_HOME") {
        return Some(PathBuf::from(val));
    }

    if cfg!(target_family = "windows") {
        let user = env::var("USERPROFILE").ok()?;
        return Some(PathBuf::from(user).join(".cargo"));
    }

    let home = env::var("HOME").ok()?;
    Some(PathBuf::from(home).join(".cargo"))
}

fn default_cache_root() -> PathBuf {
    if let Ok(val) = env::var("MUX_RUNTIME_CACHE_DIR") {
        return PathBuf::from(val);
    }

    if cfg!(target_family = "windows") {
        if let Ok(base) = env::var("LOCALAPPDATA") {
            return PathBuf::from(base).join("mux-lang");
        }
        if let Ok(base) = env::var("USERPROFILE") {
            return PathBuf::from(base).join(".mux-lang");
        }
    } else if cfg!(target_os = "macos") {
        if let Ok(home) = env::var("HOME") {
            return PathBuf::from(home).join("Library").join("Caches").join("mux-lang");
        }
    } else {
        if let Ok(base) = env::var("XDG_CACHE_HOME") {
            return PathBuf::from(base).join("mux-lang");
        }
        if let Ok(home) = env::var("HOME") {
            return PathBuf::from(home).join(".cache").join("mux-lang");
        }
    }

    env::temp_dir().join("mux-lang")
}

fn find_runtime_source_dir() -> Option<PathBuf> {
    if let Ok(src) = env::var("MUX_RUNTIME_SRC") {
        let path = PathBuf::from(src);
        if path.join("Cargo.toml").exists() {
            return Some(path);
        }
        eprintln!(
            "MUX_RUNTIME_SRC is set but Cargo.toml was not found: {}",
            path.display()
        );
    }

    let cargo_home = cargo_home_dir()?;
    let registry_src = cargo_home.join("registry").join("src");
    let version = env!("CARGO_PKG_VERSION");
    let dir_name = format!("mux-runtime-{}", version);

    for entry in fs::read_dir(registry_src).ok()? {
        let entry = entry.ok()?;
        let candidate = entry.path().join(&dir_name);
        if candidate.join("Cargo.toml").exists() {
            return Some(candidate);
        }
    }

    None
}

fn build_runtime_in_cache(profile: &str) -> Option<PathBuf> {
    let cache_root = default_cache_root().join("runtime").join(env!("CARGO_PKG_VERSION"));
    let profile_dir = cache_root.join(profile);

    if let Some(lib) = find_runtime_lib_in_dir(&profile_dir) {
        return lib.parent().map(|p| p.to_path_buf());
    }

    let runtime_src = match find_runtime_source_dir() {
        Some(path) => path,
        None => {
            eprintln!("Could not locate mux-runtime source in cargo registry.");
            return None;
        }
    };

    if fs::create_dir_all(&cache_root).is_err() {
        eprintln!(
            "Failed to create runtime cache directory: {}",
            cache_root.display()
        );
        return None;
    }

    eprintln!("Building mux-runtime (first run)...");
    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--manifest-path")
        .arg(runtime_src.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &cache_root);

    if profile == "release" {
        cmd.arg("--release");
    }

    let output = match cmd.output() {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Failed to run cargo: {}", err);
            return None;
        }
    };

    if !output.status.success() {
        eprintln!("Failed to build mux-runtime.");
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        return None;
    }

    if let Some(lib) = find_runtime_lib_in_dir(&profile_dir) {
        return lib.parent().map(|p| p.to_path_buf());
    }

    eprintln!("mux-runtime build completed but library was not found.");
    None
}

fn resolve_runtime_lib_dir(profile: &str) -> Option<PathBuf> {
    if let Some(dir) = runtime_lib_from_env() {
        return Some(dir);
    }

    if let Some(dir) = runtime_lib_from_build_config() {
        return Some(dir);
    }

    build_runtime_in_cache(profile)
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

            let profile = runtime_profile();
            let mut runtime_ready = resolve_runtime_lib_dir(profile).is_some();
            if !runtime_ready {
                println!("Mux runtime not found. Building it now...");
                runtime_ready = build_runtime_in_cache(profile).is_some();
            }

            if runtime_ready {
                println!("Mux runtime is available.");
            } else {
                println!("Mux runtime is not available.");
            }

            if llvm_installed && clang_installed && runtime_ready {
                println!("Your system is ready to use the Mux compiler!");
            } else {
                println!("Please install the missing dependencies and try again.");
                process::exit(1);
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

    let profile = runtime_profile();
    let lib_dir = match resolve_runtime_lib_dir(profile) {
        Some(dir) => dir,
        None => {
            eprintln!();
            eprintln!("Could not locate mux-runtime.");
            eprintln!("You can set MUX_RUNTIME_LIB to a built library path.");
            eprintln!("You can set MUX_RUNTIME_SRC to a local mux-runtime source.");
            eprintln!("Example:");
            eprintln!("  MUX_RUNTIME_LIB=/path/to/libmux_runtime.a mux run file.mux");
            process::exit(1);
        }
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
