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
use diagnostic::{ColorConfig, DiagnosticEmitter, FileId, Files, StandardEmitter, ToDiagnostic};
use module_resolver::ModuleResolver;
use source::Source;
use std::cell::RefCell;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command, Stdio};
use std::rc::Rc;

const REQUIRED_LLVM_MAJOR: u32 = 17;

fn emit_diagnostics<E: ToDiagnostic>(files: &Files, file_id: FileId, errors: &[E]) {
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
    /// Check system dependencies for the Mux compiler
    Doctor {
        /// Validate contributor toolchain requirements (LLVM 17)
        #[arg(long)]
        dev: bool,
    },
    /// Print the Mux version
    Version {},
}

fn find_clang_command() -> Option<String> {
    if let Ok(cc) = env::var("CC") {
        let output = Command::new(&cc).arg("--version").output();
        if output.is_ok_and(|o| o.status.success()) {
            return Some(cc);
        }
    }

    let candidates = ["clang", "clang-17"];
    for candidate in candidates {
        let output = match Command::new(candidate).arg("--version").output() {
            Ok(output) => output,
            Err(_) => continue,
        };
        if output.status.success() {
            return Some(candidate.to_string());
        }
    }
    None
}

fn llvm_config_candidates() -> Vec<String> {
    let mut candidates = Vec::new();

    if let Ok(prefix) = env::var("LLVM_SYS_170_PREFIX") {
        let from_prefix = PathBuf::from(prefix).join("bin").join("llvm-config");
        if from_prefix.exists() {
            candidates.push(from_prefix.to_string_lossy().to_string());
        }
    }

    if let Ok(path) = env::var("LLVM_CONFIG") {
        candidates.push(path);
    }

    candidates.push(format!("llvm-config-{}", REQUIRED_LLVM_MAJOR));
    candidates.push("llvm-config".to_string());

    candidates
}

fn collect_llvm_versions() -> Vec<(String, String, u32)> {
    let mut found = Vec::new();
    let mut seen = HashSet::new();

    for candidate in llvm_config_candidates() {
        if !seen.insert(candidate.clone()) {
            continue;
        }

        let output = match Command::new(&candidate).arg("--version").output() {
            Ok(output) => output,
            Err(_) => continue,
        };
        if !output.status.success() {
            continue;
        }

        let raw = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let major_str = raw.split('.').next().unwrap_or("");
        let major = match major_str.parse::<u32>() {
            Ok(major) => major,
            Err(_) => continue,
        };
        found.push((candidate, raw, major));
    }

    found
}

fn pick_llvm_for_dev(versions: &[(String, String, u32)]) -> Option<(String, String, u32)> {
    versions
        .iter()
        .find(|(_, _, major)| *major == REQUIRED_LLVM_MAJOR)
        .map(|(tool, version, major)| (tool.clone(), version.clone(), *major))
}

fn print_llvm_install_help() {
    if cfg!(target_os = "linux") {
        println!("Install LLVM {} and clang:", REQUIRED_LLVM_MAJOR);
        println!(
            "  Ubuntu/Debian: sudo apt-get install llvm-{0}-dev clang-{0} libpolly-{0}-dev",
            REQUIRED_LLVM_MAJOR
        );
        println!(
            "  Arch Linux (AUR, requires yay): yay -S llvm{0} llvm{0}-libs clang{0} lld{0}",
            REQUIRED_LLVM_MAJOR
        );
        println!(
            "Then set: LLVM_SYS_170_PREFIX=/usr/lib/llvm-{}",
            REQUIRED_LLVM_MAJOR
        );
    } else if cfg!(target_os = "macos") {
        println!(
            "Install LLVM {} and clang via Homebrew:",
            REQUIRED_LLVM_MAJOR
        );
        println!("  brew install llvm@{}", REQUIRED_LLVM_MAJOR);
        println!(
            "Then set: LLVM_SYS_170_PREFIX=$(brew --prefix llvm@{})",
            REQUIRED_LLVM_MAJOR
        );
    } else if cfg!(target_family = "windows") {
        println!(
            "Install LLVM {} and clang (for example via Chocolatey):",
            REQUIRED_LLVM_MAJOR
        );
        println!("  choco install llvm --version=17.0.6");
        println!("Then set LLVM_SYS_170_PREFIX to the LLVM install directory.");
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

    eprintln!(
        "MUX_RUNTIME_LIB is set but does not exist: {}",
        path.display()
    );
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

fn runtime_lib_near_executable() -> Option<PathBuf> {
    let exe = env::current_exe().ok()?;
    let exe_dir = exe.parent()?;
    if let Some(parent) =
        find_runtime_lib_in_dir(exe_dir).and_then(|p| p.parent().map(|d| d.to_path_buf()))
    {
        return Some(parent);
    }

    if let Some(parent_dir) = exe_dir.parent() {
        let bundled_dirs = [parent_dir.join("lib"), parent_dir.join("lib").join("mux")];
        for lib_dir in bundled_dirs {
            if !lib_dir.exists() {
                continue;
            }

            if let Some(parent) =
                find_runtime_lib_in_dir(&lib_dir).and_then(|p| p.parent().map(|d| d.to_path_buf()))
            {
                return Some(parent);
            }
        }
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
            return PathBuf::from(home)
                .join("Library")
                .join("Caches")
                .join("mux-lang");
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
    let cache_root = default_cache_root()
        .join("runtime")
        .join(env!("CARGO_PKG_VERSION"));
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
    runtime_lib_from_env()
        .or_else(runtime_lib_near_executable)
        .or_else(runtime_lib_from_build_config)
        .or_else(|| build_runtime_in_cache(profile))
}

fn print_detected_llvm_versions(llvm_versions: &[(String, String, u32)]) {
    if llvm_versions.is_empty() {
        println!("No llvm-config command found on PATH.");
        return;
    }

    println!("Detected llvm-config versions:");
    for (tool, version, major) in llvm_versions {
        println!("  - {} => {} (major {})", tool, version, major);
    }
}

fn validate_llvm_for_doctor(
    dev_mode: bool,
    selected_dev_llvm: Option<(String, String, u32)>,
) -> bool {
    if dev_mode {
        return match selected_dev_llvm {
            Some((tool, version, _)) => {
                println!(
                    "LLVM development requirement satisfied with {} ({}).",
                    version, tool
                );
                true
            }
            None => {
                println!(
                    "Contributor mode requires LLVM {}.x (llvm-config-{}).",
                    REQUIRED_LLVM_MAJOR, REQUIRED_LLVM_MAJOR
                );
                print_llvm_install_help();
                false
            }
        };
    }

    if let Some((tool, version, _)) = selected_dev_llvm {
        println!("Using LLVM {} from {}.", version, tool);
    } else {
        println!(
            "LLVM {} was not detected. This is okay for prebuilt installs, but source builds need LLVM {}.",
            REQUIRED_LLVM_MAJOR, REQUIRED_LLVM_MAJOR
        );
    }

    true
}

fn report_clang_for_doctor(clang: Option<&str>) -> bool {
    if let Some(clang_cmd) = clang {
        println!("Clang is installed: {}.", clang_cmd);
        return true;
    }

    println!("Clang is not installed.");
    print_llvm_install_help();
    false
}

fn ensure_runtime_for_doctor() -> bool {
    let profile = runtime_profile();
    let runtime_ok = if resolve_runtime_lib_dir(profile).is_some() {
        true
    } else {
        println!("Mux runtime not found. Building it now...");
        build_runtime_in_cache(profile).is_some()
    };

    if runtime_ok {
        println!("Mux runtime is available.");
    } else {
        println!("Mux runtime is not available.");
    }

    runtime_ok
}

fn run_doctor(dev_mode: bool) {
    let llvm_versions = collect_llvm_versions();
    let selected_dev_llvm = pick_llvm_for_dev(&llvm_versions);

    print_detected_llvm_versions(&llvm_versions);
    let llvm_ok = validate_llvm_for_doctor(dev_mode, selected_dev_llvm);
    let clang = find_clang_command();
    let clang_ok = report_clang_for_doctor(clang.as_deref());
    let runtime_ok = ensure_runtime_for_doctor();

    if llvm_ok && clang_ok && runtime_ok {
        println!("Your system is ready to use the Mux compiler!");
    } else {
        println!("Please install the missing dependencies and try again.");
        process::exit(1);
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
        Commands::Doctor { dev } => {
            run_doctor(*dev);
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
    };

    if !file_path.to_string_lossy().ends_with(".mux") {
        eprintln!("Error: Input file must have a .mux extension.");
        process::exit(1);
    }

    let mut files = Files::new();

    let source_str = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    };
    let file_id = files.add(file_path, source_str.clone());
    let mut src = Source::from_string(source_str);

    let mut lex = lexer::Lexer::new(&mut src);
    let tokens = match lex.lex_all() {
        Ok(t) => t,
        Err(e) => {
            emit_diagnostics(&files, file_id, &[e]);
            process::exit(1);
        }
    };

    let mut parser = parser::Parser::new(&tokens);
    let nodes = match parser.parse() {
        Ok(n) => n,
        Err((_, errors)) => {
            emit_diagnostics(&files, file_id, &errors);
            process::exit(1);
        }
    };

    let base_path = file_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .to_path_buf();
    let resolver = Rc::new(RefCell::new(ModuleResolver::new(base_path)));

    let mut analyzer = semantics::SemanticAnalyzer::new_with_resolver(resolver);
    let errors = analyzer.analyze(&nodes, Some(&mut files));
    if !errors.is_empty() {
        emit_diagnostics(&files, file_id, &errors);
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

    let clang_cmd = match find_clang_command() {
        Some(cmd) => cmd,
        None => {
            eprintln!("clang is required to link Mux programs but was not found on PATH.");
            print_llvm_install_help();
            process::exit(1);
        }
    };
    let clang_output = Command::new(&clang_cmd)
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
        let status = Command::new(&run_path)
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status();

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
