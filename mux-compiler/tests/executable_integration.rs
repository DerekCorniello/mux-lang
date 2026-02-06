use insta::assert_snapshot;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn compile_and_execute_file(test_file: &Path) -> (String, String) {
    // Convert to absolute path to avoid issues with working directory
    let abs_path = fs::canonicalize(test_file).unwrap_or_else(|e| {
        panic!(
            "Failed to get absolute path for {}: {}",
            test_file.display(),
            e
        )
    });
    let path_str = abs_path.to_string_lossy();

    // Get the directory containing the test file
    let test_dir = test_file.parent().unwrap_or_else(|| Path::new("."));

    // Check if this is an error case (in error_cases subdirectory)
    let is_error_case = test_file
        .ancestors()
        .any(|p| p.file_name().is_some_and(|n| n == "error_cases"));

    // Compile the file using the mux compiler
    let mut compile_cmd = Command::new("cargo");
    compile_cmd
        .args(["run", "--bin", "mux", "--", "run", &path_str])
        .current_dir("../") // Run from project root
        .env("RUST_BACKTRACE", "1");

    println!("Compiling: {}", path_str);
    let compile_output = compile_cmd
        .output()
        .unwrap_or_else(|e| panic!("Failed to execute compile command for {}: {}", path_str, e));

    let stderr = String::from_utf8_lossy(&compile_output.stderr).to_string();

    if !compile_output.status.success() {
        // For error cases, return stderr as the error output
        if is_error_case {
            return (String::new(), stderr);
        }
        // If compilation fails (non-error-case), return empty output for snapshot
        return (String::from(" "), String::from(" "));
    }

    // Determine the executable name (should be same as file without extension)
    let exec_name = test_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("test_executable");

    let exec_path = test_dir.join(exec_name);

    // Execute the compiled binary
    println!("Executing: {}", exec_path.display());
    let mut exec_cmd = Command::new(&exec_path);
    exec_cmd.current_dir(test_dir); // Run in test directory so relative paths work

    let exec_output = match exec_cmd.output() {
        Ok(output) => output,
        Err(_) => {
            // If execution fails (e.g., binary doesn't exist), return empty output
            return (String::from(" "), String::from(" "));
        }
    };

    let exec_stdout = String::from_utf8_lossy(&exec_output.stdout).to_string();
    let exec_stderr = String::from_utf8_lossy(&exec_output.stderr).to_string();

    // Clean up the executable
    if exec_path.exists() {
        fs::remove_file(&exec_path).unwrap_or_else(|e| {
            eprintln!(
                "Warning: Failed to clean up executable {}: {}",
                exec_path.display(),
                e
            )
        });
    }

    // For error cases, we only care about stderr
    // For regular cases, return both stdout and stderr
    if is_error_case {
        (String::new(), stderr)
    } else {
        (exec_stdout, exec_stderr)
    }
}

#[test]
fn test_executable_all_mux_files_in_dir() {
    let test_dir = "../test_scripts";
    let dir_path = PathBuf::from(&test_dir);

    if !dir_path.exists() {
        panic!("Test scripts directory not found: {}", dir_path.display());
    }

    println!(
        "Scanning directory for executable tests: {}",
        dir_path.display()
    );
    fn collect_mux_files(dir: &Path) -> Vec<PathBuf> {
        let mut files = Vec::new();
        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    files.extend(collect_mux_files(&path));
                } else if path.extension().and_then(|s| s.to_str()) == Some("mux") {
                    files.push(path);
                }
            }
        }
        files
    }

    let mut test_files = collect_mux_files(&dir_path);

    // Sort files for consistent test order
    test_files.sort();

    for path in test_files {
        let file_name = path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown");
        println!("\n=== Testing executable for file: {} ===", file_name);

        match std::panic::catch_unwind(|| {
            println!("Compiling and executing file: {}", path.display());
            let (stdout, stderr) = compile_and_execute_file(&path);

            let snapshot_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown_file");

            // For error cases, we only capture stderr
            // For regular cases, capture stdout
            let output_to_snapshot = if stderr.is_empty() {
                stdout.clone()
            } else {
                stderr.clone()
            };

            println!("Creating executable snapshot for: {}", snapshot_name);
            assert_snapshot!(
                format!("executable_integration__{}", snapshot_name),
                output_to_snapshot
            );
            println!("✓ Successfully processed executable for: {}", file_name);
        }) {
            Ok(_) => {}
            Err(e) => {
                println!(
                    "❌ Error processing executable for file {}: {:?}",
                    file_name, e
                );
                panic!("Executable test failed while processing: {}", file_name);
            }
        }
    }
}
