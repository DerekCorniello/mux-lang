use insta::assert_snapshot;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn compile_and_execute_file(test_file: &Path) -> (String, String) {
    let abs_path = fs::canonicalize(test_file).unwrap_or_else(|e| {
        panic!(
            "Failed to get absolute path for {}: {}",
            test_file.display(),
            e
        )
    });
    let path_str = abs_path.to_string_lossy();

    // Use absolute path's directory for exec_path (where binary is created)
    let abs_dir = abs_path.parent().unwrap_or_else(|| Path::new("."));

    let exec_name = test_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("test_executable");

    let exec_path = abs_dir.join(exec_name);

    // Clean up any existing binary before compiling
    if exec_path.exists() {
        fs::remove_file(&exec_path).unwrap_or_else(|e| {
            eprintln!(
                "Warning: Failed to clean up old executable {}: {}",
                exec_path.display(),
                e
            )
        });
    }

    // Compile the file using the mux compiler (build, not run)
    let mut compile_cmd = Command::new("cargo");
    compile_cmd
        .args(["run", "--quiet", "--bin", "mux", "--", "build", &path_str])
        .current_dir("../")
        .env("RUST_BACKTRACE", "1");

    println!("Compiling: {}", path_str);
    let compile_output = compile_cmd
        .output()
        .unwrap_or_else(|e| panic!("Failed to execute compile command for {}: {}", path_str, e));

    let compile_stderr = String::from_utf8_lossy(&compile_output.stderr).to_string();

    // Check if binary was created (indicates successful compilation)
    if !exec_path.exists() {
        // Compilation failed - return the error output
        return (String::new(), compile_stderr);
    }

    // Execute the compiled binary
    println!("Executing: {}", exec_path.display());
    let mut exec_cmd = Command::new(&exec_path);
    exec_cmd.current_dir(abs_dir);

    let exec_output = match exec_cmd.output() {
        Ok(output) => output,
        Err(e) => {
            // Clean up before returning
            let _ = fs::remove_file(&exec_path);
            return (String::new(), format!("Failed to execute binary: {}", e));
        }
    };

    let mut exec_stdout = String::from_utf8_lossy(&exec_output.stdout).to_string();
    let exec_stderr = String::from_utf8_lossy(&exec_output.stderr).to_string();

    // If binary exited with non-zero status, append exit status to output
    if !exec_output.status.success() {
        exec_stdout.push_str(&format!(
            "Program exited with status: {}\n",
            exec_output.status
        ));
    }

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

    (exec_stdout, exec_stderr)
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

            // Capture stdout for runtime output, stderr for compile errors
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
