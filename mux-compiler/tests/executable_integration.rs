use insta::assert_snapshot;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::env;

fn compile_and_execute_file(test_file: &Path) -> (String, String) {
    // Convert to absolute path to avoid issues with working directory
    let abs_path = fs::canonicalize(test_file)
        .unwrap_or_else(|e| panic!("Failed to get absolute path for {}: {}", test_file.display(), e));
    let path_str = abs_path.to_string_lossy();
    
    // Get the directory containing the test file
    let test_dir = test_file.parent().unwrap_or_else(|| Path::new("."));
    
    // Compile the file using the mux_compiler
    let mut compile_cmd = Command::new("cargo");
    compile_cmd
        .args(&["run", "--bin", "mux_compiler", "--", &path_str])
        .current_dir("../") // Run from project root
        .env("RUST_BACKTRACE", "1");
    
    println!("Compiling: {}", path_str);
    let compile_output = compile_cmd.output()
        .unwrap_or_else(|e| panic!("Failed to execute compile command for {}: {}", path_str, e));
    
    if !compile_output.status.success() {
        // If compilation fails, return empty output for snapshot
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
    
    let stdout = String::from_utf8_lossy(&exec_output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&exec_output.stderr).to_string();
    
    // Clean up the executable
    if exec_path.exists() {
        fs::remove_file(&exec_path)
            .unwrap_or_else(|e| eprintln!("Warning: Failed to clean up executable {}: {}", exec_path.display(), e));
    }
    
    (stdout, stderr)
}

#[test]
fn test_executable_all_mux_files_in_dir() {
    let test_dir = "../test_scripts";
    let dir_path = PathBuf::from(&test_dir);

    if !dir_path.exists() {
        panic!(
            "Test scripts directory not found: {}",
            dir_path.display()
        );
    }

    println!("Scanning directory for executable tests: {}", dir_path.display());

    let entries = fs::read_dir(&dir_path).unwrap_or_else(|e| {
        panic!(
            "Failed to read test directory {}: {}",
            dir_path.display(),
            e
        )
    });

    let mut test_files = Vec::new();
    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("mux") {
            test_files.push(path);
        }
    }

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
            
            // Combine stdout and stderr for complete output snapshot
            let full_output = if stderr.is_empty() {
                stdout.clone()
            } else {
                format!("STDOUT:\n{}\nSTDERR:\n{}", stdout, stderr)
            };
            
            println!("Creating executable snapshot for: {}", snapshot_name);
            assert_snapshot!(format!("executable_integration__{}", snapshot_name), full_output);
            println!("✓ Successfully processed executable for: {}", file_name);
        }) {
            Ok(_) => {}
            Err(e) => {
                println!("❌ Error processing executable for file {}: {:?}", file_name, e);
                panic!("Executable test failed while processing: {}", file_name);
            }
        }
    }
}