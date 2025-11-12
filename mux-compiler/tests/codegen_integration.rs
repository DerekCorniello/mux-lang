use insta::assert_snapshot;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn compile_and_read_ll_file(test_file: &Path) -> String {
    // Convert to absolute path to avoid issues with working directory
    let abs_path = fs::canonicalize(test_file)
        .unwrap_or_else(|e| panic!("Failed to get absolute path for {}: {}", test_file.display(), e));
    let path_str = abs_path.to_string_lossy();
    
    // Compile the file to generate .ll file
    let mut compile_cmd = Command::new("cargo");
    compile_cmd
        .args(&["run", "--bin", "mux_compiler", "--", &path_str])
        .current_dir("../") // Run from project root
        .env("RUST_BACKTRACE", "1");
    
    println!("Compiling to generate .ll file: {}", path_str);
    let compile_output = compile_cmd.output()
        .unwrap_or_else(|e| panic!("Failed to execute compile command for {}: {}", path_str, e));
    
    if !compile_output.status.success() {
        // If compilation fails, return empty content for snapshot
        return String::from(" ");
    }
    
    // Determine the .ll file path (same as test file but with .ll extension)
    let ll_file = test_file.with_extension("ll");
    
    // Read the generated .ll file
    let ll_content = fs::read_to_string(&ll_file)
        .unwrap_or_else(|e| panic!("Failed to read generated .ll file {}: {}", ll_file.display(), e));
    
    // Clean up the .ll file
    fs::remove_file(&ll_file)
        .unwrap_or_else(|e| eprintln!("Warning: Failed to clean up .ll file {}: {}", ll_file.display(), e));
    
    ll_content
}

#[test]
fn test_codegen_all_mux_files_in_dir() {
    let test_dir = "../test_scripts";
    let dir_path = PathBuf::from(&test_dir);

    if !dir_path.exists() {
        panic!(
            "Test scripts directory not found: {}",
            dir_path.display()
        );
    }

    println!("Scanning directory for codegen tests: {}", dir_path.display());

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
        println!("\n=== Testing codegen for file: {} ===", file_name);

        match std::panic::catch_unwind(|| {
            println!("Compiling and reading .ll file for: {}", path.display());
            let ll_content = compile_and_read_ll_file(&path);
            let snapshot_name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("unknown_file");
            println!("Creating codegen snapshot for: {}", snapshot_name);
            assert_snapshot!(format!("codegen_integration__{}", snapshot_name), ll_content);
            println!("✓ Successfully processed codegen for: {}", file_name);
        }) {
            Ok(_) => {}
            Err(e) => {
                println!("❌ Error processing codegen for file {}: {:?}", file_name, e);
                panic!("Codegen test failed while processing: {}", file_name);
            }
        }
    }
}