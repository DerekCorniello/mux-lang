use insta::assert_debug_snapshot;
use mux_compiler::lexer::Lexer;
use mux_compiler::source::Source;
use std::fs;
use std::path::PathBuf;

#[test]
#[rustfmt::skip]
fn test_file_lexer() {
    let test_dir = "../test_scripts";
    let dir_path = PathBuf::from(&test_dir);

    if !dir_path.exists() {
        panic!(
            "Test scripts directory not found: {} (set MUX_TEST_SCRIPTS_DIR to override)",
            dir_path.display()
        );
    }

    println!("Scanning directory: {}", dir_path.display());

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

    let mut actual_tokens = Vec::new();
    for path in test_files {
        let file_name = path.file_name().and_then(|s| s.to_str()).unwrap_or("unknown");
        println!("\n=== Testing file: {} ===", file_name);
        
        let mut src = Source::new(&path.to_string_lossy())
            .unwrap_or_else(|_| panic!("Failed to open source file: {}", path.display()));

        let mut lexer = Lexer::new(&mut src);
        let tokens = lexer
            .lex_all()
            .unwrap_or_else(|e| panic!("Lexing failed for file {}: {}", file_name, e));
        
        actual_tokens.extend(tokens);
        println!("✓ Successfully processed: {}", file_name);
    }

    assert_debug_snapshot!("file_lexer", actual_tokens);
}
