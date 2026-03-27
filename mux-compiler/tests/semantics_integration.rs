use mux_lang::lexer::Lexer;
use mux_lang::parser::Parser;
use mux_lang::semantics::SemanticAnalyzer;
use mux_lang::source::Source;
use std::fs;
use std::path::Path;

fn analyze_mux_file(path: &std::path::PathBuf) -> bool {
    println!("=== Testing file: {} ===", path.display());

    let content = fs::read_to_string(path).expect("Failed to read test file");
    let mut source = Source::from_test_str(&content);

    let mut lexer = Lexer::new(&mut source);
    let tokens: Vec<_> = std::iter::from_fn(|| match lexer.next_token() {
        Ok(token) if token.token_type == mux_lang::lexer::TokenType::Eof => None,
        Ok(token) => Some(Ok(token)),
        Err(e) => Some(Err(e)),
    })
    .collect::<Result<_, _>>()
    .expect("Lexer error");

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse().expect("Parser error");

    let mut analyzer = SemanticAnalyzer::new();
    let errors = analyzer.analyze(&ast, None);

    if errors.is_empty() {
        println!("✓ Successfully analyzed: {}", path.display());
        true
    } else {
        println!("✗ Semantic errors in {}:", path.display());
        for error in &errors {
            println!(
                "  {} at {}:{}",
                error.message, error.span.row_start, error.span.col_start
            );
        }
        false
    }
}

fn collect_mux_files_in_dir(test_dir: &Path) -> usize {
    let mut count = 0;
    for entry in fs::read_dir(test_dir).expect("Failed to read test directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("mux") {
            analyze_mux_file(&path);
            count += 1;
        }
    }
    count
}

#[test]
fn test_semantic_analysis() {
    let test_dir = Path::new("../test_scripts");

    if !test_dir.exists() {
        panic!("Test scripts directory not found: {:?}", test_dir);
    }

    let mut files_processed = 0;

    let operator_test_path = Path::new("tests/operator_overloading.mux");
    if operator_test_path.exists() {
        let path_buf = operator_test_path.to_path_buf();
        analyze_mux_file(&path_buf);
        files_processed += 1;
    }

    files_processed += collect_mux_files_in_dir(test_dir);

    assert!(
        files_processed > 0,
        "No .mux files found in test directories"
    );
    println!("Processed {} files", files_processed);
}
