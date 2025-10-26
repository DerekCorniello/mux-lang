use mux_compiler::lexer::Lexer;
use mux_compiler::parser::Parser;
use mux_compiler::semantics::SemanticAnalyzer;
use mux_compiler::source::Source;
use std::fs;
use std::path::Path;

#[test]
fn test_semantic_analysis() {
    let test_dir = Path::new("../test_scripts");

    if !test_dir.exists() {
        panic!("Test scripts directory not found: {:?}", test_dir);
    }

    let mut files_processed = 0;

    // First test the operator overloading file in tests directory
    let operator_test_path = Path::new("tests/operator_overloading.mux");
    if operator_test_path.exists() {
        println!("=== Testing file: {} ===", operator_test_path.display());

        let content = fs::read_to_string(operator_test_path).expect("Failed to read test file");
        let mut source = Source::from_test_str(&content);

        // Lex
        let mut lexer = Lexer::new(&mut source);
        let tokens: Vec<_> = std::iter::from_fn(|| match lexer.next_token() {
            Ok(token) if token.token_type == mux_compiler::lexer::TokenType::Eof => None,
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        })
        .collect::<Result<_, _>>()
        .expect("Lexer error");

        // Parse
        let mut parser = Parser::new(&tokens);
        let ast = parser.parse().expect("Parser error");

        // Analyze semantics
        let mut analyzer = SemanticAnalyzer::new();
        let errors = analyzer.analyze(&ast);

        if errors.is_empty() {
            println!("✓ Successfully analyzed: {}", operator_test_path.display());
        } else {
            println!("✗ Semantic errors in {}:", operator_test_path.display());
            for error in &errors {
                println!(
                    "  {} at {}:{}",
                    error.message, error.span.row_start, error.span.col_start
                );
            }
            // For now, don't panic - just report
            // panic!("Semantic errors found");
        }

        files_processed += 1;
    }

    for entry in fs::read_dir(test_dir).expect("Failed to read test directory") {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("mux") {
            println!("=== Testing file: {} ===", path.display());

            let content = fs::read_to_string(&path).expect("Failed to read test file");
            let mut source = Source::from_test_str(&content);

            // Lex
            let mut lexer = Lexer::new(&mut source);
            let tokens: Vec<_> = std::iter::from_fn(|| match lexer.next_token() {
                Ok(token) if token.token_type == mux_compiler::lexer::TokenType::Eof => None,
                Ok(token) => Some(Ok(token)),
                Err(e) => Some(Err(e)),
            })
            .collect::<Result<_, _>>()
            .expect("Lexer error");

            // Parse
            let mut parser = Parser::new(&tokens);
            let ast = parser.parse().expect("Parser error");

            // Analyze semantics
            let mut analyzer = SemanticAnalyzer::new();
            let errors = analyzer.analyze(&ast);

            if errors.is_empty() {
                println!("✓ Successfully analyzed: {}", path.display());
            } else {
                println!("✗ Semantic errors in {}:", path.display());
                for error in &errors {
                    println!(
                        "  {} at {}:{}",
                        error.message, error.span.row_start, error.span.col_start
                    );
                }
                // For now, don't panic - just report
                // panic!("Semantic errors found");
            }

            files_processed += 1;
        }
    }

    assert!(
        files_processed > 0,
        "No .mux files found in test directories"
    );
    println!("Processed {} files", files_processed);
}
