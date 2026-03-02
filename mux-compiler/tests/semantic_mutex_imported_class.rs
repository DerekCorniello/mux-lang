use mux_lang::lexer::Lexer;
use mux_lang::parser::Parser;
use mux_lang::semantics::SemanticAnalyzer;
use mux_lang::source::Source;
use std::fs;
use std::path::Path;

#[test]
fn test_resolve_unqualified_stdlib_class() {
    let path = Path::new("../test_scripts/error_cases/semantic_mutex_new_wrong_arg_count.mux");
    assert!(path.exists(), "test script not found: {:?}", path);

    let content = fs::read_to_string(path).expect("Failed to read test file");
    let mut source = Source::from_test_str(&content);

    // Lex
    let mut lexer = Lexer::new(&mut source);
    let tokens: Vec<_> = std::iter::from_fn(|| match lexer.next_token() {
        Ok(token) if token.token_type == mux_lang::lexer::TokenType::Eof => None,
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
    let errors = analyzer.analyze(&ast, None);

    // Debug: ensure imported_symbols populated
    let imported_keys: Vec<String> = analyzer.imported_symbols().keys().cloned().collect();
    eprintln!("imported namespaces: {:?}", imported_keys);
    if let Some(sync_symbols) = analyzer.imported_symbols().get("sync") {
        let keys: Vec<String> = sync_symbols.keys().cloned().collect();
        eprintln!("sync exported symbols: {:?}", keys);
    }

    // Expect at least one error
    assert!(!errors.is_empty(), "Expected semantic errors but got none");

    // Ensure we do not get "Undefined variable 'Mutex'"
    let joined = errors
        .iter()
        .map(|e| e.message.clone())
        .collect::<Vec<_>>()
        .join("\n");
    eprintln!("semantic errors:\n{}", joined);
    assert!(
        !joined.contains("Undefined variable 'Mutex'"),
        "Found unexpected undefined-variable error: {}",
        joined
    );

    // Expect a constructor argument/arity error or similar (conservative)
    assert!(
        joined.contains("expects") || joined.contains("argument"),
        "Did not find constructor/argument error in messages: {}",
        joined
    );
}
