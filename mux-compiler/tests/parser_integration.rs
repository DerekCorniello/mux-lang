use insta::assert_debug_snapshot;
use mux_compiler::lexer::Lexer;
use mux_compiler::parser::Parser;
use mux_compiler::source::Source;
use std::path::Path;

fn parse_file_to_ast(test_file: &str) -> String {
    let mut src = Source::new(test_file)
        .unwrap_or_else(|_| panic!("Failed to open source file: {}", test_file));

    let mut lexer = Lexer::new(&mut src);
    let tokens = lexer.lex_all()
        .unwrap_or_else(|e| panic!("Lexing failed: {}", e));
        
    let mut parser = Parser::new(&tokens);
    let result = parser.parse()
        .unwrap_or_else(|(_, errors)| {
            panic!("Parsing failed with errors: {:#?}", errors);
        });
    
    // Convert the AST to a nicely formatted string for snapshots
    let mut output = String::new();
    for node in result {
        output.push_str(&format!("{:#?}\n\n", node));
    }
    output
}

#[test]
fn test_control_flow_parsing() {
    let test_file = "../test_scripts/control_flow.mux";
    
    // Get the AST as a string
    let ast_string = parse_file_to_ast(test_file);
    
    // Create a snapshot name based on the test file name
    let snapshot_name = Path::new(test_file)
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap();
    
    // Assert the snapshot
    assert_debug_snapshot!(snapshot_name, ast_string);
}
