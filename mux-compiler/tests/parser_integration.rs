use insta::assert_snapshot;
use mux_compiler::lexer::Lexer;
use mux_compiler::lexer::Span;
use mux_compiler::parser::{
    AstNode, BinaryOp, ExpressionKind, ExpressionNode, LiteralNode, Parser, StatementKind,
    StatementNode, TypeKind, TypeNode,
};
use mux_compiler::source::Source;
use std::fs;
use std::option::Option::Some;

// Helper function to create a Span with all fields set
fn span(row_start: usize, col_start: usize, row_end: usize, col_end: usize) -> Span {
    Span {
        row_start,
        row_end: Some(row_end),
        col_start,
        col_end: Some(col_end),
    }
}

#[test]
fn test_control_flow_parsing() {
    let test_file = "../test_scripts/control_flow.mux";
    
    println!("Testing file: {}", test_file);
    
    let mut src = Source::new(test_file)
        .unwrap_or_else(|_| panic!("Failed to open source file: {}", test_file));

    let mut lexer = Lexer::new(&mut src);
    let tokens = lexer.lex_all()
        .unwrap_or_else(|e| panic!("Lexing failed: {}", e));
        
    let mut parser = Parser::new(&tokens);
    let result = parser.parse();
    
    // Just check that parsing didn't fail
    if let Err((_, errors)) = result {
        panic!("Parsing failed with errors: {:?}", errors);
    }
    
    // If we get here, parsing was successful
}
