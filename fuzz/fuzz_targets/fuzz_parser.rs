#![no_main]
use libfuzzer_sys::fuzz_target;
use mux_lang::source::Source;
use mux_lang::lexer::Lexer;
use mux_lang::parser::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let mut source = Source::from_string(input.to_string());
        let mut lexer = Lexer::new(&mut source);
        if let Ok(tokens) = lexer.lex_all() {
            let mut parser = Parser::new(&tokens);
            // The parser must never panic on arbitrary token streams.
            // Errors are fine, panics are bugs.
            let _ = parser.parse();
        }
    }
});
