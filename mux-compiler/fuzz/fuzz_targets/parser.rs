#![no_main]

use libfuzzer_sys::fuzz_target;
use mux_lang::lexer::Lexer;
use mux_lang::parser::Parser;
use mux_lang::source::Source;

fuzz_target!(|data: &[u8]| {
    let input = String::from_utf8_lossy(data).into_owned();
    let mut source = Source::from_string(input);
    let mut lexer = Lexer::new(&mut source);

    if let Ok(tokens) = lexer.lex_all() {
        let mut parser = Parser::new(&tokens);
        let _ = parser.parse();
    }
});
