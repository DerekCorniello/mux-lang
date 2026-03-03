#![no_main]
use libfuzzer_sys::fuzz_target;
use mux_lang::source::Source;
use mux_lang::lexer::Lexer;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let mut source = Source::from_string(input.to_string());
        let mut lexer = Lexer::new(&mut source);
        // The lexer must never panic on arbitrary input.
        // Errors are fine, panics are bugs.
        let _ = lexer.lex_all();
    }
});
