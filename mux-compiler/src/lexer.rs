#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Let,
    Func,
    Return,
    If,
    Else,
    For,
    While,
    Match,
    Const,
    Class,
    Interface,
    Enum,
    Is,
    Some,
    None,
    Ok,
    Err,
    
    // Symbols
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Colon,     // :
    Eq,        // =
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Lt,
    Gt,
    Le,
    Ge,
    EqEq,
    NotEq,
    
    // Literals
    Int(i64), // just using 64 bits rn for simplicity
    Float(f64), // same thing here
    Bool(bool),
    Char(char),
    String(String),
    
    // Identifiers
    Ident(String),
    
    // End-of-file
    Eof,

    // New Line
    NewLine,
}
