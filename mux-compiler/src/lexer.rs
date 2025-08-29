use crate::source::Source;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // keywords
    Let,
    Func,
    Returns,
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
    As,
    In,

    // symbols
    OpenParen,    // (
    CloseParen,   // )
    OpenBrace,    // {
    CloseBrace,   // }
    OpenBracket,  // [
    CloseBracket, // ]
    Dot,          // .
    Comma,        
    Colon,        
    Semicolon,
    Eq,           
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
    Bang,
    // increment/decrement operators (prefix and postfix)
    Incr,    // i++ or ++i
    Decr,    // i-- or --i
    // compound assignment operators
    PlusEq,  // +=
    MinusEq, // -=
    StarEq,  // *=
    SlashEq, // /=
    Ampersand,
    And,
    Or,

    // literals
    Int(i64),   // just using 64 bits rn for simplicity
    Float(f64), // same thing here
    Bool(bool),
    Char(char),
    Str(String),
    Underscore,

    // identifiers
    Id(String),

    // end-of-file
    Eof,

    // new Line
    NewLine,

    // keep comments around for dev tools later
    LineComment(String),
    MultilineComment(String),
}

pub struct Lexer<'a> {
    source: &'a mut Source,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a mut Source) -> Self {
        Lexer { source }
    }

    pub fn lex_all(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        loop {
            let tok = self.next_token()?;
            if tok == Token::Eof {
                break;
            }
            tokens.push(tok);
        }

        Ok(tokens)
    }

    fn skip_space(&mut self) {
        // only skip spaces and tabs, not newlines
        while let Some(ch) = self.source.peek() {
            match ch {
                ' ' | '\t' => {
                    self.source.next_char();
                }
                _ => break,
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_space();

        // handle both unix (\n) and windows (\r\n) newlines
        if let Some('\r') = self.source.peek() {
            self.source.next_char(); // consume '\r'
            if self.source.peek() == Some('\n') {
                self.source.next_char(); // consume '\n'
            }
            return Ok(Token::NewLine);
        } else if self.source.peek() == Some('\n') {
            self.source.next_char();
            return Ok(Token::NewLine);
        }

        let start_loc = self.source.get_location_string();
        let ch = match self.source.next_char() {
            Some(c) => c,
            None => return Ok(Token::Eof),
        };

        match ch {
            '(' => Ok(Token::OpenParen),
            ')' => Ok(Token::CloseParen),
            '{' => Ok(Token::OpenBrace),
            '}' => Ok(Token::CloseBrace),
            '[' => Ok(Token::OpenBracket),
            ']' => Ok(Token::CloseBracket),
            ',' => Ok(Token::Comma),
            ':' => Ok(Token::Colon),
            ';' => Ok(Token::Semicolon),
            '*' => self.get_star_token(),
            '%' => Ok(Token::Percent),
            '_' => Ok(Token::Underscore),
            '&' => Ok(Token::Ampersand),
            '.' => match self.source.peek() {
                Some(c) if c.is_ascii_digit() => self.read_number(ch, start_loc),
                _ => Ok(Token::Dot),
            },
            _ => self.get_multichar_token(ch, start_loc),
        }
    }

    fn get_star_token(&mut self) -> Result<Token, String> {
        if self.source.peek() == Some('=') {
            self.source.next_char();
            Ok(Token::StarEq)
        } else {
            Ok(Token::Star)
        }
    }

    fn get_multichar_token(
        &mut self,
        first_char: char,
        start_loc: String,
    ) -> Result<Token, String> {
        match first_char {
            '=' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    Ok(Token::EqEq)
                } else {
                    Ok(Token::Eq)
                }
            }
            '!' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    Ok(Token::NotEq)
                } else {
                    Ok(Token::Bang)
                }
            }
            '+' => {
                match self.source.peek() {
                    Some('+') => {
                        self.source.next_char();
                        Ok(Token::Incr)
                    }
                    Some('=') => {
                        self.source.next_char();
                        Ok(Token::PlusEq)
                    }
                    _ => Ok(Token::Plus),
                }
            }
            '-' => {
                match self.source.peek() {
                    Some('-') => {
                        self.source.next_char();
                        Ok(Token::Decr)
                    }
                    Some('=') => {
                        self.source.next_char();
                        Ok(Token::MinusEq)
                    }
                    _ => Ok(Token::Minus),
                }
            }
            '<' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    Ok(Token::Le)
                } else {
                    Ok(Token::Lt)
                }
            }
            '>' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    Ok(Token::Ge)
                } else {
                    Ok(Token::Gt)
                }
            }
            '/' => match self.source.peek() {
                Some('/') => {
                    self.source.next_char();
                    self.skip_space();
                    let text = self.source.consume_until('\n');
                    Ok(Token::LineComment(text.trim().to_string()))
                }
                Some('*') => {
                    self.source.next_char();
                    self.skip_space();
                    let text = self.source.consume_multiline_comment();
                    Ok(Token::MultilineComment(text.trim().to_string()))
                }
                Some('=') => {
                    self.source.next_char();
                    Ok(Token::SlashEq)
                }
                _ => Ok(Token::Slash),
            },
            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(nc) = self.source.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        ident.push(self.source.next_char().unwrap());
                    } else {
                        break;
                    }
                }
                let token = match ident.as_str() {
                    "let" => Token::Let,
                    "func" => Token::Func,
                    "returns" => Token::Returns,
                    "return" => Token::Return,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "for" => Token::For,
                    "while" => Token::While,
                    "match" => Token::Match,
                    "const" => Token::Const,
                    "class" => Token::Class,
                    "interface" => Token::Interface,
                    "enum" => Token::Enum,
                    "is" => Token::Is,
                    "as" => Token::As,
                    "in" => Token::In,
                    "true" => Token::Bool(true),
                    "false" => Token::Bool(false),
                    "and" => Token::And,
                    "or" => Token::Or,
                    _ => Token::Id(ident),
                };
                Ok(token)
            }
            c if c.is_ascii_digit() => self.read_number(first_char, start_loc),
            '"' => self.read_string(start_loc),
            '\'' => self.read_char(start_loc),
            _ => Err(format!(
                "Unexpected character: '{}' at {}",
                first_char, start_loc
            )),
        }
    }

    fn read_string(&mut self, start_loc: String) -> Result<Token, String> {
        let mut s = String::new();
        while let Some(c) = self.source.next_char() {
            match c {
                '"' => return Ok(Token::Str(s)),
                '\\' => {
                    let esc = self.source.next_char().ok_or_else(|| {
                        let err_loc = self.source.get_location_string();
                        format!("Incomplete escape sequence at {}", err_loc)
                    })?;
                    let esc_char = match esc {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '0' => '\0',
                        '\\' => '\\',
                        '\"' => '\"',
                        '\'' => '\'',
                        _ => {
                            let err_loc = self.source.get_location_string();
                            return Err(format!("Unknown escape sequence `\\{}` at {}", esc, err_loc));
                        }
                    };
                    s.push(esc_char);
                }
                '\n' => s.push('\n'),
                _ => s.push(c),
            }
        }
        Err(format!("Unterminated string at {}", start_loc))
    }

    fn read_number(&mut self, first_char: char, start_loc: String) -> Result<Token, String> {
        let mut num = String::new();
        let mut dot_count = 0;

        if first_char == '.' {
            match self.source.peek() {
                Some(c) if c.is_ascii_digit() => {
                    dot_count += 1;
                    num.push('.');
                }
                _ => return Ok(Token::Dot),
            }
        } else {
            num.push(first_char);
        }

        while let Some(c) = self.source.peek() {
            match c {
                '_' => {
                    self.source.next_char();
                }
                '.' => {
                    dot_count += 1;
                    if dot_count > 1 {
                        return Err(format!("Multiple decimals in float at {}", start_loc));
                    }
                    num.push(self.source.next_char().unwrap());
                }
                '0'..='9' => num.push(self.source.next_char().unwrap()),
                _ => break,
            }
        }

        if dot_count > 0 {
            num.parse::<f64>()
                .map(Token::Float)
                .map_err(|_| format!("Invalid float `{}` at {}", num, start_loc))
        } else {
            num.parse::<i64>()
                .map(Token::Int)
                .map_err(|_| format!("Invalid int `{}` at {}", num, start_loc))
        }
    }

    fn read_char(&mut self, start_loc: String) -> Result<Token, String> {
        let c = match self.source.next_char() {
            Some('\\') => {
                let esc = self
                    .source
                    .next_char()
                    .ok_or_else(|| format!("Incomplete char escape at {}", start_loc))?;
                match esc {
                    'n' => '\n',
                    't' => '\t',
                    '\\' => '\\',
                    '\'' => '\'',
                    'r' => '\r',
                    _ => {
                        return Err(format!("Unknown escape `\\{}` at {}", esc, start_loc));
                    }
                }
            }
            Some(ch) => ch,
            None => {
                return Err(format!("Unexpected end of char at {}", start_loc));
            }
        };

        match self.source.next_char() {
            Some('\'') => Ok(Token::Char(c)),
            Some(_) => Err(format!(
                "Char literal must be exactly one character at {}",
                start_loc
            )),
            None => Err(format!("Unterminated char literal at {}", start_loc)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_all(source_text: &str) -> Result<Vec<Token>, String> {
        let mut source = Source::from_test_str(source_text).unwrap();
        let mut lexer = Lexer::new(&mut source);
        let mut tokens = Vec::new();
        loop {
            let tok = lexer.next_token()?;
            if tok == Token::Eof {
                break;
            }
            tokens.push(tok);
        }
        Ok(tokens)
    }

    #[test]
    fn test_position_tracking_across_lines() {
        // Test a more complex example across multiple lines
        let input = "let x = 42\nfunc test() {\n  return x\n}";
        let mut source = Source::from_test_str(input).unwrap();
        let mut lexer = Lexer::new(&mut source);

        assert_eq!(lexer.next_token().unwrap(), Token::Let);
        assert_eq!(lexer.next_token().unwrap(), Token::Id("x".to_string()));
        assert_eq!(lexer.next_token().unwrap(), Token::Eq);
        assert_eq!(lexer.next_token().unwrap(), Token::Int(42));
        assert_eq!(lexer.next_token().unwrap(), Token::NewLine);

        assert_eq!(lexer.next_token().unwrap(), Token::Func);
        assert_eq!(lexer.next_token().unwrap(), Token::Id("test".to_string()));
        assert_eq!(lexer.next_token().unwrap(), Token::OpenParen);
        assert_eq!(lexer.next_token().unwrap(), Token::CloseParen);
        assert_eq!(lexer.next_token().unwrap(), Token::OpenBrace);
        assert_eq!(lexer.next_token().unwrap(), Token::NewLine);

        assert_eq!(lexer.next_token().unwrap(), Token::Return);
        assert_eq!(lexer.next_token().unwrap(), Token::Id("x".to_string()));
        assert_eq!(lexer.next_token().unwrap(), Token::NewLine);

        assert_eq!(lexer.next_token().unwrap(), Token::CloseBrace);
        assert_eq!(lexer.next_token().unwrap(), Token::Eof);
    }

    #[test]
    fn test_error_positions_with_actual_positions() {
        let test_cases = vec![
            (
                "x = 1.2.3",
                "Multiple decimals in float at line 1, column 5",
            ),
            (
                "let x = 'ab'",
                "Char literal must be exactly one character at line 1, column 9",
            ),
            ("\"unterminated", "Unterminated string at line 1, column 1"),
            (
                "x =\n  'ab'",
                "Char literal must be exactly one character at line 2, column 3",
            ),
        ];

        for (input, expected_error_part) in test_cases {
            let result = lex_all(input);
            assert!(result.is_err(), "Expected error for input: {}", input);
            let error = result.unwrap_err();
            assert!(
                error.contains(expected_error_part),
                "Error '{}' should contain '{}' for input '{}'",
                error,
                expected_error_part,
                input
            );
        }
    }

    #[test]
    fn test_numbers() {
        let tokens = lex_all("42 1_000 3.45 .5 5.");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Int(42),
                Token::Int(1000),
                Token::Float(3.45),
                Token::Float(0.5),
                Token::Float(5.0)
            ])
        );
    }

    #[test]
    fn test_simple_strings() {
        let tokens = lex_all(r#""hello" "world""#);
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Str("hello".to_string()),
                Token::Str("world".to_string())
            ])
        );
    }

    #[test]
    fn test_multiline_string() {
        let tokens = lex_all(
            r#"
"hello
world"
"#,
        );
        assert_eq!(
            tokens,
            Ok(vec![
                Token::NewLine,
                Token::Str("hello\nworld".to_string()),
                Token::NewLine
            ])
        );
    }

    #[test]
    fn test_char_literals() {
        let tokens = lex_all(r#"'a' '\n' '\''"#);
        assert_eq!(
            tokens,
            Ok(vec![Token::Char('a'), Token::Char('\n'), Token::Char('\'')])
        );
    }

    #[test]
    fn test_strings_with_escapes() {
        // basic escapes
        assert_eq!(
            lex_all(r#""a\n\t\\\"\'\r\0""#).unwrap(),
            vec![Token::Str("a\n\t\\\"'\r\0".to_string())]
        );
        
        // test that Some, None, Ok, Err are treated as identifiers
        assert_eq!(
            lex_all("Some None Ok Err").unwrap(),
            vec![
                Token::Id("Some".to_string()),
                Token::Id("None".to_string()),
                Token::Id("Ok".to_string()),
                Token::Id("Err".to_string()),
            ]
        );

        // boolean test
        assert_eq!(
            lex_all("true false").unwrap(),
            vec![
                Token::Bool(true),
                Token::Bool(false),
            ]
        );
        
        // test that keywords are case-sensitive
        assert_eq!(
            lex_all("some none ok err").unwrap(),
            vec![
                Token::Id("some".to_string()),
                Token::Id("none".to_string()),
                Token::Id("ok".to_string()),
                Token::Id("err".to_string()),
            ]
        );

        // unterminated string
        assert!(lex_all("\"unterminated").is_err());

        // unknown escape sequence
        assert!(lex_all(r#""\a""#).is_err());
        assert!(lex_all(r#""\c""#).is_err());
    }

    #[test]
    fn test_keywords_and_identifiers() {
        let tokens = lex_all("let x = 42 if else for while match const class interface enum is as in range list map Optional Result Some None Ok Err true false and or");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::Let,
                Token::Id("x".to_string()),
                Token::Eq,
                Token::Int(42),
                Token::If,
                Token::Else,
                Token::For,
                Token::While,
                Token::Match,
                Token::Const,
                Token::Class,
                Token::Interface,
                Token::Enum,
                Token::Is,
                Token::As,
                Token::In,
                Token::Id("range".to_string()),
                Token::Id("list".to_string()),
                Token::Id("map".to_string()),
                Token::Id("Optional".to_string()),
                Token::Id("Result".to_string()),
                Token::Id("Some".to_string()),
                Token::Id("None".to_string()),
                Token::Id("Ok".to_string()),
                Token::Id("Err".to_string()),
                Token::Bool(true),
                Token::Bool(false),
                Token::And,
                Token::Or,
            ])
        );
    }

    #[test]
    fn test_operators() {
        assert_eq!(
            lex_all("= == ! != < <= > >=").unwrap(),
            vec![
                Token::Eq,
                Token::EqEq,
                Token::Bang,
                Token::NotEq,
                Token::Lt,
                Token::Le,
                Token::Gt,
                Token::Ge,
            ]
        );

        // arithmetic operators
        assert_eq!(
            lex_all("+ - * / %").unwrap(),
            vec![
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash,
                Token::Percent
            ]
        );

        // increment/decrement
        assert_eq!(
            lex_all("++ --").unwrap(),
            vec![Token::Incr, Token::Decr]
        );

        // compound assignment operators
        assert_eq!(
            lex_all("+= -= *= /=").unwrap(),
            vec![
                Token::PlusEq,
                Token::MinusEq,
                Token::StarEq,
                Token::SlashEq,
            ]
        );

        // test that operators don't merge incorrectly
        assert_eq!(
            lex_all("a+=1 b-=2 c*=3 d/=4").unwrap(),
            vec![
                Token::Id("a".to_string()),
                Token::PlusEq,
                Token::Int(1),
                Token::Id("b".to_string()),
                Token::MinusEq,
                Token::Int(2),
                Token::Id("c".to_string()),
                Token::StarEq,
                Token::Int(3),
                Token::Id("d".to_string()),
                Token::SlashEq,
                Token::Int(4),
            ]
        );
    }

    #[test]
    fn test_comments() {
        let tokens = lex_all("// line comment\n/* multi\nline */");
        assert_eq!(
            tokens,
            Ok(vec![
                Token::LineComment("line comment".into()),
                Token::NewLine,
                Token::MultilineComment("multi\nline".into())
            ])
        );
    }

    #[test]
    fn test_dots_and_newlines() {
        let tokens = lex_all(".\n..");
        assert_eq!(
            tokens,
            Ok(vec![Token::Dot, Token::NewLine, Token::Dot, Token::Dot])
        );
    }
}
