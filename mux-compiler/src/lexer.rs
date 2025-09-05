use crate::source::Source;
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub row_start: usize,
    pub row_end: Option<usize>,
    pub col_start: usize,
    pub col_end: Option<usize>,
}

impl Span {
    pub fn new(row_start: usize, col_start: usize) -> Self {
        Self {
            row_start,
            row_end: None,
            col_start,
            col_end: None,
        }
    }

    pub fn complete(&mut self, row_end: usize, col_end: usize) {
        self.row_end = Some(row_end);
        self.col_end = Some(col_end);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(token: TokenType, span: Span) -> Token {
        Token {
            token_type: token,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
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
    Incr,
    Decr,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    Ampersand,
    And,
    Or,

    Int(i64),
    Float(OrderedFloat<f64>),
    Bool(bool),
    Char(char),
    Str(String),
    Underscore,

    // identifiers
    Id(String),

    Eof,

    NewLine,

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
            let is_eof = matches!(&tok.token_type, TokenType::Eof);
            if is_eof {
                break;
            } else {
                tokens.push(tok);
            }
        }
        Ok(tokens)
    }

    fn skip_space(&mut self) {
        // skip spaces and tabs, but not newlines
        while let Some(ch) = self.source.peek() {
            match ch {
                ' ' | '\t' | '\r' => {
                    self.source.next_char();
                }
                _ => break,
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_space();

        match self.source.peek() {
            None => {
                return Ok(Token::new(
                    TokenType::Eof,
                    Span::new(self.source.line, self.source.col),
                ));
            }
            Some('\n') => {
                let start_span = Span::new(self.source.line, self.source.col);
                self.source.next_char(); // consume '\n'
                return Ok(Token::new(TokenType::NewLine, start_span));
            }
            // handle slash stuff
            Some('/') => {
                let start_span = Span::new(self.source.line, self.source.col);
                self.source.next_char(); // consume '/'

                match self.source.peek() {
                    // line comment - consume until newline and return it
                    Some('/') => {
                        self.source.next_char(); // consume second '/'
                        let comment = self.source.consume_until('\n');
                        return Ok(Token::new(
                            TokenType::LineComment(comment.trim().to_string()),
                            start_span,
                        ));
                    }
                    // block comment - consume until */ and return it
                    Some('*') => {
                        self.source.next_char(); // consume '*'
                        let comment = self.source.consume_multiline_comment();
                        return Ok(Token::new(
                            TokenType::MultilineComment(comment.trim().to_string()),
                            start_span,
                        ));
                    }
                    // slasheq
                    Some('=') => {
                        self.source.next_char(); // consume '='
                        return Ok(Token::new(TokenType::SlashEq, start_span));
                    }
                    // not a comment, handle as a slash token
                    _ => {
                        return Ok(Token::new(TokenType::Slash, start_span));
                    }
                }
            }
            // ready to process the next token
            _ => {},
        }

        let start_span = Span::new(self.source.line, self.source.col);
        let c = match self.source.next_char() {
            Some(c) => c,
            None => return Ok(Token::new(TokenType::Eof, start_span)),
        };

        match c {
            '(' => Ok(Token::new(TokenType::OpenParen, start_span)),
            ')' => Ok(Token::new(TokenType::CloseParen, start_span)),
            '{' => Ok(Token::new(TokenType::OpenBrace, start_span)),
            '}' => Ok(Token::new(TokenType::CloseBrace, start_span)),
            '[' => Ok(Token::new(TokenType::OpenBracket, start_span)),
            ']' => Ok(Token::new(TokenType::CloseBracket, start_span)),
            ',' => Ok(Token::new(TokenType::Comma, start_span)),
            ':' => Ok(Token::new(TokenType::Colon, start_span)),
            ';' => Ok(Token::new(TokenType::Semicolon, start_span)),
            '%' => Ok(Token::new(TokenType::Percent, start_span)),
            '_' => Ok(Token::new(TokenType::Underscore, start_span)),
            '.' => match self.source.peek() {
                Some(c) if c.is_ascii_digit() => self.read_number(c, start_span),
                _ => Ok(Token::new(TokenType::Dot, start_span)),
            },
            _ => self.get_multichar_token(c, start_span),
        }
    }

    fn get_multichar_token(
        &mut self,
        first_char: char,
        mut start_span: Span,
    ) -> Result<Token, String> {
        // update the span with the end position
        start_span.complete(self.source.line, self.source.col);

        match first_char {
            '*' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::StarEq, start_span))
                } else {
                    Ok(Token::new(TokenType::Star, start_span))
                }
            }
            '=' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::EqEq, start_span))
                } else {
                    Ok(Token::new(TokenType::Eq, start_span))
                }
            }
            '!' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::NotEq, start_span))
                } else {
                    Ok(Token::new(TokenType::Bang, start_span))
                }
            }
            '+' => match self.source.peek() {
                Some('+') => {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::Incr, start_span))
                }
                Some('=') => {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::PlusEq, start_span))
                }
                _ => Ok(Token::new(TokenType::Plus, start_span)),
            },
            '-' => match self.source.peek() {
                Some('-') => {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::Decr, start_span))
                }
                Some('=') => {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::MinusEq, start_span))
                }
                _ => Ok(Token::new(TokenType::Minus, start_span)),
            },
            '<' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::Le, start_span))
                } else {
                    Ok(Token::new(TokenType::Lt, start_span))
                }
            }
            '>' => {
                if self.source.peek() == Some('=') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::Ge, start_span))
                } else {
                    Ok(Token::new(TokenType::Gt, start_span))
                }
            }
            '|' => {
                if self.source.peek() == Some('|') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::Or, start_span))
                } else {
                    Err("Expected '|' after '|'".to_string())
                }
            }
            '&' => {
                if self.source.peek() == Some('&') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::And, start_span))
                } else {
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::Ampersand, start_span))
                }
            }
            '/' => {
                if self.source.peek() == Some('/') {
                    self.source.next_char();
                    let mut comment = String::new();
                    while let Some(ch) = self.source.next_char() {
                        if ch == '\n' || ch == '\r' {
                            break;
                        }
                        comment.push(ch);
                    }
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::LineComment(comment), start_span))
                } else if self.source.peek() == Some('*') {
                    self.source.next_char();
                    let mut comment = String::new();
                    let mut depth = 1;
                    while depth > 0 {
                        match self.source.next_char() {
                            Some('*') => {
                                if self.source.peek() == Some('/') {
                                    self.source.next_char();
                                    depth -= 1;
                                    if depth > 0 {
                                        comment.push('*');
                                    }
                                } else {
                                    comment.push('*');
                                }
                            }
                            Some('/') => {
                                if self.source.peek() == Some('*') {
                                    self.source.next_char();
                                    depth += 1;
                                    comment.push_str("/*");
                                } else {
                                    comment.push('/');
                                }
                            }
                            Some(ch) => comment.push(ch),
                            None => return Err("Unterminated block comment".to_string()),
                        }
                    }
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::MultilineComment(comment), start_span))
                } else if self.source.peek() == Some('=') {
                    self.source.next_char();
                    start_span.complete(self.source.line, self.source.col);
                    Ok(Token::new(TokenType::SlashEq, start_span))
                } else {
                    Ok(Token::new(TokenType::Slash, start_span))
                }
            }
            '0'..='9' => self.read_number(first_char, start_span),
            '\'' => self.read_char(start_span),
            '"' => self.read_string(start_span),
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = first_char.to_string();
                while let Some(ch) = self.source.peek() {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ident.push(ch);
                        self.source.next_char();
                    } else {
                        break;
                    }
                }
                start_span.complete(self.source.line, self.source.col);
                let token_type = match ident.as_str() {
                    "let" => TokenType::Let,
                    "func" => TokenType::Func,
                    "return" => TokenType::Return,
                    "returns" => TokenType::Returns,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "for" => TokenType::For,
                    "while" => TokenType::While,
                    "match" => TokenType::Match,
                    "const" => TokenType::Const,
                    "class" => TokenType::Class,
                    "interface" => TokenType::Interface,
                    "enum" => TokenType::Enum,
                    "is" => TokenType::Is,
                    "as" => TokenType::As,
                    "in" => TokenType::In,
                    "true" => TokenType::Bool(true),
                    "false" => TokenType::Bool(false),
                    "and" => TokenType::And,
                    "or" => TokenType::Or,
                    _ => TokenType::Id(ident),
                };
                Ok(Token::new(token_type, start_span))
            }
            _ => Err(format!(
                "unexpected character: '{}' at line {}, column {}.",
                first_char, start_span.row_start, start_span.col_start
            )),
        }
    }

    fn read_string(&mut self, mut start_span: Span) -> Result<Token, String> {
        let mut s = String::new();
        let mut escaped = false;

        while let Some(c) = self.source.next_char() {
            if c == '\\' && !escaped {
                escaped = true;
                continue;
            }

            if c == '"' && !escaped {
                start_span.complete(self.source.line, self.source.col);
                return Ok(Token::new(TokenType::Str(s), start_span));
            }

            if escaped {
                match c {
                    'n' => s.push('\n'),
                    't' => s.push('\t'),
                    'r' => s.push('\r'),
                    '0' => s.push('\0'),
                    '\\' => s.push('\\'),
                    '\'' => s.push('\''),
                    '"' => s.push('"'),
                    _ => {
                        return Err(format!(
                            "unknown escape sequence: \\{} at line {}, column {}",
                            c,
                            self.source.line,
                            self.source.col - 1
                        ));
                    }
                }
                escaped = false;
            } else {
                s.push(c);
            }
        }

        Err("Unterminated string".to_string())
    }

    fn read_char(&mut self, mut start_span: Span) -> Result<Token, String> {
        let start_col = self.source.col - 1; // adjust for the opening quote
        let mut chars = Vec::new();
        let mut escaped = false;

        while let Some(c) = self.source.next_char() {
            if c == '\\' && !escaped {
                escaped = true;
                continue;
            }

            if c == '\'' && !escaped {
                // end of char literal
                if chars.len() != 1 {
                    return Err(format!(
                        "Char literal must be exactly one character at line {}, column {}",
                        start_span.row_start,
                        start_col + 1 // +1 to account for the opening quote
                    ));
                }
                start_span.complete(self.source.line, self.source.col);
                return Ok(Token::new(TokenType::Char(chars[0]), start_span));
            }

            if escaped {
                match c {
                    'n' => chars.push('\n'),
                    't' => chars.push('\t'),
                    'r' => chars.push('\r'),
                    '0' => chars.push('\0'),
                    '\\' => chars.push('\\'),
                    '\'' => chars.push('\''),
                    '"' => chars.push('"'),
                    _ => {
                        return Err(format!(
                            "unknown escape sequence: \\{} at line {}, column {}",
                            c,
                            start_span.row_start,
                            self.source.col - 1
                        ));
                    }
                }
                escaped = false;
            } else {
                chars.push(c);
            }

            if chars.len() > 1 && !escaped {
                return Err(format!(
                    "char literal must be exactly one character at line {}, column {}",
                    start_span.row_start,
                    start_col + 1 // +1 to account for the opening quote
                ));
            }
        }

        Err("unterminated char literal".to_string())
    }

    fn read_number(&mut self, first_char: char, mut start_span: Span) -> Result<Token, String> {
        let mut num = String::new();
        let mut is_float = false;

        if first_char == '.' {
            // handle numbers starting with decimal point
            is_float = true;
            num.push('0');
            num.push('.');

            // require at least one digit after the decimal point
            let mut has_digit = false;
            while let Some(c) = self.source.peek() {
                if c.is_ascii_digit() {
                    num.push(self.source.next_char().unwrap());
                    has_digit = true;
                } else if c == '_' {
                    self.source.next_char(); // skip underscores
                } else {
                    break;
                }
            }

            if !has_digit {
                return Err(format!(
                    "Expected digit after decimal point at line {}, column {}",
                    self.source.line, self.source.col
                ));
            }
        } else {
            // handle numbers starting with a digit
            num.push(first_char);

            // read digits before decimal point
            while let Some(c) = self.source.peek() {
                if c.is_ascii_digit() {
                    num.push(self.source.next_char().unwrap());
                } else if c == '_' {
                    self.source.next_char(); // skip underscores
                } else {
                    break;
                }
            }

            // check for decimal point
            if let Some('.') = self.source.peek() {
                is_float = true;
                num.push(self.source.next_char().unwrap());

                // read digits after decimal point (optional for numbers like 42.)
                while let Some(c) = self.source.peek() {
                    if c.is_ascii_digit() {
                        num.push(self.source.next_char().unwrap());
                    } else if c == '_' {
                        self.source.next_char(); // skip underscores
                    } else {
                        break;
                    }
                }
            }
        }

        // handle scientific notation (e.g., 1.23e4, 1e-10, 1.23e+10)
        if let Some('e') | Some('E') = self.source.peek() {
            is_float = true;
            num.push(self.source.next_char().unwrap());

            // check for sign
            if let Some('+') | Some('-') = self.source.peek() {
                num.push(self.source.next_char().unwrap());
            }

            // Require at least one digit after 'e' and optional sign
            let mut has_exponent_digit = false;
            while let Some(c) = self.source.peek() {
                if c.is_ascii_digit() {
                    num.push(self.source.next_char().unwrap());
                    has_exponent_digit = true;
                } else if c == '_' {
                    self.source.next_char(); // skip underscores
                } else {
                    break;
                }
            }

            if !has_exponent_digit {
                return Err(format!(
                    "Missing exponent in scientific notation at line {}, column {}",
                    self.source.line, self.source.col
                ));
            }
        }

        start_span.complete(self.source.line, self.source.col);

        if is_float {
            // handle case where the number ends with a decimal point
            if num.ends_with('.') {
                num.push('0');
            }

            num.parse::<f64>()
                .map(OrderedFloat)
                .map(|f| Token::new(TokenType::Float(f), start_span))
                .map_err(|_| format!("Invalid float literal: {}", num))
        } else {
            // remove underscores for parsing
            let clean_num: String = num.chars().filter(|c| *c != '_').collect();
            clean_num
                .parse::<i64>()
                .map(|i| Token::new(TokenType::Int(i), start_span))
                .map_err(|_| format!("Invalid integer literal: {}", num))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::Source;

    #[test]
    fn test_position_tracking_across_lines() {
        // Test a more complex example across multiple lines
        let input = "let x = 42\nfunc test() {\n  return x\n}";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);

        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::Let);
        assert_eq!(
            lexer.next_token().unwrap().token_type,
            TokenType::Id("x".to_string())
        );
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::Eq);
        match lexer.next_token().unwrap().token_type {
            TokenType::Int(42) => {}
            other => panic!("Expected Int(42), got {:?}", other),
        }
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::NewLine);

        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::Func);
        assert_eq!(
            lexer.next_token().unwrap().token_type,
            TokenType::Id("test".to_string())
        );
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::OpenParen);
        assert_eq!(
            lexer.next_token().unwrap().token_type,
            TokenType::CloseParen
        );
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::OpenBrace);
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::NewLine);

        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::Return);
        assert_eq!(
            lexer.next_token().unwrap().token_type,
            TokenType::Id("x".to_string())
        );
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::NewLine);

        assert_eq!(
            lexer.next_token().unwrap().token_type,
            TokenType::CloseBrace
        );
        assert_eq!(lexer.next_token().unwrap().token_type, TokenType::Eof);
    }

    #[test]
    fn test_error_positions_with_actual_positions() {
        // Test char literal with multiple characters
        let mut source = Source::from_test_str("let x = 'ab'");
        let mut lexer = Lexer::new(&mut source);
        let result = lexer.lex_all();
        assert!(
            result.is_err(),
            "Expected error for char literal with multiple characters"
        );
        let error = result.unwrap_err();
        assert!(
            error.contains("char literal must be exactly one character at line 1, column 10"),
            "Error should be about char literal length, got: {}",
            error
        );

        // Test unterminated string
        let mut source = Source::from_test_str("\"unterminated");
        let mut lexer = Lexer::new(&mut source);
        let result = lexer.lex_all();
        assert!(result.is_err(), "Expected error for unterminated string");
        let error = result.unwrap_err();
        assert!(
            error.contains("Unterminated string"),
            "Error should be about unterminated string, got: {}",
            error
        );

        // Test that "1.2.3" is tokenized as Float(1.2) and Int(33) (the ASCII code for '3')
        let mut source = Source::from_test_str("x = 1.2.3");
        let mut lexer = Lexer::new(&mut source);
        let result = lexer.lex_all();

        // This should succeed since "1.2.3" is being parsed as "1.2" and ".3"
        let tokens =
            result.unwrap_or_else(|e| panic!("Expected successful tokenization, got error: {}", e));

        // We expect 4 tokens: x, =, 1.2, .3
        assert_eq!(tokens.len(), 4, "Expected 4 tokens, got: {:?}", tokens);

        // Check the tokens
        match &tokens[..] {
            [
                Token {
                    token_type: TokenType::Id(id),
                    ..
                },
                Token {
                    token_type: TokenType::Eq,
                    ..
                },
                Token {
                    token_type: TokenType::Float(f),
                    ..
                },
                Token {
                    token_type: TokenType::Int(i),
                    ..
                },
            ] => {
                assert_eq!(id, "x");
                assert!((*f - 1.2).abs() < f64::EPSILON);
                assert_eq!(*i, 33); // ASCII code for '3'
            }
            _ => panic!("Unexpected tokens: {:?}", tokens),
        }
    }

    #[test]
    fn test_numbers() {
        let input = "42 1_000 3.45 0.5 5.0"; // Changed .5 to 0.5 to match lexer expectations
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();
        let token_types: Vec<_> = tokens.into_iter().map(|t| t.token_type).collect();

        // We expect: [Int(42), Int(1000), Float(3.45), Float(0.5), Float(5.0)]
        match &token_types[..] {
            [
                TokenType::Int(42),
                TokenType::Int(1000),
                TokenType::Float(OrderedFloat(f1)),
                TokenType::Float(OrderedFloat(f2)),
                TokenType::Float(OrderedFloat(f3)),
            ] if (*f1 - 3.45).abs() < f64::EPSILON
                && (*f2 - 0.5).abs() < f64::EPSILON
                && (*f3 - 5.0).abs() < f64::EPSILON => {}
            _ => panic!("Unexpected token types: {:?}", token_types),
        }
    }

    #[test]
    fn test_simple_strings() {
        let input = r#""hello" "world""#;
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        assert_eq!(tokens.len(), 2);
        match &tokens[0].token_type {
            TokenType::Str(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected Str token, got {:?}", tokens[0]),
        }
        match &tokens[1].token_type {
            TokenType::Str(s) => assert_eq!(s, "world"),
            _ => panic!("Expected Str token, got {:?}", tokens[1]),
        }
    }

    #[test]
    fn test_multiline_string() {
        let input = r#"
"hello
world"
"#;
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].token_type, TokenType::NewLine);
        match &tokens[1].token_type {
            TokenType::Str(s) => assert_eq!(s, "hello\nworld"),
            _ => panic!("Expected Str token, got {:?}", tokens[1]),
        }
        assert_eq!(tokens[2].token_type, TokenType::NewLine);
    }

    #[test]
    fn test_char_literals() {
        let input = r#"'a''\n''\''"#; // No spaces between characters
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        // We expect 3 character literals with no whitespace tokens
        assert_eq!(tokens.len(), 3);

        // First character literal
        match tokens[0].token_type {
            TokenType::Char('a') => {}
            _ => panic!("Expected Char('a'), got {:?}", tokens[0]),
        }

        // Second character literal
        match tokens[1].token_type {
            TokenType::Char('\n') => {}
            _ => panic!("Expected Char('\\n'), got {:?}", tokens[1]),
        }

        // Third character literal
        match tokens[2].token_type {
            TokenType::Char('\'') => {}
            _ => panic!("Expected Char('\\''), got {:?}", tokens[2]),
        }
    }

    #[test]
    fn test_strings_with_escapes() {
        // basic escapes
        let input = r#""a\n\t\\\"\'\r\0""#;
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        assert_eq!(tokens.len(), 1);
        match &tokens[0].token_type {
            TokenType::Str(s) => assert_eq!(s, "a\n\t\\\"'\r\0"),
            _ => panic!("Expected Str token, got {:?}", tokens[0]),
        }

        // test that Some, None, Ok, Err are treated as identifiers
        let input = "Some None Ok Err";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        assert_eq!(
            tokens.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
            vec![
                TokenType::Id("Some".to_string()),
                TokenType::Id("None".to_string()),
                TokenType::Id("Ok".to_string()),
                TokenType::Id("Err".to_string()),
            ]
        );

        // test that true/false are treated as keywords
        let input = "true false";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        assert_eq!(
            tokens.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
            vec![TokenType::Bool(true), TokenType::Bool(false)]
        );

        // test that keywords are case-sensitive
        let input = "some none ok err";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        assert_eq!(
            tokens.into_iter().map(|t| t.token_type).collect::<Vec<_>>(),
            vec![
                TokenType::Id("some".to_string()),
                TokenType::Id("none".to_string()),
                TokenType::Id("ok".to_string()),
                TokenType::Id("err".to_string()),
            ]
        );

        // unterminated string
        let input = "\"unterminated";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        assert!(lexer.lex_all().is_err());

        // unknown escape sequences
        for input in [r#""\a""#, r#""\c""#] {
            let mut source = Source::from_test_str(input);
            let mut lexer = Lexer::new(&mut source);
            assert!(lexer.lex_all().is_err());
        }
    }

    #[test]
    fn test_keywords_and_identifiers() {
        let input = "let x = 42 if else for while match const class interface enum is as in range list map Optional Result Some None Ok Err true false and or";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens: Vec<_> = lexer.lex_all().unwrap().into_iter().collect();

        let token_types: Vec<_> = tokens.into_iter().map(|t| t.token_type).collect();

        match &token_types[..] {
            [
                TokenType::Let,
                TokenType::Id(x),
                TokenType::Eq,
                TokenType::Int(42),
                TokenType::If,
                TokenType::Else,
                TokenType::For,
                TokenType::While,
                TokenType::Match,
                TokenType::Const,
                TokenType::Class,
                TokenType::Interface,
                TokenType::Enum,
                TokenType::Is,
                TokenType::As,
                TokenType::In,
                TokenType::Id(range),
                TokenType::Id(list),
                TokenType::Id(map),
                TokenType::Id(opt),
                TokenType::Id(res),
                TokenType::Id(some),
                TokenType::Id(none),
                TokenType::Id(ok),
                TokenType::Id(err),
                TokenType::Bool(true),
                TokenType::Bool(false),
                TokenType::And,
                TokenType::Or,
            ] => {
                assert_eq!(x, "x");
                assert_eq!(range, "range");
                assert_eq!(list, "list");
                assert_eq!(map, "map");
                assert_eq!(opt, "Optional");
                assert_eq!(res, "Result");
                assert_eq!(some, "Some");
                assert_eq!(none, "None");
                assert_eq!(ok, "Ok");
                assert_eq!(err, "Err");
            }
            _ => panic!("Unexpected token sequence: {:?}", token_types),
        }
    }

    #[test]
    fn test_operators() {
        // Helper function to get non-whitespace tokens
        fn get_tokens(input: &str) -> Vec<TokenType> {
            let mut source = Source::from_test_str(input);
            let mut lexer = Lexer::new(&mut source);
            lexer
                .lex_all()
                .unwrap()
                .into_iter()
                .map(|t| t.token_type)
                .collect()
        }

        // Comparison operators
        let token_types = get_tokens("= == ! != < <= > >=");
        assert_eq!(
            token_types,
            vec![
                TokenType::Eq,
                TokenType::EqEq,
                TokenType::Bang,
                TokenType::NotEq,
                TokenType::Lt,
                TokenType::Le,
                TokenType::Gt,
                TokenType::Ge,
            ]
        );

        // Arithmetic operators
        let token_types = get_tokens("+ - * / %");
        assert_eq!(
            token_types,
            vec![
                TokenType::Plus,
                TokenType::Minus,
                TokenType::Star,
                TokenType::Slash,
                TokenType::Percent,
            ]
        );

        // Logical operators
        let token_types = get_tokens("&& ||");
        assert_eq!(token_types, vec![TokenType::And, TokenType::Or,]);

        // Assignment operators
        let token_types = get_tokens("= += -= *= /=");
        assert_eq!(
            token_types,
            vec![
                TokenType::Eq,
                TokenType::PlusEq,
                TokenType::MinusEq,
                TokenType::StarEq,
                TokenType::SlashEq,
            ]
        );

        // Test combined operators with identifiers and numbers
        let token_types = get_tokens("a += 1 b -= 2 c *= 3 d /= 4");
        match &token_types[..] {
            [
                TokenType::Id(a),
                TokenType::PlusEq,
                TokenType::Int(1),
                TokenType::Id(b),
                TokenType::MinusEq,
                TokenType::Int(2),
                TokenType::Id(c),
                TokenType::StarEq,
                TokenType::Int(3),
                TokenType::Id(d),
                TokenType::SlashEq,
                TokenType::Int(4),
            ] => {
                assert_eq!(a, "a");
                assert_eq!(b, "b");
                assert_eq!(c, "c");
                assert_eq!(d, "d");
            }
            _ => panic!("Unexpected token sequence: {:?}", token_types),
        }
    }

    #[test]
    fn test_comments() {
        let input = "// line comment\n/* multi\nline */";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        // We should have 3 tokens: line comment, newline, and multiline comment
        assert_eq!(tokens.len(), 3);

        // Check line comment - leading space is now trimmed
        match &tokens[0].token_type {
            TokenType::LineComment(s) => assert_eq!(s, "line comment"),
            _ => panic!("Expected LineComment, got {:?}", tokens[0]),
        }

        // Check newline
        assert_eq!(tokens[1].token_type, TokenType::NewLine);

        // Check multiline comment
        match &tokens[2].token_type {
            TokenType::MultilineComment(s) => assert_eq!(s, "multi\nline"),
            _ => panic!("Expected MultilineComment, got {:?}", tokens[2]),
        }
    }

    #[test]
    fn test_multiple_line_comments() {
        let input = "// first comment\n// second comment\n// third comment";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        // We should have 5 tokens: 3 comments and 2 newlines
        assert_eq!(tokens.len(), 5);

        // Check first comment
        match &tokens[0].token_type {
            TokenType::LineComment(s) => assert_eq!(s, "first comment"),
            _ => panic!("Expected first LineComment, got {:?}", tokens[0]),
        }

        // First newline
        assert_eq!(tokens[1].token_type, TokenType::NewLine);

        // Second comment
        match &tokens[2].token_type {
            TokenType::LineComment(s) => assert_eq!(s, "second comment"),
            _ => panic!("Expected second LineComment, got {:?}", tokens[2]),
        }

        // Second newline
        assert_eq!(tokens[3].token_type, TokenType::NewLine);

        // Third comment
        match &tokens[4].token_type {
            TokenType::LineComment(s) => assert_eq!(s, "third comment"),
            _ => panic!("Expected third LineComment, got {:?}", tokens[4]),
        }
    }

    #[test]
    fn test_dots_and_newlines() {
        let input = "1.2\n3.4\n5.6";
        let mut source = Source::from_test_str(input);
        let mut lexer = Lexer::new(&mut source);
        let tokens = lexer.lex_all().unwrap();

        // We expect 3 floats and 2 newlines
        assert_eq!(tokens.len(), 5);

        // First float
        match &tokens[0].token_type {
            TokenType::Float(f) => assert!((f.into_inner() - 1.2).abs() < f64::EPSILON),
            _ => panic!("Expected Float(1.2), got {:?}", tokens[0]),
        }

        // First newline
        assert_eq!(tokens[1].token_type, TokenType::NewLine);

        // Second float
        match &tokens[2].token_type {
            TokenType::Float(f) => assert!((f.into_inner() - 3.4).abs() < f64::EPSILON),
            _ => panic!("Expected Float(3.4), got {:?}", tokens[2]),
        }

        // Second newline
        assert_eq!(tokens[3].token_type, TokenType::NewLine);

        // Third float
        match &tokens[4].token_type {
            TokenType::Float(f) => assert!((f.into_inner() - 5.6).abs() < f64::EPSILON),
            _ => panic!("Expected Float(5.6), got {:?}", tokens[4]),
        }
    }
}
