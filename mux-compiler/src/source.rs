use std::io::{Error, ErrorKind};
use std::path::Path;

pub struct Source {
    pub input: String,
    pub pos: usize, // position in bytes
    pub line: usize,
    pub col: usize,
}

impl Source {
    pub fn new(file_path: &str) -> std::io::Result<Self> {
        if Path::new(file_path).exists() {
            // if file doesnt exist or there is an issue opening
            // it, then it will return the error with the ? op
            let input = std::fs::read_to_string(file_path)?;
            return Ok(Self {
                input,
                pos: 0,
                line: 1,
                col: 1,
            });
        }
        Err(Error::new(ErrorKind::NotFound, "File does not exist"))
    }

    // create a source from a test snippet
    pub fn from_test_str(string: &str) -> std::io::Result<Self> {
        Ok(Self {
            input: string.to_string(), // FIXED: was converting to Vec<char> instead of String
            pos: 0,
            line: 1,
            col: 1,
        })
    }

    // get next char and move pos
    pub fn next_char(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        }
        let ch = self.input[self.pos..].chars().next()?;
        let char_len = ch.len_utf8();
        self.pos += char_len;

        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        Some(ch)
    }

    // gets next char without consuming
    pub fn peek(&self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        }
        self.input[self.pos..].chars().next()
    }

    // consume characters until we see the closing */ or eof
    pub fn consume_multiline_comment(&mut self) -> String {
        let mut buf = String::new();
        
        while let Some(c) = self.next_char() {
            if c == '*' && self.peek() == Some('/') {
                self.next_char(); // eat the '/'
                break; // Don't add the */ to the buffer
            } else {
                buf.push(c);
            }
        }
        buf.trim().to_string()
    }

    // consume chars til we get to the stop char
    pub fn consume_until(&mut self, stop: char) -> String {
        let mut buf = String::new();
        while let Some(c) = self.peek() {
            if c == stop {
                break;
            }
            buf.push(c);
            self.next_char();
        }
        buf
    }

    pub fn get_location_string(&self) -> String {
        format!("line {}, column {}", self.line, self.col)
    }
}

#[cfg(test)]
mod tests {
    use super::Source;

    #[test]
    fn test_next_char_basic() {
        let mut src = Source {
            input: "abc\n".to_string(),
            pos: 0,
            line: 1,
            col: 1,
        };

        assert_eq!(src.next_char(), Some('a'));
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 2);

        assert_eq!(src.next_char(), Some('b'));
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 3);

        assert_eq!(src.next_char(), Some('c'));
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 4);

        assert_eq!(src.next_char(), Some('\n'));
        assert_eq!(src.line, 2);
        assert_eq!(src.col, 1);

        assert_eq!(src.next_char(), None);
    }

    #[test]
    fn test_peek() {
        let mut src = Source {
            input: "xy".to_string(),
            pos: 0,
            line: 1,
            col: 1,
        };

        assert_eq!(src.peek(), Some('x')); // peek doesn't advance pos
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 1); // position unchanged after peek

        assert_eq!(src.next_char(), Some('x'));
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 2);

        assert_eq!(src.peek(), Some('y'));
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 2); // position unchanged after peek

        assert_eq!(src.next_char(), Some('y'));
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 3);

        assert_eq!(src.peek(), None);
    }

    #[test]
    fn test_multiline_positioning() {
        let mut src = Source::from_test_str("hello\nworld\n").unwrap();

        // Test that positions track correctly across multiple lines
        for expected_char in "hello".chars() {
            assert_eq!(src.next_char(), Some(expected_char));
        }
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 6);

        assert_eq!(src.next_char(), Some('\n')); // newline
        assert_eq!(src.line, 2);
        assert_eq!(src.col, 1);

        for expected_char in "world".chars() {
            assert_eq!(src.next_char(), Some(expected_char));
        }
        assert_eq!(src.line, 2);
        assert_eq!(src.col, 6);
    }

    #[test]
    fn test_consume_until() {
        let mut src = Source::from_test_str("hello world").unwrap();

        let result = src.consume_until(' ');
        assert_eq!(result, "hello");
        assert_eq!(src.line, 1);
        assert_eq!(src.col, 6); // position should be at the space
        assert_eq!(src.peek(), Some(' ')); // space should still be there
    }

    #[test]
    fn test_consume_multiline_comment() {
        // the multiline comment will already pass in the text 
        // without the opening comment, thus the missing /*
        let mut src = Source::from_test_str(" hello\nworld*/").unwrap();

        let result = src.consume_multiline_comment();
        assert_eq!(result, "hello\nworld");
        assert_eq!(src.line, 2); // should be on line 2 after the newline
        assert_eq!(src.col, 8); // after "world */"
    }

    #[test]
    fn test_unicode_handling() {
        let mut src = Source::from_test_str("héllo 🌟").unwrap();

        assert_eq!(src.next_char(), Some('h'));
        assert_eq!(src.col, 2);

        assert_eq!(src.next_char(), Some('é')); // multi-byte char
        assert_eq!(src.col, 3); // column should increment by 1, not byte count

        // Skip to the emoji
        src.next_char(); // l
        src.next_char(); // l  
        src.next_char(); // o
        src.next_char(); // space

        assert_eq!(src.next_char(), Some('🌟')); // 4-byte emoji
        assert_eq!(src.col, 8); // should still increment by 1 column
    }

    #[test]
    fn test_from_test_str_type_consistency() {
        let src = Source::from_test_str("test").unwrap();
        assert_eq!(src.input, "test".to_string()); // Should be String, not Vec<char>
    }
}
