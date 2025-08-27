use std::io::{Error, ErrorKind};
use std::path::Path;

pub struct Source {
    input: String,
    pos: usize, // position in bytes
    line: usize,
    col: usize,
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
        assert_eq!(src.next_char(), Some('c'));
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

        assert_eq!(src.peek(), Some('x')); // peek doesn’t advance pos
        assert_eq!(src.next_char(), Some('x'));
        assert_eq!(src.peek(), Some('y'));
        assert_eq!(src.next_char(), Some('y'));
        assert_eq!(src.peek(), None);
    }
}
