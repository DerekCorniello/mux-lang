//! Parser error types.

use crate::ast::ParseError;
use crate::diagnostic::{Diagnostic, FileId, Label};
use crate::lexer::{Span, Token};

/// The result type for parser operations.
pub type ParserResult<T> = Result<T, ParserError>;

/// An error that occurred during parsing.
#[derive(Debug, Clone, PartialEq)]
pub struct ParserError {
    pub message: String,
    pub span: Span,
}

impl ParserError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn from_token(message: impl Into<String>, token: &Token) -> Self {
        Self {
            message: message.into(),
            span: token.span,
        }
    }

    #[allow(unused)]
    pub fn with_help(message: impl Into<String>, span: Span, help: impl Into<String>) -> Self {
        Self {
            message: format!("{}\n  = help: {}", message.into(), help.into()),
            span,
        }
    }

    /// Convert to a Diagnostic for formatted output.
    pub fn to_diagnostic(&self, file_id: FileId) -> Diagnostic {
        // Split message and help if present
        let parts: Vec<&str> = self.message.splitn(2, "\n  = help: ").collect();
        let main_message = parts[0];
        let help_message = parts.get(1).copied();

        Diagnostic::error()
            .with_message(main_message)
            .with_label(Label::primary(self.span, ""))
            .with_help(help_message)
            .with_file_id(file_id)
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Parser error at {}:{} - {}",
            self.span.row_start, self.span.col_start, self.message
        )
    }
}

impl std::error::Error for ParserError {}

impl From<ParseError> for ParserError {
    fn from(err: ParseError) -> Self {
        Self {
            message: err.message,
            span: err.span,
        }
    }
}
