//! Parser error types.

use crate::ast::ParseError;
use crate::diagnostic::{self, Diagnostic, FileId, ToDiagnostic};
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

    pub fn with_help(message: impl Into<String>, span: Span, help: impl Into<String>) -> Self {
        Self {
            message: diagnostic::format_with_help(message, help),
            span,
        }
    }
}

impl ToDiagnostic for ParserError {
    fn to_diagnostic(&self, file_id: FileId) -> Diagnostic {
        diagnostic::error_diagnostic(&self.message, self.span, file_id)
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
