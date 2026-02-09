//! Error types for the lexer.

use super::span::Span;
use crate::diagnostic::{self, Diagnostic, FileId, ToDiagnostic};

/// An error that occurred during lexical analysis.
#[derive(Debug, Clone, PartialEq)]
pub struct LexerError {
    pub message: String,
    pub span: Span,
}

impl LexerError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn with_help(message: impl Into<String>, span: Span, help: impl Into<String>) -> Self {
        Self {
            message: diagnostic::format_with_help(message, help),
            span,
        }
    }
}

impl ToDiagnostic for LexerError {
    fn to_diagnostic(&self, file_id: FileId) -> Diagnostic {
        diagnostic::error_diagnostic(&self.message, self.span, file_id)
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer error at {}:{} - {}",
            self.span.row_start, self.span.col_start, self.message
        )
    }
}

impl std::error::Error for LexerError {}
