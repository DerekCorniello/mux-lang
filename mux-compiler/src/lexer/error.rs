//! Error types for the lexer.

use super::span::Span;
use crate::diagnostic::{Diagnostic, FileId, Label};

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
