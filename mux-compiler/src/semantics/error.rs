use crate::diagnostic::{self, Diagnostic, FileId, ToDiagnostic};
use crate::lexer::Span;
use crate::semantics::format::format_span_location;

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticError {
    pub message: String,
    pub span: Span,
}

impl SemanticError {
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

impl ToDiagnostic for SemanticError {
    fn to_diagnostic(&self, file_id: FileId) -> Diagnostic {
        diagnostic::error_diagnostic(&self.message, self.span, file_id)
    }
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Semantic error at {}: {}",
            format_span_location(&self.span),
            self.message
        )
    }
}

impl std::error::Error for SemanticError {}
