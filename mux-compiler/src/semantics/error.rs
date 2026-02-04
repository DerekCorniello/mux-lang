use crate::diagnostic::{Diagnostic, FileId, Label};
use crate::lexer::Span;
use crate::semantics::format::format_span_location;

// represents a semantic error with location information
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
            message: format!("{}\n  = help: {}", message.into(), help.into()),
            span,
        }
    }

    #[allow(unused)]
    pub fn with_suggestion(message: impl Into<String>, span: Span, suggestion: &str) -> Self {
        Self {
            message: format!("{}\n  = help: {}", message.into(), suggestion),
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
