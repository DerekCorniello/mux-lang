//! Diagnostic system for error reporting and formatting.
//!
//! Provides centralized diagnostics inspired by Rust's error formatting.
//! Supports color-coded output, multi-line span highlighting, and grouped error reporting.

mod emitter;
mod files;
mod styles;

pub use emitter::{DiagnosticEmitter, StandardEmitter};
pub use files::{FileId, Files};
pub use styles::{ColorConfig, Styles};

use crate::lexer::Span;

/// Help text separator embedded in error messages.
const HELP_SEPARATOR: &str = "\n  = help: ";

/// The severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Level {
    Error,
    Warning,
    Note,
    Help,
}

/// The style of a label (primary or secondary).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum LabelStyle {
    Primary,
    Secondary,
}

/// A label that points to a specific span in the source code.
#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: Option<String>,
    pub style: LabelStyle,
}

impl Label {
    pub fn primary(span: Span, message: impl Into<String>) -> Self {
        let msg = message.into();
        Self {
            span,
            message: if msg.is_empty() { None } else { Some(msg) },
            style: LabelStyle::Primary,
        }
    }

    pub fn secondary(span: Span, message: impl Into<String>) -> Self {
        let msg = message.into();
        Self {
            span,
            message: if msg.is_empty() { None } else { Some(msg) },
            style: LabelStyle::Secondary,
        }
    }
}

/// A diagnostic message with associated labels and help text.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub labels: Vec<Label>,
    pub help: Option<String>,
    pub file_id: Option<FileId>,
}

impl Diagnostic {
    pub fn error() -> Self {
        Self {
            level: Level::Error,
            message: String::new(),
            labels: Vec::new(),
            help: None,
            file_id: None,
        }
    }

    // the next two functions are not currently used, but they are provided for completeness and
    // future use, do not remove them
    #[allow(dead_code)]
    pub fn warning() -> Self {
        Self {
            level: Level::Warning,
            message: String::new(),
            labels: Vec::new(),
            help: None,
            file_id: None,
        }
    }

    #[allow(dead_code)]
    pub fn note() -> Self {
        Self {
            level: Level::Note,
            message: String::new(),
            labels: Vec::new(),
            help: None,
            file_id: None,
        }
    }
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    pub fn with_help(mut self, help: Option<impl Into<String>>) -> Self {
        self.help = help.map(|h| h.into());
        self
    }

    pub fn with_file_id(mut self, file_id: FileId) -> Self {
        self.file_id = Some(file_id);
        self
    }
}

/// Trait for types that can be converted to a diagnostic.
pub trait ToDiagnostic {
    fn to_diagnostic(&self, file_id: FileId) -> Diagnostic;
}

/// Build a diagnostic from a message+span pair, splitting embedded help text.
pub fn error_diagnostic(message: &str, span: Span, file_id: FileId) -> Diagnostic {
    let (main_message, help_message) = split_help_text(message);
    Diagnostic::error()
        .with_message(main_message)
        .with_label(Label::primary(span, ""))
        .with_help(help_message)
        .with_file_id(file_id)
}

/// Format a message string with embedded help text (using the `\n  = help: ` separator).
pub fn format_with_help(message: impl Into<String>, help: impl Into<String>) -> String {
    format!("{}{}{}", message.into(), HELP_SEPARATOR, help.into())
}

/// Split a message string into the main message and optional help text.
fn split_help_text(message: &str) -> (&str, Option<&str>) {
    let parts: Vec<&str> = message.splitn(2, HELP_SEPARATOR).collect();
    (parts[0], parts.get(1).copied())
}
