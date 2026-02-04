//! Diagnostic system for error reporting and formatting.
//!
//! This module provides a centralized diagnostic system inspired by Rust's error formatting.
//! It supports color-coded output, multi-line span highlighting, and grouped error reporting.

mod emitter;
mod files;
mod styles;

pub use emitter::{DiagnosticEmitter, StandardEmitter};
pub use files::{FileId, Files};
pub use styles::{ColorConfig, Styles};

use crate::lexer::Span;

/// The severity level of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Level {
    Error,
    Warning,
    Note,
    Help,
}

impl Level {
    #[allow(dead_code)]
    pub fn as_str(&self) -> &'static str {
        match self {
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Note => "note",
            Level::Help => "help",
        }
    }
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

    #[allow(dead_code)]
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
