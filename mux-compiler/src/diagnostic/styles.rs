//! ANSI color codes and styling for diagnostic output.

use std::io::IsTerminal;

/// Configuration for color output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorConfig {
    Auto,
}

impl ColorConfig {
    pub fn should_colorize(&self) -> bool {
        match self {
            ColorConfig::Auto => std::io::stderr().is_terminal(),
        }
    }
}

/// ANSI escape code wrapper for styled text output.
pub struct Styles {
    enabled: bool,
}

impl Styles {
    pub fn new(config: ColorConfig) -> Self {
        Self {
            enabled: config.should_colorize(),
        }
    }

    fn styled(&self, code: &str, text: &str) -> String {
        if self.enabled {
            format!("\x1b[{}m{}\x1b[0m", code, text)
        } else {
            text.to_string()
        }
    }

    pub fn error(&self, text: &str) -> String {
        self.styled("1;31", text)
    }

    pub fn warning(&self, text: &str) -> String {
        self.styled("1;33", text)
    }

    pub fn success(&self, text: &str) -> String {
        self.styled("1;32", text)
    }

    pub fn note(&self, text: &str) -> String {
        self.styled("1;36", text)
    }

    pub fn help(&self, text: &str) -> String {
        self.styled("1;36", text)
    }

    pub fn bold(&self, text: &str) -> String {
        self.styled("1", text)
    }

    pub fn dim(&self, text: &str) -> String {
        self.styled("2", text)
    }

    pub fn primary_label(&self, text: &str) -> String {
        self.styled("31", text)
    }

    pub fn secondary_label(&self, text: &str) -> String {
        self.styled("34", text)
    }

    pub fn line_number(&self, text: &str) -> String {
        self.styled("34;1", text)
    }
}

impl Default for Styles {
    fn default() -> Self {
        Self::new(ColorConfig::Auto)
    }
}
