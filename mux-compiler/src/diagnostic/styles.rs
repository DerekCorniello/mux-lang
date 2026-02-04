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

    pub fn error(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[1;31m{}\x1b[0m", text) // Bold red
        } else {
            text.to_string()
        }
    }

    pub fn warning(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[1;33m{}\x1b[0m", text) // Bold yellow
        } else {
            text.to_string()
        }
    }

    pub fn success(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[1;32m{}\x1b[0m", text) // Bold green
        } else {
            text.to_string()
        }
    }

    pub fn note(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[1;36m{}\x1b[0m", text) // Bold cyan
        } else {
            text.to_string()
        }
    }

    pub fn help(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[1;36m{}\x1b[0m", text) // Bold cyan
        } else {
            text.to_string()
        }
    }

    pub fn bold(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[1m{}\x1b[0m", text)
        } else {
            text.to_string()
        }
    }

    pub fn dim(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[2m{}\x1b[0m", text)
        } else {
            text.to_string()
        }
    }

    pub fn primary_label(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[31m{}\x1b[0m", text) // Red (not bold)
        } else {
            text.to_string()
        }
    }

    pub fn secondary_label(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[34m{}\x1b[0m", text) // Blue
        } else {
            text.to_string()
        }
    }

    pub fn line_number(&self, text: &str) -> String {
        if self.enabled {
            format!("\x1b[34;1m{}\x1b[0m", text) // Bold blue
        } else {
            text.to_string()
        }
    }
}

impl Default for Styles {
    fn default() -> Self {
        Self::new(ColorConfig::Auto)
    }
}
