//! Diagnostic emitter for formatted error output.

use super::{ColorConfig, Diagnostic, Files, LabelStyle, Level, Styles};
use crate::lexer::Span;
use std::cmp::{max, min};

/// Trait for emitting diagnostics to output.
pub trait DiagnosticEmitter {
    fn emit(&self, diagnostic: &Diagnostic, files: &Files);
    fn emit_batch(&self, diagnostics: &[Diagnostic], files: &Files);
}

/// Standard diagnostic emitter with Rust-style formatting.
pub struct StandardEmitter {
    pub styles: Styles,
}

impl StandardEmitter {
    pub fn new(config: ColorConfig) -> Self {
        Self {
            styles: Styles::new(config),
        }
    }

    /// Get the line number width for proper alignment.
    fn line_number_width(&self, max_line: usize) -> usize {
        max_line.to_string().len().max(2)
    }

    /// Render a single line of source with line number.
    fn render_source_line(&self, line_number: usize, line_content: &str, width: usize) -> String {
        let line_num_str =
            self.styles
                .line_number(&format!("{:width$}", line_number, width = width));
        format!("{} | {}", line_num_str, line_content.trim_end())
    }

    /// Render the gutter (line number column) without source.
    fn render_gutter(&self, width: usize) -> String {
        format!("{:width$} |", "", width = width)
    }

    /// Render underline/caret indicators for a label.
    fn render_label_underline(
        &self,
        span: &Span,
        line_number: usize,
        line_content: &str,
        style: LabelStyle,
        width: usize,
    ) -> String {
        let gutter = self.render_gutter(width);

        // Calculate column positions
        let start_col = if span.row_start == line_number {
            span.col_start.saturating_sub(1)
        } else {
            0
        };

        let end_col = if let Some(end_line) = span.row_end {
            if end_line == line_number {
                span.col_end.unwrap_or(span.col_start).saturating_sub(1)
            } else {
                line_content.len()
            }
        } else {
            // Single position span - just show caret
            start_col
        };

        let underline_len = (end_col.saturating_sub(start_col)).max(1);
        let indicator = "^".repeat(underline_len);
        let colored_indicator = match style {
            LabelStyle::Primary => self.styles.primary_label(&indicator),
            LabelStyle::Secondary => self.styles.secondary_label(&indicator),
        };

        format!("{} {}{}", gutter, " ".repeat(start_col), colored_indicator)
    }

    /// Emit a single diagnostic with source context.
    fn emit_single(&self, diagnostic: &Diagnostic, source: &str, file_path: &str) {
        // Print error header
        let level_str = match diagnostic.level {
            Level::Error => self.styles.error("error"),
            Level::Warning => self.styles.warning("warning"),
            Level::Note => self.styles.note("note"),
            Level::Help => self.styles.help("help"),
        };

        eprintln!("{}: {}", level_str, self.styles.bold(&diagnostic.message));

        // If no labels, just show the message
        if diagnostic.labels.is_empty() {
            eprintln!();
            return;
        }

        let lines: Vec<&str> = source.lines().collect();

        // Find the range of lines we need to display
        let mut min_line = usize::MAX;
        let mut max_line = 0;

        for label in &diagnostic.labels {
            min_line = min(min_line, label.span.row_start);
            max_line = max(max_line, label.span.row_end.unwrap_or(label.span.row_start));
        }

        let width = self.line_number_width(max_line);

        // Print file location
        eprintln!(
            "{} {}:{}:{}",
            self.styles.dim("-->"),
            file_path,
            min_line,
            diagnostic
                .labels
                .first()
                .map(|l| l.span.col_start)
                .unwrap_or(1)
        );

        // Print gutter separator
        eprintln!("{}", self.render_gutter(width));

        // Print each relevant line with labels
        for line_num in min_line..=max_line {
            let line_idx = line_num.saturating_sub(1);

            if line_idx >= lines.len() {
                break;
            }

            let line_content = lines[line_idx];

            // Print the source line
            eprintln!("{}", self.render_source_line(line_num, line_content, width));

            // Print underlines for labels on this line
            for label in &diagnostic.labels {
                let label_covers_line = label.span.row_start == line_num
                    || label
                        .span
                        .row_end
                        .is_some_and(|end| end >= line_num && label.span.row_start <= line_num);
                if label_covers_line {
                    eprintln!(
                        "{}",
                        self.render_label_underline(
                            &label.span,
                            line_num,
                            line_content,
                            label.style,
                            width
                        )
                    );

                    // Print label message if present
                    if let Some(ref msg) = label.message {
                        let colored_msg = match label.style {
                            LabelStyle::Primary => self.styles.primary_label(msg),
                            LabelStyle::Secondary => self.styles.secondary_label(msg),
                        };
                        eprintln!("{} {}", self.render_gutter(width), colored_msg);
                    }
                }
            }
        }

        // Print help text if present
        if let Some(ref help) = diagnostic.help {
            eprintln!("{}", self.render_gutter(width));
            eprintln!(
                "{} {} {}",
                self.styles.dim("="),
                self.styles.help("help:"),
                help
            );
        }

        eprintln!();
    }
}

impl Default for StandardEmitter {
    fn default() -> Self {
        Self::new(ColorConfig::Auto)
    }
}

impl DiagnosticEmitter for StandardEmitter {
    fn emit(&self, diagnostic: &Diagnostic, files: &Files) {
        let file_id = diagnostic.file_id.expect("Diagnostic must have a file_id");
        let file_info = files.get(file_id).expect("FileId must exist in Files");
        let file_path = file_info.path.to_string_lossy();
        let source = &file_info.source;

        self.emit_single(diagnostic, source, &file_path);
    }

    fn emit_batch(&self, diagnostics: &[Diagnostic], files: &Files) {
        // Group diagnostics by file
        use std::collections::HashMap;
        let mut by_file: HashMap<String, Vec<&Diagnostic>> = HashMap::new();

        for diagnostic in diagnostics {
            if let Some(file_id) = diagnostic.file_id {
                if let Some(file_info) = files.get(file_id) {
                    let file = file_info.path.to_string_lossy().to_string();
                    by_file.entry(file).or_default().push(diagnostic);
                }
            }
        }

        // Count total errors
        let error_count = diagnostics
            .iter()
            .filter(|d| d.level == Level::Error)
            .count();

        if error_count > 1 {
            eprintln!(
                "{}: {} errors found\n",
                self.styles.error("error"),
                error_count
            );
        }

        // Emit diagnostics grouped by file
        for (file_path, file_diagnostics) in by_file {
            for diagnostic in file_diagnostics {
                if let Some(file_id) = diagnostic.file_id {
                    if let Some(file_info) = files.get(file_id) {
                        self.emit_single(diagnostic, &file_info.source, &file_path);
                    }
                }
            }
        }
    }
}
