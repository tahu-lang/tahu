use std::io::{self, Write};

use crate::{context::DiagnosticContext, diagnostic::Diagnostic, emitter::Emitter, reporter::DiagnosticReporter, severity::Severity};

pub struct TerminalEmitter<W: Write> {
    writer: W,
    use_colors: bool,
}

impl<W: Write> TerminalEmitter<W> {
    pub fn new(writer: W, use_colors: bool) -> Self {
        Self { writer, use_colors }
    }

    fn write_colored(&mut self, text: &str, color: &str) -> io::Result<()> {
        if self.use_colors {
            write!(self.writer, "{}{}{}", color, text, Severity::reset_code())?;
        } else {
            write!(self.writer, "{}", text)?;
        }
        Ok(())
    }

    fn emit_source_snippet(&mut self, diagnostic: &Diagnostic, context: &DiagnosticContext) -> io::Result<()> {
        let Some(span) = diagnostic.span else { return Ok(()) };
        let Some(file) = context.get_file(span.file_id) else { return Ok(()) };

        // File location
        writeln!(self.writer, "   --> {}:{}:{}", 
            file.path, span.start.line, span.start.column)?;
        writeln!(self.writer, "    |")?;

        // Handle multi-line spans
        let start_line = span.start.line;
        let end_line = span.end.line;

        for line_num in start_line..=end_line {
            let Some(line_content) = file.line_content(line_num) else { continue };
            
            // Line number and content
            write!(self.writer, "{:3} | ", line_num)?;
            writeln!(self.writer, "{}", line_content.trim_end())?;
            
            // Underline for this line
            write!(self.writer, "    | ")?;
            
            let (start_col, end_col) = if start_line == end_line {
                // Single line case - exact columns
                ((span.start.column as usize).saturating_sub(1), 
                 (span.end.column as usize).saturating_sub(1))
            } else if line_num == start_line {
                // First line of multi-line span - from start column to end of line
                ((span.start.column as usize).saturating_sub(1), 
                 line_content.trim_end().len())
            } else if line_num == end_line {
                // Last line of multi-line span - from beginning to end column
                (0, (span.end.column as usize).saturating_sub(1))
            } else {
                // Middle lines of multi-line span - entire line
                (0, line_content.trim_end().len())
            };
            
            // Add spaces before underline
            for _ in 0..start_col {
                write!(self.writer, " ")?;
            }
            
            // Draw underline
            let underline_len = (end_col - start_col).max(1);
            if self.use_colors {
                write!(self.writer, "{}", diagnostic.severity.color_code())?;
            }
            
            for _ in 0..underline_len {
                write!(self.writer, "^")?;
            }
            
            if self.use_colors {
                write!(self.writer, "{}", Severity::reset_code())?;
            }
            
            // Show message only on the first line
            if line_num == start_line {
                write!(self.writer, " {}", diagnostic.message)?;
            }
            
            writeln!(self.writer)?;
        }

        Ok(())
    }
}

impl<W: Write> Emitter for TerminalEmitter<W> {
    fn emit(&mut self, diagnostic: &Diagnostic, context: &DiagnosticContext) {
        // Error header
        self.write_colored(diagnostic.severity.as_str(), diagnostic.severity.color_code()).unwrap();
        writeln!(self.writer, ": {}", diagnostic.message).unwrap();

        // Source snippet
        self.emit_source_snippet(diagnostic, context).unwrap();

        // Related diagnostics
        for related in &diagnostic.related {
            writeln!(self.writer, "   |").unwrap();
            write!(self.writer, "note: ").unwrap();
            if let Some(file) = context.get_file(related.span.file_id) {
                writeln!(self.writer, "{} at {}:{}:{}", 
                    related.message, file.path, related.span.start.line, related.span.start.column).unwrap();
            } else {
                writeln!(self.writer, "{}", related.message).unwrap();
            }
        }

        // Note
        if let Some(note) = &diagnostic.note {
            writeln!(self.writer, "    |").unwrap();
            writeln!(self.writer, "    = note: {}", note).unwrap();
        }

        // Suggestions
        for suggestion in &diagnostic.suggestions {
            writeln!(self.writer, "    |").unwrap();
            self.write_colored("    = help: ", "\x1b[36m").unwrap();
            write!(self.writer, "{}", suggestion.message).unwrap();
            write!(self.writer, " ({} replacement{}): ", 
                suggestion.replacements.len(), 
                if suggestion.replacements.len() == 1 { "" } else { "s" }).unwrap();
            for (i, replacement) in suggestion.replacements.iter().enumerate() {
                if i > 0 {
                    write!(self.writer, ", ").unwrap();
                }
                let span = replacement.span;
                if let Some(file) = context.get_file(span.file_id) {
                    write!(self.writer, "{}:{}:{}", 
                        file.path, span.start.line, span.start.column).unwrap();
                } else {
                    write!(self.writer, "unknown location").unwrap();
                }
                write!(self.writer, " -> {}", replacement.replacement).unwrap();
                
                if i < suggestion.replacements.len() - 1 {
                    write!(self.writer, "; ").unwrap();
                }

            }
        }

        writeln!(self.writer).unwrap();
    }

    fn emit_summary(&mut self, reporter: DiagnosticReporter) {
        let error_count = reporter.error_count();
        let warning_count = reporter.warning_count();
        
        if error_count > 0 {
            self.write_colored(&format!("Compilation failed: {} error{}", 
                error_count, if error_count == 1 { "" } else { "s" }), "\x1b[31m").unwrap();
            if warning_count > 0 {
                write!(self.writer, ", {} warning{}", 
                    warning_count, if warning_count == 1 { "" } else { "s" }).unwrap();
            }
            writeln!(self.writer).unwrap();
        } else if warning_count > 0 {
            self.write_colored(&format!("Compilation succeeded with {} warning{}", 
                warning_count, if warning_count == 1 { "" } else { "s" }), "\x1b[33m").unwrap();
            writeln!(self.writer).unwrap();
        } else {
            self.write_colored("Compilation succeeded", "\x1b[32m").unwrap();
            writeln!(self.writer).unwrap();
        }
    }
}

pub fn stderr_emitter() -> TerminalEmitter<io::Stderr> {
    let use_colors = std::io::IsTerminal::is_terminal(&io::stderr());
    TerminalEmitter::new(io::stderr(), use_colors)
}