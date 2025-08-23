use std::io::Write;

use crate::{context::DiagnosticContext, diagnostic::Diagnostic, emitter::Emitter, reporter::DiagnosticReporter};

pub struct CompactEmitter<W: Write> {
    writer: W,
}

impl<W: Write> CompactEmitter<W> {
    pub fn new(writer: W) -> Self {
        Self { writer }
    }
}

impl<W: Write> Emitter for CompactEmitter<W> {
    fn emit(&mut self, diagnostic: &Diagnostic, context: &DiagnosticContext) {
        if let Some(span) = diagnostic.span {
            if let Some(file) = context.get_file(span.file_id) {
                writeln!(self.writer, "{}:{}:{}: [{}]: {}", 
                    file.path, 
                    span.start.line, 
                    span.start.column,
                    diagnostic.severity.as_str(),
                    diagnostic.message
                ).unwrap();
            }
        } else {
            writeln!(self.writer, "[{}]: {}", 
                diagnostic.severity.as_str(),
                diagnostic.message
            ).unwrap();
        }
    }

    fn emit_summary(&mut self, reporter: DiagnosticReporter) {
        let error_count = reporter.error_count();
        let warning_count = reporter.warning_count();
        
        if error_count > 0 {
            writeln!(self.writer, "Compilation failed: {} errors, {} warnings", 
                error_count, warning_count).unwrap();
        } else if warning_count > 0 {
            writeln!(self.writer, "Compilation succeeded: {} warnings", warning_count).unwrap();
        } else {
            writeln!(self.writer, "Compilation succeeded").unwrap();
        }
    }
}