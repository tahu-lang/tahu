use crate::{context::DiagnosticContext, diagnostic::Diagnostic, emitter::Emitter, severity::Severity};

#[derive(Debug, Clone)]
pub struct DiagnosticReporter {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
    info_count: usize,
    max_diagnostics: usize,
}

impl DiagnosticReporter {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            error_count: 0,
            warning_count: 0,
            info_count: 0,
            max_diagnostics: 1000, // Default limit, can be adjusted
        }
    }

    pub fn report(&mut self, diagnostic: Diagnostic) {
        if self.diagnostics.len() >= self.max_diagnostics {
            eprintln!("Warning: Maximum number of diagnostics reached ({})", self.max_diagnostics);
            return;
        }
        match diagnostic.severity {
            Severity::Error | Severity::Fatal => self.error_count += 1,
            Severity::Warning => self.warning_count += 1,
            Severity::Info => self.info_count += 1,
            _ => {}
        }
        self.diagnostics.push(diagnostic);
    }

    pub fn emit_all<E: Emitter>(&self, emitter: &mut E, context: &DiagnosticContext) {
        for diagnostic in &self.diagnostics {
            emitter.emit(diagnostic, context);
        }
    }

    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    pub fn has_warnings(&self) -> bool {
        self.warning_count > 0
    }

    pub fn error_count(&self) -> usize {
        self.error_count
    }

    pub fn warning_count(&self) -> usize {
        self.warning_count
    }

    pub fn info_count(&self) -> usize {
        self.info_count
    }

    pub fn total_count(&self) -> usize {
        self.diagnostics.len()
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear();
        self.error_count = 0;
        self.warning_count = 0;
        self.info_count = 0;
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

impl Default for DiagnosticReporter {
    fn default() -> Self {
        Self::new()
    }
}