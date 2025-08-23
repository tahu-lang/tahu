use crate::{context::DiagnosticContext, diagnostic::Diagnostic, reporter::DiagnosticReporter};

pub mod terminal;
pub mod json;
pub mod compact;

pub trait Emitter {
    fn emit(&mut self, diagnostic: &Diagnostic, context: &DiagnosticContext);
    fn emit_summary(&mut self, reporter: DiagnosticReporter);
}