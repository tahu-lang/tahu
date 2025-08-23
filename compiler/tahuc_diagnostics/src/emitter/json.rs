use std::io::Write;
use serde::{Deserialize, Serialize};
use tahuc_span::Span;

use crate::{context::DiagnosticContext, diagnostic::Diagnostic, emitter::Emitter, reporter::DiagnosticReporter};

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonDiagnostic {
    pub severity: String,
    pub message: String,
    pub span: Option<JsonSpan>,
    pub suggestions: Vec<JsonSuggestion>,
    pub related: Vec<JsonRelated>,
    pub note: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonSpan {
    pub file: String,
    pub start: JsonPosition,
    pub end: JsonPosition,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonPosition {
    pub line: u32,
    pub column: u32,
    pub offset: u32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonSuggestion {
    pub message: String,
    pub replacements: Vec<JsonReplacement>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonReplacement {
    pub span: JsonSpan,
    pub replacement: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonRelated {
    pub span: JsonSpan,
    pub message: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonOutput {
    pub diagnostics: Vec<JsonDiagnostic>,
    pub summary: JsonSummary,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct JsonSummary {
    pub error_count: usize,
    pub warning_count: usize,
    pub info_count: usize,
}

pub struct JsonEmitter<W: Write> {
    writer: W,
    diagnostics: Vec<JsonDiagnostic>,
}

impl<W: Write> JsonEmitter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            diagnostics: Vec::new(),
        }
    }

    fn convert_span(span: Span, context: &DiagnosticContext) -> Option<JsonSpan> {
        let file = context.get_file(span.file_id)?;
        Some(JsonSpan {
            file: file.path.clone(),
            start: JsonPosition {
                line: span.start.line,
                column: span.start.column,
                offset: span.start.offset,
            },
            end: JsonPosition {
                line: span.end.line,
                column: span.end.column,
                offset: span.end.offset,
            },
        })
    }
}

impl<W: Write> Emitter for JsonEmitter<W> {
    fn emit(&mut self, diagnostic: &Diagnostic, context: &DiagnosticContext) {
        let json_diagnostic = JsonDiagnostic {
            severity: diagnostic.severity.as_str().to_string(),
            message: diagnostic.message.clone(),
            span: diagnostic.span.and_then(|s| Self::convert_span(s, context)),
            suggestions: diagnostic.suggestions.iter().map(|s| JsonSuggestion {
                message: s.message.clone(),
                replacements: s.replacements.iter().filter_map(|r| {
                    Self::convert_span(r.span, context).map(|span| JsonReplacement {
                        span,
                        replacement: r.replacement.clone(),
                    })
                }).collect(),
            }).collect(),
            related: diagnostic.related.iter().filter_map(|r| {
                Self::convert_span(r.span, context).map(|span| JsonRelated {
                    span,
                    message: r.message.clone(),
                })
            }).collect(),
            note: diagnostic.note.clone(),
        };

        self.diagnostics.push(json_diagnostic);
    }

    fn emit_summary(&mut self, reporter: DiagnosticReporter) {
        let error_count = reporter.error_count();
        let warning_count = reporter.warning_count();
        let info_count = reporter.info_count();
        
        let output = JsonOutput {
            diagnostics: std::mem::take(&mut self.diagnostics),
            summary: JsonSummary {
                error_count,
                warning_count,
                info_count,
            },
        };

        serde_json::to_writer_pretty(&mut self.writer, &output).unwrap();
        writeln!(self.writer).unwrap();
    }
}