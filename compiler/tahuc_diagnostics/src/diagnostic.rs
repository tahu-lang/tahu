use tahuc_span::Span;

use crate::{severity::Severity, suggestion::Suggestion};

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<Span>,
    pub suggestions: Vec<Suggestion>,
    pub related: Vec<RelatedDiagnostic>,
    pub note: Option<String>,
}

#[derive(Debug, Clone)]
pub struct RelatedDiagnostic {
    pub span: Span,
    pub message: String,
}

impl Diagnostic {
    pub fn new(severity: Severity, message: impl Into<String>) -> Self {
        Self {
            severity,
            message: message.into(),
            span: None,
            suggestions: Vec::new(),
            related: Vec::new(),
            note: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self::new(Severity::Error, message)
    }

    pub fn warning(message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, message)
    }

    pub fn info(message: impl Into<String>) -> Self {
        Self::new(Severity::Info, message)
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    pub fn with_suggestions(mut self, suggestions: Vec<Suggestion>) -> Self {
        self.suggestions.extend(suggestions);
        self
    }

    pub fn with_related(mut self, span: Span, message: impl Into<String>) -> Self {
        self.related.push(RelatedDiagnostic {
            span,
            message: message.into(),
        });
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }

    pub fn has_fixes(&self) -> bool {
        self.suggestions.iter().any(|s| !s.replacements.is_empty())
    }
}
