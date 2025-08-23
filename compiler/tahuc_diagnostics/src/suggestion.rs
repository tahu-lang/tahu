use tahuc_span::Span;

#[derive(Debug, Clone)]
pub struct Suggestion {
    pub message: String,
    pub replacements: Vec<Replacement>,
    pub applies_to: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Replacement {
    pub span: Span,
    pub replacement: String,
}

impl Suggestion {
    pub fn simple_replacement(
        span: Span, 
        message: impl Into<String>, 
        replacement: impl Into<String>
    ) -> Self {
        Self {
            message: message.into(),
            replacements: vec![Replacement {
                span,
                replacement: replacement.into(),
            }],
            applies_to: Some(span),
        }
    }

    pub fn multi_replacement(
        message: impl Into<String>,
        replacements: Vec<Replacement>,
    ) -> Self {
        Self {
            message: message.into(),
            replacements,
            applies_to: None,
        }
    }

    pub fn hint(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            replacements: Vec::new(),
            applies_to: None,
        }
    }

    pub fn insert_before(span: Span, message: impl Into<String>, text: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            replacements: vec![Replacement {
                span: Span::point(span.start, span.file_id),
                replacement: text.into(),
            }],
            applies_to: Some(span),
        }
    }

    pub fn insert_after(span: Span, message: impl Into<String>, text: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            replacements: vec![Replacement {
                span: Span::point(span.end, span.file_id),
                replacement: text.into(),
            }],
            applies_to: Some(span),
        }
    }
}