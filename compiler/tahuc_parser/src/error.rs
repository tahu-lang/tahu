use tahuc_diagnostics::{diagnostic::Diagnostic, severity::Severity};
use tahuc_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    UnexpectedTopLevel {
        found: String,
        span: Span,
    },

    Expected {
        expected: String,
        found: String,
        span: Span,
    },

    Unexpected {
        unexcepted: String,
        span: Span,
    },
}

impl ParserError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            ParserError::UnexpectedTopLevel {found, span} => {
                let allowed = "Allowed declarations(struct, extern function, function, variabel(val, var)) in top level scope";
                Diagnostic::new(Severity::Error, format!("{} bot got {}", allowed, found)).with_span(*span)
            }
            ParserError::Expected {expected, found, span} => {
                Diagnostic::new(Severity::Error, format!("{} {}", expected, found)).with_span(*span)
            }
            ParserError::Unexpected {unexcepted, span} => {
                Diagnostic::new(Severity::Error, format!("Unexpected {}", unexcepted)).with_span(*span)
            }
        }
    }
}