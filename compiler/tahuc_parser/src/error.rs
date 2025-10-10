use tahuc_diagnostics::{diagnostic::Diagnostic, severity::Severity};
use tahuc_span::{Span, StrId};

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    UnexpectedTopLevel {
        found: StrId,
        span: Span,
    },

    Expected {
        expected: String,
        found: String,
        span: Span,
    },
    ExpectedToken {
        expected: String,
        span: Span,
    },

    Unexpected {
        unexcepted: StrId,
        span: Span,
    },
}

impl ParserError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            ParserError::UnexpectedTopLevel {found, span} => {
                let allowed = "Allowed declarations in top level";
                Diagnostic::new(Severity::Error, format!("{} bot got {}", allowed, found)).with_span(*span)
            }
            ParserError::Expected {expected, found, span} => {
                Diagnostic::new(Severity::Error, format!("Expected {} but got `{}`", expected, found)).with_span(*span)
            }
            ParserError::ExpectedToken {expected, span} => {
                Diagnostic::new(Severity::Error, format!("Expected {}", expected)).with_span(*span)
            }
            ParserError::Unexpected {unexcepted, span} => {
                Diagnostic::new(Severity::Error, format!("Unexpected {}", unexcepted)).with_span(*span)
            }
        }
    }
}