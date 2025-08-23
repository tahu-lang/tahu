use tahuc_diagnostics::{diagnostic::Diagnostic, suggestion::Suggestion};
use tahuc_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    UnterminatedString { span: Span },
    InvalidEscapeSequence(char),
    InvalidUnicodeEscape,
    UnexpectedEof,
    InvalidTemplateExpression,
    UnbalancedBraces,
    InvalidFloatFormat,
    IntegerOverflow,
    InvalidBinaryLiteral,
    InvalidOctalLiteral,
    InvalidHexLiteral,
}

impl LexerError {
    pub fn to_diagnostic(&self, span: Span) -> Diagnostic {
        match self {
            LexerError::UnexpectedCharacter(ch) => {
                Diagnostic::error(format!("Unexpected character: {}", ch)).with_span(span)
            }
            LexerError::UnterminatedString { span } => Diagnostic::error("Unterminated string")
                .with_span(*span)
                .with_suggestion(Suggestion::insert_after(*span, "Add closing quote", "\""))
                .with_note("String literals must be closed with a matching quote"),
            LexerError::InvalidEscapeSequence(ch) => {
                Diagnostic::error(format!("Invalid escape sequence '\\{}'", ch))
                    .with_span(span)
                    .with_note("Valid escape sequences are: \\n, \\r, \\t, \\\\, \\\", \\', \\0")
            }
            LexerError::InvalidUnicodeEscape => {
                Diagnostic::error("Invalid unicode escape sequence")
                    .with_span(span)
                    .with_note("Unicode escapes must be in format \\u{XXXX} where X is a hex digit")
            }
            LexerError::UnexpectedEof => {
                Diagnostic::error("Unexpected end of file")
                    .with_span(span)
                    .with_note("The file ended unexpectedly while parsing a token")
            }
            LexerError::InvalidTemplateExpression => {
                Diagnostic::error("Invalid expression in template string interpolation")
                    .with_span(span)
                    .with_note("Template expressions must be valid syntax")
            }
            LexerError::UnbalancedBraces => {
                Diagnostic::error("Unbalanced braces in template string interpolation")
                    .with_span(span)
                    .with_note("Each '{' must have a matching '}'")
            }
            LexerError::InvalidFloatFormat => {
                Diagnostic::error("Invalid floating point number format")
                    .with_span(span)
                    .with_note("Floating point numbers must have digits before and after the decimal point")
            }
            LexerError::IntegerOverflow => {
                Diagnostic::error("Integer literal is too large")
                    .with_span(span)
                    .with_note("Integer literals must fit within the range of i64")
            }
            LexerError::InvalidBinaryLiteral => {
                Diagnostic::error("Invalid binary literal")
                    .with_span(span)
                    .with_note("Binary literals must contain only 0 and 1 digits after '0b'")
            }
            
            LexerError::InvalidOctalLiteral => {
                Diagnostic::error("Invalid octal literal")
                    .with_span(span)
                    .with_note("Octal literals must contain only digits 0-7 after '0o'")
            }
            
            LexerError::InvalidHexLiteral => {
                Diagnostic::error("Invalid hexadecimal literal")
                    .with_span(span)
                    .with_note("Hexadecimal literals must contain only digits 0-9 and letters A-F after '0x'")
            }
        }
    }
}
