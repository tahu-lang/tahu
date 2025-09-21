use tahuc_ast::{nodes::op::UnaryOp, Type};
use tahuc_diagnostics::{diagnostic::Diagnostic, suggestion::Suggestion};
use tahuc_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticError {
    Duplicate {
        name: String,
        span: Span,
        previous: String,
        previous_span: Span,
    },
    DuplicateExternFn {
        name: String,
        span: Span,
        previous: String,
        previous_span: Span,
    },
    PrivateFunction {
        name: String,
        span: Span,
    },
    Undefined {
        name: String,
        span: Span,
    },
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    InvalidCall {
        function: String,
        expected: Vec<String>,
        found: Vec<String>,
        span: Span,
    },
    CannotAssignImmutable {
        name: String,
        span: Span,
    },
    InvalidReturn {
        expected: String,
        found: String,
        span: Span,
    },
    InvalidPointerOp {
        op: UnaryOp,
        span: Span,
        operand_type: Type,
        reason: String,
    },
    InvalidUnaryOp {
        op: UnaryOp,
        span: Span,
        reason: String,
    },
    Unreachable {
        span: Span,
    },

    Raw {
        message: String,
        span: Span,
    }
}

impl SemanticError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            SemanticError::Duplicate { name, span, previous, previous_span } => {
                Diagnostic::error(format!("duplicate definition of `{}`", name))
                    .with_span(*span)
                    .with_note(format!("`{}` was previously defined here", name))
                    .with_related(*previous_span, previous)
            }

            SemanticError::DuplicateExternFn { name, span, previous, previous_span } => {
                Diagnostic::error(format!("duplicate extern function `{}`", name))
                    .with_span(*span)
                    .with_note(format!("`{}` was previously declare here", name))
                    .with_related(*previous_span, previous)
            }

            SemanticError::PrivateFunction { name, span } => {
                Diagnostic::error(format!("function `{}` is private", name))
                    .with_span(*span)
                    .with_note(format!("`{}` is declared as private here", name))
            }

            SemanticError::Undefined { name, span } => {
                Diagnostic::error(format!("undefined variable `{}`", name))
                    .with_span(*span)
                    .with_suggestion(Suggestion::insert_before(
                        *span,
                        &format!("declare `{}` before using it", name),
                        &format!("var {} = ...;", name),
                    ))
            }

            SemanticError::TypeMismatch { expected, found, span } => {
                Diagnostic::error("type mismatch")
                    .with_span(*span)
                    .with_note(format!("expected `{}`, found `{}`", expected, found))
            }

            SemanticError::InvalidCall { function, expected, found, span } => {
                Diagnostic::error(format!("invalid call to function `{}`", function))
                    .with_span(*span)
                    .with_note(format!(
                        "expected arguments ({:?}), but found arguments ({:?})",
                        expected, found
                    ))
            }

            SemanticError::CannotAssignImmutable { name, span } => {
                Diagnostic::error(format!("cannot assign to immutable variable `{}`", name))
                    .with_span(*span)
                    .with_suggestion(Suggestion::simple_replacement(
                        *span,
                        "declare with `var` instead of `val`",
                        format!("var {}", name),
                    ))
            }

            SemanticError::InvalidReturn { expected, found, span } => {
                Diagnostic::error("return type mismatch")
                    .with_span(*span)
                    .with_note(format!("expected `{}`, found `{}`", expected, found))
            }
            SemanticError::InvalidPointerOp {span, reason, .. } => {
                Diagnostic::error(reason)
                    .with_span(*span)
            }

            SemanticError::InvalidUnaryOp {span, reason, .. } => {
                Diagnostic::error(reason)
                    .with_span(*span)
            }

            SemanticError::Unreachable { span } => {
                Diagnostic::warning("unreachable code")
                    .with_span(*span)
            }

            SemanticError::Raw { message, span } => {
                Diagnostic::error(message).with_span(*span)
            }
        }
    }
}
