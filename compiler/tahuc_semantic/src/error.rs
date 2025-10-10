use std::rc::Rc;

use tahuc_ast::{nodes::op::UnaryOp, ty::Type};
use tahuc_diagnostics::{diagnostic::Diagnostic, suggestion::Suggestion};
use tahuc_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticError {
    ImportRedefined {
        name: String,
        span: Span,
        previous: String,
        previous_span: Span,
    },
    ImportDuplicate {
        name: String,
        span: Span,
    },
    ImportWildcardDuplicate {
        span: Span,
    },

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

    Ambiguous {
        name: String,
        span: Span,
        related: Vec<(Span, String)>
    },
    AmbiguousType {
        name: String,
        span: Span,
        related: Vec<(Span, String)>
    },

    PrivateFunction {
        name: String,
        span: Span,
    },
    PrivateStruct {
        name: String,
        span: Span,
    },
    PrivateVariable {
        name: String,
        span: Span,
    },

    Undefined {
        name: String,
        span: Span,
    },
    UndefinedStruct {
        name: String,
        span: Span,
    },
    UndefinedFunction {
        name: String,
        span: Span,
    },
    UndefinedVariable {
        name: String,
        span: Span,
    },
    
    TypeMismatch {
        msg: String,
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
        operand_type: Rc<Type>,
        reason: String,
    },
    InvalidUnaryOp {
        op: UnaryOp,
        span: Span,
        reason: String,
    },
    Unreachable {
        message: String,
        span: Span,
    },

    NotStruct {
        name: String,
        span: Span,
    },

    NotFunction {
        name: String,
        span: Span,
    },

    NotVariable {
        name: String,
        span: Span,
    },

    UninitializedVariable {
        name: String,
        span: Span,
    },

    MissingReturn {
        name: String,
        span: Span,
    },
    InvalidContinue {
        span: Span,
    },
    InvalidBreak {
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
            SemanticError::ImportRedefined { name, span, previous, previous_span } => {
                Diagnostic::error(format!("duplicate import of `{}`", name))
                    .with_span(*span)
                    .with_note(format!("`{}` was previously imported here", name))
                    .with_related(*previous_span, previous)
            }
            SemanticError::ImportDuplicate { name, span } => {
                Diagnostic::error(format!("duplicate import of `{}`", name))
                    .with_span(*span)
            }
            SemanticError::ImportWildcardDuplicate { span } => {
                Diagnostic::error("duplicate symbol import in wildcard")
                    .with_span(*span)
                    .with_suggestion(Suggestion::hint("try to import the symbols individually"))
            }

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

            SemanticError::Ambiguous { name, span, related } => {
                let mut diagnostic = Diagnostic::error(format!("ambiguous definition of `{}`", name))
                    .with_span(*span);

                for (span, msg) in related {
                    diagnostic = diagnostic.with_related(*span, msg);
                }

                diagnostic
            }

            SemanticError::AmbiguousType { name, span, related } => {
                let mut diagnostic = Diagnostic::error(format!("ambiguous type `{}`", name))
                    .with_span(*span);

                for (span, msg) in related {
                    diagnostic = diagnostic.with_related(*span, msg);
                }

                diagnostic
            }

            SemanticError::PrivateFunction { name, span } => {
                Diagnostic::error(format!("function `{}` is private", name))
                    .with_span(*span)
                    .with_note(format!("`{}` is declared as private here", name))
            }

            SemanticError::PrivateStruct { name, span } => {
                Diagnostic::error(format!("struct `{}` is private", name))
                    .with_span(*span)
                    .with_note(format!("`{}` is declared as private here", name))
            }

            SemanticError::PrivateVariable { name, span } => {
                Diagnostic::error(format!("variable `{}` is private", name))
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
            SemanticError::UndefinedStruct { name, span } => {
                Diagnostic::error(format!("undefined struct `{}`", name))
                    .with_span(*span)
                    .with_suggestion(Suggestion::insert_before(
                        *span,
                        &format!("declare `{}` before using it", name),
                        &format!("struct {} = ...;", name),
                    ))
            }
            SemanticError::UndefinedFunction { name, span } => {
                Diagnostic::error(format!("undefined function `{}`", name))
                    .with_span(*span)
                    .with_suggestion(Suggestion::insert_before(
                        *span,
                        &format!("declare `{}` before using it", name),
                        &format!("fn {} = ...;", name),
                    ))
            }
            SemanticError::UndefinedVariable { name, span } => {
                Diagnostic::error(format!("undefined type `{}`", name))
                    .with_span(*span)
                    .with_suggestion(Suggestion::insert_before(
                        *span,
                        &format!("declare `{}` before using it", name),
                        &format!("type {} = ...;", name),
                    ))
            }

            SemanticError::TypeMismatch { msg, expected, found, span } => {
                Diagnostic::error(format!("type mismatch {}", msg))
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

            SemanticError::Unreachable { message, span } => {
                Diagnostic::warning(message)
                    .with_span(*span)
            }

            SemanticError::NotStruct { name, span } => {
                Diagnostic::error(format!("`{}` is not a struct", name))
                    .with_span(*span)
            }

            SemanticError::NotFunction { name, span } => {
                Diagnostic::error(format!("`{}` is not a function", name))
                    .with_span(*span)
            }

            SemanticError::NotVariable { name, span } => {
                Diagnostic::error(format!("`{}` is not a variable", name))
                    .with_span(*span)
            }

            SemanticError::UninitializedVariable { name, span } => {
                Diagnostic::error(format!("variable `{}` is uninitialized", name))
                    .with_span(*span)
                    .with_suggestion(Suggestion::simple_replacement(
                        *span,
                        "initialize variable",
                        format!("var {} = ...;", name),
                    ))
            }

            SemanticError::MissingReturn { name, span } => {
                Diagnostic::error(format!("missing return in function `{}`", name))
                    .with_span(*span)
            }
            SemanticError::InvalidContinue { span } => {
                Diagnostic::error("invalid `continue` statement")
                    .with_span(*span)
            }
            SemanticError::InvalidBreak { span } => {
                Diagnostic::error("invalid `break` statement")
                    .with_span(*span)
            }

            SemanticError::Raw { message, span } => {
                Diagnostic::error(message).with_span(*span)
            }
        }
    }
}
