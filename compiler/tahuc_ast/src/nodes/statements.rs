use tahuc_span::Span;

use crate::{nodes::{ast::AstNode, declarations::Visibility, Expression}, Type};

pub type Statement = AstNode<StatementKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Block(Block),
    Variable(Variable),
    Return(Option<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub visibility: Visibility,
    pub name: String,
    pub r#type: Option<Type>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub span: Span,
}

impl Variable {
    pub fn new(visibility: Visibility, name: String, r#type: Option<Type>, initializer: Option<Expression>, is_mutable: bool, span: Span) -> Self {
        Self {
            visibility,
            name,
            r#type,
            initializer,
            is_mutable,
            span,
        }
    }
}