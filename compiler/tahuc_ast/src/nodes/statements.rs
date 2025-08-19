use tahuc_span::Span;

use crate::{nodes::{ast::AstNode, Expression}, Type};

pub type Statement = AstNode<StatementKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Block(Block),
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub r#type: Option<Type>,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
}

impl Variable {
    pub fn new(name: String, r#type: Option<Type>, initializer: Option<Expression>, is_mutable: bool) -> Self {
        Self {
            name,
            r#type,
            initializer,
            is_mutable,
        }
    }
}