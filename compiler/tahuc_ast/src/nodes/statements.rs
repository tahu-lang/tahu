use tahuc_span::Span;

use crate::{nodes::{ast::AstNode, declarations::Visibility, op::AssignmentOp, Expression}, Type};

pub type Statement = AstNode<StatementKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Variable(Variable),
    Assignment {
        left: Box<Expression>,
        op: AssignmentOp,
        right: Box<Expression>,
    },
    Return(Option<Expression>),
    IfStatement(IfStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    ForInStatement(ForInStatement),
    Continue,
    Break,
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
    pub variable_type: Type,
    pub initializer: Option<Expression>,
    pub is_mutable: bool,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElseBranch {
    Block(Block),
    If(Box<IfStatement>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub initializer: Box<Option<Statement>>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInStatement {
    pub identifier: String,
    pub collection: Expression,
    pub body: Block,
    pub span: Span,
}