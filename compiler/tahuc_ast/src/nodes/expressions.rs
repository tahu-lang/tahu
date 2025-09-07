use tahuc_lexer::token::Literal;
use tahuc_span::Span;

use crate::nodes::{ast::AstNode, op::{BinaryOp, UnaryOp}};

pub type Expression = AstNode<ExpressionKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    TemplateString {
        parts: Vec<TemplatePart>
    },

    Identifier(String),

    Ternary {
        condition: Box<Expression>,
        then: Box<Expression>,
        otherwise: Box<Expression>,
    },

    Binary {
        left: Box<Expression>,
        op: BinaryOp,
        right: Box<Expression>,
    },

    Unary {
        op: UnaryOp,
        operand: Box<Expression>,
    },
    ArrayLiteral {
        elements: Vec<Expression>,
    },
    ArrayAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    MemberAccess {
        object: Box<Expression>,
        member: String,
    },
    FunctionCall(Box<FunctionCall>),
    Grouping(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplatePart {
    Text(String),
    Expression(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub function: Expression,
    pub arguments: Vec<Argument>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Positional(Expression),
    Named {
        name: String,
        value: Expression,
        span: Span,
    }
}