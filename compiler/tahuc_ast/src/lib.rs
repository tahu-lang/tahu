use std::fmt;

use tahuc_lexer::token::Literal;
use tahuc_span::{FileId, Span};

use crate::nodes::{
    Expression,
    ast::{AstNode, NodeId},
    declarations::{Declaration, DeclarationKind, Function},
    expressions::{BinaryOp, ExpressionKind, UnaryOp},
    statements::{Statement, StatementKind, Variable},
};

pub mod nodes;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Double,
    Boolean,

    Named(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Double => write!(f, "Double"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Named(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub file: FileId,
    pub declaration: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstBuilder {
    node_id: NodeId,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self { node_id: 0 }
    }

    pub fn next_id(&mut self) -> NodeId {
        let id = self.node_id;
        self.node_id += id;
        id
    }

    pub fn fn_declaration(
        &mut self,
        span: Span,
        file_id: FileId,
        function: Function,
    ) -> Declaration {
        AstNode::new(self.next_id(), span, file_id, DeclarationKind::Fn(function))
    }

    pub fn variable_declaration(&mut self, span: Span, file_id: FileId, varibale: Variable) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::Variable(varibale),
        )
    }

    pub fn binary(
        &mut self,
        span: Span,
        file_id: FileId,
        left: Expression,
        op: BinaryOp,
        right: Expression,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Binary {
                left: Box::new(left),
                op: op,
                right: Box::new(right),
            },
        )
    }

    pub fn unary(
        &mut self,
        span: Span,
        file_id: FileId,
        op: UnaryOp,
        operand: Expression,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Unary {
                op: op,
                operand: Box::new(operand),
            },
        )
    }

    pub fn literal(&mut self, span: Span, file_id: FileId, literal: Literal) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Literal(literal),
        )
    }

    pub fn expression_statement(
        &mut self,
        span: Span,
        file_id: FileId,
        expression: Expression,
    ) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::Expression(expression),
        )
    }
}
