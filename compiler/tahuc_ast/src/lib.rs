use std::fmt;

use tahuc_lexer::token::Literal;
use tahuc_span::{FileId, Span};

use crate::nodes::{
    Expression,
    ast::{AstNode, NodeId},
    declarations::{Class, Declaration, DeclarationKind, Function},
    expressions::{Argument, ExpressionKind, FunctionCall},
    op::{AssignmentOp, BinaryOp, UnaryOp},
    statements::{Statement, StatementKind, Variable},
};

pub mod nodes;
pub mod printer;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String,
    Int,
    Double,
    Boolean,
    Void,
    Any,

    Named(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "String"),
            Type::Int => write!(f, "Int"),
            Type::Double => write!(f, "Double"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Void => write!(f, "Void"),
            Type::Any => write!(f, "Any"),
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

    pub fn class_declaration(&mut self, span: Span, file_id: FileId, class: Class) -> Declaration {
        AstNode::new(self.next_id(), span, file_id, DeclarationKind::Class(class))
    }

    pub fn fn_declaration(
        &mut self,
        span: Span,
        file_id: FileId,
        function: Function,
    ) -> Declaration {
        AstNode::new(self.next_id(), span, file_id, DeclarationKind::Fn(function))
    }

    pub fn variable_declaration(
        &mut self,
        span: Span,
        file_id: FileId,
        variable: Variable,
    ) -> Declaration {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            DeclarationKind::Variable(variable),
        )
    }

    pub fn variable_declaration_stmt(
        &mut self,
        span: Span,
        file_id: FileId,
        variable: Variable,
    ) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::Variable(variable),
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

    pub fn literal(&mut self, span: Span, file_id: FileId, literal: Literal) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Literal(literal),
        )
    }

    pub fn identifier(&mut self, span: Span, file_id: FileId, name: String) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Identifier(name),
        )
    }

    pub fn ternary(&mut self, span: Span, file_id: FileId, condition: Expression, then: Expression, otherwise: Expression) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Ternary {
                condition: Box::new(condition),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            }
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

    pub fn array_access(&mut self, span: Span, file_id: FileId, array: Expression, index: Expression) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::ArrayAccess{
                array: Box::new(array),
                index: Box::new(index),
            }
        )
    }

    pub fn member_access(&mut self, span: Span, file_id: FileId, object: Expression, member: String) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::MemberAccess{
                object: Box::new(object),
                member: member,
            }
        )
    }

    pub fn function_call(
        &mut self,
        span: Span,
        file_id: FileId,
        function: Expression,
        args: Vec<Argument>,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::FunctionCall(Box::new(FunctionCall {
                function: function,
                arguments: args,
                span: span,
            })),
        )
    }

    pub fn grouping(&mut self, span: Span, file_id: FileId, expression: Expression) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Grouping(Box::new(expression)),
        )
    }

    pub fn assignment(
        &mut self,
        span: Span,
        file_id: FileId,
        left: Expression,
        op: AssignmentOp,
        right: Expression,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Assignment {
                left: Box::new(left),
                op: op,
                right: Box::new(right),
            },
        )
    }

    pub fn return_statement(
        &mut self,
        span: Span,
        file_id: FileId,
        expression: Option<Expression>,
    ) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::Return(expression),
        )
    }
}
