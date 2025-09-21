use std::fmt;

use tahuc_lexer::token::Literal;
use tahuc_span::{FileId, Span};

use crate::nodes::{
    Expression,
    ast::{AstNode, NodeId},
    declarations::{Class, Declaration, DeclarationKind, ExternFn, Function, ParameterKind},
    expressions::{Argument, ExpressionKind, FunctionCall, TemplatePart},
    op::{AssignmentOp, BinaryOp, UnaryOp},
    statements::{IfStatement, Statement, StatementKind, Variable, WhileStatement},
};

pub mod nodes;
pub mod printer;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    String,
    Char,
    Int,
    Double,
    Boolean,
    Void,
    Null,
    Array {
        ty: Box<Type>,
        size: usize
    },
    Nullable(Box<Type>),
    Pointer(Box<Type>),

    // placeholder
    Inferred,
    // for error
    Error,

    Function(Vec<Type>, Box<Type>),

    // for custom type
    Named(String),
}

impl Type {
    pub fn is_inferred(&self) -> bool {
        matches!(self, Type::Inferred)
    }

    pub fn is_nullable(&self) -> bool {
        match self {
            Type::Nullable(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "String"),
            Type::Char => write!(f, "Char"),
            Type::Int => write!(f, "Int"),
            Type::Double => write!(f, "Double"),
            Type::Boolean => write!(f, "Boolean"),
            Type::Void => write!(f, "Void"),
            Type::Null => write!(f, "Null"),
            Type::Array { ty, size } => write!(f, "[{} x {}]", size, ty),
            Type::Nullable(ty) => write!(f, "{}?", ty),
            Type::Pointer(type_) => write!(f, "*{}", type_),
            Type::Inferred => write!(f, "Inferred"),
            Type::Error => write!(f, "Error"),
            Type::Function(params, type_) => write!(
                f,
                "({}) => {}",
                params
                    .iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                type_
            ),
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
        self.node_id += 1;
        id
    }

    pub fn build_extern_function(
        &mut self,
        span: Span,
        file_id: FileId,
        name: String,
        params: Vec<AstNode<ParameterKind>>,
        return_type: Type,
    ) -> Declaration {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            DeclarationKind::Extern(ExternFn {
                name: name,
                parameters: params,
                return_type: return_type,
                span: span,
            }),
        )
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

    pub fn build_assigment_statement(
        &mut self,
        span: Span,
        file_id: FileId,
        left: Expression,
        op: AssignmentOp,
        right: Expression,
    ) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::Assignment {
                left: Box::new(left),
                op: op,
                right: Box::new(right),
            },
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

    pub fn template_string(
        &mut self,
        span: Span,
        file_id: FileId,
        parts: Vec<TemplatePart>,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::TemplateString { parts: parts },
        )
    }

    pub fn cast(
        &mut self,
        span: Span,
        file_id: FileId,
        ty: Type,
        expression: Expression,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Cast {
                ty: ty,
                expression: Box::new(expression),
            },
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

    pub fn ternary(
        &mut self,
        span: Span,
        file_id: FileId,
        condition: Expression,
        then: Expression,
        otherwise: Expression,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::Ternary {
                condition: Box::new(condition),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            },
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

    pub fn array_access(
        &mut self,
        span: Span,
        file_id: FileId,
        array: Expression,
        index: Expression,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::ArrayAccess {
                array: Box::new(array),
                index: Box::new(index),
            },
        )
    }

    pub fn member_access(
        &mut self,
        span: Span,
        file_id: FileId,
        object: Expression,
        member: String,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::MemberAccess {
                object: Box::new(object),
                member: member,
            },
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

    pub fn build_array_literal(
        &mut self,
        span: Span,
        file_id: FileId,
        expressions: Vec<Expression>,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::ArrayLiteral {
                elements: expressions,
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

    pub fn if_statement(&mut self, span: Span, file_id: FileId, if_stmt: IfStatement) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::IfStatement(if_stmt),
        )
    }

    pub fn while_statement(
        &mut self,
        span: Span,
        file_id: FileId,
        while_stmt: WhileStatement,
    ) -> Statement {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StatementKind::WhileStatement(while_stmt),
        )
    }

    pub fn build_continue_statement(&mut self, span: Span, file_id: FileId) -> Statement {
        AstNode::new(self.next_id(), span, file_id, StatementKind::Continue)
    }

    pub fn build_break_statement(&mut self, span: Span, file_id: FileId) -> Statement {
        AstNode::new(self.next_id(), span, file_id, StatementKind::Break)
    }
}
