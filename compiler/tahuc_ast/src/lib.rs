use tahuc_lexer::token::Literal;
use tahuc_span::{FileId, Span};

use crate::{
    nodes::{
        Expression,
        ast::{AstNode, NodeId},
        declarations::*,
        expressions::{Argument, ExpressionKind, FunctionCall, StructLiteralField, TemplatePart},
        op::{AssignmentOp, BinaryOp, UnaryOp},
        statements::*,
    },
    ty::Type,
};

pub mod nodes;
pub mod printer;
pub mod ty;

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

    pub fn build_import(
        &mut self,
        span: Span,
        file_id: FileId,
        import: Import,
    ) -> Declaration {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            DeclarationKind::Import(import)
        )
    }

    pub fn build_import_table(
        &mut self,
        span: Span,
        file_id: FileId,
        table: ImportTable,
    ) -> AstNode<ImportTable> {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            table
        )
    }

    pub fn build_struct_declaration(
        &mut self,
        span: Span,
        file_id: FileId,
        visibility: Visibility,
        name: String,
        fields: Vec<AstNode<StructField>>,
        ty: Type,
    ) -> Declaration {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            DeclarationKind::Struct(Struct {
                visibility,
                name: name,
                fields: fields,
                ty,
                span: span,
            }),
        )
    }

    pub fn build_struct_field(
        &mut self,
        span: Span,
        file_id: FileId,
        name: String,
        r#type: Type,
    ) -> AstNode<StructField> {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StructField {
                name: name,
                r#type: r#type,
                span: span,
            },
        )
    }

    pub fn build_struct_literal(
        &mut self,
        span: Span,
        file_id: FileId,
        object: Expression,
        fields: Vec<AstNode<StructLiteralField>>,
    ) -> Expression {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            ExpressionKind::StructLiteral {
                object: Box::new(object),
                fields: fields,
            },
        )
    }

    pub fn build_struct_literal_field(
        &mut self,
        span: Span,
        file_id: FileId,
        name: Expression,
        value: Option<Expression>,
    ) -> AstNode<StructLiteralField> {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            StructLiteralField {
                name: name,
                value: value,
                span: span,
            },
        )
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
