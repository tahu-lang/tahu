use std::rc::Rc;

use tahuc_span::{FileId, Span, StrId};

use crate::{
    AstBuilder, Type,
    nodes::{
        Expression,
        ast::AstNode,
        statements::{Block, Variable},
    },
};

pub type Declaration = AstNode<DeclarationKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationKind {
    Import(Import),
    Struct(Struct),
    Fn(Function),
    Variable(Variable),
    Extern(ExternFn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub imports: Vec<AstNode<ImportTable>>,
    pub is_wildcard: bool,
    pub path: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportTable {
    pub name: StrId,
    pub alias: Option<StrId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub visibility: Visibility,
    pub name: StrId,
    pub fields: Vec<AstNode<StructField>>,
    pub ty: Rc<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: StrId,
    pub r#type: Rc<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFn {
    pub name: StrId,
    pub parameters: Vec<Parameter>,
    pub return_type: Rc<Type>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        match self {
            Visibility::Public => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub is_constanta: bool,
    pub visibility: Visibility,
    pub name: String,
    pub is_mutable: bool,
    pub r#type: Rc<Type>,
    pub init: Expression,
    pub span: Span,
}

pub type Function = AstNode<FunctionKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionKind {
    pub visibility: Visibility,
    pub decorators: Vec<AstNode<Decorator>>,
    pub name: StrId,
    pub parameters: Vec<Parameter>,
    pub return_type: Rc<Type>,
    pub body: Block,
    pub span: Span,
}

pub type Parameter = AstNode<ParameterKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterKind {
    pub name: StrId,
    pub r#type: Rc<Type>,
    pub default: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator {
    pub name: StrId,
    pub arguments: Vec<DecoratorValue>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecoratorValue {
    pub name: Expression,
    pub value: Option<Expression>,
    pub span: Span,
}

impl AstBuilder {
    pub fn build_function(
        &mut self,
        span: Span,
        file_id: FileId,
        visibility: Visibility,
        decorators: Vec<AstNode<Decorator>>,
        name: StrId,
        parameters: Vec<Parameter>,
        return_type: Type,
        body: Block,
    ) -> Function {
        AstNode::new(
            self.next_id(),
            span,
            file_id,
            FunctionKind {
                visibility,
                decorators,
                name,
                parameters,
                return_type: Rc::new(return_type),
                body,
                span,
            }
        )
    }

    pub fn build_parameter(
        &mut self,
        span: Span,
        file_id: FileId,
        parameter: ParameterKind,
    ) -> AstNode<ParameterKind> {
        AstNode::new(self.next_id(), span, file_id, parameter)
    }

    pub fn build_decorator(
        &mut self,
        span: Span,
        file_id: FileId,
        name: StrId,
        arguments: Vec<DecoratorValue>,
    ) -> AstNode<Decorator> {
        AstNode::new(self.next_id(), span, file_id, Decorator {
            name,
            arguments,
            span,
        })
    }
}
