use tahuc_span::{FileId, Span};

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
    Interface(Interface),
    Class(Class),
    Fn(Function),
    Variable(Variable),
    Extern(ExternFn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFn {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Interface {
    pub visibility: Visibility,
    pub name: String,
    pub methods: Vec<Function>,
    pub extends: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub visibility: Visibility,
    pub name: String,
    pub super_class: Option<String>,
    pub interfaces: Vec<String>,
    pub is_abstract: bool,
    pub body: ClassBody,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassBody {
    pub fields: Vec<Variable>,
    pub methods: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub is_constanta: bool,
    pub visibility: Visibility,
    pub name: String,
    pub is_mutable: bool,
    pub r#type: Type,
    pub init: Expression,
    pub span: Span,
}

pub type Function = AstNode<FunctionKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionKind {
    pub visibility: Visibility,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
    pub span: Span,
}

pub type Parameter = AstNode<ParameterKind>;

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterKind {
    pub name: String,
    pub r#type: Type,
    pub default: Option<Expression>,
    pub span: Span,
}

impl AstBuilder {
    pub fn build_function(
        &mut self,
        span: Span,
        file_id: FileId,
        visibility: Visibility,
        name: String,
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
                name,
                parameters,
                return_type,
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
}
