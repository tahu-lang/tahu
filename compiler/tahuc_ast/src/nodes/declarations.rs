use tahuc_span::Span;

use crate::{nodes::{ast::AstNode, statements::{Block, Variable}, Expression}, Type};

pub type Declaration = AstNode<DeclarationKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationKind {
    Interface(Interface),
    Class(Class),
    Fn(Function),
    Variable(Variable),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub visibility: Visibility,
    pub name: String,
    pub paramaters: Vec<Paramater>,
    pub body: Block,
    pub span: Span,
}

impl Function {
    pub fn new(visibility: Visibility, name: String, paramaters: Vec<Paramater>, body: Block, span: Span) -> Self {
        Self {
            visibility,
            name: name,
            paramaters,
            body: body,
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Paramater {
    pub name: String,
    pub r#type: Type,
    pub default: Option<Expression>,
    pub span: Span,
}