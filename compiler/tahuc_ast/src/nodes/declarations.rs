use crate::nodes::{ast::AstNode, statements::{Block}};

pub type Declaration = AstNode<DeclarationKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationKind {
    Fn(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Block,
}

impl Function {
    pub fn new(name: String, body: Block) -> Self {
        Self {
            name: name,
            body: body,
        }
    }
}
