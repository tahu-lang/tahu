use tahuc_span::{FileId, Span};

pub type NodeId = u32;

#[derive(Debug, Clone, PartialEq)]
pub struct AstNode<T> {
    pub id: NodeId,
    pub span: Span,
    pub file_id: FileId,
    pub kind: T,
}

impl<T> AstNode<T> {
    pub fn new(id: NodeId, span: Span, file_id: FileId, kind: T) -> Self {
        Self { id, span, file_id, kind }
    }
}