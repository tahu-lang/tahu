use tahuc_span::FileId;

use crate::mir::function::{MirExternFunction, MirFunction};

pub mod function;
pub mod instruction;
pub mod block;
pub mod ty;

pub type BasicBlockId = u32;
pub type LocalId = u32;

#[derive(Debug, Clone)]
pub struct MirModule {
    pub file_id: FileId,
    pub functions: Vec<MirFunction>,
    pub extern_functions: Vec<MirExternFunction>,
}