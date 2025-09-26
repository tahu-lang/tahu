use std::collections::HashMap;

use tahuc_span::FileId;

use crate::mir::{function::{MirExternFunction, MirFunction}, ty::MirType};

pub mod function;
pub mod instruction;
pub mod block;
pub mod ty;

pub type GlobalId = u32;
pub type BasicBlockId = u32;
pub type LocalId = u32;

#[derive(Debug, Clone)]
pub struct MirModule {
    pub file_id: FileId,
    pub functions: Vec<MirFunction>,
    pub extern_functions: Vec<MirExternFunction>,
    pub globals: HashMap<GlobalId, MirType>,
    global_id: GlobalId,
}

impl MirModule {
    pub fn new() -> Self {
        Self {
            extern_functions: Vec::new(),
            functions: Vec::new(),
            globals: HashMap::new(),
            file_id: FileId(0),
            global_id: 0,
        }
    }

    pub fn new_global(&mut self, ty: MirType) -> GlobalId {
        let id = self.global_id;
        self.global_id += 1;
        self.globals.insert(id, ty);
        id
    }

    pub fn add_function(&mut self, func: MirFunction) {
        self.functions.push(func);
    }

    pub fn add_extern_function(&mut self, func: MirExternFunction) {
        self.extern_functions.push(func);
    }
}