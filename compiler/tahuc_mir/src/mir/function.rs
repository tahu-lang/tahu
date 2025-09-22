use std::collections::{HashMap, HashSet};

use tahuc_hir::hir::{FunctionId, HirVisibility};

use crate::mir::{
    BasicBlockId, LocalId,
    block::{MirBasicBlock, MirTerminator},
    ty::MirType,
};

#[derive(Debug, Clone)]
pub struct MirFunction {
    pub id: FunctionId,
    pub name: String,
    pub parameters: Vec<MirParameter>,
    pub return_type: MirType,
    pub visibility: HirVisibility,

    pub basic_blocks: HashMap<BasicBlockId, MirBasicBlock>,
    pub block_order: Vec<BasicBlockId>,
    pub entry_block: BasicBlockId,

    pub next_block_id: BasicBlockId,
    pub next_local_id: LocalId,

    pub locals: HashMap<LocalId, MirType>,
}

#[derive(Debug, Clone)]
pub struct MirExternFunction {
    pub id: FunctionId,
    pub name: String,
    pub parameters: Vec<MirType>,
    pub return_type: MirType,
}

#[derive(Debug, Clone)]
pub struct MirParameter {
    pub id: LocalId,
    pub name: String,
    pub ty: MirType,
}

impl MirFunction {
    pub fn new(
        id: FunctionId,
        name: String,
        return_type: MirType,
        vibility: HirVisibility,
    ) -> Self {
        Self {
            id,
            name,
            parameters: Vec::new(),
            return_type,
            visibility: vibility,

            basic_blocks: HashMap::new(),
            block_order: Vec::new(),
            entry_block: 0,

            next_block_id: 0,
            next_local_id: 0,
            locals: HashMap::new(),
        }
    }

    pub fn compute_block_order(&mut self) {
        let mut visited = HashSet::new();
        let mut postorder = Vec::new();

        fn dfs(
            block: BasicBlockId,
            func: &MirFunction,
            visited: &mut HashSet<BasicBlockId>,
            postorder: &mut Vec<BasicBlockId>,
        ) {
            if !visited.insert(block) {
                return;
            }
            if let Some(succs) = func.basic_blocks.get(&block) {
                for &succ in &succs.successors {
                    dfs(succ, func, visited, postorder);
                }
            }
            postorder.push(block);
        }

        dfs(self.entry_block, self, &mut visited, &mut postorder);

        // Reverse postorder
        postorder.reverse();
        self.block_order = postorder;
    }

    pub fn add_parameter(&mut self, ty: MirType, name: String) -> LocalId {
        let id = self.next_local_id;
        self.next_local_id += 1;

        let param = MirParameter {
            id,
            name: name,
            ty: ty.clone(),
        };

        self.parameters.push(param);
        self.locals.insert(id, ty);

        id
    }

    pub fn new_local(&mut self, ty: MirType) -> LocalId {
        let id = self.next_local_id;
        self.next_local_id += 1;

        self.locals.insert(id, ty);
        id
    }

    pub fn is_parameter(&self, id: LocalId) -> bool {
        self.parameters.iter().any(|param| param.id == id)
    }

    /// Create a new basic block
    pub fn new_block(&mut self, name: &str) -> BasicBlockId {
        let id = self.next_block_id;
        self.next_block_id += 1;

        let bb = MirBasicBlock {
            id,
            name: format!("{}_{}", name, id),
            instructions: Vec::new(),
            terminator: MirTerminator::Unreachable,
            predecessors: Vec::new(),
            successors: Vec::new(),
        };

        self.basic_blocks.insert(id, bb);
        id
    }

    pub fn get_block(&self, id: BasicBlockId) -> Option<&MirBasicBlock> {
        self.basic_blocks.get(&id)
    }

    pub fn get_block_mut(&mut self, id: BasicBlockId) -> Option<&mut MirBasicBlock> {
        self.basic_blocks.get_mut(&id)
    }

    /// Add an edge between basic blocks
    pub fn add_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        if let Some(from_block) = self.basic_blocks.get_mut(&from) {
            if !from_block.successors.contains(&to) {
                from_block.successors.push(to);
            }
        }

        if let Some(to_block) = self.basic_blocks.get_mut(&to) {
            if !to_block.predecessors.contains(&from) {
                to_block.predecessors.push(from);
            }
        }
    }
}
