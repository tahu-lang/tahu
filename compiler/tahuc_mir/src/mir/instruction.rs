use tahuc_ast::nodes::op::{BinaryOp, UnaryOp};
use tahuc_hir::hir::FunctionId;

use crate::mir::{function::MirFunction, ty::{MirConstant, MirType}, BasicBlockId, LocalId};

#[derive(Debug, Clone)]
pub enum MirInstruction {
    Alloca {
        target: LocalId,
        ty: MirType,
    },

    Load {
        target: LocalId,
        ptr: MirOperand,
        ty: MirType,
    },

    Store {
        /// Left
        ptr: MirOperand,
        /// Right
        value: MirOperand,
        ty: MirType,
    },

    /// Get element pointer (for arrays/structs)
    GetElementPtr {
        target: LocalId,
        /// memory location
        base: MirOperand, 
        /// offsets/indexes to reach the sub-element
        indices: Vec<MirOperand>,
        /// Type of the resulting pointer
        ty: MirType,
    },

    /// Binary operation
    Binary {
        target: LocalId,
        left: MirOperand,
        op: BinaryOp,
        right: MirOperand,
        ty: MirType,
    },

    /// Unary operation
    Unary {
        target: LocalId,
        op: UnaryOp,
        value: MirOperand,
        ty: MirType,
    },

    /// Function Call
    Call {
        target: Option<LocalId>,
        function: FunctionId,
        arguments: Vec<MirOperand>,
        ty: MirType,
    },

    /// Branch instruction
    Phi {
        target: LocalId,
        values: Vec<(MirOperand, BasicBlockId)>,
        ty: MirType,
    },

    /// Select
    Select {
        target: LocalId,
        condition: MirOperand,
        then_branch: MirOperand,
        else_branch: MirOperand,
    },
}

/// Memory Operand
/// R-Value
#[derive(Debug, Clone)]
pub enum MirOperand {
    Local(LocalId),
    Constant(MirConstant),
}

impl MirOperand {
    pub fn get_type(&self, mir_func: &MirFunction) -> MirType {
        match self {
            MirOperand::Local(id) => {
                if let Some(local) = mir_func.locals.get(id) {
                    local.clone()
                } else {
                    panic!("Local id `{}` not found in current functoin {}", id, mir_func.name);
                }
            }
            MirOperand::Constant(c) => c.get_type()
        }
    }

    pub fn get_local(&self) -> Option<LocalId> {
        match self {
            MirOperand::Local(id) => Some(*id),
            _ => None,
        }
    }

    pub fn new_constant_null() -> Self {
        MirOperand::Constant(MirConstant::Null(MirType::Null))
    }

    pub fn new_constant_int(value: i64) -> Self {
        MirOperand::Constant(MirConstant::Integer(value))
    }
}