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
        /// Base type
        base_ty: MirType,
        /// Type of the resulting pointer
        inner_ty: MirType,
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

    /// Casting
    Cast {
        target: LocalId,
        kind: CastKind,
        value: MirOperand,
        from: MirType,
        to: MirType,
    },
}

#[derive(Debug, Clone)]
pub enum CastKind {
    /// No operation, used for type conversions that don't require any code generation.
    NoOp,
    /// Zero-extension
    ZExt,
    /// Sign-extension
    SExt,
    /// Truncation
    Trunc,
    /// Floating-point to unsigned integer
    FPToUI,
    /// Floating-point to signed integer
    FPToSI,
    /// Unsigned integer to floating-point
    UIToFP,
    /// Signed integer to floating-point
    SIToFP,
    /// Bitcast (reinterpret bits without changing value)
    BitCast,
    /// Pointer to integer
    PtrToInt,
    /// Integer to pointer
    IntToPtr,
}

impl CastKind {
    pub fn is_noop(&self) -> bool {
        matches!(self, CastKind::NoOp)
    }
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
        MirOperand::Constant(MirConstant::Int { value: value, ty: MirType::I32 })
    }

    pub fn is_parameter(&self, mir_func: &MirFunction) -> bool {
        match self {
            MirOperand::Local(id) => {
                mir_func.is_parameter(*id)
            }
            _ => false,
        }
    }
}