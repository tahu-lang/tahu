use tahuc_ast::nodes::op::{BinaryOp, UnaryOp};
use tahuc_hir::hir::FunctionId;

use crate::{
    builder::builder::Builder,
    mir::{
        instruction::{CastKind, MirInstruction, MirOperand}, ty::MirType, BasicBlockId, LocalId
    },
};

impl Builder {
    pub(crate) fn alloca(&mut self, target: LocalId, ty: MirType) {
        self.add_instruction(MirInstruction::Alloca { target, ty });
    }

    pub(crate) fn store(&mut self, ptr: MirOperand, value: MirOperand, ty: MirType) {
        self.add_instruction(MirInstruction::Store { ptr, value, ty });
    }

    pub(crate) fn load(&mut self, target: LocalId, ptr: MirOperand, ty: MirType) {
        self.add_instruction(MirInstruction::Load { target, ptr, ty });
    }

    pub(crate) fn get_element_ptr(
        &mut self,
        target: LocalId,
        base: MirOperand,
        indices: Vec<MirOperand>,
        ty: MirType,
    ) {
        self.add_instruction(MirInstruction::GetElementPtr {
            target,
            base,
            indices,
            ty,
        });
    }

    pub(crate) fn binary_op(
        &mut self,
        target: LocalId,
        left: MirOperand,
        right: MirOperand,
        op: BinaryOp,
        ty: MirType,
    ) {
        self.add_instruction(MirInstruction::Binary {
            target,
            left,
            right,
            op,
            ty,
        });
    }

    pub(crate) fn unary_op(
        &mut self,
        target: LocalId,
        value: MirOperand,
        op: UnaryOp,
        ty: MirType,
    ) {
        self.add_instruction(MirInstruction::Unary {
            target,
            value,
            op,
            ty,
        });
    }

    pub(crate) fn call(
        &mut self,
        target: Option<LocalId>,
        function: FunctionId,
        arguments: Vec<MirOperand>,
        ty: MirType,
    ) {
        self.add_instruction(MirInstruction::Call {
            target,
            function,
            arguments,
            ty,
        });
    }

    pub(crate) fn phi(
        &mut self,
        target: LocalId,
        values: Vec<(MirOperand, BasicBlockId)>,
        ty: MirType,
    ) {
        self.add_instruction(MirInstruction::Phi {
            target,
            values,
            ty,
        });
    }

    pub(crate) fn select(
        &mut self,
        target: LocalId,
        condition: MirOperand,
        then_branch: MirOperand,
        else_branch: MirOperand,
    ) {
        self.add_instruction(MirInstruction::Select {
            target,
            condition,
            then_branch,
            else_branch,
        });
    }

    pub(crate) fn cast(
        &mut self,
        target: LocalId,
        kind: CastKind,
        value: MirOperand,
        from: MirType,
        to: MirType,
    ) {
        self.add_instruction(MirInstruction::Cast {
            target,
            kind,
            value,
            from,
            to,
        });
    }

    fn add_instruction(&mut self, instruction: MirInstruction) {
        let block_id = self.current_block();
        let func = self.current_function.as_mut().unwrap();
        if let Some(block) = func.get_block_mut(block_id) {
            block.add_instruction(instruction);
        }
    }
}
