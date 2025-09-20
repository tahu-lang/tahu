use tahuc_ast::{Type, nodes::op::UnaryOp};
use tahuc_hir::hir::HirExpression;

use crate::{builder::builder::Builder, mir::{instruction::MirOperand, ty::ToMirType}};

impl Builder {
    pub(crate) fn build_unary_op(
        &mut self,
        op: UnaryOp,
        operand: &HirExpression,
        ty: &Type,
    ) -> MirOperand {
        match op {
            UnaryOp::AddressOf => self.build_expression(operand, true),
            UnaryOp::Deref => {
                let operand = self.build_expression(&operand, false);
                let temp = self.new_local(ty.to_mir_ty());
                self.load(temp, operand, ty.to_mir_ty());
                MirOperand::Local(temp)
            }
            _ => {
                let operand = self.build_expression(operand, false);
                let temp = self.new_local(ty.to_mir_ty());
                self.unary_op(temp, operand, op.clone(), ty.to_mir_ty());
                MirOperand::Local(temp)
            }
        }
    }

    fn build_unary_to_binary(&mut self, op: UnaryOp, operand: &HirExpression) {}
}
