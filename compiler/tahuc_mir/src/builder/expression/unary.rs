use tahuc_ast::{ty::Type, nodes::op::UnaryOp};
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
            UnaryOp::PostDecrement
            | UnaryOp::PostIncrement
            | UnaryOp::PreDecrement
            | UnaryOp::PreIncrement => {
                let ty = operand.get_type().to_mir_ty();
                let target = self.new_local(ty.clone());

                let binary_op = match op {
                    UnaryOp::PreIncrement | UnaryOp::PostIncrement => {
                        tahuc_ast::nodes::op::BinaryOp::Add
                    }
                    UnaryOp::PreDecrement | UnaryOp::PostDecrement => {
                        tahuc_ast::nodes::op::BinaryOp::Sub
                    }
                    _ => unreachable!(),
                };

                let left = self.build_expression(operand, false);
                let right = MirOperand::new_constant_int(1);

                self.binary_op(target, left.clone(), right, binary_op, ty.clone());
                let right_ptr = self.build_expression(operand, true);
                let value = MirOperand::Local(target);
                self.store(right_ptr, value.clone(), ty.clone());

                let target = match op {
                    UnaryOp::PreIncrement | UnaryOp::PreDecrement => value,
                    UnaryOp::PostIncrement | UnaryOp::PostDecrement => left,
                    _ => unreachable!(),
                };

                target
            }
            _ => {
                let operand = self.build_expression(operand, false);
                let temp = self.new_local(ty.to_mir_ty());
                self.unary_op(temp, operand, op.clone(), ty.to_mir_ty());
                MirOperand::Local(temp)
            }
        }
    }
}
