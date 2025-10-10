use tahuc_ast::{nodes::op::UnaryOp, ty::Type};
use tahuc_hir::hir::HirExpression;
use tahuc_module::config::Config;

use crate::{builder::builder::Builder, mir::instruction::MirOperand};

impl Builder {
    pub(crate) fn build_unary_op(
        &mut self,
        config: &mut Config,
        op: UnaryOp,
        operand: &HirExpression,
        ty: &Type,
    ) -> MirOperand {
        match op {
            UnaryOp::AddressOf => self.build_expression(config, operand, true),
            UnaryOp::Deref => {
                let ty = self.ty_to_mir(config, ty);
                let operand = self.build_expression(config, &operand, false);
                let temp = self.new_local(ty.clone());
                self.load(temp, operand, ty);
                MirOperand::Local(temp)
            }
            UnaryOp::PostDecrement
            | UnaryOp::PostIncrement
            | UnaryOp::PreDecrement
            | UnaryOp::PreIncrement => {
                let ty = self.ty_to_mir(config, &operand.get_type());
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

                let left = self.build_expression(config, operand, false);
                let right = MirOperand::new_constant_int(1);

                self.binary_op(target, left.clone(), right, binary_op, ty.clone());
                let right_ptr = self.build_expression(config, operand, true);
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
                let ty = self.ty_to_mir(config, ty);
                let operand = self.build_expression(config, operand, false);
                let temp = self.new_local(ty.clone());
                self.unary_op(temp, operand, op.clone(), ty);
                MirOperand::Local(temp)
            }
        }
    }
}
