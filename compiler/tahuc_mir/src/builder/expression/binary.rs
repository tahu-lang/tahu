use tahuc_ast::{ty::Type, nodes::op::BinaryOp};
use tahuc_hir::hir::HirExpression;

use crate::{builder::builder::Builder, mir::{instruction::MirOperand, ty::ToMirType}};

impl Builder {
    pub(crate) fn build_binary_op(
        &mut self,
        left: &HirExpression,
        op: BinaryOp,
        right: &HirExpression,
        ty: &Type,
    ) -> MirOperand {
        let (left, right) = match op {
            BinaryOp::Eq | BinaryOp::Ne => {
                let left = if left.get_type().is_nullable() {
                    let ptr = self.build_expression(&left, true);
                    self.nullable_access_is_null(ptr, left.get_type().to_mir_ty())
                } else {
                    self.build_expression(left, false)
                };

                let right = if matches!(right.get_type(), Type::Nullable(_)) {
                    let ptr = self.build_expression(right, true);
                    self.nullable_access_is_null(ptr, right.get_type().to_mir_ty())
                } else {
                    self.build_expression(right, false)
                };

                (left, right)
            }
            _ => {
                let left = if matches!(left.get_type(), Type::Nullable(_)) {
                    let inner_ty = match left.get_type() {
                        Type::Nullable(inner) => *inner,
                        _ => unreachable!(),
                    };
                    let ptr = self.build_expression(&left, true);
                    self.nullable_access_value(
                        ptr,
                        left.get_type().to_mir_ty(),
                        inner_ty.to_mir_ty(),
                    )
                } else {
                    self.build_expression(left, false)
                };
                let right = if matches!(right.get_type(), Type::Nullable(_)) {
                    let inner_ty = match right.get_type() {
                        Type::Nullable(inner) => *inner,
                        _ => unreachable!(),
                    };
                    let ptr = self.build_expression(right, true);
                    self.nullable_access_value(
                        ptr,
                        right.get_type().to_mir_ty(),
                        inner_ty.to_mir_ty(),
                    )
                } else {
                    self.build_expression(right, false)
                };
                (left, right)
            }
        };
        // let left = self.build_expression(left, false);
        // let right = self.build_expression(right, false);
        let local_id = self.new_local(ty.to_mir_ty());

        self.binary_op(local_id, left, right, op.clone(), ty.to_mir_ty());

        MirOperand::Local(local_id)
    }
}
