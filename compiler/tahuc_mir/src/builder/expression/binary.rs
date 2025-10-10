use tahuc_ast::{nodes::op::BinaryOp, ty::Type};
use tahuc_hir::hir::HirExpression;
use tahuc_module::config::Config;

use crate::{builder::builder::Builder, mir::instruction::MirOperand};

impl Builder {
    pub(crate) fn build_binary_op(
        &mut self,
        config: &mut Config,
        left: &HirExpression,
        op: BinaryOp,
        right: &HirExpression,
        ty: &Type,
    ) -> MirOperand {
        let left_ty = self.ty_to_mir(config, &left.get_type());
        let right_ty = self.ty_to_mir(config, &right.get_type());
        let ty = self.ty_to_mir(config, ty);

        let (left, right) = match op {
            BinaryOp::Eq | BinaryOp::Ne => {
                let left = if left.get_type().is_nullable() {
                    let ptr = self.build_expression(config, &left, true);
                    self.nullable_access_is_null(ptr, left_ty)
                } else {
                    self.build_expression(config, left, false)
                };

                let right = if matches!(&*right.get_type(), Type::Nullable(_)) {
                    let ptr = self.build_expression(config, right, true);
                    self.nullable_access_is_null(ptr, right_ty)
                } else {
                    self.build_expression(config, right, false)
                };

                (left, right)
            }
            _ => {
                let left = if matches!(&*left.get_type(), Type::Nullable(_)) {
                    let inner_ty = match &*left.get_type() {
                        Type::Nullable(inner) => inner.clone(),
                        _ => unreachable!(),
                    };
                    let inner_ty = self.ty_to_mir(config, &inner_ty);
                    let ptr = self.build_expression(config, &left, true);
                    self.nullable_access_value(ptr, left_ty, inner_ty)
                } else {
                    self.build_expression(config, left, false)
                };
                let right = if matches!(&*right.get_type(), Type::Nullable(_)) {
                    let inner_ty = match &*right.get_type() {
                        Type::Nullable(inner) => inner.clone(),
                        _ => unreachable!(),
                    };
                    let inner_ty = self.ty_to_mir(config, &inner_ty);
                    let ptr = self.build_expression(config, right, true);
                    self.nullable_access_value(ptr, right_ty, inner_ty)
                } else {
                    self.build_expression(config, right, false)
                };
                (left, right)
            }
        };
        // let left = self.build_expression(left, false);
        // let right = self.build_expression(right, false);
        let local_id = self.new_local(ty.clone());

        self.binary_op(local_id, left, right, op.clone(), ty);

        MirOperand::Local(local_id)
    }
}
