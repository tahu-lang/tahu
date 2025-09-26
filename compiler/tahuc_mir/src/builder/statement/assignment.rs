use tahuc_ast::{nodes::op::AssignmentOp, ty::Type};
use tahuc_hir::hir::{HirExpression, HirLValue};

use crate::{builder::builder::Builder, mir::{instruction::MirOperand, ty::ToMirType}};

impl Builder {
    pub(crate) fn build_assigment_stmt(&mut self, target: &HirLValue, op: AssignmentOp, value: &HirExpression) {
        match target {
            HirLValue::Variable { id, .. } => {
                let rhs = self.build_expression(value, false);
                let variable = self.read_variable(*id);
                println!("DEBUG MIDDLE: {:?}", variable);

                if variable.ctx.is_nullable() {
                    let inner_ty = match value.get_type() {
                        Type::Nullable(inner) => inner.to_mir_ty(),
                        _ => value.get_type().to_mir_ty(),
                    };
                    self.build_nullable(
                        value,
                        MirOperand::Local(variable.id),
                        &value.get_type().to_mir_ty(),
                        &inner_ty,
                    );
                } else {
                    self.store(MirOperand::Local(variable.id), rhs, variable.ty.clone());
                }
            }
            HirLValue::ArrayAccess { array, index, arr_ty, ty, .. } => {
                // let ty = array.get_type().to_mir_ty();
                let target = self.array_access(array, index, arr_ty.to_mir_ty(), ty.to_mir_ty(), true);
                let value_ty = value.get_type().to_mir_ty();

                match value_ty.is_aggregate() {
                    true => {
                        self.build_aggregate(&value, target, &value_ty);
                    }
                    false => {
                        let rhs = self.build_expression(value, false);
                        self.store(target, rhs.clone(), ty.to_mir_ty());
                    }
                }
            }
            HirLValue::Deref { value, .. } => {
                let operand = self.build_expression(value, false);

                let rhs = self.build_expression(value, false);
                let rhs_ty = rhs.get_type(self.current_function.as_ref().unwrap());

                self.store(operand, rhs, rhs_ty);
            }
            HirLValue::FieldAccess { object, field_id, base_ty, ty, .. } => {
                let base = self.build_expression(object, true);
                let indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::new_constant_int(*field_id as i64),
                ];

                let local_id = self.new_local(ty.to_mir_ty());

                self.get_element_ptr(local_id, base, indices, base_ty.to_mir_ty(), base_ty.to_mir_ty());

                let value_ty = value.get_type().to_mir_ty();

                match value_ty.is_aggregate() {
                    true => {
                        self.build_aggregate(value, MirOperand::Local(local_id), &value_ty);
                    }
                    false => {
                        let rhs = self.build_expression(value, false);
                        self.store(MirOperand::Local(local_id), rhs, ty.to_mir_ty());
                    }
                }
            }
            _ => {}
        }
    }
}
