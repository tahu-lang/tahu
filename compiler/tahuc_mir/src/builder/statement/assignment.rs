use tahuc_ast::{nodes::op::AssignmentOp, ty::Type};
use tahuc_hir::hir::{HirExpression, HirLValue};
use tahuc_module::config::Config;

use crate::{builder::builder::Builder, mir::{instruction::MirOperand}};

impl Builder {
    pub(crate) fn build_assigment_stmt(&mut self, config: &mut Config, target: &HirLValue, _op: AssignmentOp, value: &HirExpression) {
        match target {
            HirLValue::Variable { id, .. } => {
                let rhs = self.build_expression(config, value, false);
                let variable = self.read_variable(*id);

                if variable.ctx.is_nullable() {
                    let value_ty = self.ty_to_mir(config, &value.get_type());
                    let inner_ty = match &*value.get_type() {
                        Type::Nullable(inner) => {
                            self.ty_to_mir(config, &inner)
                        },
                        _ => value_ty.clone(),
                    };
                    self.build_nullable(
                        config,
                        value,
                        MirOperand::Local(variable.id),
                        &value_ty,
                        &inner_ty,
                    );
                } else {
                    self.store(MirOperand::Local(variable.id), rhs, variable.ty.clone());
                }
            }
            HirLValue::ArrayAccess { array, index, arr_ty, ty, .. } => {
                // let ty = array.get_type().to_mir_ty();
                let arr_ty = self.ty_to_mir(config, arr_ty);
                let ty = self.ty_to_mir(config, ty);
                let target = self.array_access(config, array, index, arr_ty, ty.clone(), true);
                let value_ty = self.ty_to_mir(config, &value.get_type());

                if value_ty.is_aggregate() {
                    self.build_aggregate(config, value, target, &value_ty);
                } else {
                    let rhs = self.build_expression(config, value, false);
                    self.store(target, rhs.clone(), ty);
                }
            }
            HirLValue::Deref { value, .. } => {
                let operand = self.build_expression(config, value, false);

                let rhs = self.build_expression(config, value, false);
                let rhs_ty = rhs.get_type(self.current_function.as_ref().unwrap());

                self.store(operand, rhs, rhs_ty);
            }
            HirLValue::FieldAccess { object, field_id, base_ty, ty, .. } => {
                let base = self.build_expression(config, object, true);
                let indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::new_constant_int(*field_id as i64),
                ];

                let ty = self.ty_to_mir(config, ty);
                let base_ty = self.ty_to_mir(config, base_ty);

                let local_id = self.new_local(ty.clone());

                self.get_element_ptr(local_id, base, indices, base_ty.clone(), base_ty);

                let value_ty = self.ty_to_mir(config, &value.get_type());

                match value_ty.is_aggregate() {
                    true => {
                        self.build_aggregate(config, value, MirOperand::Local(local_id), &value_ty);
                    }
                    false => {
                        let rhs = self.build_expression(config, value, false);
                        self.store(MirOperand::Local(local_id), rhs, ty);
                    }
                }
            }
        }
    }
}
