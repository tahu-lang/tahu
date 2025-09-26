use tahuc_ast::ty::Type;
use tahuc_hir::hir::HirExpression;

use crate::{
    builder::builder::Builder,
    mir::{
        instruction::MirOperand,
        ty::{MirType, ToMirType},
    },
};

impl Builder {
    pub(crate) fn build_aggregate(
        &mut self,
        expression: &HirExpression,
        addr: MirOperand,
        value_ty: &MirType,
    ) -> MirOperand {
        match expression {
            HirExpression::Variable { id, .. } => {
                let variable = self.read_variable(*id);

                if variable.ctx.is_nullable() || variable.ctx.is_parameter() {
                    MirOperand::Local(variable.id)
                } else {
                    let temp = self.new_local(variable.ty.clone());
                    self.load(temp, MirOperand::Local(variable.id), variable.ty.clone());
                    MirOperand::Local(temp)
                }
            }
            HirExpression::ArrayLiteral { elements, ty, .. } => {
                let element_ty = match ty {
                    Type::Array { ty, .. } => ty.as_ref().clone(),
                    _ => panic!("ArrayLiteral must have Array type"),
                };

                for (i, e_expr) in elements.iter().enumerate() {
                    let gep_operand = self.new_local(element_ty.to_mir_ty());

                    self.get_element_ptr(
                        gep_operand,
                        addr.clone(),
                        vec![
                            MirOperand::new_constant_int(0),
                            MirOperand::new_constant_int(i as i64),
                        ],
                        ty.to_mir_ty(),
                        ty.to_mir_ty(),
                    );

                    // Build value
                    match e_expr.get_type().to_mir_ty() {
                        ty if ty.is_aggregate() => {
                            self.build_aggregate(e_expr, MirOperand::Local(gep_operand), value_ty);
                        }
                        _ => {
                            let value = self.build_expression(e_expr, false);
                            self.store(
                                MirOperand::Local(gep_operand),
                                value,
                                element_ty.to_mir_ty(),
                            );
                        }
                    }
                }

                addr.clone()
            }
            HirExpression::StructLiteral { fields, ty, .. } => {
                for (id, e_expr) in fields.iter() {
                    let gep_operand = self.new_local(ty.to_mir_ty());

                    self.get_element_ptr(
                        gep_operand,
                        addr.clone(),
                        vec![
                            MirOperand::new_constant_int(0),
                            MirOperand::new_constant_int(*id as i64),
                        ],
                        ty.to_mir_ty(),
                        ty.to_mir_ty(),
                    );

                    match e_expr.get_type().to_mir_ty() {
                        ty if ty.is_aggregate() => {
                            self.build_aggregate(e_expr, MirOperand::Local(gep_operand), value_ty);
                        }
                        _ => {
                            let value = self.build_expression(e_expr, false);
                            self.store(
                                MirOperand::Local(gep_operand),
                                value,
                                ty.to_mir_ty(),
                            );
                        }
                    }
                }

                addr.clone()
            }
            HirExpression::FieldAccess { object, field_id, base_ty, ty, .. } => {
                let base = self.build_expression(object, true);
                let indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::new_constant_int(*field_id as i64),
                ];

                let local_id = self.new_local(ty.to_mir_ty());

                self.get_element_ptr(local_id, base, indices, base_ty.to_mir_ty(), ty.to_mir_ty());

                let load_id = self.new_local(ty.to_mir_ty());
                self.load(load_id, MirOperand::Local(local_id), ty.to_mir_ty());
                self.store(addr.clone(), MirOperand::Local(load_id), ty.to_mir_ty());
                

                addr.clone()
            }
            HirExpression::Call { callee, arguments, signature, ty, .. } => {
                let target = addr.get_local().unwrap();
                self.build_call_expression(target, callee, arguments, signature, ty)
            }
            _ => {
                panic!("Trying to build {:?}", expression);
            }
        }
    }
}
