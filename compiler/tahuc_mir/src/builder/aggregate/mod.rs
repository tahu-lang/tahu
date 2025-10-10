use tahuc_ast::ty::Type;
use tahuc_hir::hir::HirExpression;
use tahuc_module::config::Config;

use crate::{
    builder::builder::Builder,
    mir::{instruction::MirOperand, ty::MirType},
};

impl Builder {
    pub(crate) fn build_aggregate(
        &mut self,
        config: &mut Config,
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
                let element_ty = match &**ty {
                    Type::Array { ty, .. } => ty.as_ref().clone(),
                    _ => panic!("ArrayLiteral must have Array type"),
                };

                let ty = self.ty_to_mir(config, ty);
                let element_ty = self.ty_to_mir(config, &element_ty);

                for (i, e_expr) in elements.iter().enumerate() {
                    let gep_operand = self.new_local(element_ty.clone());

                    self.get_element_ptr(
                        gep_operand,
                        addr.clone(),
                        vec![
                            MirOperand::new_constant_int(0),
                            MirOperand::new_constant_int(i as i64),
                        ],
                        ty.clone(),
                        ty.clone(),
                    );

                    // Build value
                    match self.ty_to_mir(config, &e_expr.get_type()) {
                        ty if ty.is_aggregate() => {
                            self.build_aggregate(config, e_expr, MirOperand::Local(gep_operand), value_ty);
                        }
                        _ => {
                            let value = self.build_expression(config, e_expr, false);
                            self.store(MirOperand::Local(gep_operand), value, element_ty.clone());
                        }
                    }
                }

                addr.clone()
            }
            HirExpression::StructLiteral { fields, ty, .. } => {
                let ty = self.ty_to_mir(config, ty);
                for (id, e_expr) in fields.iter() {
                    let gep_operand = self.new_local(ty.clone());

                    self.get_element_ptr(
                        gep_operand,
                        addr.clone(),
                        vec![
                            MirOperand::new_constant_int(0),
                            MirOperand::new_constant_int(*id as i64),
                        ],
                        ty.clone(),
                        ty.clone(),
                    );

                    let expr_ty = self.ty_to_mir(config, &e_expr.get_type());
                    match expr_ty {
                        ty if ty.is_aggregate() => {
                            self.build_aggregate(config, e_expr, MirOperand::Local(gep_operand), value_ty);
                        }
                        _ => {
                            let value = self.build_expression(config, e_expr, false);
                            self.store(MirOperand::Local(gep_operand), value, ty.clone());
                        }
                    }
                }

                addr.clone()
            }
            HirExpression::FieldAccess {
                object,
                field_id,
                base_ty,
                ty,
                ..
            } => {
                let base = self.build_expression(config, object, true);
                let indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::new_constant_int(*field_id as i64),
                ];
                let ty = self.ty_to_mir(config, ty);
                let base_ty = self.ty_to_mir(config, base_ty);

                let local_id = self.new_local(ty.clone());

                self.get_element_ptr(local_id, base, indices, base_ty, ty.clone());

                let load_id = self.new_local(ty.clone());
                self.load(load_id, MirOperand::Local(local_id), ty.clone());
                self.store(addr.clone(), MirOperand::Local(load_id), ty);

                addr.clone()
            }
            HirExpression::Call {
                callee,
                arguments,
                signature,
                ty,
                ..
            } => {
                let target = addr.get_local().unwrap();
                let ty = self.ty_to_mir(config, ty);
                self.build_call_expression(config, target, callee, arguments, signature, ty)
            }
            _ => {
                panic!("Trying to build {:?}", expression);
            }
        }
    }
}
