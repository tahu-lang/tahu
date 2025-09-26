use tahuc_hir::hir::HirExpression;

use crate::{
    builder::builder::Builder,
    mir::{instruction::{MirOperand}, ty::ToMirType},
};

pub(crate) mod array;
pub(crate) mod binary;
pub(crate) mod unary;
pub(crate) mod cast;
pub(crate) mod call;

impl Builder {
    pub(crate) fn build_expression(
        &mut self,
        expression: &HirExpression,
        need_addr: bool,
    ) -> MirOperand {
        match expression {
            HirExpression::Literal { value, ty, .. } => {
                let constant = self.build_literal(value, ty.to_mir_ty());
                MirOperand::Constant(constant)
            }
            HirExpression::Variable { id, .. } => {
                let variable = self.read_variable(*id);

                if need_addr || variable.ctx.is_nullable() || variable.ctx.is_parameter() {
                    MirOperand::Local(variable.id)
                } else {
                    let temp = self.new_local(variable.ty.clone());
                    self.load(temp, MirOperand::Local(variable.id), variable.ty.clone());
                    MirOperand::Local(temp)
                }
            }
            HirExpression::Ternary { condition, then_branch, else_branch, .. } => {
                let target = self.new_local(then_branch.get_type().to_mir_ty());
                let condition = self.build_expression(condition, false);
                let then_branch = self.build_expression(then_branch, false);
                let else_branch = self.build_expression(else_branch, false);
                self.select(target, condition, then_branch, else_branch);
                MirOperand::Local(target)
            }
            HirExpression::Binary {
                left,
                op,
                right,
                ty,
                ..
            } => self.build_binary_op(left, op.clone(), right, ty),
            HirExpression::Unary { op, operand, ty, .. } => {
                self.build_unary_op(op.clone(), operand, ty)
            }
            HirExpression::ArrayLiteral { elements, ty, .. } => self.build_array_literal(elements, &ty),
            HirExpression::ArrayAccess { array, index, arr_ty, ty, .. } => {
                self.array_access(array, index, arr_ty.to_mir_ty(), ty.to_mir_ty(), need_addr)
            }
            HirExpression::Call {
                callee,
                arguments,
                signature,
                ty,
                ..
            } => {
                let local_id = self.new_local(ty.to_mir_ty());
                self.build_call_expression(local_id, callee, arguments, signature, ty)
            }
            HirExpression::Cast { value, from, ty, .. } => {
                self.build_cast(value, from, ty)
            }
            HirExpression::Grouping { value, .. } => {
                self.build_expression(value, false)
            }
            HirExpression::FieldAccess { object, field_id, base_ty, ty, .. } => {
                let base = self.build_expression(object, true);
                let indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::new_constant_int(*field_id as i64),
                ];

                let local_id = self.new_local(ty.to_mir_ty());

                self.get_element_ptr(local_id, base, indices, base_ty.to_mir_ty(), base_ty.to_mir_ty());

                if need_addr {
                    MirOperand::Local(local_id)
                } else {
                    let load_id = self.new_local(ty.to_mir_ty());
                    self.load(load_id, MirOperand::Local(local_id), ty.to_mir_ty());
                    MirOperand::Local(load_id)
                }
            }
            HirExpression::MemberAccess { object, member, ty, .. } => {
                let base = self.build_expression(object, true);
                let indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::Constant(crate::mir::ty::MirConstant::String(member.to_string())),
                ];

                let local_id = self.new_local(ty.to_mir_ty());

                // self.get_element_ptr(local_id, base, indices, ty.to_mir_ty());

                println!("DEBUG MIR: MemberAccess {:?} {} {}", object, member, ty);
                // self.member_access(local_id, object, member, ty.to_mir_ty());
                MirOperand::Local(local_id)
            }
            HirExpression::StructType { id, ty, .. } => {
                let struct_id = self.read_struct(*id);

                if need_addr {
                    let local_id = self.new_local(ty.to_mir_ty());
                    self.alloca(local_id, ty.to_mir_ty());
                    MirOperand::Local(local_id)
                } else {
                    let local_id = self.new_local(ty.to_mir_ty());
                    self.load(local_id, MirOperand::Local(struct_id), ty.to_mir_ty());
                    MirOperand::Local(local_id)
                }
            }
            HirExpression::StructLiteral { object, fields, ty, .. } => {
                let base = self.build_expression(object, true);

                for (id, e_expr) in fields.iter() {
                    let gep_operand = self.new_local(ty.to_mir_ty());

                    self.get_element_ptr(
                        gep_operand,
                        base.clone(),
                        vec![
                            MirOperand::new_constant_int(0),
                            MirOperand::new_constant_int(*id as i64),
                        ],
                        ty.to_mir_ty(),
                        ty.to_mir_ty(),
                    );

                    match e_expr.get_type().to_mir_ty() {
                        ty if ty.is_aggregate() => {
                            self.build_aggregate(e_expr, MirOperand::Local(gep_operand), &ty);
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

                base
            }
            _ => {
                panic!("Unsupported expression: {:?}", expression);
            }
        }
    }
}
