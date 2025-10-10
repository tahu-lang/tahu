use tahuc_hir::hir::HirExpression;
use tahuc_module::config::Config;

use crate::{builder::builder::Builder, mir::instruction::MirOperand};

pub(crate) mod array;
pub(crate) mod binary;
pub(crate) mod call;
pub(crate) mod cast;
pub(crate) mod unary;

impl Builder {
    pub(crate) fn build_expression(
        &mut self,
        config: &mut Config,
        expression: &HirExpression,
        need_addr: bool,
    ) -> MirOperand {
        match expression {
            HirExpression::Literal { value, ty, .. } => {
                let ty = self.ty_to_mir(config, ty);
                let constant = self.build_literal(config, value, ty);
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
            HirExpression::Ternary {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let then_ty = self.ty_to_mir(config, &then_branch.get_type());
                let target = self.new_local(then_ty);
                let condition = self.build_expression(config, condition, false);
                let then_branch = self.build_expression(config, then_branch, false);
                let else_branch = self.build_expression(config, else_branch, false);
                self.select(target, condition, then_branch, else_branch);
                MirOperand::Local(target)
            }
            HirExpression::Binary {
                left,
                op,
                right,
                ty,
                ..
            } => self.build_binary_op(config, left, op.clone(), right, ty),
            HirExpression::Unary {
                op, operand, ty, ..
            } => self.build_unary_op(config, op.clone(), operand, ty),
            HirExpression::ArrayLiteral { elements, ty, .. } => {
                self.build_array_literal(config, elements, &ty)
            }
            HirExpression::ArrayAccess {
                array,
                index,
                arr_ty,
                ty,
                ..
            } => {
                let arr_ty = self.ty_to_mir(config, arr_ty);
                let ty = self.ty_to_mir(config, ty);
                self.array_access(config, array, index, arr_ty, ty, need_addr)
            }
            HirExpression::Call {
                callee,
                arguments,
                signature,
                ty,
                ..
            } => {
                let ty = self.ty_to_mir(config, ty);
                let local_id = self.new_local(ty.clone());
                self.build_call_expression(config, local_id, callee, arguments, signature, ty)
            }
            HirExpression::Cast {
                value, from, ty, ..
            } => self.build_cast(config, value, from, ty),
            HirExpression::Grouping { value, .. } => self.build_expression(config, value, false),
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

                self.get_element_ptr(local_id, base, indices, base_ty.clone(), base_ty);

                if need_addr {
                    MirOperand::Local(local_id)
                } else {
                    let load_id = self.new_local(ty.clone());
                    self.load(load_id, MirOperand::Local(local_id), ty.clone());
                    MirOperand::Local(load_id)
                }
            }
            HirExpression::MemberAccess {
                object, member, ty, ..
            } => {
                let _base = self.build_expression(config, object, true);
                let _indices = vec![
                    MirOperand::new_constant_int(0),
                    MirOperand::Constant(crate::mir::ty::MirConstant::String(member.to_string())),
                ];

                let ty = self.ty_to_mir(config, ty);
                let local_id = self.new_local(ty.clone());

                // self.get_element_ptr(local_id, base, indices, ty.to_mir_ty());

                println!("DEBUG MIR: MemberAccess {:?} {} {}", object, member, ty);
                // self.member_access(local_id, object, member, ty.to_mir_ty());
                MirOperand::Local(local_id)
            }
            HirExpression::StructType { id, ty, .. } => {
                let struct_id = self.read_struct(*id);
                let ty = self.ty_to_mir(config, ty);

                if need_addr {
                    let local_id = self.new_local(ty.clone());
                    self.alloca(local_id, ty);
                    MirOperand::Local(local_id)
                } else {
                    let local_id = self.new_local(ty.clone());
                    self.load(local_id, MirOperand::Local(struct_id), ty);
                    MirOperand::Local(local_id)
                }
            }
            HirExpression::StructLiteral {
                object, fields, ty, ..
            } => {
                let base = self.build_expression(config, object, true);
                let ty = self.ty_to_mir(config, ty);

                for (id, e_expr) in fields.iter() {
                    let gep_operand = self.new_local(ty.clone());

                    self.get_element_ptr(
                        gep_operand,
                        base.clone(),
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
                            self.build_aggregate(config, e_expr, MirOperand::Local(gep_operand), &ty);
                        }
                        _ => {
                            let value = self.build_expression(config, e_expr, false);
                            self.store(MirOperand::Local(gep_operand), value, ty.clone());
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
