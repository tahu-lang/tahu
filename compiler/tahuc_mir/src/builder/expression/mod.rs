use tahuc_ast::Type;
use tahuc_hir::hir::HirExpression;

use crate::{
    builder::builder::Builder,
    mir::{instruction::MirOperand, ty::ToMirType},
};

pub(crate) mod array;
pub(crate) mod binary;
pub(crate) mod unary;

impl Builder {
    pub(crate) fn build_expression(
        &mut self,
        expression: &HirExpression,
        need_addr: bool,
    ) -> MirOperand {
        match expression {
            HirExpression::Literal { value, .. } => {
                let constant = self.build_literal(value);
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
            // HirExpression::Ternary { condition, then_branch, else_branch } => {}
            HirExpression::Binary {
                left,
                op,
                right,
                ty,
            } => self.build_binary_op(left, op.clone(), right, ty),
            HirExpression::Unary { op, operand, ty } => {
                self.build_unary_op(op.clone(), operand, ty)
            }
            HirExpression::ArrayLiteral { elements, ty } => self.build_array_literal(elements, &ty),
            HirExpression::ArrayAccess { array, index, ty } => {
                self.array_access(array, index, ty.to_mir_ty(), need_addr)
            }
            HirExpression::Call {
                callee,
                arguments,
                signature,
                ty,
            } => {
                let local_id = self.new_local(ty.to_mir_ty());

                let target = if ty.to_mir_ty().is_void() {
                    Some(local_id)
                } else {
                    None
                };

                let mut args = Vec::new();

                for (i, argument) in arguments.iter().enumerate() {
                    // argument is null and signature not null get field
                    let param_ty = signature.args.get(i).unwrap();
                    // argument is nullable
                    if argument.get_type().is_nullable() {
                        // argument nullable and param nullable return addr
                        if matches!(param_ty, Type::Nullable(_)) {
                            args.push(self.build_expression(argument, true));
                        } else {
                            let inner_ty = match argument.get_type() {
                                Type::Nullable(inner) => *inner,
                                _ => unreachable!(),
                            };
                            let ptr = self.build_expression(argument, true);
                            let base_ty = argument.get_type().to_mir_ty();
                            args.push(self.nullable_access_value(
                                ptr,
                                base_ty,
                                inner_ty.to_mir_ty(),
                            ));
                        }
                    } else {
                        match param_ty {
                            Type::Array { .. } => {
                                args.push(self.build_expression(argument, true));
                            }
                            _ => {
                                args.push(self.build_expression(argument, false));
                            }
                        }
                    }
                }

                self.call(target, *callee, args, ty.to_mir_ty());

                MirOperand::new_constant_null()
            }
            _ => {
                panic!("Unsupported expression: {:?}", expression);
            }
        }
    }
}
