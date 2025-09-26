use tahuc_ast::ty::Type;
use tahuc_hir::hir::{FunctionSignature, HirExpression};

use crate::{builder::builder::Builder, mir::{instruction::MirOperand, ty::ToMirType, LocalId}};

impl Builder {
    pub(crate) fn build_call_expression(
        &mut self,
        target: LocalId,
        callee: &u32,
        arguments: &Vec<HirExpression>,
        signature: &FunctionSignature,
        ty: &Type,
    ) -> MirOperand {
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
            } else if argument.get_type().is_struct() {
                args.push(self.build_expression(argument, true));
            } else {
                match param_ty {
                    a if a.to_mir_ty().is_aggregate() => {
                        args.push(self.build_expression(argument, true));
                    }
                    _ => {
                        args.push(self.build_expression(argument, false));
                    }
                }
            }
        }

        self.call(Some(target), *callee, args, ty.to_mir_ty());

        MirOperand::Local(target)
    }
}
