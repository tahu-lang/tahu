use tahuc_ast::ty::Type;
use tahuc_hir::hir::{FunctionSignature, HirExpression};
use tahuc_module::config::Config;

use crate::{
    builder::builder::Builder,
    mir::{instruction::MirOperand, ty::MirType, LocalId},
};

impl Builder {
    pub(crate) fn build_call_expression(
        &mut self,
        config: &mut Config,
        target: LocalId,
        callee: &u32,
        arguments: &Vec<HirExpression>,
        signature: &FunctionSignature,
        ty: MirType,
    ) -> MirOperand {
        let mut args = Vec::new();

        for (i, argument) in arguments.iter().enumerate() {
            // argument is null and signature not null get field
            let param_ty = signature.args.get(i).unwrap();
            // argument is nullable
            if argument.get_type().is_nullable() {
                // argument nullable and param nullable return addr
                if matches!(&**param_ty, Type::Nullable(_)) {
                    args.push(self.build_expression(config, argument, true));
                } else {
                    let inner_ty = match &*argument.get_type() {
                        Type::Nullable(inner) => inner.clone(),
                        _ => unreachable!(),
                    };
                    let inner_ty = self.ty_to_mir(config, &inner_ty);
                    let ptr = self.build_expression(config, argument, true);
                    let base_ty = self.ty_to_mir(config, &argument.get_type());
                    args.push(self.nullable_access_value(ptr, base_ty, inner_ty));
                }
            } else if argument.get_type().is_struct() {
                args.push(self.build_expression(config, argument, true));
            } else {
                let param_ty = self.ty_to_mir(config, param_ty);
                match param_ty {
                    a if a.is_aggregate() => {
                        args.push(self.build_expression(config, argument, true));
                    }
                    _ => {
                        args.push(self.build_expression(config, argument, false));
                    }
                }
            }
        }

        self.call(target, *callee, args, ty);

        MirOperand::Local(target)
    }
}
