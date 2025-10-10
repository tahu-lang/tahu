use tahuc_ast::ty::Type;
use tahuc_hir::hir::{HirFunction, HirParameters};
use tahuc_module::config::Config;
use tahuc_span::StrId;

use crate::{
    builder::builder::{Builder, Variable, VariableContext},
    mir::{
        LocalId, block::MirTerminator, function::MirFunction, instruction::MirOperand, ty::MirType,
    },
};

impl Builder {
    pub(crate) fn build_function(&mut self, config: &mut Config, function: &HirFunction) -> MirFunction {
        self.reset_state();

        let func = self.new_fn(config, function);
        self.set_fn(func);

        // create entry block
        let entry = self.new_block("entry");
        // switch to entry block
        self.switch_to_block(entry);

        self.build_parameter(config, &function.parameters);

        self.build_block(config, &function.body);

        // fallback return if dont have return
        self.fallback_return();

        self.compute_block_order();
        self.current_function.take().unwrap()
    }

    fn new_fn(&mut self, config: &mut Config, function: &HirFunction) -> MirFunction {
        let ret_ty = self.ty_to_mir(config, &function.return_type);
        MirFunction::new(
            function.id,
            function.name,
            ret_ty,
            function.visibility,
        )
    }

    fn compute_block_order(&mut self) {
        self.current_function
            .as_mut()
            .unwrap()
            .compute_block_order();
    }

    fn build_parameter(&mut self, config: &mut Config, parameters: &Vec<HirParameters>) {
        for param in parameters.iter() {
            let (ty, inner, ctx) = self.get_param_ty(config, param);

            let id = self.add_parameter(ty.clone(), param.name);
            self.add_variable_cfg(
                param.id,
                Variable {
                    id,
                    ctx,
                    inner_ty: inner.clone(),
                    ty: ty.clone(),
                },
            );

            if ty.is_primitive() {
                let alias_id = self.new_local(ty.clone());
                self.add_variable_as(
                    id,
                    Variable {
                        id: alias_id,
                        ty: ty.clone(),
                        inner_ty: inner.clone(),
                        ctx: VariableContext::Local,
                    },
                );

                self.alloca(alias_id, ty.clone());
                self.store(
                    MirOperand::Local(alias_id),
                    MirOperand::Local(id),
                    ty.clone(),
                );
            }
        }
    }

    fn get_current_function(&mut self) -> &mut MirFunction {
        self.current_function.as_mut().unwrap()
    }

    fn add_parameter(&mut self, ty: MirType, name: StrId) -> LocalId {
        let func = self.get_current_function();
        func.add_parameter(ty, name)
    }

    fn get_param_ty(
        &mut self,
        config: &mut Config,
        param: &HirParameters,
    ) -> (MirType, Option<MirType>, VariableContext) {
        if matches!(&*param.ty, Type::Nullable(_)) {
            let inner = match &*param.ty {
                Type::Nullable(inner) => inner,
                _ => unreachable!(),
            };
            let param_ty = self.ty_to_mir(config, &param.ty);
            let ty = MirType::Pointer(Box::new(param_ty));
            let inner_ty = self.ty_to_mir(config, &inner);
            return (ty, Some(inner_ty), VariableContext::ParameterNullable);
        }
        if param.ty.is_struct() {
            let param_ty = self.ty_to_mir(config, &param.ty);
            let ty = MirType::Pointer(Box::new(param_ty));
            return (ty, None, VariableContext::Parameter);
        }

        let ty = self.ty_to_mir(config, &param.ty);
        (ty, None, VariableContext::Parameter)
    }

    /// falback return if function is return void or unit
    fn fallback_return(&mut self) {
        if let Some(block_id) = self.current_block {
            let func = self.current_function.as_mut().unwrap();
            if let Some(block) = func.get_block_mut(block_id) {
                if matches!(block.terminator, MirTerminator::Unreachable) {
                    block.set_terminator(MirTerminator::Return { value: None });
                }
            }
        }
    }
}
