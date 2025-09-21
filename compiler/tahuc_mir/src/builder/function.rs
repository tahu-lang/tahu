use tahuc_ast::Type;
use tahuc_hir::hir::{HirFunction, HirParameters};

use crate::{
    builder::builder::{Builder, Variable, VariableContext},
    mir::{
        block::MirTerminator,
        function::MirFunction,
        ty::{MirType, ToMirType}, LocalId,
    },
};

impl Builder {
    pub(crate) fn build_function(&mut self, function: &HirFunction) -> MirFunction {
        self.reset_state();

        let func = self.new_fn(function);
        self.set_fn(func);

        self.build_parameter(&function.parameters);

        // create entry block
        let entry = self.new_block("entry");
        // switch to entry block
        self.switch_to_block(entry);

        self.build_block(&function.body);

        // fallback return if dont have return
        self.fallback_return();

        self.compute_block_order();
        self.current_function.take().unwrap()
    }

    fn new_fn(&self, function: &HirFunction) -> MirFunction {
        MirFunction::new(
            function.id,
            function.name.clone(),
            function.return_type.to_mir_ty(),
            function.visibility.clone(),
        )
    }

    fn compute_block_order(&mut self) {
        self.current_function
            .as_mut()
            .unwrap()
            .compute_block_order();
    }

    fn build_parameter(&mut self, parameters: &Vec<HirParameters>) {
        for param in parameters.iter() {
            let (ty, inner, ctx) = self.get_param_ty(param);

            let id = self.add_parameter(ty.clone(), param.name.clone());
            self.add_variable_cfg(
                param.id,
                Variable {
                    id,
                    ctx,
                    inner_ty: inner,
                    ty: ty.clone(),
                },
            );
        }
    }

    fn get_current_function(&mut self) -> &mut MirFunction {
        self.current_function.as_mut().unwrap()
    }

    fn add_parameter(&mut self, ty: MirType, name: String) -> LocalId {
        let func = self.get_current_function();
        func.add_parameter(ty, name)
    }

    fn get_param_ty(&self, param: &HirParameters) -> (MirType, Option<MirType>, VariableContext) {
        if matches!(param.ty, Type::Nullable(_)) {
            let inner = match &param.ty {
                Type::Nullable(inner) => inner,
                _ => unreachable!(),
            };
            let ty = MirType::Pointer(Box::new(param.ty.to_mir_ty()));
            (
                ty,
                Some(inner.to_mir_ty()),
                VariableContext::ParameterNullable,
            )
        } else {
            let ty = param.ty.to_mir_ty();
            (ty, None, VariableContext::Parameter)
        }
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
