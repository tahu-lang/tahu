use tahuc_mir::mir::function::{MirExternFunction, MirFunction};

use crate::codegen::Codegen;

impl Codegen {
    pub(crate) fn declare_extern_function(&mut self, function: &MirExternFunction) {
        let ty = self.ty_to_llvm(&function.return_type);
        let params = function
            .parameters
            .iter()
            .map(|p| self.ty_to_llvm(&p))
            .collect::<Vec<_>>();

        let func_ty = ty.fn_ty(params.as_slice(), false);

        let func = self.module.add_function(&function.name, func_ty);
        self.functions_map.insert(function.id, func);
    }

    pub(crate) fn declare_function(&mut self, function: &MirFunction) {
        let ty = self.ty_to_llvm(&function.return_type);
        let params = function
            .parameters
            .iter()
            .map(|p| self.ty_to_llvm(&p.ty))
            .collect::<Vec<_>>();

        let func_ty = ty.fn_ty(params.as_slice(), false);

        let func = self.module.add_function(&function.name, func_ty);
        self.functions_map.insert(function.id, func);
    }

    fn reset_state(&mut self) {
        self.basic_blocks_map.clear();
        self.ptr_map.clear();
        self.value_map.clear();
    }

    pub(crate) fn compile_function(&mut self, function: &MirFunction) {
        self.reset_state();

        let func = self.functions_map[&function.id];

        for id in &function.block_order {
            let block = function.get_block(*id).unwrap();
            let bb = self.context.append_basic_block(func, &block.name);
            self.basic_blocks_map.insert(block.id, bb);
        }

        let entry = self.basic_blocks_map.get(&function.entry_block).unwrap();
        self.builder.position_at_end(*entry);

        for (i, param) in function.parameters.iter().enumerate() {
            let id = param.id;
            let value = func.get_param_at(i);
            self.add_value(id, value);
        }

        for id in &function.block_order {
            let block = function.get_block(*id).unwrap();
            self.compile_basic_block(block);
        }
    }
}