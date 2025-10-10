use tahuc_mir::mir::function::{MirExternFunction, MirFunction};
use tahuc_module::config::Config;

use crate::codegen::Codegen;

trait ToLlvmVisibility {
    fn to_llvm_visibility(&self) -> crate::llvm::Visibility;
    fn to_llvm_linkage(&self) -> crate::llvm::Linkage;
}

impl ToLlvmVisibility for tahuc_hir::hir::HirVisibility {
    fn to_llvm_visibility(&self) -> crate::llvm::Visibility {
        match self {
            tahuc_hir::hir::HirVisibility::Public => crate::llvm::Visibility::Public,
            tahuc_hir::hir::HirVisibility::Private => crate::llvm::Visibility::Private,
        }
    }

    fn to_llvm_linkage(&self) -> crate::llvm::Linkage {
        match self {
            tahuc_hir::hir::HirVisibility::Public => crate::llvm::Linkage::External,
            tahuc_hir::hir::HirVisibility::Private => crate::llvm::Linkage::Internal,
        }
    }
}

impl Codegen {
    pub(crate) fn declare_extern_function(&mut self, config: &mut Config, function: &MirExternFunction) {
        let ty = self.ty_to_llvm(config, &function.return_type);
        let params = function
            .parameters
            .iter()
            .map(|p| self.ty_to_llvm(config, &p))
            .collect::<Vec<_>>();

        let func_ty = ty.fn_ty(params.as_slice(), false);

        let fn_name = config.str_pool.get(function.name);
        let func = self.module.add_function(fn_name, func_ty);
        self.functions_map.insert(function.id, func);
    }

    pub(crate) fn declare_function(&mut self, config: &mut Config, function: &MirFunction) {
        let ty = self.ty_to_llvm(config, &function.return_type);
        let params = function
            .parameters
            .iter()
            .map(|p| self.ty_to_llvm(config, &p.ty))
            .collect::<Vec<_>>();

        let func_ty = ty.fn_ty(params.as_slice(), false);

        let fn_name = config.str_pool.get(function.name);
        let func = self.module.add_function(fn_name, func_ty);
        func.set_visibility(function.visibility.to_llvm_visibility());
        func.set_linkage(function.visibility.to_llvm_linkage());

        self.functions_map.insert(function.id, func);
    }

    fn reset_state(&mut self) {
        self.basic_blocks_map.clear();
        self.ptr_map.clear();
        self.value_map.clear();
    }

    pub(crate) fn compile_function(&mut self, config: &mut Config, function: &MirFunction) {
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
            self.compile_basic_block(config, block);
        }
    }
}