use std::{collections::HashMap, process::exit};

use tahuc_hir::hir::FunctionId;
use tahuc_mir::mir::{
    BasicBlockId, GlobalId, LocalId, MirModule, instruction::MirOperand, ty::MirConstant,
};
use tahuc_span::FileId;

use crate::llvm::{
    BasicBlock, BasicValue, Builder, CodeModel, Context, FileType, FunctionValue, Module, OptLevel,
    PointerValue, RelocMode, StructType, Target, TargetTriple, Type,
};

pub struct Codegen {
    // llvm
    pub(crate) context: Context,
    pub(crate) module: Module,
    pub(crate) builder: Builder,

    // current file id
    pub(crate) file_id: FileId,

    // mapping function
    pub(crate) functions_map: HashMap<FunctionId, FunctionValue>,

    // mapping for current function
    pub(crate) basic_blocks_map: HashMap<BasicBlockId, BasicBlock>,
    pub(crate) ptr_map: HashMap<LocalId, PointerValue>,
    pub(crate) value_map: HashMap<LocalId, BasicValue>,

    pub(crate) struct_types: HashMap<String, StructType>,
    pub(crate) global_types: HashMap<GlobalId, Type>,
}

impl Codegen {
    pub fn new(context: Context, name: String) -> Self {
        let builder = context.create_builder();
        let module = context.create_module(&name);

        Self {
            context,
            module,
            builder,

            file_id: FileId(0),

            functions_map: HashMap::new(),

            basic_blocks_map: HashMap::new(),
            ptr_map: HashMap::new(),
            value_map: HashMap::new(),

            struct_types: HashMap::new(),
            global_types: HashMap::new(),
        }
    }

    pub fn context() -> Context {
        Context::create()
    }

    pub fn compile_module(&mut self, module: &MirModule) {
        self.file_id = module.file_id;

        for (id, ty) in &module.globals {
            let ty = self.ty_to_llvm(ty);
            self.global_types.insert(*id, ty);
        }

        for function in &module.extern_functions {
            self.declare_extern_function(function);
        }

        for function in &module.functions {
            self.declare_function(function);
        }

        for function in &module.functions {
            self.compile_function(function);
        }
    }

    pub(crate) fn operand_ptr(&self, operand: &MirOperand) -> Result<PointerValue, String> {
        match operand {
            MirOperand::Local(id) => self
                .ptr_map
                .get(id)
                .copied()
                .ok_or_else(|| format!("Local ptr {} not found", id)),
            MirOperand::Constant(c) => {
                Err(format!("Constant cannot to pointer {} {:?}", c, operand))
            }
        }
    }

    pub(crate) fn operand_value(&self, operand: &MirOperand) -> Result<BasicValue, String> {
        match operand {
            MirOperand::Local(id) => self
                .value_map
                .get(id)
                .copied()
                .ok_or_else(|| format!("Local value {} not found", id)),
            MirOperand::Constant(c) => Ok(self.compile_constant(c)),
        }
    }

    pub(crate) fn compile_constant(&self, constant: &MirConstant) -> BasicValue {
        match constant {
            MirConstant::Int { value, ty } => {
                let ty = self.int_ty(ty);
                let value = ty.const_int(*value as u64, true);
                value.into()
            }
            MirConstant::Float { value, ty } => {
                let ty = self.float_ty(ty);
                let value = ty.const_float(*value as f64);
                value.into()
            }
            MirConstant::Bool(b) => self.context.bool_ty().const_bool(*b as u64, false).into(),
            MirConstant::Null(_) => self.context.ptr_ty().const_null().into(),
            MirConstant::Char(c) => self.context.i8_ty().const_int(*c as u64, false).into(),
            MirConstant::String(s) => {
                let value = self.context.const_string(s.as_bytes(), false);
                let global_string = self
                    .module
                    .add_global(value.get_type(), &format!("str_{}", self.file_id.0));
                global_string.set_initializer(value.into());
                global_string.as_ptr().into()
            }
        }
    }

    pub(crate) fn add_value(&mut self, target: LocalId, value: BasicValue) {
        if value.is_pointer_value() {
            self.ptr_map.insert(target, value.into_ptr_value());
        }
        self.value_map.insert(target, value);
    }

    pub(crate) fn add_ptr(&mut self, target: LocalId, ptr: PointerValue) {
        self.ptr_map.insert(target, ptr);
        self.value_map.insert(target, ptr.into());
    }

    pub fn verify(&self) {
        let res = self.module.verify();
        if let Err(_) = res {
            exit(1);
        }
    }

    pub fn to_string(&self) -> String {
        self.module.print_to_string()
    }

    pub fn write_object_file(&self, path: &str) {
        Target::init_x86();

        let triple = TargetTriple::default();
        let target = Target::from_triple(triple);

        let machine = target.create_target_machine(
            triple,
            "",
            "",
            OptLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        );

        let result = machine.emit_to_file(self.module, path, FileType::Object);

        if let Err(s) = result {
            panic!("failed to write object file {}", s);
        }
    }
}