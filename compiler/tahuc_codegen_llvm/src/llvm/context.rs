use std::ffi::CString;

use tahuc_llvm::{
    core::{
        LLVMArrayType, LLVMConstStringInContext, LLVMStructCreateNamed,
        context::*,
    },
    opaque::LLVMContextRef,
};

use crate::llvm::{
    ArrayType, ArrayValue, BasicBlock, BoolType, Builder, FloatType, FunctionValue, IntType,
    Module, PointerType, StructType, Type, Values, VoidType,
};

#[derive(Debug, Clone, Copy)]
pub struct Context {
    ctx: LLVMContextRef,
}

impl Context {
    pub fn new(ctx: LLVMContextRef) -> Self {
        Self { ctx }
    }

    pub fn create() -> Self {
        let ctx = unsafe { LLVMContextCreate() };

        Context::new(ctx)
    }

    pub fn create_builder(&self) -> Builder {
        let builder = unsafe { LLVMCreateBuilderInContext(self.ctx) };
        Builder::new(builder)
    }

    pub fn create_module(&self, name: &str) -> Module {
        let name = CString::new(name).unwrap();
        let name = name.as_ptr();

        let module = unsafe { LLVMModuleCreateWithNameInContext(name, self.ctx) };

        Module::new(module)
    }

    pub fn append_basic_block(&self, function: FunctionValue, name: &str) -> BasicBlock {
        let name = CString::new(name).unwrap();
        let name = name.as_ptr();

        BasicBlock::new(unsafe {
            LLVMAppendBasicBlockInContext(self.ctx, function.as_value_ref(), name)
        })
    }

    pub fn bool_ty(&self) -> BoolType {
        let ty = Type::new(unsafe { LLVMInt1TypeInContext(self.ctx) });

        BoolType::new(ty)
    }

    pub fn i8_ty(&self) -> IntType {
        let ty = Type::new(unsafe { LLVMInt8TypeInContext(self.ctx) });

        IntType::new(ty)
    }

    pub fn i16_ty(&self) -> IntType {
        let ty = Type::new(unsafe { LLVMInt16TypeInContext(self.ctx) });

        IntType::new(ty)
    }

    pub fn i32_ty(&self) -> IntType {
        let ty = Type::new(unsafe { LLVMInt32TypeInContext(self.ctx) });

        IntType::new(ty)
    }

    pub fn i64_ty(&self) -> IntType {
        let ty = Type::new(unsafe { LLVMInt64TypeInContext(self.ctx) });

        IntType::new(ty)
    }

    pub fn f32_ty(&self) -> FloatType {
        let ty = Type::new(unsafe { LLVMFloatTypeInContext(self.ctx) });

        FloatType::new(ty)
    }

    pub fn f64_ty(&self) -> FloatType {
        let ty = Type::new(unsafe { LLVMDoubleTypeInContext(self.ctx) });

        FloatType::new(ty)
    }

    pub fn custom_width_int_ty(&self, width: u32) -> IntType {
        let ty = Type::new(unsafe { LLVMIntTypeInContext(self.ctx, width) });

        IntType::new(ty)
    }

    pub fn void_ty(&self) -> VoidType {
        let ty = Type::new(unsafe { LLVMVoidTypeInContext(self.ctx) });

        VoidType::new(ty)
    }

    pub fn array_ty(&self, element_ty: Type, element_count: u32) -> ArrayType {
        let ty = Type::new(unsafe { LLVMArrayType(element_ty.as_type_ref(), element_count) });

        ArrayType::new(ty, element_count)
    }

    pub fn ptr_ty(&self) -> PointerType {
        let ty = Type::new(unsafe { LLVMPointerTypeInContext(self.ctx, 0) });

        PointerType::new(ty)
    }

    pub fn struct_ty(&self, name: &str) -> StructType {
        let name = CString::new(name).unwrap();
        let name = name.as_ptr();

        let ty = Type::new(unsafe { LLVMStructCreateNamed(self.ctx, name) });

        StructType::new(ty)
    }

    pub fn const_string(self, string: &[u8], null_terminated: bool) -> ArrayValue {
        let value = Values::new(unsafe {
            LLVMConstStringInContext(
                self.ctx,
                string.as_ptr() as *const i8,
                string.len() as u32,
                !null_terminated as i32,
            )
        });

        ArrayValue::new(value)
    }
}
