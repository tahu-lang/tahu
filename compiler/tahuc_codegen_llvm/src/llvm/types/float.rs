use tahuc_llvm::{core::LLVMConstReal, opaque::LLVMTypeRef};

use crate::llvm::{FloatValue, FunctionType, Type, Values};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FloatType {
    ty: Type,
}

impl FloatType {
    pub fn new(ty: Type) -> Self {
        Self { ty }
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty.as_type_ref()
    }

    pub fn fn_ty(self, params: &[Type], is_var_arg: bool) -> FunctionType {
        self.ty.fn_ty(params, is_var_arg)
    }

    pub fn const_float(self, value: f64) -> FloatValue {
        let value = Values::new(unsafe { LLVMConstReal(self.as_type_ref(), value) });
        FloatValue::new(value)
    }

    pub fn as_ty(self) -> Type {
        self.ty
    }
}
