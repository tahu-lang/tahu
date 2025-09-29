use tahuc_llvm::{core::LLVMConstPointerNull, opaque::LLVMTypeRef};

use crate::llvm::{FunctionType, PointerValue, Type, Values};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PointerType {
    ty: Type,
}

impl PointerType {
    pub fn new(ty: Type) -> Self {
        Self { ty }
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty.as_type_ref()
    }

    pub fn fn_ty(self, params: &[Type], is_var_arg: bool) -> FunctionType {
        self.ty.fn_ty(params, is_var_arg)
    }

    pub fn const_null(self) -> PointerValue {
        let value = Values::new(unsafe { LLVMConstPointerNull(self.as_type_ref()) });
        PointerValue::new(value)
    }

    pub fn as_ty(self) -> Type {
        self.ty
    }
}
