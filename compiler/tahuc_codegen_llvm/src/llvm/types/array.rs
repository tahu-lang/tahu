use tahuc_llvm::{opaque::LLVMTypeRef};

use super::super::types::{fn_ty::FunctionType, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArrayType {
    ty: Type,
    size: u32,
}

impl ArrayType {
    pub fn new(ty: Type, size: u32) -> Self {
        Self { ty, size }
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty.as_type_ref()
    }

    pub fn as_ty(self) -> Type {
        self.ty
    }

    pub fn fn_ty(self, params: &[Type], is_var_arg: bool) -> FunctionType {
        self.ty.fn_ty(params, is_var_arg)
    }
}