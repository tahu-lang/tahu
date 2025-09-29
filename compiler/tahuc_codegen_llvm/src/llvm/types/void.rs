use tahuc_llvm::{opaque::LLVMTypeRef};

use crate::llvm::{FunctionType, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VoidType {
    ty: Type,
}

impl VoidType {
    pub fn new(ty: Type) -> Self {
        Self { ty }
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
