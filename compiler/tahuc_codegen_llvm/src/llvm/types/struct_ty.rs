use tahuc_llvm::{core::LLVMStructSetBody, opaque::LLVMTypeRef};

use crate::llvm::{FunctionType, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructType {
    ty: Type,
}

impl StructType {
    pub fn new(ty: Type) -> Self {
        Self { ty }
    }

    pub fn set_body(self, element_types: &[Type], packed: bool) {
        let mut element_ty: Vec<LLVMTypeRef> =
            element_types.iter().map(|val| val.as_type_ref()).collect();
        unsafe {
            LLVMStructSetBody(
                self.ty.as_type_ref(),
                element_ty.as_mut_ptr(),
                element_ty.len() as std::ffi::c_uint,
                packed as std::ffi::c_int,
            );
        }
    }

    pub fn as_ty(self) -> Type {
        self.ty
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty.as_type_ref()
    }

    pub fn fn_ty(self, params: &[Type], is_var_arg: bool) -> FunctionType {
        self.ty.fn_ty(params, is_var_arg)
    }
}
