mod array;
mod bool;
mod float;
mod fn_ty;
mod int;
mod pointee;
mod struct_ty;
mod void;

pub use array::*;
pub use bool::*;
pub use float::*;
pub use fn_ty::*;
pub use int::*;
pub use pointee::*;
pub use void::*;
pub use struct_ty::*;

use tahuc_llvm::{core::LLVMFunctionType, opaque::LLVMTypeRef};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Type {
    ty: LLVMTypeRef,
}

impl Type {
    pub fn new(ty: LLVMTypeRef) -> Self {
        Self { ty }
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty
    }

    pub fn fn_ty(self, params: &[Type], is_var_arg: bool) -> FunctionType {
        let mut param_ref: Vec<LLVMTypeRef> = params.iter().map(|t| t.as_type_ref()).collect();
        let ty = Type::new(unsafe {
            LLVMFunctionType(
                self.as_type_ref(),
                param_ref.as_mut_ptr(),
                params.len() as std::ffi::c_uint,
                is_var_arg as std::ffi::c_int,
            )
        });

        FunctionType::new(ty)
    }

    pub fn into_int_ty(self) -> IntType {
        IntType::new(self)
    }
}
