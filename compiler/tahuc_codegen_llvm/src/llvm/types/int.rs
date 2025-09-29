use tahuc_llvm::{core::{LLVMConstInt, LLVMGetIntTypeWidth}, opaque::LLVMTypeRef};

use crate::llvm::{FunctionType, IntValue, Type, Values};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IntType {
    ty: Type,
}

impl IntType {
    pub fn new(ty: Type) -> Self {
        Self { ty }
    }

    pub fn as_type_ref(self) -> LLVMTypeRef {
        self.ty.as_type_ref()
    }

    pub fn fn_ty(self, params: &[Type], is_var_arg: bool) -> FunctionType {
        self.ty.fn_ty(params, is_var_arg)
    }

    pub fn const_int(self, value: u64, sign_extend: bool) -> IntValue {
        let value =
            Values::new(unsafe { LLVMConstInt(self.as_type_ref(), value, sign_extend as i32) });
        IntValue::new(value)
    }

    pub fn as_ty(self) -> Type {
        self.ty
    }

    pub fn get_bit_width(self) -> u32 {
        unsafe { LLVMGetIntTypeWidth(self.as_type_ref()) }
    }
}
